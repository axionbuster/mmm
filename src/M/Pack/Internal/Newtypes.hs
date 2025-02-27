-- |
-- Module: M.Pack.Internal.Newtypes
-- Description: Special-purpose serialization wrappers
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Defines newtype wrappers that control how values are packed and unpacked,
-- including enum indices, fixed-point numbers, angles, and identifiers.
module M.Pack.Internal.Newtypes
  ( EnumIndex (..),
    Fixed' (..),
    Int8Angle (..),
    Identifier (..),
    IDorX (..),
    IDSet (..),
    TakeRest (..),
    PackFoldableVI (..),
    PackFoldable0 (..),
    UnpackRepresentable0 (..),
    degtoi8angle,
    i8angledeg,
  )
where

import Control.Applicative.Combinators
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BB
import Data.Char
import Data.Coerce
import Data.Data
import Data.Fixed
import Data.Foldable
import Data.Foldable1
import Data.Functor.Classes
import Data.Functor.Rep
import Data.Hashable
import Data.Int
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import FlatParse.Stateful qualified as F
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.LEB
import M.Pack.Internal.Etc ()
import M.Pack.Internal.FromIntegral
import M.Pack.Internal.Num
import M.Pack.Internal.Types

-- | represent any 'Enum' type using a zero-based index
newtype EnumIndex i a = EnumIndex {enumindex :: a}
  deriving stock (Generic, Typeable, Data, Functor, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)
  deriving newtype (Enum, Bounded)

instance (Enum a, Integral i, Pack i) => Pack (EnumIndex i a) where
  pack = pack @i . fi . fromEnum . enumindex
  {-# INLINEABLE pack #-}

instance
  (Enum a, Bounded a, Integral i, Unpack i) =>
  Unpack (EnumIndex i a)
  where
  unpack = do
    let bx x = fromEnum (minBound @a) <= x && x <= fromEnum (maxBound @a)
    fi <$> unpack @i >>= \case
      n | bx n -> pure (EnumIndex (toEnum n))
      n -> F.err $ coerce $ "EnumIndex: out of bounds: " <> show n

-- | use an integer type @i@ for serialization of a fixed-point number
-- with resolution @r@ (see 'HasResolution', 'Fixed'), with underlying
-- numeric representation @f@
newtype Fixed' i r f = Fixed' {unfixed' :: f}
  deriving stock (Generic, Typeable, Data, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)
  deriving newtype (Enum, Num, Real, Fractional, RealFrac)

unfix :: Fixed r -> Integer
unfix (MkFixed x) = x
{-# INLINE unfix #-}

instance
  (Integral i, Pack i, Real f, HasResolution r) =>
  Pack (Fixed' i r f)
  where
  pack =
    pack @i
      . fi
      . (unfix :: Fixed r -> Integer)
      . realToFrac
      . unfixed'
  {-# INLINEABLE pack #-}

instance
  (Integral i, Unpack i, Fractional f, HasResolution r) =>
  Unpack (Fixed' i r f)
  where
  unpack =
    Fixed'
      . (realToFrac :: Fixed r -> f)
      . MkFixed
      . fi
      <$> unpack @i
  {-# INLINEABLE unpack #-}

-- | a signed angle; divides the circle into 256 parts
newtype Int8Angle = Int8Angle {int8angle :: Int8}
  deriving stock (Generic, Typeable, Data, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)
  deriving newtype (Enum, Num, Real, Integral, Pack, Unpack)

-- | convert degrees to 'Int8Angle'
degtoi8angle :: (RealFrac a) => a -> Int8Angle
degtoi8angle x = Int8Angle (round ((x * (256 / 360)) `mod'` 256))
{-# INLINEABLE degtoi8angle #-}

-- | convert 'Int8Angle' to degrees
i8angledeg :: (RealFrac a) => Int8Angle -> a
i8angledeg (Int8Angle x) = fromIntegral x * (360 / 256)
{-# INLINEABLE i8angledeg #-}

-- | an \"identifier\" string, used for names, tags, etc.
--
-- example: @minecraft:stone@ or @stone/stone@
--
-- validation only happens when unpacking ('unpack')
newtype Identifier = Identifier {identifier :: Text}
  deriving stock
    ( Generic,
      Typeable,
      Data,
      Lift
    )
  deriving anyclass (NFData)
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Read,
      Hashable,
      Semigroup,
      Monoid,
      IsString
    )

instance Unpack Identifier where
  unpack =
    unpackleb32
      >>= guardnat "Identifier.unpack length"
      >>= (`F.isolate` unpackid)
  {-# INLINE unpack #-}

instance Pack Identifier where
  pack = pack . identifier
  {-# INLINE pack #-}

unpackid :: Parser st r Identifier
unpackid =
  -- ':' may happen at most once
  -- if never occurs, then whole thing is the 'value' part of the ID
  -- if occurs, the earlier part is the namespace, the later part is the value
  -- the namespace may not be empty
  -- the namespace consists of [a-z0-9.-_]
  -- the value consists of [a-z0-9._-/] -- note that '/' is allowed anywhere
  -- the value may not be empty
  let name = F.satisfyAscii
        \c ->
          isAsciiLower c
            || isDigit c
            || c `elem` (".-_" :: String)
      value = F.satisfyAscii
        \c ->
          isAsciiLower c
            || isDigit c
            || c `elem` ("._-/" :: String)
      colon = F.satisfyAscii (== ':')
      cvt = Identifier . T.pack
      cmb (ns, v) w = cvt $ mconcat [ns, [v], w]
   in fmap cvt (some value <* F.eof)
        <|> liftA2 cmb (manyTill_ name colon) (some value <* F.eof)
        <|> F.err "unpack Identifier: invalid or empty identifier"

-- | unresolved value; either an ID or an inline value
newtype IDorX a = IDorX
  { -- | VarInt ID (NOT +1) or inline value
    idorvalue :: Either Int32 a
  }
  deriving stock (Generic, Typeable, Data, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData, Functor)
  deriving newtype (Applicative, Monad, Foldable, Semigroup)

instance (Pack a) => Pack (IDorX a) where
  pack = \case
    IDorX (Left regid) -> packleb32 (regid + 1) -- Registry ID + 1
    IDorX (Right val) -> pack (0 :: LEB Int32) <> pack val -- Inline value

instance (Unpack a) => Unpack (IDorX a) where
  unpack = do
    i <- unpackleb32 >>= guardnat "IDorX.n"
    if i == 0
      then IDorX . Right <$> unpack
      else pure $ IDorX $ Left (i - 1)

-- | potentially unresolved ID set; either an identifier for its location or
-- an inline set of IDs
newtype IDSet = IDSet
  { -- | name of ID set or inline set
    setnameorids :: Either Text (V.Vector Int32)
  }
  deriving stock (Generic, Typeable, Data)
  deriving newtype (Eq, Ord, Show, Read, NFData, Semigroup)

instance Pack IDSet where
  pack (IDSet (Left setname)) = "\0" <> pack setname
  pack (IDSet (Right ids)) = packleb32 (V.length ids + 1) <> V.foldMap' pack ids

instance Unpack IDSet where
  unpack =
    unpackleb32 >>= guardnat "IDSet.n" >>= \case
      0 -> IDSet . Left . identifier <$> unpack
      n -> IDSet . Right <$> V.replicateM (n - 1) unpackleb32

-- | a newtype wrapper over 'ByteString'; not length-prefixed
newtype TakeRest = TakeRest {gettakerest :: ByteString}
  deriving stock (Generic, Typeable, Data, Lift)
  deriving newtype (Show, Read, Eq, Ord, Hashable, NFData, Semigroup, Monoid)

-- | 'TakeRest' is serialized as-is
instance Pack TakeRest where
  pack = BB.byteString . gettakerest
  {-# INLINE pack #-}

-- | 'TakeRest' is deserialized as-is and reads the rest of the input
instance Unpack TakeRest where
  unpack = TakeRest <$> F.takeRest
  {-# INLINE unpack #-}

-- | General 'Pack' instance provider for 'Foldable's
--
-- length-prefixed ('VarInt'), strict left fold
newtype PackFoldableVI f a = PackFoldableVI {getpackfoldablevi :: f a}
  deriving stock (Generic, Generic1, Typeable, Data, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData, Functor)
  deriving newtype (Eq1, Ord1, Show1, Read1, Semigroup, Monoid)
  deriving newtype (Foldable, Foldable1, Applicative, Monad)

instance (Pack a, Foldable f) => Pack (PackFoldableVI f a) where
  pack =
    packleb32 . length . getpackfoldablevi
      <> foldMap' pack . getpackfoldablevi
  {-# INLINEABLE pack #-}

-- | General 'Pack' instance provider for 'Foldable's
--
-- no length prefix, strict left fold
newtype PackFoldable0 f a = PackFoldable0 {getpackfoldable0 :: f a}
  deriving stock (Generic, Generic1, Typeable, Data, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData, Functor)
  deriving newtype (Eq1, Ord1, Show1, Read1, Semigroup, Monoid)
  deriving newtype (Foldable, Foldable1, Applicative, Monad)

-- | @'foldMap' 'pack'@
instance (Pack a, Foldable f) => Pack (PackFoldable0 f a) where
  pack = foldMap' pack . getpackfoldable0
  {-# INLINEABLE pack #-}

-- | General 'Unpack' instance provider for 'Representable's that are also
-- 'Traversable'
newtype UnpackRepresentable0 f a = UnpackRepresentable0
  {getunpackrepresentable0 :: f a}
  deriving stock (Generic, Generic1, Typeable, Data, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData, Functor)
  deriving newtype (Eq1, Ord1, Show1, Read1, Semigroup, Monoid)
  deriving newtype (Foldable, Foldable1, Applicative, Monad)

-- | @'sequenceA' ('tabulate' ('const' 'unpack'))@
instance
  (Unpack a, Representable f, Traversable f) =>
  Unpack (UnpackRepresentable0 f a)
  where
  unpack = UnpackRepresentable0 <$> sequenceA (tabulate (const unpack))
  {-# INLINEABLE unpack #-}
