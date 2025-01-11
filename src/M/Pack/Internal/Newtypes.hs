-- | Define newtypes that control the way values are packed and unpacked
module M.Pack.Internal.Newtypes
  ( EnumIndex (..),
    Fixed' (..),
    Int8Angle (..),
    Identifier (..),
    IDorX (..),
    IDSet (..),
    degtoi8angle,
    i8angledeg,
  )
where

import Control.Applicative.Combinators
import Control.DeepSeq
import Data.Coerce
import Data.Data
import Data.Fixed
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
  deriving newtype (Enum, Num, Real, Integral)

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
-- example: `minecraft:stone` or `stone/stone`
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

ck :: (Integral a, Show a) => String -> a -> Parser st r a
ck ctx n
  | n < 0 = F.err $ ParseError $ ctx <> ": negative length " <> show n
  | otherwise = pure n
{-# INLINE ck #-}

instance Unpack Identifier where
  unpack = unpackleb32 >>= ck "unpack Identifier" >>= (`F.isolate` unpackid)
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
          'a' <= c && c <= 'z'
            || '0' <= c && c <= '9'
            || c `elem` (".-_" :: String)
      value = F.satisfyAscii
        \c ->
          'a' <= c && c <= 'z'
            || '0' <= c && c <= '9'
            || c `elem` ("._-/" :: String)
      colon = F.satisfyAscii (== ':')
      cvt = Identifier . T.pack
      cmb (ns, v) w = cvt $ mconcat [ns, [v], w]
   in fmap cvt (some value <* F.eof)
        <|> liftA2 cmb (manyTill_ name colon) (some value <* F.eof)
        <|> F.err "unpack Identifier: invalid or empty identifier"

-- | unresolved value; either an ID or an inline value
newtype IDorX a = IDorX
  { -- | VarInt ID + 1 if registry lookup, or inline value
    idorvalue :: Either Int32 a
  }
  deriving stock (Generic, Typeable, Data, Functor, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)

instance (Pack a) => Pack (IDorX a) where
  pack = \case
    IDorX (Left regid) -> packleb32 (regid + 1) -- Registry ID + 1
    IDorX (Right val) -> pack (0 :: LEB (Int32)) <> pack val -- Inline value

instance (Unpack a) => Unpack (IDorX a) where
  unpack = do
    i <- unpackleb32
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
  deriving newtype (Eq, Ord, Show, Read, NFData)

instance Pack IDSet where
  pack (IDSet (Left setname)) = "\0" <> pack setname
  pack (IDSet (Right ids)) = packleb32 (V.length ids + 1) <> V.foldMap' pack ids

instance Unpack IDSet where
  unpack =
    unpackleb32 >>= \case
      0 -> IDSet . Left . identifier <$> unpack
      n -> IDSet . Right <$> V.replicateM (n - 1) unpackleb32
