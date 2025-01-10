-- | Define newtypes that control the way values are packed and unpacked
module M.Pack.Internal.Newtypes
  ( EnumIndex (..),
    Fixed' (..),
    Int8Angle (..),
    degtoi8angle,
    i8angledeg,
  )
where

import Control.DeepSeq
import Data.Coerce
import Data.Data
import Data.Fixed
import Data.Hashable
import Data.Int
import FlatParse.Stateful qualified as F
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.Pack.Internal.FromIntegral
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

-- | represent a 'Fixed' value but fix an integer type for serialization
-- ('Integer' is still used for the actual value)
newtype Fixed' i r = Fixed' {unfixed' :: Fixed r}
  deriving stock (Generic, Typeable, Data, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)
  deriving newtype (Enum, Num, Real, Fractional, RealFrac)

unfix :: Fixed r -> Integer
unfix (MkFixed x) = x
{-# INLINE unfix #-}

instance (Integral i, Pack i) => Pack (Fixed' i r) where
  pack = pack @i . fi . unfix . unfixed'
  {-# INLINEABLE pack #-}

instance (Integral i, Unpack i) => Unpack (Fixed' i r) where
  unpack = Fixed' . MkFixed . fi <$> unpack @i
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
