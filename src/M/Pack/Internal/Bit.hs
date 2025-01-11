{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for encoding product types consisting of boolean flags
-- into compact bit representations.
--
-- = Usage
--
-- To use this module, define a record type with boolean fields and derive Generic:
--
-- @
-- data Flags = Flags
--   { isEnabled :: Bool
--   , isVisible :: Bool
--   , isLocked  :: Bool
--   } deriving Generic
--
-- instance 'Bitreppable' 'Word8' Flags
-- @
--
-- Then you can encode/decode using the Bitwise wrapper:
--
-- @
-- let flags = Bitwise (Flags True False True)
-- let packed = 'pack' flags   -- Packs into a single byte
-- let unpacked = 'unpack' packed  -- Recovers the original flags
-- @
--
-- The bits are assigned from least significant to most significant based on field order.
-- In the example above:
--
-- * isEnabled = 'bit' 0
-- * isVisible = 'bit' 1
-- * isLocked = 'bit' 2
--
-- The module provides two types of bit sets:
--
-- 1. Variable-length 'Bitset':
--
-- @
-- let bs = Bitset 5  -- Sets bits 0 and 2 (binary 101)
-- testBit bs 0  -- True
-- testBit bs 1  -- False
-- testBit bs 2  -- True
-- @
--
-- 2. Fixed-length 'FixedBitset':
--
-- @
-- let fbs = FixedBitset \@8 5  -- 8-bit bitset with bits 0 and 2 set
-- -- When packed/unpacked, always uses exactly 8 bits,
-- -- padding with zeros or truncating as needed
-- @
--
-- Both types of bitsets can be packed/unpacked for network transmission.
-- Variable-length bitsets are stored as little-endian vectors of 'Int64's,
-- while fixed-length bitsets are padded or truncated to their specified size.
module M.Pack.Internal.Bit
  ( Bitwise (..),
    Bitreppable (..),
    Bitset (..),
    FixedBitset (..),
  )
where

import Control.DeepSeq
import Data.Bits
import Data.ByteString.Builder (Builder)
import Data.Data
import Data.Hashable
import Data.Int
import Data.Vector.Unboxed qualified as VU
import GHC.Generics
import GHC.TypeLits
import Language.Haskell.TH.Syntax (Lift)
import M.Pack.Internal.Etc ()
import M.Pack.Internal.Types

-- | a wrapper type that enables bit-level packing of boolean product types.
-- the type parameter @i@ specifies the underlying integral type used to store
-- the bits (e.g. Word8, Word16, etc). the type parameter @a@ is the product
-- type containing the boolean fields to be encoded.
newtype Bitwise i a = Bitwise {unbitwise :: a}
  deriving stock (Generic, Typeable, Data, Functor, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)

instance (Bitreppable i a) => Pack (Bitwise i a) where
  pack = tobits0 @i . unbitwise
  {-# INLINEABLE pack #-}

instance (Bitreppable i a) => Unpack (Bitwise i a) where
  unpack = Bitwise <$> frombits0 @i
  {-# INLINEABLE unpack #-}

-- | type class for types that can be represented as bit flags.
-- provides methods for converting to and from bit representations.
--
-- most users should not need to implement this directly - just derive 'Generic'
-- for your type and declare an instance without implementations:
--
-- @
-- instance Bitreppable Word8 MyFlags
-- @
class Bitreppable i a where
  tobits0 :: a -> Builder
  default tobits0 ::
    (Generic a, GBitRep (Rep a), Integral i, Bits i, Pack i) =>
    a -> Builder
  tobits0 = pack . fst . tobits (0 :: i) 0 . from
  {-# INLINE tobits0 #-}
  frombits0 :: Parser st r a
  default frombits0 ::
    forall r st.
    (Generic a, GBitRep (Rep a), Integral i, Bits i, Unpack i) =>
    Parser st r a
  frombits0 = to . fst . flip frombits 0 <$> unpack @i
  {-# INLINE frombits0 #-}

-- internal class for encoding/decoding bit flags
class GBitRep f where
  tobits ::
    (Bits i, Integral i) =>
    i -> -- accumulator
    Int -> -- current bit position
    f p ->
    (i, Int) -- new accumulator, new position
  frombits ::
    (Bits i, Integral i) =>
    -- accumulator
    i ->
    -- current bit position
    Int ->
    -- representation, new position
    (f p, Int)

instance GBitRep V1 where
  tobits _ _ _ = (0, 0)
  {-# INLINE tobits #-}
  frombits _ p = (undefined, p)
  {-# INLINE frombits #-}

instance GBitRep U1 where
  tobits i p _ = (i, p)
  {-# INLINE tobits #-}
  frombits _ p = (U1, p)
  {-# INLINE frombits #-}

instance GBitRep (K1 R Bool) where
  tobits i p (K1 True) = (setBit i p, p + 1)
  tobits i p (K1 False) = (clearBit i p, p + 1)
  {-# INLINE tobits #-}
  frombits i p = (K1 (testBit i p), p + 1)
  {-# INLINE frombits #-}

instance (GBitRep f) => GBitRep (M1 i c f) where
  tobits i p (M1 x) = tobits i p x
  frombits i p = case frombits i p of
    (x, p') -> (M1 x, p')

instance (GBitRep f, GBitRep g) => GBitRep (f :*: g) where
  tobits i p (x :*: y) =
    let (i', p') = tobits i p x
     in tobits i' p' y
  frombits i p = case frombits i p of
    (x, p') -> case frombits i p' of
      (y, p'') -> (x :*: y, p'')

-- | variable-length bitset
--
-- (network representation: little-endian vector of 'Int64's)
newtype Bitset = Bitset {getbitset :: Integer}
  deriving stock (Generic, Typeable, Data, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)
  deriving newtype (Enum, Num, Real, Bits)

instance Pack Bitset where
  pack = pack . bitsettovui64
  {-# INLINE pack #-}

instance Unpack Bitset where
  unpack = bitsetfromvui64 <$> unpack
  {-# INLINE unpack #-}

-- sort of a naive implementation, but it's fine for our purposes
--
-- actually returns the highest bit location + 1
highestbitloc :: Integer -> Int
highestbitloc x | x < 0 = error "highestbitloc: negative argument"
highestbitloc 0 = 0
highestbitloc x = 1 + highestbitloc (x .>>. 1)

bitsettovuin :: Int -> Bitset -> VU.Vector Int8
bitsettovuin n (Bitset x) = VU.fromListN (m + 1) (go x)
  where
    m = (highestbitloc x + n - 1) `div` n
    go 0 = []
    go y = fromIntegral y : go (y .>>. n)
{-# INLINE bitsettovuin #-}

bitsetfromvuin :: Int -> VU.Vector Int8 -> Bitset
bitsetfromvuin n = Bitset . go 0 0
  where
    go a s v =
      case VU.uncons v of
        Nothing -> a
        Just (x, xs) -> go (a .|. (fromIntegral x .<<. s)) (s + n) xs

bitsettovui64 :: Bitset -> VU.Vector Int8
bitsettovui64 = bitsettovuin 64

bitsetfromvui64 :: VU.Vector Int8 -> Bitset
bitsetfromvui64 = bitsetfromvuin 64

bitsettovui8 :: Bitset -> VU.Vector Int8
bitsettovui8 = bitsettovuin 8

bitsetfromvui8 :: VU.Vector Int8 -> Bitset
bitsetfromvui8 = bitsetfromvuin 8

-- | a fixed-size bitset with @i@ bits
--
-- (implemented identically to 'Bitset'; only difference is that
-- when ser/de occurs, it pads missing bits with zeroes. hence it is
-- also possible to access out-of-bounds bits, and these bits will
-- get silently truncated when ser/de occurs)
newtype FixedBitset i = FixedBitset {getfixedbitset :: Integer}
  deriving stock (Generic, Typeable, Data, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)
  deriving newtype (Enum, Num, Real, Bits)

-- if too few bytes, pad with zeroes; if too many, truncate
trim :: Int -> VU.Vector Int8 -> VU.Vector Int8
trim n v
  | VU.length v < n = v <> VU.replicate (n - VU.length v) 0
  | otherwise = VU.take n v

instance (KnownNat i) => Pack (FixedBitset i) where
  pack =
    pack
      . trim (fromIntegral (natVal (Proxy @i) + 7 `div` 8) + 1)
      . bitsettovui8
      . Bitset
      . getfixedbitset
  {-# INLINE pack #-}

instance (KnownNat i) => Unpack (FixedBitset i) where
  unpack =
    FixedBitset
      . getbitset
      . bitsetfromvui8
      . trim (fromIntegral (natVal (Proxy @i) + 7 `div` 8) + 1)
      <$> unpack
  {-# INLINE unpack #-}
