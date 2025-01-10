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
module M.Pack.Internal.Bit (Bitwise (..), Bitreppable (..)) where

import Control.DeepSeq
import Data.Bits
import Data.ByteString.Builder (Builder)
import Data.Data
import Data.Hashable
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
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
  {-# INLINE tobits #-}
  frombits i p = case frombits i p of
    (x, p') -> (M1 x, p')
  {-# INLINE frombits #-}

instance (GBitRep f, GBitRep g) => GBitRep (f :*: g) where
  tobits i p (x :*: y) =
    let (i', p') = tobits i p x
     in tobits i' p' y
  {-# INLINE tobits #-}
  frombits i p = case frombits i p of
    (x, p') -> case frombits i p' of
      (y, p'') -> (x :*: y, p'')
  {-# INLINE frombits #-}
