{-# OPTIONS_GHC -Wno-orphans #-}

-- | implement 'Pack' and 'Unpack' for number types and 'Bool'
module M.Pack.Internal.Num
  ( packleb32,
    unpackleb32,
  )
where

import Data.Bits
import Data.ByteString.Builder
import Data.Coerce
import Data.Int
import Data.Word
import FlatParse.Stateful qualified as F
import GHC.Float
import M.LEB
import M.Pack.Internal.FromIntegral
import M.Pack.Internal.Types

instance Pack Int8 where
  pack = int8
  {-# INLINE pack #-}

instance Unpack Int8 where
  unpack = F.anyInt8
  {-# INLINE unpack #-}

instance Pack Word8 where
  pack = word8
  {-# INLINE pack #-}

instance Unpack Word8 where
  unpack = F.anyWord8
  {-# INLINE unpack #-}

instance Pack Int16 where
  pack = int16BE
  {-# INLINE pack #-}

instance Unpack Int16 where
  unpack = F.anyInt16be
  {-# INLINE unpack #-}

instance Pack Word16 where
  pack = word16BE
  {-# INLINE pack #-}

instance Unpack Word16 where
  unpack = F.anyWord16be
  {-# INLINE unpack #-}

instance Pack Int32 where
  pack = int32BE
  {-# INLINE pack #-}

instance Unpack Int32 where
  unpack = F.anyInt32be
  {-# INLINE unpack #-}

instance Pack Word32 where
  pack = word32BE
  {-# INLINE pack #-}

instance Unpack Word32 where
  unpack = F.anyWord32be
  {-# INLINE unpack #-}

instance Pack Int64 where
  pack = int64BE
  {-# INLINE pack #-}

instance Unpack Int64 where
  unpack = F.anyInt64be
  {-# INLINE unpack #-}

instance Pack Word64 where
  pack = word64BE
  {-# INLINE pack #-}

instance Unpack Word64 where
  unpack = F.anyWord64be
  {-# INLINE unpack #-}

instance Pack Float where
  pack = floatBE
  {-# INLINE pack #-}

instance Unpack Float where
  unpack = castWord32ToFloat <$> F.anyWord32be
  {-# INLINE unpack #-}

instance Pack Double where
  pack = doubleBE
  {-# INLINE pack #-}

instance Unpack Double where
  unpack = castWord64ToDouble <$> F.anyWord64be
  {-# INLINE unpack #-}

instance Pack Bool where
  pack = word8 . fi . fromEnum
  {-# INLINEABLE pack #-}

instance Unpack Bool where
  unpack =
    F.anyWord8 >>= \case
      0 -> pure False
      1 -> pure True
      n -> F.err $ coerce $ "illegal Bool representation: " <> show n
  {-# INLINEABLE unpack #-}

instance (FiniteBits a, Integral a) => Pack (LEB a) where
  pack = encodeleb
  {-# INLINE pack #-}

instance (FiniteBits a, Num a) => Unpack (LEB a) where
  unpack = decodeleb F.anyWord8
  {-# INLINE unpack #-}

-- pack as leb encoded integers

-- | pack as an LEB128-encoded 'Int32'
packleb32 :: (Integral a) => a -> Builder
packleb32 = pack @(LEB Int32) . fromIntegral
{-# INLINE packleb32 #-}

-- | unpack from an LEB128-encoded 'Int32'
unpackleb32 :: (Integral a) => Parser st r a
unpackleb32 = fromIntegral <$> unpack @(LEB Int32)
{-# INLINE unpackleb32 #-}
