-- |
-- Module: M.LEB
-- Description: LEB128 encoding support for integers
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Implements LEB128 (Little-Endian Base 128) variable-length encoding
-- for arbitrary finite-bit integers, used in the Minecraft protocol.
module M.LEB (LEB (..), VarInt, VarLong, decodeleb, encodeleb) where

import Control.DeepSeq
import Control.Monad.Fix
import Data.Bits
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.Data
import Data.Function
import Data.Hashable
import Data.Int
import Data.Word
import GHC.Generics hiding (S)
import Language.Haskell.TH.Syntax (Lift)

-- | a LEB128 (unsigned) encoded integer. the integer itself may or
-- may not be signed
newtype LEB a = LEB {getleb :: a}
  deriving newtype
    ( Show,
      Read,
      Eq,
      Ord,
      Enum,
      Bounded,
      Num,
      Real,
      Integral,
      Hashable
    )
  deriving stock (Generic, Typeable, Data, Functor, Lift)
  deriving anyclass (NFData)

-- | VarInt
type VarInt = LEB Int32

-- | VarLong
type VarLong = LEB Int64

-- internal state
data S a = S !Int !a -- shift, accumulator

-- | decode an unsigned LEB128 encoded integer
--
-- the actual integer may or may not be signed; the encoding itself is
-- \"unsigned\"
decodeleb ::
  forall m a.
  (Monad m, FiniteBits a, Num a) =>
  -- | accept any 'Word8' value
  m Word8 ->
  -- | LEB128 encoded integer
  m (LEB a)
decodeleb word8 =
  S 0 0 & fix \f -> \case
    S s n | s >= finiteBitSize (undefined :: a) -> pure (LEB n)
    S s n -> do
      c <- word8
      let d = n .|. shift (clearBit (fromIntegral c) 7) s
      if testBit c 7
        then f (S (s + 7) d)
        else pure (LEB d)
{-# INLINEABLE decodeleb #-}

-- | encode an LEB128 encoded integer into a 'Builder'
encodeleb :: (FiniteBits a, Integral a) => LEB a -> Builder
encodeleb (LEB n) =
  let m = complement (0x7f `rotate` (-7))
      l = clearBit (fromIntegral n) 7
      r = shift n (-7) .&. m -- force unsigned shift
      (o, next)
        | n .&. complement 0x7f /= 0 = (setBit l 7, encodeleb (LEB r))
        | otherwise = (l, mempty)
   in BB.word8 o <> next
{-# INLINEABLE encodeleb #-}
{-# SPECIALIZE encodeleb :: LEB Int32 -> Builder #-}
{-# SPECIALIZE encodeleb :: LEB Int64 -> Builder #-}
