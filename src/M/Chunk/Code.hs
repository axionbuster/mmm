-- |
-- Module: M.Chunk.Code
-- Description: Encode and decode paletted containers.
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Encode and decode paletted containers. Contents and purpose of
-- the module will change soon.
module M.Chunk.Code (upb, pkb) where

import Data.Bits
import Data.Int
import Data.Word

-- | unpack a paletted container
-- (Minecraft, Java Edition, padded longs).
--
-- see also: 'pkb'.
--
-- @1 <= b <= 8@ must hold.
upb ::
  forall a.
  (FiniteBits a, Integral a) =>
  -- | bits per entry
  Int ->
  -- | longs; least significant word first.
  [a] ->
  -- | octets; least significant octet first.
  [Word8]
upb b = e
  where
    -- f: extract bits from a word64, skip to next word64 when it should.
    -- case 1: no current word, no more words. halt.
    -- case 2: not enough bits in word; next word, reset state.
    -- case 3: extract bits.
    e
      | 1 <= b && b <= 8 = f c 0
      | otherwise = error $ "incorrect bits per entry in 'upb': b = " ++ show b
    f _ _ [] = []
    f n _ (_ : ws) | n < b = f c 0 ws
    f n s (w : ws)
      | Just d <- g w s = fi d : f (n - b) (s + b) (w : ws)
      | otherwise = f (n - b) 0 ws
    -- g: extract a single value from a word w at shift position s.
    -- case 1: not enough bits.
    -- case 2: ok, extract bits.
    g w s
      | s + b > bsz = Nothing
      | otherwise = Just $ (w .&. shift m s) .>>. s
    -- c: how many complete bit groups fit in a word.
    c = b * div bsz b
    -- m: mask; 0...01...1 <- 'b' count of 1's.
    m = shift 1 b - 1
    -- bsz: bit size. expected to be 64.
    bsz = finiteBitSize (undefined :: a)
    -- fi: take 8 least significant bits from a word.
    fi = fromIntegral @a @Word8
{-# SPECIALIZE upb :: Int -> [Int64] -> [Word8] #-}
{-# SPECIALIZE upb :: Int -> [Word64] -> [Word8] #-}

-- | pack bits into a paletted container (list of words).
--
-- see also: 'upb'.
--
-- note that 'pkb' is lazy on the list stem, so
--
-- @'take' 10 ('pkb' 5 ('repeat' 1)) :: ['Word64']@
--
-- will converge and return
--
-- @[0x0084210842108421 .. (10 times)]@
--
-- while being strict on the values to prevent thunk buildup.
--
-- @1 <= b <= 8@ must hold.
pkb ::
  forall a.
  (FiniteBits a, Integral a) =>
  -- | bits per entry
  Int ->
  -- | octets; least significant octet first.
  [Word8] ->
  -- | longs; least significant long first.
  [a]
pkb b = c
  where
    -- c (check): very special initial condition check.
    -- empty list input is degenerate anyway, but, if
    -- you do this, you can have the identity upb b (pkb b xs) == xs
    -- for all inputs, including [].
    c [] = [] -- prevents returning [0] when [] was given as input.
    c os =
      if 1 <= b && b <= 8
        then f 0 0 os -- entry point.
        else error $ "incorrect bits per entry in 'pkb': b = " ++ show b
    -- f: pack octet into current word or else skip to next word.
    -- case 1: there are no octets left. produce word and quit.
    -- case 2: put octet if possible; reset & produce word if not.
    f _ !w [] = [w]
    f s !w (o : os)
      | s + b > bsz = w : f 0 0 (o : os)
      | otherwise = f (s + b) (w .|. shift (fi o) s) os
    bsz = finiteBitSize (undefined :: a)
    fi = fromIntegral @Word8 @a
{-# SPECIALIZE pkb :: Int -> [Word8] -> [Int64] #-}
{-# SPECIALIZE pkb :: Int -> [Word8] -> [Word64] #-}
