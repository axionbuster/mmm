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
import Text.Printf

-- | unpack a paletted container
-- (Minecraft, Java Edition, padded words).
--
-- see also: 'pkb'.
upb ::
  forall w c.
  (FiniteBits w, Integral w, FiniteBits c, Integral c) =>
  -- | bits per entry
  Int ->
  -- | words; least significant word first.
  [w] ->
  -- | chars; least significant char first.
  [c]
upb b = e
  where
    -- e: entry point / error guard.
    e
      | wsz < csz =
          error $
            printf
              "incorrect bit combination in 'upb': wsz = %d, csz = %d"
              wsz
              csz
      | 1 <= b && b <= csz = f c 0
      | otherwise = error $ "incorrect bits per entry in 'upb': b = " ++ show b
    -- f: extract bits from a word64, skip to next word64 when it should.
    -- case 1: no current word, no more words. halt.
    -- case 2: not enough bits in word; next word, reset state.
    -- case 3: extract bits.
    f _ _ [] = []
    f n _ (_ : ws) | n < b = f c 0 ws
    f n s (w : ws)
      | Just d <- g w s = fi d : f (n - b) (s + b) (w : ws)
      | otherwise = f (n - b) 0 ws
    -- g: extract a single value from a word w at shift position s.
    -- case 1: not enough bits.
    -- case 2: ok, extract bits.
    g w s
      | s + b > wsz = Nothing
      | otherwise = Just $ (w .&. shift m s) .>>. s
    -- c: how many complete bit groups fit in a word.
    c = b * div wsz b
    -- m: mask; 0...01...1 <- 'b' count of 1's.
    m = shift 1 b - 1
    wsz = finiteBitSize (undefined :: w)
    csz = finiteBitSize (undefined :: c)
    -- fi: take least significant bits from a word.
    fi = fromIntegral @w @c
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
pkb ::
  forall w c.
  (FiniteBits w, Integral w, FiniteBits c, Integral c) =>
  -- | bits per entry
  Int ->
  -- | chars; least significant char first.
  [c] ->
  -- | words; least significant word first.
  [w]
pkb b = e
  where
    -- e: entry point / error guard.
    e os
      | wsz < csz =
          error $
            printf
              "incorrect bit combination in 'pkb': wsz = %d, csz = %d"
              wsz
              csz
      | 1 <= b && b <= csz = f 0 0 os
      | otherwise = error $ "incorrect bits per entry in 'pkb': b = " ++ show b
    -- f: pack char into current word or else skip to next word.
    -- case 1: there are no chars left. produce word and quit.
    -- case 2: put char if possible; reset & produce word if not.
    f _ !w [] = [w]
    f s !w (c : cs)
      | s + b > wsz = w : f 0 0 (c : cs)
      | otherwise = f (s + b) (w .|. shift (fi c) s) cs
    wsz = finiteBitSize (undefined :: w)
    csz = finiteBitSize (undefined :: c)
    fi = fromIntegral @c @w
{-# SPECIALIZE pkb :: Int -> [Word8] -> [Int64] #-}
{-# SPECIALIZE pkb :: Int -> [Word8] -> [Word64] #-}
