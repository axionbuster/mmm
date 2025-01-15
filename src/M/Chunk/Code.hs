-- |
-- Module: M.Chunk.Code
-- Description: Encode and decode paletted containers.
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Encode and decode paletted containers for block states and biomes.
module M.Chunk.Code (upb, pkb, MkEncoder (..), mkencoder) where

import Data.Bits
import Data.ByteString.Builder
import Data.Foldable
import Data.Int
import Data.IntMap.Strict qualified as M
import Data.Vector.Unboxed qualified as V
import Data.Word
import M.Pack
import Text.Printf

-- | Configuration for the encoder
data MkEncoder = MkEncoder
  { -- | Minimum palette size (if using palette)
    indirlowlim :: !Int,
    -- | Maximum palette size (if using palette)
    indirupplim :: !Int,
    -- | Bits per entry for direct encoding
    directbpe :: !Int
  }

-- | Encode values using either direct, indirect (palette), or single value encoding
mkencoder ::
  forall a.
  (Integral a, FiniteBits a, V.Unbox a) =>
  -- | Encoder configuration
  MkEncoder ->
  -- | Function to directly encode individual values. Expected to be
  -- 'word64BE', but works with other choices.
  (a -> Builder) ->
  -- | Input values to encode
  V.Vector a ->
  -- | Encoded data
  Builder
mkencoder MkEncoder {..} directpack = choose1
  where
    -- Main strategy selector based on input characteristics
    choose1 vs
      -- Safety checks
      | finiteBitSize (undefined :: a) > finiteBitSize (undefined :: Int) =
          error "mkencoder/choose1: bit size"
      | indirlowlim <= 0 = error "mkencoder/choose1: indirlowlim"
      | indirupplim <= 0 || indirupplim < indirlowlim =
          error "mkencoder/choose1: indirupplim"
      | directbpe <= 0 = "mkencoder/choose1: directbpe"
      | V.null vs = error "mkencoder/choose1: empty"
      -- Single value case - when vector has only one value
      | Just (v, w) <- V.uncons vs, V.null w = single v
      -- Single value case - when all values are identical
      | Just (_, m, _) <- computepalette vs, M.size m == 1 = single (V.head vs)
      -- Try palette encoding if possible
      | Just p <- computepalette vs = indirect p vs
      -- Fallback to direct encoding
      | otherwise = direct vs

    -- Encode a single value: [0: bpe][value][0: # longs to follow = none]
    single v = word8 0 <> packleb32 v <> word8 0

    -- Palette-based encoding
    indirect (palsiz, pal, pallis) vs =
      let lg n -- Calculate bits needed to represent n values
            | n <= 1 = 0
            | otherwise = finiteBitSize n - countLeadingZeros (pred n)
          bpe = lg palsiz -- Bits per entry
          lut = map ((pal M.!) . fromIntegral) -- Convert values to pal. indices
          wor = pkb bpe $ lut $ V.toList vs -- Pack indices into words
       in packleb32 bpe -- Format: [bpe][palette size]
            <> packleb32 palsiz -- [palette entries...]
            <> pallis -- [data length][packed data]
            <> packleb32 (length wor)
            <> foldMap' word64BE wor

    -- Direct encoding without palette
    direct vs =
      packleb32 directbpe -- Format: [bpe][length]
        <> packleb32 (V.length vs) -- [raw values...]
        <> V.foldMap' directpack vs

    -- Try to create an efficient palette
    computepalette vs =
      let (m', l') = V.foldl' f (M.empty, "") vs
          f (m, l) (fromIntegral -> v) -- Build palette map and entry list
            | v `M.member` m = (m, l) -- Skip if value already in palette
            | otherwise = (M.insert v (M.size m) m, l <> packleb32 v)
       in if
            | M.size m' < 2 -> Nothing -- Too few unique values
            | M.size m' < shift 1 indirlowlim -> -- Pad to minimum size
                let re = foldMap' word8 do
                      take (indirlowlim - M.size m') (repeat 0)
                 in Just (indirlowlim, m', l' <> re)
            | M.size m' > shift 1 indirupplim -> Nothing -- Too many uniques
            | otherwise -> Just (M.size m', m', l') -- Just right

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
