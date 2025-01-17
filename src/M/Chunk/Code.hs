-- |
-- Module: M.Chunk.Code
-- Description: Encode and decode paletted containers.
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Encode and decode paletted containers for block states and biomes.
module M.Chunk.Code
  ( ChunkSection (..),
    ChunkSectionEncoding (..),
    mkcscodec,
  )
where

import Control.Monad
import Data.Bits
import Data.ByteString.Builder
import Data.Foldable
import Data.Int
import Data.IntMap.Strict qualified as M
import Data.Vector.Unboxed qualified as V
import Data.Word
import FlatParse.Stateful qualified as F
import GHC.Generics
import M.Pack
import Text.Printf

-- | a chunk section where @c@ is the numeric type for block states and
-- @m@ is the same for biomes
data ChunkSection c m = ChunkSection
  { -- | number of non-air blocks (tracked for optimization)
    csnonempty :: !Int16,
    -- | block states (4,096 entries; 16x16x16, access @[y][z][x]@)
    csblockstates :: !(V.Vector c),
    -- | biomes (64 entries; 4x4x4, access @[y][z][x]@)
    csbiomes :: !(V.Vector m)
  }
  deriving (Eq, Ord, Generic)

-- | encoding configuration for @ChunkSection@
data ChunkSectionEncoding = ChunkSectionEncoding
  { -- | number of possible block states
    cseblockstates :: !Int,
    -- | number of possible biomes
    csebiomes :: !Int
  }

-- | create a codec for @ChunkSection@s using the provided settings
mkcscodec ::
  (V.Unbox m, V.Unbox c, FiniteBits m, FiniteBits c, Integral m, Integral c) =>
  -- | encoding settings
  ChunkSectionEncoding ->
  -- | a pair of an encoder and a decoder, respectively
  (ChunkSection c m -> Builder, Parser st r (ChunkSection c m))
mkcscodec cse =
  -- the [4, 8] and [1, 3] ranges have been hardcoded in the protocol spec
  -- for some time
  let -- configure codecs with protocol-specified ranges
      bscodec = MkCodec 4 8 (lg2 cse.cseblockstates) cse.cseblockstates
      bmcodec = MkCodec 1 3 (lg2 cse.csebiomes) cse.csebiomes
      -- create encoder/decoder pairs for blocks and biomes
      (bsencode, bmencode) = (mkencoder bscodec, mkencoder bmcodec)
      (bsdecode, bmdecode) = (mkdecoder bscodec, mkdecoder bmcodec)
      -- format: [blockcount][blockstates][biomes]
      encode cs =
        int16BE cs.csnonempty
          <> bsencode cs.csblockstates
          <> bmencode cs.csbiomes
      decode = do
        blockcount <- F.anyInt16be >>= checkbc
        ChunkSection blockcount <$> bsdecode <*> bmdecode
        where
          -- verify block count is within Minecraft's limits
          checkbc n
            | n < 0 = F.err "mkcscodec/decode: negative non-air block count"
            | n > 4096 -- max blocks in 16x16x16 section
              =
                F.err $ ParseError do
                  "mkcscodec/decode: non-air block count too many: " ++ show n
            | otherwise = pure n
   in (encode, decode)

-- Calculate bits needed to represent n values
lg2 :: (FiniteBits a, Ord a, Num a) => a -> Int
lg2 n
  | n <= 1 = 0
  | otherwise = finiteBitSize n - countLeadingZeros (n - 1)

-- | Configuration for encoding and decoding
data MkCodec = MkCodec
  { -- | Minimum palette size (if using palette)
    lowlim :: !Int,
    -- | Maximum palette size (if using palette)
    upplim :: !Int,
    -- | Bits per entry for direct encoding
    directbpe :: !Int,
    -- | Count for single value encoding
    singlecount :: !Int
  }

-- | Encode values using either direct, indirect (palette), or single value encoding
--
-- == Usage
--
-- Plug in the first argument ('MkCodec' configuration) and store the
-- closure in a variable. This closure is the actual encoder function.
-- Then, use the closure to encode values.
mkencoder ::
  forall a.
  (Integral a, FiniteBits a, V.Unbox a) =>
  -- | Encoder configuration
  MkCodec ->
  -- | Input values to encode
  V.Vector a ->
  -- | Encoded data
  Builder
mkencoder MkCodec {..} = choose1
  where
    -- Main strategy selector based on input characteristics
    choose1 vs
      -- Safety checks
      | finiteBitSize (undefined :: a) > finiteBitSize (undefined :: Int) =
          error "mkencoder/choose1: bit size"
      | lowlim <= 0 = error "mkencoder/choose1: lowlim"
      | upplim <= 0 || upplim < lowlim =
          error "mkencoder/choose1: upplim"
      | directbpe <= 0 = error "mkencoder/choose1: directbpe"
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
      let bpe = lg2 palsiz -- Bits per entry
          lut = (pal M.!) . fromIntegral -- Convert values to pal. indices
          wor = pkb bpe $ V.toList $ V.map lut vs -- Pack indices into words
       in packleb32 bpe -- Format: [bpe][palette size]
            <> packleb32 palsiz -- [palette entries...]
            <> pallis -- [data length][packed data]
            <> packleb32 (length wor)
            <> foldMap' word64BE wor

    -- Direct encoding without palette
    direct vs =
      let p = V.fromList $ pkb directbpe $ V.toList vs
       in packleb32 directbpe -- Format: [bpe][length]
            <> packleb32 (V.length p) -- [raw values...]
            <> V.foldMap' (pack @Word64) p

    -- Try to create an efficient palette
    computepalette vs =
      let (m', l') = V.foldl' f (M.empty, "") vs
          f (m, l) (fromIntegral -> v) -- Build palette map and entry list
            | v `M.member` m = (m, l) -- Skip if value already in palette
            | otherwise = (M.insert v (M.size m) m, l <> packleb32 v)
       in if
            | M.size m' < 2 -> Just (M.size m', m', l') -- single-value mode
            | M.size m' < shift 1 lowlim -> -- Pad to minimum size
                let re = foldMap' word8 do
                      take (lowlim - M.size m') (repeat 0)
                 in Just (lowlim, m', l' <> re)
            | M.size m' > shift 1 upplim -> Nothing -- Too many uniques
            | otherwise -> Just (M.size m', m', l') -- Just right

-- | Decode values from a paletted container
--
-- == Usage
--
-- Plug in the first argument ('MkCodec' configuration) and store the
-- closure in a variable. This closure is the actual decoder function.
-- Then, use the closure to decode values.
mkdecoder ::
  (Integral a, FiniteBits a, V.Unbox a) =>
  MkCodec ->
  Parser st r (V.Vector a)
mkdecoder MkCodec {..} = choose1
  where
    -- Main decoder selection based on bits-per-entry (bpe)
    choose1 = do
      bpe <- unpackleb32 >>= guardnat "mkdecoder/choose1: bits per entry"
      if
        -- Select encoding format based on bpe value
        | bpe == 0 -> single -- Single value encoding
        | bpe > upplim -> direct -- Direct encoding
        | otherwise -> paletted bpe -- Palette encoding

    -- Single value format: [0][value][0] -> replicate value n times
    single = do
      value <- unpackleb32 -- Read the single value
      F.word8 0 -- Skip trailing zero
      pure $ V.replicate singlecount value

    -- Palette encoding: [bpe][palsize][pal...][count][packed...]
    paletted (max lowlim -> bpe) = do
      pln <- unpackleb32 >>= guardnat "mkdecoder/paletted: palette length"
      pal <- V.replicateM pln unpackleb32 -- Read palette entries
      longs <- unpackleb32 >>= guardnat "mkdecoder/paletted: # of longs"
      -- Read packed words, unpack bits, map through palette
      V.fromList . map (pal V.!) . upb bpe <$> replicateM longs (unpack @Word64)

    -- Direct encoding format: [bpe][count][value1][value2]...
    direct = do
      nlongs <- unpackleb32 >>= checklongs -- Read # of 64-bit words
      V.fromList . upb directbpe <$> replicateM nlongs (unpack @Word64)
      where
        -- Safety check for number of words
        checklongs n
          | n < 0 = F.err "mkdecoder/direct: negative longs"
          | n < 2048 = pure n -- Arbitrary size limit
          | otherwise = F.err $ ParseError do
              "mkdecoder/direct: too many longs (" ++ show n ++ ")"

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
