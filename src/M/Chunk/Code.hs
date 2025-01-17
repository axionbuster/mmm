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
import Data.Data
import Data.Foldable
import Data.Int
import Data.IntMap.Strict qualified as M
import Data.Vector.Unboxed qualified as V
import Data.Word
import FlatParse.Stateful qualified as F
import GHC.Generics hiding (S)
import M.Pack
import Text.Printf
import Prelude hiding (words)

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
  deriving (Eq, Ord, Generic, Typeable, Data)

-- | uses paletted view with hard-coded settings to compress what's been shown
instance
  ( Show c,
    Integral c,
    FiniteBits c,
    V.Unbox c,
    Show m,
    Integral m,
    FiniteBits m,
    V.Unbox m
  ) =>
  Show (ChunkSection c m)
  where
  show ChunkSection {..} =
    printf
      "ChunkSection {csnonempty = %d, csblockstates = (length %d numbers; \
      \paletted view) %s, csbiomes = (length %d numbers; \
      \paletted view) %s}"
      csnonempty
      -- often reasonable encoding settings, since protocol default;
      -- but could be surprising if using expanded number of block states
      -- and/or biomes.
      (V.length csblockstates)
      (show $ mkencoder (MkCodec 4 8 15 4096) csblockstates)
      (V.length csbiomes)
      (show $ mkencoder (MkCodec 1 3 6 64) csbiomes)

-- | encoding configuration for @ChunkSection@
data ChunkSectionEncoding = ChunkSectionEncoding
  { -- | number of possible block states
    cseblockstates :: !Int,
    -- | number of possible biomes
    csebiomes :: !Int
  }
  deriving (Show, Read, Eq, Ord, Generic, Data, Typeable)

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
      bscodec = MkCodec 4 8 (lg2 cse.cseblockstates) 4096
      bmcodec = MkCodec 1 3 (lg2 cse.csebiomes) 64
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
lg2 :: (FiniteBits a, Ord a, Num a, Num b) => a -> b
lg2 n
  | n <= 1 = 0
  | otherwise = fromIntegral $ finiteBitSize n - countLeadingZeros (n - 1)

-- | Configuration for encoding and decoding
data MkCodec = MkCodec
  { -- | Minimum palette size (if using palette)
    lowlim :: !Word8,
    -- | Maximum palette size (if using palette)
    upplim :: !Word8,
    -- | Bits per entry for direct encoding
    directbpe :: !Word8,
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
          wor = pkbv bpe $ V.map lut vs -- Pack indices into words
          chk
            | bpe > 8 = error $ printf "mkencoder/indirect: bpe (%d) > 8" bpe
            | otherwise = fromIntegral bpe
       in word8 chk -- Format: [bpe][palette size]
            <> packleb32 palsiz -- [palette entries...]
            <> pallis -- [data length][packed data]
            <> packleb32 (V.length wor)
            <> V.foldMap' word64BE wor

    -- Direct encoding without palette
    direct vs =
      let p = pkbv (fromIntegral directbpe) vs
       in word8 directbpe -- Format: [bpe][length]
            <> packleb32 (V.length p) -- [raw values...]
            <> V.foldMap' (pack @Word64) p

    -- Try to create an efficient palette
    computepalette vs =
      let (m', l') = V.foldl' f (M.empty, "") vs
          f (m, l) (fromIntegral -> v) -- Build palette map and entry list
            | v `M.member` m = (m, l) -- Skip if value already in palette
            | otherwise = (M.insert v (M.size m) m, l <> packleb32 v)
          lowlim' = fromIntegral lowlim
          upplim' = fromIntegral upplim
       in if
            | M.size m' < 2 -> Just (M.size m', m', l') -- single-value mode
            | M.size m' < shift 1 lowlim' -> -- Pad to minimum size
                let re = foldMap' word8 do
                      take (lowlim' - M.size m') (repeat 0)
                 in Just (lowlim', m', l' <> re)
            | M.size m' > shift 1 upplim' -> Nothing -- Too many uniques
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
      bpe <- unpack @Word8
      if
        -- Select encoding format based on bpe value
        | bpe == 0 -> single -- Single value encoding
        | bpe > upplim -> direct -- Direct encoding
        | otherwise -> paletted bpe -- Palette encoding

    -- Single value format: [0][value][0] -> replicate value n times
    single = do
      value <- unpackleb32 -- Read the single value
      F.cut (F.word8 0) "mkdecoder/single: data array length is not zero"
      pure $ V.replicate singlecount value

    -- Palette encoding: [bpe][palsize][pal...][count][packed...]
    paletted (fromIntegral . max lowlim -> bpe) = do
      pln <- unpackleb32 >>= guardnat "mkdecoder/paletted: palette length"
      pal <- V.replicateM pln unpackleb32 -- Read palette entries
      longs <- unpackleb32 >>= guardnat "mkdecoder/paletted: # of longs"
      -- Read packed words, unpack bits, map through palette
      V.map (pal V.!) . upbv bpe <$> V.replicateM longs (unpack @Word64)

    -- Direct encoding format: [bpe][count][value1][value2]...
    direct = do
      nlongs <- unpackleb32 >>= checklongs -- Read # of 64-bit words
      upbv (fromIntegral directbpe) <$> V.replicateM nlongs (unpack @Word64)
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
-- see also: 'pkbv'.
upbv ::
  forall w c.
  ( FiniteBits w,
    Integral w,
    V.Unbox w,
    FiniteBits c,
    Integral c,
    V.Unbox c
  ) =>
  Int -> V.Vector w -> V.Vector c
upbv b = e
  where
    e
      | wsz < csz = error "upbv: incorrect bit combination"
      | b < 1 || csz < b = error "upbv: incorrect bits per entry"
      | otherwise = \words ->
          let m = unsafeShiftL 1 b - 1
              len = V.length words * cpw
              cpw = wsz `div` b
              fi = fromIntegral @w @c
           in V.generate len \i ->
                let w = i `div` cpw
                    c = b * (i `rem` cpw)
                 in fi $ ((words V.! w) `unsafeShiftR` c) .&. m
    wsz = finiteBitSize (undefined :: w)
    csz = finiteBitSize (undefined :: c)

data S a = S !Int !a -- shift, accumulator

uns :: S a -> a
uns (S _ a) = a

-- | pack bits into a paletted container (list of words).
--
-- see also: 'upb'.
pkbv ::
  forall w c.
  (FiniteBits w, Integral w, V.Unbox w, FiniteBits c, Integral c, V.Unbox c) =>
  -- | bits per entry
  Int ->
  -- | chars; least significant char first.
  V.Vector c ->
  -- | words; least significant word first.
  V.Vector w
pkbv b = e
  where
    e
      | wsz < csz = error "pkbv: incorrect bit combination"
      | b < 1 || csz < b = error "pkbv: incorrect bits per entry"
      | otherwise = \chars ->
          let cpw = wsz `div` b
              w = (V.length chars + cpw - 1) `div` cpw
              f (S s a) (fi -> q) = S (s + b) (a .|. unsafeShiftL q s)
           in V.generate w \i -> uns do
                let j = i * cpw
                    l = min cpw (V.length chars - i * cpw)
                 in V.foldl' f (S 0 0) $ V.slice j l chars
    wsz = finiteBitSize (undefined :: w)
    csz = finiteBitSize (undefined :: c)
    fi = fromIntegral @c @w
