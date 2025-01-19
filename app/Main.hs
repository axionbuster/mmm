module Main (main) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.Int
import Data.Semigroup
import Data.Vector.Unboxed qualified as VU
import Data.Word
import M.Chunk.Code
import M.Chunk.Net
import M.NBT
import M.Pack
import M.V769.P qualified as P

reifybuilder :: Builder -> ByteString
reifybuilder = B.toStrict . BB.toLazyByteString

basiccolumn :: Builder
basiccolumn =
  let (enc, _) = mkcscodec (ChunkSectionEncoding (shift 1 15) (shift 1 6))
      cempty = ChunkSection 0 (VU.replicate 4096 0) (VU.replicate 64 0)
      cstone = ChunkSection 4096 (VU.replicate 4096 1) (VU.replicate 64 0)
      eempty = enc (cempty :: ChunkSection Word16 Word64)
      estone = enc (cstone :: ChunkSection Word16 Word64)
   in stimes (7 :: Int) eempty <> estone <> stimes (16 :: Int) eempty

chunk0 :: Int32 -> Int32 -> Builder
chunk0 x y =
  let height =
        Compound
          [ ("MOTION_BLOCKING", LongArray []),
            ("WORLD_SURFACE", LongArray [])
          ]
      packet = P.ChunkDataAndUpdateLight x y cdata ldata
      cdata = ChunkData height (reifybuilder basiccolumn) []
      bszero = Bitset 0
      ldata = LightData bszero bszero bszero bszero [] []
   in pack packet

main :: IO ()
main = print (chunk0 0 0)
