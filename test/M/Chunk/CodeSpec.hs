module M.Chunk.CodeSpec (spec) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder
import Data.Int
import Data.Vector.Unboxed qualified as V
import FlatParse.Stateful qualified as F
import M.Chunk.Code
import M.Pack
import Test.Hspec

(encode, decode) = mkcscodec (ChunkSectionEncoding (shift 1 15) (shift 1 6))

reify :: Builder -> ByteString
reify = B.toStrict . toLazyByteString

spec :: Spec
spec = do
  describe "encode" do
    it "encodes empty chunk section correctly" do
      reify
        ( encode
            ( ChunkSection
                0
                (V.replicate 4096 (0 :: Int16))
                (V.replicate 64 (0 :: Int8))
            )
        )
        `shouldBe` B.replicate (2 + 3 + 3) 0
  describe "decode" do
    it "decodes empty chunk section correctly" do
      let F.OK x _ _ = parsepure0 decode (B.replicate (2 + 3 + 3) 0)
       in x `shouldBe` (ChunkSection 0 (V.replicate 4096 0) (V.replicate 64 0))
