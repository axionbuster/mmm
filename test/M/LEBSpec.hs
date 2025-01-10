-- | test suite for LEB128 encoding/decoding
module M.LEBSpec (spec) where

import Data.Binary.Get
import Data.Bits
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.Word
import M.LEB
import Test.Hspec

-- Helper to convert Builder to ByteString
buildertobs :: Builder -> BL.ByteString
buildertobs = BB.toLazyByteString

-- Helper to decode LEB from ByteString
decodelebfrombs :: (FiniteBits a, Num a) => BL.ByteString -> LEB a
decodelebfrombs = runGet (decodeleb getWord8)

spec :: Spec
spec = do
  describe "LEB128" do
    it "encodes 0" do
      buildertobs (encodeleb (LEB (0 :: Word32))) `shouldBe` BL.singleton 0x00

    it "encodes 1" do
      buildertobs (encodeleb (LEB (1 :: Word32))) `shouldBe` BL.singleton 0x01

    it "encodes 127" do
      buildertobs (encodeleb (LEB (127 :: Word32))) `shouldBe` BL.singleton 0x7F

    it "encodes 128" do
      buildertobs (encodeleb (LEB (128 :: Word32))) `shouldBe` BL.pack [0x80, 0x01]

    it "encodes 255" do
      buildertobs (encodeleb (LEB (255 :: Word32))) `shouldBe` BL.pack [0xFF, 0x01]

    it "encodes 2097151" do
      buildertobs (encodeleb (LEB (2_097_151 :: Word32))) `shouldBe` BL.pack [0xFF, 0xFF, 0x7F]

    it "roundtrip test for various values" do
      let testvals = [0, 1, 127, 128, 255, 2_097_151, 214_7483_647] :: [Word32]
          roundtrip = decodelebfrombs . buildertobs . encodeleb . LEB
      map roundtrip testvals `shouldBe` map LEB testvals

    it "decodes example from wiki.vg" do
      -- Example from Minecraft protocol: VarInt 25565 = [0xDD, 0xC7, 0x01]
      decodelebfrombs (BL.pack [0xDD, 0xC7, 0x01]) `shouldBe` LEB (25565 :: Word32)

    it "encodes example from wiki.vg" do
      buildertobs (encodeleb (LEB (25565 :: Word32))) `shouldBe` BL.pack [0xDD, 0xC7, 0x01]

    describe "Int64 stress tests" do
      it "handles extreme values" do
        let minInt = minBound :: Int64
        let maxInt = maxBound :: Int64
        decodelebfrombs (buildertobs (encodeleb (LEB minInt))) `shouldBe` LEB minInt
        decodelebfrombs (buildertobs (encodeleb (LEB maxInt))) `shouldBe` LEB maxInt

      it "handles values around zero" do
        let nearZero = [-10 .. 10] :: [Int64]
        let roundtrip = decodelebfrombs . buildertobs . encodeleb . LEB
        map roundtrip nearZero `shouldBe` map LEB nearZero

      it "handles common Minecraft values" do
        -- Test cases from Minecraft protocol examples
        buildertobs (encodeleb (LEB (-1 :: Int64))) `shouldBe` BL.pack [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01]
        buildertobs (encodeleb (LEB (-2147483648 :: Int64))) `shouldBe` BL.pack [0x80, 0x80, 0x80, 0x80, 0xf8, 0xff, 0xff, 0xff, 0xff, 0x01]

      it "roundtrip test for various Int64 values" do
        let testvals =
              [ -2147483648, -- Minecraft's minimum chunk position
                -1234567,
                -1,
                0,
                1,
                1234567,
                2147483647 -- Minecraft's maximum chunk position
              ] ::
                [Int64]
        let roundtrip = decodelebfrombs . buildertobs . encodeleb . LEB
        map roundtrip testvals `shouldBe` map LEB testvals
