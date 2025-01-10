module M.J.NBT.Internal.JSSpec (spec) where

import Data.ByteString qualified as B
import M.J.NBT.Internal.JS
import Test.Hspec

spec :: Spec
spec = do
  describe "JS (JavaString)" do
    describe "round-trip tests" do
      it "handles empty string" do
        let input = ""
        (cesu8astext . textascesu8) input `shouldBe` Just input

      it "handles ASCII strings" do
        let input = "Hello, World!"
        (cesu8astext . textascesu8) input `shouldBe` Just input

      it "handles null characters" do
        let input = "before\0after"
        (cesu8astext . textascesu8) input `shouldBe` Just input

      it "handles non-ASCII Unicode" do
        let input = "привет こんにちは 안녕하세요"
        (cesu8astext . textascesu8) input `shouldBe` Just input

      it "handles emoji (surrogate pairs)" do
        let input = "🌍🌎🌏"
        (cesu8astext . textascesu8) input `shouldBe` Just input

    describe "specific encoding tests" do
      it "encodes null as C0 80" do
        textascesu8 "\0" `shouldBe` B.pack [0xC0, 0x80]

      it "encodes ASCII one-byte per character" do
        let input = "ABC"
        let expected = B.pack [0x41, 0x42, 0x43]
        textascesu8 input `shouldBe` expected

      it "encodes simple Unicode as expected" do
        -- "д" (CYRILLIC SMALL LETTER DE)
        textascesu8 "д" `shouldBe` B.pack [0xD0, 0xB4]

    describe "specific decoding tests" do
      it "decodes valid CESU-8" do
        -- "Hello" in CESU-8
        let input = B.pack [0x48, 0x65, 0x6C, 0x6C, 0x6F]
        cesu8astext input `shouldBe` Just "Hello"

      it "handles invalid sequences" do
        -- Invalid UTF-8/CESU-8 sequence
        let input = B.pack [0xFF]
        cesu8astext input `shouldBe` Nothing

      it "handles truncated sequences" do
        -- Truncated two-byte sequence
        let input = B.pack [0xC0]
        cesu8astext input `shouldBe` Nothing

        -- Truncated three-byte sequence
        let input2 = B.pack [0xE0, 0x80]
        cesu8astext input2 `shouldBe` Nothing
