module M.IO.Internal.ReadSpec (spec) where

import Control.Exception
import FlatParse.Stateful
import M.IO.Internal.Read
import M.Pack
import System.IO.Streams qualified as Streams
import Test.Hspec

spec :: Spec
spec = do
  describe "parseio0" do
    it "copes with empty input" do
      input <- Streams.fromList []
      result <- parseio0 @IOException input (pure ())
      result `shouldBe` ()

    it "copes with empty chunks" do
      input <- Streams.fromList ["", "\x80", "", "", "\x01"]
      result <- parseio0 input (unpackleb32 @Int)
      result `shouldBe` 128

    it "can parse a VarInt from a single chunk" do
      input <- Streams.fromList ["\x2a"] -- 42 encoded as VarInt
      result <- parseio0 input (unpackleb32 @Int)
      result `shouldBe` 42

    it "can parse a VarInt split across chunks" do
      input <- Streams.fromList ["\x80", "\x01"] -- 128 encoded as VarInt
      result <- parseio0 input (unpackleb32 @Int)
      result `shouldBe` 128

    it "throws error on unexpected end of input" do
      input <- Streams.fromList ["\x80"] -- incomplete VarInt
      parseio0 input (unpackleb32 @Int) `shouldThrow` anyIOException

    it "can parse multiple VarInts" do
      input <- Streams.fromList ["\x01\x02\x03"] -- three single-byte VarInts
      nums <- sequence $ replicate 3 $ parseio0 input (unpackleb32 @Int)
      nums `shouldBe` [1, 2, 3]

    it "throws custom errors" do
      let customErr = "custom error" :: ParseError
      input <- Streams.fromList ["\xff"]
      let parser = anyWord8 *> err customErr
      parseio0 input parser `shouldThrow` (== customErr)

    it "handles zero correctly" do
      input <- Streams.fromList ["\x00"]
      result <- parseio0 input (unpackleb32 @Int)
      result `shouldBe` 0

    it "can parse large VarInts" do
      -- 624485 encoded as VarInt
      input <- Streams.fromList ["\xe5\x8e\x26"]
      result <- parseio0 input (unpackleb32 @Int)
      result `shouldBe` 624485
