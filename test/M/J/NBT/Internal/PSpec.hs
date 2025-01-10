module M.J.NBT.Internal.PSpec (spec) where

import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.HashMap.Strict qualified as M
import Data.Int
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import FlatParse.Stateful qualified as F
import M.J.NBT.Internal.P ()
import M.J.NBT.Internal.Types
import M.Pack
import Test.Hspec

spec :: Spec
spec = do
  describe "NBT format" do
    describe "round-trip tests" do
      it "tests example 1" do
        let input = Compound $ M.fromList [("name", String "Bananrama")]
         in packbytes input `shouldBe` (B.toStrict . BB.toLazyByteString) do
              BB.byteString [0x0A, 0x08, 0x00, 0x04]
                <> BB.string7 "name"
                <> BB.byteString [0x00, 0x09]
                <> BB.string7 "Bananrama"
                <> BB.byteString [0x00]

      it "handles basic types" do
        let input =
              Compound $
                M.fromList
                  [ ("byte", Byte 42),
                    ("short", Short 12345),
                    ("int", Int 987654),
                    ("long", Long 123456789),
                    ("float", Float 3.14),
                    ("double", Double 2.71828),
                    ("string", String "Hello, NBT!")
                  ]
        roundTrip input `shouldBe` Just input

      it "handles arrays" do
        let input =
              Compound $
                M.fromList
                  [ ( "byteArray",
                      ByteArray $ B.pack [1, 2, 3, 4, 5]
                    ),
                    ( "intArray",
                      IntArray $ VU.fromList [10, 20, 30, 40, 50 :: Int32]
                    ),
                    ( "longArray",
                      LongArray $ VU.fromList [100, 200, 300 :: Int64]
                    )
                  ]
        roundTrip input `shouldBe` Just input

      it "handles nested compounds" do
        let input =
              Compound $
                M.fromList
                  [ ( "nested",
                      Compound $
                        M.fromList
                          [ ("value", Int 42),
                            ( "deeper",
                              Compound $
                                M.fromList [("found", String "treasure")]
                            )
                          ]
                    )
                  ]
        roundTrip input `shouldBe` Just input

      it "handles lists" do
        let input =
              Compound $
                M.fromList
                  [ ( "stringList",
                      List TString $
                        V.fromList
                          [ String "one",
                            String "two",
                            String "three"
                          ]
                    ),
                    ( "numberList",
                      List TInt $
                        V.fromList
                          [ Int 1,
                            Int 2,
                            Int 3
                          ]
                    )
                  ]
        roundTrip input `shouldBe` Just input

      it "handles empty compounds" do
        let input = Compound M.empty
        roundTrip input `shouldBe` Just input

      it "handles compound with empty string keys" do
        let input =
              Compound $
                M.fromList [("", String "empty key")]
        roundTrip input `shouldBe` Just input

      it "handles special characters in strings" do
        let input =
              Compound $
                M.fromList
                  [ ( "specialChars",
                      String "Hello\0World\nNewline\tTab"
                    ),
                    ( "unicodeChars",
                      String "Hello 世界"
                    )
                  ]
        roundTrip input `shouldBe` Just input

    describe "error cases" do
      it "rejects negative lengths" do
        -- ByteArray with length -1
        let byteArrayInput = B.pack [0x07, 0xFF, 0xFF, 0xFF, 0xFF]
        unpackbytes byteArrayInput `shouldBe` Nothing

        -- String with length -1
        let stringInput = B.pack [0x08, 0xFF, 0xFF]
        unpackbytes stringInput `shouldBe` Nothing

        -- List with length -1
        let listInput = B.pack [0x09, 0x01, 0xFF, 0xFF, 0xFF, 0xFF]
        unpackbytes listInput `shouldBe` Nothing

        -- IntArray with length -1
        let intArrayInput = B.pack [0x0B, 0xFF, 0xFF, 0xFF, 0xFF]
        unpackbytes intArrayInput `shouldBe` Nothing

        -- LongArray with length -1
        let longArrayInput = B.pack [0x0C, 0xFF, 0xFF, 0xFF, 0xFF]
        unpackbytes longArrayInput `shouldBe` Nothing

      it "rejects invalid tag types" do
        -- End tag is not allowed in lists
        let input = List TEnd $ V.fromList [End]
        roundTrip input `shouldBe` Nothing

      it "rejects heterogeneous lists" do
        let input =
              List TInt $
                V.fromList [Int 1, String "invalid"]
        roundTrip input `shouldBe` Nothing

-- Helper function to test round-trip conversion
roundTrip :: Tg -> Maybe Tg
roundTrip tag = unpackbytes (packbytes tag)

packbytes :: Tg -> B.ByteString
packbytes = B.toStrict . BB.toLazyByteString . pack

unpackbytes :: B.ByteString -> Maybe Tg
unpackbytes b = case parsepure0 (unpack <* F.eof) b of
  F.OK x _ _ -> Just x
  _ -> Nothing
