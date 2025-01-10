module M.BitSpec (spec) where

import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as BB
import Data.Foldable
import Data.Word
import FlatParse.Stateful qualified as F
import GHC.Generics
import M.Pack
import Test.Hspec

data TwoFlags = TwoFlags
  { flag1 :: Bool,
    flag2 :: Bool
  }
  deriving (Generic, Eq, Show)

instance Bitreppable Word8 TwoFlags

data FourFlags = FourFlags
  { flag1' :: Bool,
    flag2' :: Bool,
    flag3' :: Bool,
    flag4' :: Bool
  }
  deriving (Generic, Eq, Show)

instance Bitreppable Word8 FourFlags

pack' :: (Bitreppable Word8 a) => Bitwise Word8 a -> B.ByteString
pack' = B.toStrict . BB.toLazyByteString . pack

unpack' :: (Bitreppable Word8 a) => B.ByteString -> Result (Bitwise Word8 a)
unpack' = parsepure unpack (0 :: Int) 0 -- state can be anything

shouldbeokwith :: (Eq a, Show a) => Result a -> a -> Expectation
shouldbeokwith (F.OK a _ _) b | a == b = pure ()
shouldbeokwith a b = expectationFailure $ show a <> " /= " <> show b

spec :: Spec
spec = do
  describe "Bitwise (Word8)" do
    it "encodes and decodes TwoFlags" do
      let flags = Bitwise @Word8 (TwoFlags True False)
      unpack' (pack' flags) `shouldbeokwith` flags

    it "encodes all combinations of two flags as expected" do
      -- as a reminder, the first field is the least significant bit
      let combinations0 =
            [ Bitwise @Word8 (TwoFlags a b)
            | a <- [False, True],
              b <- [False, True]
            ]
          combinations1 = [a + 2 * b | a <- [0, 1], b <- [0, 1]]
       in for_ (zip combinations0 combinations1) \(flags, expected) ->
            (flags, pack' flags) `shouldBe` (flags, B.singleton expected)

    it "handles all combinations of two flags" do
      let combinations =
            [ Bitwise @Word8 (TwoFlags a b)
            | a <- [False, True],
              b <- [False, True]
            ]
      for_
        combinations
        \flags ->
          unpack' (pack' flags)
            `shouldbeokwith` flags

    it "encodes and decodes FourFlags" do
      let flags = Bitwise @Word8 (FourFlags True False True False)
      unpack' (pack' flags) `shouldbeokwith` flags

    it "maintains bit positions correctly" do
      let input = Bitwise @Word8 (FourFlags True False True True)
          packed = pack' input
      unpack' packed `shouldbeokwith` input
