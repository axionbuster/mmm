-- | Java's CESU-8 encoding/decoding
module M.J.NBT.Internal.JS
  ( JS (..),
    textascesu8,
    cesu8astext,
    tocesu8,
    fromcesu8,
  )
where

import Control.DeepSeq
import Control.Exception
import Control.Monad.Fix
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder
import Data.Data
import Data.Function
import Data.Functor
import Data.Hashable
import Data.String
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf16LEWith, encodeUtf8)
import Data.Text.Encoding.Error (UnicodeException (DecodeError))
import Data.Word
import FlatParse.Stateful qualified as F
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.Pack
import System.IO.Unsafe
import Text.Printf

-- | Java's CESU-8 encoding/decoding
--
-- this newtype is purely for modulation of encoding and decoding.
-- it is not intended to be used directly in the public API
--
-- use 'textascesu8' and 'cesu8astext' to convert between 'Text'
-- and 'ByteString'
--
-- see:
--
-- * https://en.wikipedia.org/wiki/UTF-8#CESU-8
-- * https://docs.oracle.com/en/java/javase/18/docs/api/java.base/java/io/DataInput.html#modified-utf-8
newtype JS = JS {getjs :: Text}
  deriving stock (Generic, Typeable, Data, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)
  deriving newtype (IsString)

instance Pack JS where
  pack = pack . tocesu8
  {-# INLINE pack #-}

instance Unpack JS where
  unpack = unpackleb32 >>= (`F.isolate` fromcesu8p)
  {-# INLINE unpack #-}

textascesu8 :: Text -> ByteString
textascesu8 = tocesu8 . JS
{-# INLINE textascesu8 #-}

cesu8astext :: ByteString -> Maybe Text
cesu8astext f = getjs <$> fromcesu8 f
{-# INLINE cesu8astext #-}

-- real meat

-- betwixt (between, inclusive)
bxt :: (Ord a) => a -> a -> a -> Bool
bxt x a b = a <= x && x <= b
{-# INLINE bxt #-}

-- | encode text to CESU-8
tocesu8 :: JS -> ByteString
tocesu8 = B.toStrict . toLazyByteString . tocesu8p
{-# INLINEABLE tocesu8 #-}

unp :: JS -> [Word8]
unp = B.unpack . encodeUtf8 . getjs
{-# INLINE unp #-}

-- yes... this code is a bit of a mess
tocesu8p :: JS -> Builder
tocesu8p s0 =
  unp s0 & fix \go -> \case
    [] -> mempty
    0 : xs -> word8 0xC0 <> word8 0x80 <> go xs
    x : xs | bxt x 0x01 0x7F -> word8 x <> go xs
    x : y : xs
      | bxt x 0xC0 0xDF && bxt y 0x80 0xBF ->
          word8 x <> word8 y <> go xs
    x : y : z : xs
      | bxt x 0xE0 0xEF && bxt y 0x80 0xBF && bxt z 0x80 0xBF ->
          word8 x <> word8 y <> word8 z <> go xs
    x : y : z : w : xs
      | bxt x 0xF0 0xF4
          && bxt y 0x80 0xBF
          && bxt z 0x80 0xBF
          && bxt w 0x80 0xBF ->
          -- the 4-byte case is special because CESU-8 encodes
          -- them as two UTF-16 surrogate pairs. this creates
          -- 6 bytes of CESU-8 data
          let fi :: (Integral a, Num b) => a -> b
              fi = fromIntegral
              (h, l) =
                let co =
                      subtract 0x10000 $
                        ((fi x .&. 0x07) .<<. 18)
                          .|. ((fi y .&. 0x3F) .<<. 12)
                          .|. ((fi z .&. 0x3F) .<<. 6)
                          .|. (fi w .&. 0x3F)
                 in (co .>>. 10 + 0xD800, co .&. 0x3FF + 0xDC00)
              w8 = word8 . fi
              e a =
                w8 ((a .>>. 12) .|. (0xE0 :: Word32))
                  <> w8 (((a .>>. 6) .&. 0x3F) .|. 0x80)
                  <> w8 ((a .&. 0x3F) .|. 0x80)
           in e h <> e l <> go xs
    -- only possible if the input is not a valid UTF-8 string
    x : _ -> error $ printf "tocesu8p: unexpected character %02x" x

-- | decode CESU-8 encoded text
fromcesu8 :: ByteString -> Maybe JS
fromcesu8 s = case parsepure fromcesu8p () 0 s of
  F.OK x _ _ -> Just x
  _ -> Nothing

-- my condolences to the reader
fromcesu8p :: Parser st r JS
fromcesu8p =
  -- CESU-8 -> UTF-16 -> Text
  F.many cp <&> de >>= \case
    Left (e :: UnicodeException) -> se $ "fromcesu8p: " <> show e
    Right t -> pure $ JS t
  where
    se = F.err . ParseError
    -- de: decode CESU or error out
    de =
      -- upon an invalid character decodeUtf16LEWith will use the given
      -- function to make a substitution. but we don't want that. we want
      -- to throw an error instead. "text" guidelines say, if that's the case,
      -- use 'throw' or 'error'. and we would like to catch the exception,
      -- so we temporarily enter the IO monad
      unsafeDupablePerformIO
        . try
        . evaluate
        . decodeUtf16LEWith ((throw .) . DecodeError)
        . B.pack
        . concatMap by
        . concat
    -- by: split a 16-bit number into two 8-bit numbers
    by x = [fromIntegral x, fromIntegral (x .>>. 8)]
    {-# INLINE by #-}
    -- an: extract 6-bit continuation from a CESU-8 byte
    -- the symbol "an" comes from "and" (.&.)
    an = unpack @Word8 <&> \x -> fromIntegral x .&. (0x3F :: Word16)
    -- sh: shift and combine
    sh a b c = (shift (fromIntegral a .&. b) c .|.)
    -- pu: pure . fromIntegral
    pu :: (Integral a, Num b, Monad m) => [a] -> m [b]
    pu = pure . fmap fromIntegral
    -- cp: decode a UTF-16 surrogate pair from CESU-8
    cp :: Parser st r [Word16]
    cp =
      unpack @Word8 >>= \case
        x | x == 0xC0 -> do
          unpack @Word8 >>= \case
            0x80 -> pu [0 :: Word16] -- 2 bytes; null
            y -> se $ printf "fromcesu8p: unexpected CESU-8 byte %02x" y
        x | bxt x 0x01 0x7F -> pu . pure $ x -- 1 byte; direct
        x | bxt x 0xC0 0xDF -> an <&> pure . sh x 0x1F 6 -- 2 bytes
        x | x == 0xED -> do
          -- 6 bytes; come in surrogate pairs
          y <- an
          z <- an
          if bxt y 0x20 0x2F
            then do
              x2 <- unpack @Word8
              if x2 == 0xED
                then do
                  y2 <- an
                  z2 <- an
                  if bxt y2 0x30 0x3F
                    then
                      let high = sh x 0x0F 12 (shift y 6 .|. z)
                          low = sh x2 0x0F 12 (shift y2 6 .|. z2)
                       in pu [high, low]
                    else
                      se $
                        printf
                          "fromcesu8p: invalid low surrogate ... \
                          \%02x is not in [0x30, 0x3F]"
                          y2
                else
                  se $
                    printf
                      "fromcesu8p: invalid surrogate pair ... \
                      \expected 0xED, got %02x"
                      x2
            else pu $ pure $ sh x 0x0F 12 (shift y 6 .|. z)
        x -- 3 bytes (exclude 0xED; yeah, spec is weird)
          | bxt x 0xE0 0xEF ->
              an >>= \p ->
                an <&> \q ->
                  pure $ sh x 0x0F 12 (shift p 6 .|. q)
        x | bxt x 0xF0 0xF4 -> do
          -- 6 bytes; come in surrogate pairs
          y <- an
          z <- an
          w <- an
          let val =
                fromIntegral (sh x 0x07 18 (shift y 12 .|. shift z 6 .|. w))
                  + (0x10000 :: Word32)
              h = 0xD800 + (val .>>. 10)
              l = 0xDC00 + (val .&. 0x3FF)
          pu [h, l]
        x -> se $ printf "fromcesu8p: unexpected CESU-8 byte %02x" x
    {-# INLINE cp #-}
