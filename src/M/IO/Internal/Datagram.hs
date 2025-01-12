-- | Datagram and uninterpreted packet parsing and building.
module M.IO.Internal.Datagram
  ( Uninterpreted (..),
    CompressionOn (..),
    EOF (..),
    makepacketstreami,
    makepacketstreamo,
  )
where

import Codec.Compression.Zlib
import Control.Applicative.Combinators
import Control.DeepSeq
import Control.Exception hiding (throw)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Builder.Extra (flush)
import Data.ByteString.Lazy qualified as BL
import Data.Data
import Data.IORef
import Data.Word
import FlatParse.Stateful
import GHC.Generics
import GHC.Stack
import Language.Haskell.TH.Syntax (Lift)
import M.IO.Internal.Read
import M.IO.Internal.Zlib
import M.Pack hiding (Parser)
import System.IO.Streams hiding (compress)
import Prelude hiding (read)

-- | uninterpreted packet
data Uninterpreted = Uninterpreted
  { pkcode :: !Word8,
    pkdata :: !ByteString
  }

-- | end of input
data EOF = EOF deriving (Show, Typeable, Exception)

-- | compression flag
data CompressionOn
  = -- | threshold, inclusive
    CompressionOn !Int
  | CompressionOff
  deriving (Eq, Ord, Show, Generic, Data, Typeable, NFData, Lift)

-- | take a packet without interpretation
takepacket :: CompressionOn -> InputStream ByteString -> IO Uninterpreted
takepacket c s = do
  t <- parseio0 @ParseError s checkedlength
  u <- readExactly t s
  let p
        | CompressionOn _ <- c = parsepostcomp
        | otherwise = parseprecomp
  runParserIO p () 0 u >>= \case
    OK a _ _ -> pure a
    Fail -> error "takepacket: parse failed"
    Err e -> throwIO e
  where
    checkedlength = do
      l <- unpackleb32 @Int
      if l < 0 || l >= 0x400000
        then throw $ "takepacket: invalid length: " <> show l <> " bytes"
        else pure l

-- | make a stream of uninterpreted packets
makepacketstreami ::
  -- | compression flag reference
  IORef CompressionOn ->
  -- | input stream
  InputStream ByteString ->
  -- | stream of uninterpreted packets
  IO (InputStream Uninterpreted)
makepacketstreami c s =
  makeInputStream do
    c' <- readIORef c
    Just <$> takepacket c' s

-- | parse a pre-compression packet
parseprecomp :: ParserIO r e Uninterpreted
parseprecomp = liftA2 Uninterpreted anyWord8 takeRest
{-# INLINE parseprecomp #-}

-- | parse a post-compression packet
parsepostcomp :: ParserIO r ParseError Uninterpreted
parsepostcomp = choice @[] [parser2, parser3, throw "parsepostcomp: no match"]
{-# INLINE parsepostcomp #-}

-- | post-compression packet, uncompressed
parser2 :: ParserIO r e Uninterpreted
parser2 = word8 0 *> parseprecomp
{-# INLINE parser2 #-}

throw :: String -> ParserIO r ParseError a
throw = err . ParseError

-- | post-compression packet, compressed
parser3 :: (HasCallStack) => ParserIO r ParseError Uninterpreted
parser3 = do
  -- uncompressed length, declared
  l <- unpackleb32 @Int >>= checkuncomplen
  d <- takeRest >>= liftIO . safedecomp l
  liftIO (runParserIO parseprecomp () 0 d) >>= \case
    OK a _ b | B.null b -> pure a
    OK _ _ b ->
      throw $
        "parser3: trailing data: "
          ++ show (B.length b)
          ++ " bytes left"
    Fail -> throw "parser3: parseprecomp failed"
    Err _ -> error "parser3: impossible"
  where
    checkuncomplen l
      | l < 0 = throw "parser3: negative length"
      | l > 0x7FFFFF = throw "parser3: length too large"
      | otherwise = pure l

-- encoding

data Building = B !Int !Builder -- size, builder

instance Semigroup Building where
  B a b <> B c d = B (a + c) (b <> d)
  {-# INLINE (<>) #-}

instance Monoid Building where
  mempty = B 0 mempty
  {-# INLINE mempty #-}

mkb :: Builder -> Building
mkb b = B (fromIntegral $ BL.length $ BB.toLazyByteString b) b
{-# INLINE mkb #-}

encodeplain :: Uninterpreted -> Building
encodeplain (Uninterpreted c d) = mkb (BB.word8 c) <> mkb (BB.byteString d)
{-# INLINE encodeplain #-}

encodeu :: Uninterpreted -> Building
encodeu u = mkb (BB.word8 0) <> encodeplain u
{-# INLINE encodeu #-}

encodez :: CompressionOn -> Uninterpreted -> Building
encodez CompressionOff u = encodeplain u
encodez (CompressionOn t) u@(Uninterpreted c d)
  | B.length d < t = encodeu u
  | otherwise =
      mkb (packleb32 (B.length d))
        <> mkb (BB.word8 c)
        <> mkb (BB.lazyByteString (compress $ B.fromStrict d))
{-# INLINE encodez #-}

-- encode a packet
encode :: CompressionOn -> Uninterpreted -> Builder
encode c u =
  let B n b = encodez c u
   in packleb32 n <> b
{-# INLINE encode #-}

-- | make an output stream of uninterpreted packets
makepacketstreamo ::
  IORef CompressionOn ->
  OutputStream Builder ->
  IO (OutputStream Uninterpreted)
makepacketstreamo c s =
  makeOutputStream \case
    Nothing -> throwIO EOF
    Just u -> do
      c' <- readIORef c
      write (Just $ encode c' u <> flush) s
