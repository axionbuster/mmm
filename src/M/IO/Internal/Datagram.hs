-- | Datagram and uninterpreted packet parsing and building.
module M.IO.Internal.Datagram
  ( -- * Types
    Uninterpreted (..),
    CompressionOn (..),
    EOF (..),

    -- * Streams
    makepacketstreami,
    makepacketstreamo,
    makedecrypting,
    makeencrypting,
  )
where

import Codec.Compression.Zlib
import Control.DeepSeq
import Control.Exception hiding (throw)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Data
import Data.IORef
import Data.Word
import FlatParse.Stateful
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.IO.Internal.Read
import M.IO.Internal.Zlib
import M.Pack hiding (Parser)
import System.IO.Streams hiding (compress)
import Text.Printf
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
  where
    takepacket d b = do
      t <- parseio0 @ParseError b checkedlength
      u <- readExactly t b
      let p
            | CompressionOn _ <- d = parsepostcomp
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
    throw = err . ParseError
    parseprecomp = liftA2 Uninterpreted anyWord8 takeRest
    parsepostcomp = parser2 <|> parser3 <|> throw "parsepostcomp: no match"
    parser2 = word8 0 *> parseprecomp
    parser3 = do
      -- uncompressed length, declared
      l <- unpackleb32 @Int >>= checkuncomplen
      d <- takeRest >>= liftIO . safedecomp l
      liftIO (runParserIO parseprecomp () 0 d) >>= \case
        OK a _ b | B.null b -> pure a
        OK _ _ b ->
          throw $ printf "parser3: trailing data: %d bytes left" (B.length b)
        Fail -> throw "parser3: parseprecomp failed"
        Err _ -> error "parser3: impossible"
      where
        checkuncomplen l
          | l < 0 = throw "parser3: negative length"
          | l > 0x7FFFFF = throw "parser3: length too large"
          | otherwise = pure l

-- encoding

-- size-tracked builder
data Building = B !Int !Builder -- size, builder

instance Semigroup Building where
  B a b <> B c d = B (a + c) (b <> d)
  {-# INLINE (<>) #-}

instance Monoid Building where
  mempty = B 0 mempty
  {-# INLINE mempty #-}

-- | make an output stream of uninterpreted packets
makepacketstreamo ::
  IORef CompressionOn ->
  OutputStream ByteString ->
  IO (OutputStream Uninterpreted)
makepacketstreamo c s =
  makeOutputStream \case
    Nothing -> throwIO EOF
    Just u -> do
      c' <- readIORef c
      write (Just $ reify $ encode c' u) s
  where
    reify = B.toStrict . BB.toLazyByteString
    -- notes on "INLINE": inline thought to be good for sharing
    -- esp. the compression part (to avoid compressing the same data twice)
    -- (remains to be checked)
    encode d u =
      let B n b = encodez d u
       in packleb32 n <> b
    {-# INLINE encode #-}
    encodez CompressionOff u = encodeplain u
    encodez (CompressionOn t) u@(Uninterpreted f d)
      | B.length d < t = encodeu u
      | otherwise =
          mkb (packleb32 (B.length d))
            <> mkb (BB.word8 f)
            <> mkb (BB.lazyByteString (compress $ B.fromStrict d))
    {-# INLINE encodez #-}
    encodeu u = mkb (BB.word8 0) <> encodeplain u
    {-# INLINE encodeu #-}
    encodeplain (Uninterpreted f d) = mkb (BB.word8 f) <> mkb (BB.byteString d)
    {-# INLINE encodeplain #-}
    mkb b = B (fromIntegral $ BL.length $ BB.toLazyByteString b) b
    {-# INLINE mkb #-}

-- | register an octet streaming decryptor to an input stream
makedecrypting ::
  -- | decryptor
  IORef (ByteString -> IO ByteString) ->
  -- | input stream
  InputStream ByteString ->
  -- | new input stream
  IO (InputStream ByteString)
makedecrypting f s = makeInputStream do
  read s >>= \case
    Nothing -> pure Nothing
    Just b -> readIORef f >>= (Just <$>) . ($ b)

-- | register an octet stremaing encryptor to an output stream
makeencrypting ::
  -- | encryptor
  IORef (ByteString -> IO ByteString) ->
  -- | output stream
  OutputStream ByteString ->
  -- | new output stream
  IO (OutputStream ByteString)
makeencrypting f s = makeOutputStream \case
  Nothing -> pure ()
  Just b -> do
    g <- readIORef f
    g b >>= \c -> write (Just c) s
