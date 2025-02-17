-- |
-- Module: M.IO.Internal.Datagram
-- Description: Packet parsing and building internals
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Internal module for handling low-level packet parsing and building,
-- including uninterpreted packets and stream transformations.
module M.IO.Internal.Datagram
  ( -- * Types
    Uninterpreted (..),
    EOF (..),

    -- * Streams
    makepacketstreami,
    makepacketstreamo,
    makedecrypting,
    makeencrypting,
  )
where

import Codec.Compression.Zlib
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception hiding (throw)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Data
import Data.Hashable
import Data.Maybe
import Data.Word
import FlatParse.Stateful
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.IO.Internal.Read
import M.IO.Internal.Zlib
import M.Pack hiding (Parser)
import System.IO.Streams
import System.Timeout
import Text.Printf
import Prelude hiding (read)

-- | uninterpreted packet
data Uninterpreted = Uninterpreted
  { pkcode :: !Word8,
    pkdata :: !ByteString
  }
  deriving (Eq, Ord, Hashable, Typeable, Generic, Data, Lift, NFData)

instance Show Uninterpreted where
  show (Uninterpreted c d) = printf "Uninterpreted %d <%d bytes>" c (B.length d)

-- | end of input
data EOF = EOF deriving (Show, Read, Generic, Data, Typeable, Lift, Exception)

-- | make a stream of uninterpreted packets
makepacketstreami ::
  -- | compression threshold reference (negative = off,
  -- non-negative = on with threshold)
  TVar Int ->
  -- | input stream
  InputStream ByteString ->
  -- | stream of uninterpreted packets
  IO (InputStream Uninterpreted)
makepacketstreami c s =
  makeInputStream do
    threshold <- readTVarIO c
    Just <$> takepacket threshold s
  where
    takepacket threshold b = do
      t <- parseio0 @ParseError b checkedlength
      u <- catch
        (fmap fromJust $ timeout 5_000_000 $ readExactly t b)
        \(e :: SomeException) -> throwIO e
      let p
            | threshold >= 0 = parsepostcomp
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
          | l > 0x800000 = throw "parser3: length too large"
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
  TVar Int ->
  OutputStream ByteString ->
  IO (OutputStream Uninterpreted)
makepacketstreamo c s =
  makeOutputStream \case
    Nothing -> throwIO EOF
    Just u -> do
      threshold <- readTVarIO c
      write (Just $ reify $ encode threshold u) s
  where
    reify = B.toStrict . BB.toLazyByteString
    -- notes on "INLINE": inline thought to be good for sharing
    -- esp. the compression part (to avoid compressing the same data twice)
    -- (remains to be checked)
    encode d u =
      let B n b = encodez d u
       in packleb32 n <> b
    {-# INLINE encode #-}
    encodez threshold u@(Uninterpreted f d)
      | threshold < 0 = encodeplain u
      | B.length d < threshold = encodeu u
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
  TVar (ByteString -> IO ByteString) ->
  -- | input stream
  InputStream ByteString ->
  -- | new input stream
  IO (InputStream ByteString)
makedecrypting f s = makeInputStream do
  read s >>= \case
    Nothing -> pure Nothing
    Just b -> readTVarIO f >>= (Just <$>) . ($ b)

-- | register an octet stremaing encryptor to an output stream
makeencrypting ::
  -- | encryptor
  TVar (ByteString -> IO ByteString) ->
  -- | output stream
  OutputStream ByteString ->
  -- | new output stream
  IO (OutputStream ByteString)
makeencrypting f s = makeOutputStream \case
  Nothing -> pure ()
  Just b -> do
    g <- readTVarIO f
    g b >>= \c -> write (Just c) s
