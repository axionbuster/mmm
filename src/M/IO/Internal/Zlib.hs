-- |
-- Module: M.IO.Internal.Zlib
-- Description: Safe zlib decompression utilities
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Provides safe decompression functionality for zlib-compressed data
-- with length validation and error handling.
module M.IO.Internal.Zlib (safedecomp) where

import Codec.Compression.Zlib
import Codec.Compression.Zlib.Internal
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder
import Data.IORef
import Text.Printf

-- | length-checked decompression of zlib-compressed data under 'IO'
safedecomp ::
  -- | expected length of decompressed data
  Int ->
  -- | compressed data
  ByteString ->
  -- | decompressed data
  IO ByteString
safedecomp l rest = do
  n <- newIORef 0
  b <- foldDecompressStream
    do ($ rest) -- "input"
    do
      -- "output"
      \ch k -> do
        m <- modifyIORef n (+ B.length ch) *> readIORef n
        when (m > l) $ fail "safedecomp: too much data"
        (byteString ch <>) <$> k
    do pure . byteString -- "finish"
    do \e -> fail $ "safedecomp: " ++ displayException e -- "error"
    do decompressIO zlibFormat defaultDecompressParams -- "decompressor"
  let g = B.toStrict $ toLazyByteString b
      h = B.length g
  unless (h == l) do
    fail do
      printf "safedecomp: wrong length (bytes): %d expected vs. %d got" l h
  pure g
