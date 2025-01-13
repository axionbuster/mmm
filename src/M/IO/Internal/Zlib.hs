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
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL

-- | length-checked decompression of zlib-compressed data under 'IO'
safedecomp ::
  -- | expected length of decompressed data
  Int ->
  -- | compressed data
  ByteString ->
  -- | decompressed data
  IO ByteString
safedecomp l comp = do
  let n = fromIntegral l
      d = BL.take n $ decompress $ BL.fromStrict comp
  if BL.length d /= n
    then fail "safedecomp: wrong length"
    else pure $ B.toStrict d
