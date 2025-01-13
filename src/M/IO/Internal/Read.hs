-- |
-- Module: M.IO.Internal.Read
-- Description: Stream parsing utilities
-- License: BSD-3-Clause
--
-- This module provides parsing utilities for reading structured data from streams.
module M.IO.Internal.Read
  ( -- * Basic parsing
    parseio,
    parseio0,

    -- * Lifted versions
    parseiolift,
    parseio0lift,
  )
where

import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Functor
import FlatParse.Stateful
import System.IO.Streams
import Prelude hiding (read)

-- | parse from a stream (with no state and int value of 0). see also:
-- 'parseio'
parseio0 ::
  (Exception e) =>
  -- | input stream
  InputStream ByteString ->
  -- | parser
  ParserIO () e a ->
  -- | result
  IO a
parseio0 = parseio () 0
{-# INLINE parseio0 #-}

-- | lifted version of 'parseio0'
parseio0lift ::
  (MonadIO m, Exception e) =>
  -- | input stream
  InputStream ByteString ->
  -- | parser
  ParserIO () e a ->
  -- | result
  m a
parseio0lift = parseiolift () 0
{-# INLINE parseio0lift #-}

-- | Parse from a stream. Automatically handles chunked input by
-- concatenating chunks until a complete parse succeeds.
--
-- May throw:
-- * The parser's error type @e@
-- * IOError "parseio: unexpected end of input"
parseio ::
  (Exception e) =>
  -- | state
  r ->
  -- | int value
  Int ->
  -- | input stream
  InputStream ByteString ->
  -- | parser
  ParserIO r e a ->
  -- | result
  IO a
parseio v i s f = parse mempty
  where
    -- controlled by a two-state machine
    parse b = runParserIO f v i b >>= check b
    check b = \case
      OK a _ r -> unRead r s $> a
      Fail ->
        read s >>= \case
          -- note: FlatParse is not a resumable parser.
          -- so, we must start over from the beginning
          Just t -> parse (b <> t) -- concatenate and start over
          Nothing -> fail "parseio: unexpected end of input"
      Err e -> throwIO e

-- | lifted version of 'parseio'
parseiolift ::
  (MonadIO m, Exception e) =>
  -- | state
  r ->
  -- | int value
  Int ->
  -- | input stream
  InputStream ByteString ->
  -- | parser
  ParserIO r e a ->
  -- | result
  m a
parseiolift = (((liftIO .) .) .) . parseio
{-# INLINE parseiolift #-}
