-- | Parse from a stream
module M.IO.Internal.Read
  ( parseio,
    parseio0,
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

-- | parse from a stream
--
-- one may either get the result or an exception. the exception may
-- be of @e@ type or a generic 'IOError' saying \"parseio: unexpected
-- end of input\"
--
-- on many small chunks, time complexity may approach O(n^2) due to
-- use of left-associated 'mappend' in 'ByteString' concatenation
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
