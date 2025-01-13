-- |
-- Module: M.IO.KeepAlive
-- Description: Keep-alive mechanism for maintaining server connections
-- License: BSD-3-Clause
--
-- This module implements the Minecraft protocol's keep-alive mechanism, which helps
-- detect stale connections by periodically exchanging random numbers.
module M.IO.KeepAlive
  ( -- * Types
    KeepAliveFail (..),

    -- * Keep-alive functions
    skeepalive, -- ^ Server keep-alive handler
  )
where

import Control.Applicative
import Control.Monad
import Data.Data
import Data.Functor
import Effectful
import Effectful.Concurrent
import Effectful.Exception
import M.IO.Internal.EffectTypes
import M.Pack
import System.Random

-- | keep-alive failure modes. Occurs when:
--
-- * the response number doesn't match the sent number ('KeepAliveFail')
-- * no response is received within timeout ('KeepAliveTimeout')
data KeepAliveFail a
  = KeepAliveFail
      -- | sent
      a
      -- | received
      a
  | KeepAliveTimeout
  deriving (Eq, Show, Typeable, Exception)

-- | server's keep-alive mechanism. sends a random number every 15 seconds
-- and verifies the client echoes it back correctly
--
-- throws:
--
-- * 'KeepAliveFail' if response doesn't match
-- * 'KeepAliveTimeout' if no response within timeout
skeepalive ::
  forall a es void.
  ( Concurrent :> es,
    Talking' es,
    IOE :> es,
    Random a,
    Show a,
    Eq a,
    Pack a,
    Unpack a,
    Typeable a
  ) =>
  Eff es void
skeepalive =
  wait *> forever do
    a <- send
    b <- wait *> (receive <|> timeout)
    unless (a == b) do
      mismatch a b
  where
    wait = threadDelay 15_000_000 -- microseconds
    send = liftIO (randomIO :: IO a) >>= \n -> say n $> n
    receive = hear @a Immediately
    mismatch = (throwIO .) . KeepAliveFail
    timeout = throwIO (KeepAliveTimeout @a)
