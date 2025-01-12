-- | Keep-alive mechanism
module M.IO.KeepAlive (KeepAliveFail (..), skeepalive) where

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

-- | keep-alive failure
data KeepAliveFail a
  = KeepAliveFail
      -- | sent
      a
      -- | received
      a
  | KeepAliveTimeout
  deriving (Eq, Show, Typeable, Exception)

-- | server's keep-alive mechanism
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
