-- |
-- Module: M.IO.Internal.EffectSocket
-- Description: Socket-based effect interpretation
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Implements the interpretation of the 'Talking' effect in terms of socket
-- connections, providing both client and server capabilities.
module M.IO.Internal.EffectSocket
  ( SocketTalkingError (..),
    withtalkingserver,
  )
where

import Control.DeepSeq
import Control.Monad
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Data
import Data.Functor
import Data.Maybe
import Debug.Trace
import Effectful
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import Effectful.NonDet
import Effectful.State.Dynamic
import M.IO.Internal.Datagram
import M.IO.Internal.EffectTypes
import M.IO.Internal.Socket
import M.Pack
import Network.SocketA hiding
  ( accept,
    bind,
    close,
    getaddrinfo,
    listen,
    socket,
    withaddrlen,
    withsocket,
  )
import Network.SocketA.Unlift
import System.IO.Streams
import Text.Printf
import UnliftIO.Exception qualified as Unfe
import Prelude hiding (read)

-- https://hackage.haskell.org/package/effectful-core-2.5.1.0/docs/Effectful.html#g:13
-- SeqUnlift: fail when calling 'run' (=unlift) from outside the spawning thread
-- SeqForkUnlift: fork (-> gain independence) at unlift
-- ConcUnlift: allow concurrent access

-- | error in communication
data SocketTalkingError
  = UnknownCode Direction TypeRep
  | Mismatch Direction TypeRep TypeRep
  deriving (Eq, Show, Typeable, Exception)

reify ::
  (State ParserState :> es) =>
  TypeRep ->
  Builder ->
  Eff es Uninterpreted
reify t b = do
  ParserState p <- get
  case p (Code Outbound t) of
    Just u -> pure $ Uninterpreted u (B.toStrict $ toLazyByteString b)
    Nothing -> throwIO $ UnknownCode Outbound t

-- Run a computation with Talking effect using a Connection
runtalking0 ::
  (IOE :> es, NonDet :> es, State ParserState :> es, Concurrent :> es) =>
  Connection -> Eff (Talking : es) a -> Eff es a
runtalking0 cx = interpret_ \case
  Hear i -> do
    liftIO $ traceIO $ printf "Hear i: i = %s" (show i)
    let x = case i of
          Immediately ->
            handleheara_immediate cx.cxinput
              <&> fromJust . castsomeunpack
          Eventually -> handleheara cx.cxinput <&> fromJust . castsomeunpack
    y <- x
    liftIO $ traceIO $ printf "Hear i: will return something"
    handle
      do \(e :: SomeException) -> do liftIO $ traceIO $ printf "Hear i: deser exception: %s" (displayException e); undefined
      do evaluate y
    liftIO $ traceIO $ printf "Hear i: evaluate was successful"
    pure y
  HearU i -> case i of
    Immediately -> handlehearu_immediate cx.cxinput
    Eventually -> handlehearu cx.cxinput
  HearA i -> case i of
    Immediately -> handleheara_immediate cx.cxinput
    Eventually -> handleheara cx.cxinput
  -- todo: it's possible to create a stream that accepts Builder instead
  -- of ByteString for the sake of efficiency (from io-streams itself)
  -- but I haven't given it much thought yet
  Say p -> reify (typeOf p) (pack p) >>= liftIO . writeTo cx.cxoutput . Just
  Setcompression th -> atomically $ writeTVar cx.cxcompth th
  Setencryption key -> atomically $ writeTVar cx.cxkey (Just key)

handlehearu :: (IOE :> es) => InputStream Uninterpreted -> Eff es Uninterpreted
handlehearu i = liftIO (read i) >>= maybe (do liftIO $ traceIO "EOF"; throwIO EOF) pure

handlehearu_immediate ::
  (IOE :> es, NonDet :> es) =>
  InputStream Uninterpreted -> Eff es Uninterpreted
handlehearu_immediate i = do
  mv <- liftIO $ peek i
  case mv of
    Nothing -> empty
    Just v -> liftIO (read i) $> v

handleheara ::
  (IOE :> es, State ParserState :> es) =>
  InputStream Uninterpreted -> Eff es SomeUnpack
handleheara i = do
  liftIO $ traceIO "h..a: entered"
  u <- handlehearu i
  liftIO $ traceIO $ printf "h..a: read u: %s" (show u)
  ParserState p <- get
  let v = p (Parse u)
  liftIO $ traceIO $ printf "h..a: v = %s" (show v)
  pure v

handleheara_immediate ::
  (IOE :> es, State ParserState :> es, NonDet :> es) =>
  InputStream Uninterpreted -> Eff es SomeUnpack
handleheara_immediate i = do
  liftIO $ traceIO "h..a_immediate: entered"
  u <- handlehearu_immediate i
  ParserState p <- get
  pure $ p (Parse u)

-- | run server accepting multiple connections
withtalkingserver ::
  ( IOE :> es,
    State ParserState :> es,
    Concurrent :> es,
    NonDet :> es
  ) =>
  -- | host (Nothing = all interfaces)
  Maybe String ->
  -- | port
  String ->
  -- | per-connection handler
  Eff (Talking : es) a ->
  -- | final result
  Eff es a
withtalkingserver host port handler = do
  liftIO $ traceIO "withtalkingserver: up"
  -- affected by the ambient unlifting strategy.
  -- set it somewhere else using withUnliftStrategy.
  let host'
        | Just h <- host = h
        | otherwise = "0.0.0.0"
  runTCPServer host' port \sock -> withRunInIO \run -> do
    traceIO "withtalkingserver: inside runTCPServer"
    withcxfromsocket sock \cx -> do
      traceIO "withtalkingserver: inside withcxfromsocket"
      run $ runtalking0 cx handler

-- compatibility with func of same name in "network" package; just unlifted
runTCPServer ::
  (MonadUnliftIO m) =>
  String -> String -> (Socket -> m b) -> m a
runTCPServer host port client = do
  let hint =
        addrinfo0
          { ai_socktype = SOCK_STREAM,
            ai_protocol = IPPROTO_TCP,
            ai_family = AF_INET
          }
      mksocket = socket hint.ai_family hint.ai_socktype hint.ai_protocol
  addr <- getaddrinfo host port (Just hint)
  Unfe.bracket mksocket close \sock -> do
    withaddrlen addr do bind sock
    listen sock
    forever do
      Unfe.bracket
        do accept sock -- wait happens here
        do close -- close the socket!
        do client -- gets passed the socket
