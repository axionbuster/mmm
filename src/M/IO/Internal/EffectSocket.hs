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
    withtalkingclient,
  )
where

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
import M.IO.Internal.WinHack
import M.Pack
import Network.Run.TCP (runTCPClient, runTCPServer)
import System.IO.Streams
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
  Hear i -> case i of
    Immediately ->
      handleheara_immediate cx.cxinput
        <&> fromJust . castsomeunpack
    Eventually -> handleheara cx.cxinput <&> fromJust . castsomeunpack
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
handlehearu i = liftIO (read i) >>= maybe (throwIO EOF) pure

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
  u <- handlehearu i
  ParserState p <- get
  pure $ p (Parse u)

handleheara_immediate ::
  (IOE :> es, State ParserState :> es, NonDet :> es) =>
  InputStream Uninterpreted -> Eff es SomeUnpack
handleheara_immediate i = do
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
  -- | unlift strategy
  UnliftStrategy ->
  -- | host (Nothing = all interfaces)
  Maybe String ->
  -- | port
  String ->
  -- | per-connection handler
  Eff (Talking : es) a ->
  -- | final result
  Eff es a
withtalkingserver u host port handler = do
  liftIO $ traceIO "withtalkingserver: up"
  withEffToIO u \run -> do
    liftIO $ traceIO "withtalkingserver: inside withEffToIO"
    -- 'network' package on Windows is broken. server gets stuck.
    -- workaround: forcibly kill process on exception.
    killonexc do
      -- see M.IO.Internal.WinHack for docs on 'killonexc'
      liftIO $ traceIO "withtalkingserver: inside killonexc"
      runTCPServer host port \sock -> do
        liftIO $ traceIO "withtalkingserver: inside runTCPServer"
        withcxfromsocket sock \cx -> do
          liftIO $ traceIO "withtalkingserver: inside withcxfromsocket"
          run $ runtalking0 cx handler

-- | run client with single connection
withtalkingclient ::
  ( IOE :> es,
    State ParserState :> es,
    Concurrent :> es,
    NonDet :> es
  ) =>
  -- | unlift strategy
  UnliftStrategy ->
  -- | host
  String ->
  -- | port
  String ->
  -- | handler
  Eff (Talking : es) a ->
  -- | result
  Eff es a
withtalkingclient u host port handler = withEffToIO u \run -> do
  runTCPClient host port \sock ->
    withcxfromsocket sock \cx ->
      run $ runtalking0 cx handler
