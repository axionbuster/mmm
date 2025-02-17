-- |
-- Module: M.IO.Internal.Socket
-- Description: Socket connection handling for Minecraft protocol
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Implements reliable duplex stream connections for the Java Minecraft protocol,
-- handling encryption and compression.
module M.IO.Internal.Socket (Connection (..), withcxfromsocket) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Debug.Trace
import M.Crypto
import M.IO.Internal.Datagram
import Network.SocketA
import System.IO.Streams
import Text.Printf

-- | a connection to either a server or a client
data Connection = Connection
  { -- | encryption key, AES-128-CFB8; doubles as IV
    cxkey :: TVar (Maybe ByteString),
    -- | compression threshold; negative = off, non-negative = on with threshold
    cxcompth :: TVar Int,
    -- | input stream
    cxinput :: InputStream Uninterpreted,
    -- | output stream
    cxoutput :: OutputStream Uninterpreted
  }

-- | create a connection from a socket
withcxfromsocket :: Socket -> (Connection -> IO a) -> IO a
withcxfromsocket sk cont = do
  traceIO "withcxfromsocket begins"
  th <- newTVarIO (-1) -- compression off by default
  (i0, o0) <- socketToStreams sk
  (ef, df) <- liftA2 (,) (newTVarIO pure) (newTVarIO pure)
  -- (i1, o1) <- liftA2 (,) (makedecrypting df i0) (makeencrypting ef o0)
  let (i1, o1) = (i0, o0)
  (i2, o2) <- liftA2 (,) (makepacketstreami th i1) (makepacketstreamo th o1)
  k <- newTVarIO Nothing
  traceIO "withcxfromsocket: past k"
  -- need to go from the easy way to the hard way.
  -- why? because Datagram.hs expects functions to be passed in
  -- for crypto, so we need to convert encryption keys to
  -- encryption functions
  let watchk = do
        kold <- newTVarIO Nothing
        forever do
          k' <- atomically do
            kold' <- readTVar kold
            knew <- readTVar k
            when (knew == kold') retry
            writeTVar kold knew
            pure knew
          case k' of
            Nothing -> atomically do
              writeTVar ef pure
              writeTVar df pure
            Just key -> do
              aese <- aesnew @'Encrypt key
              aesd <- aesnew @'Decrypt key
              atomically do
                writeTVar ef (aesupdate aese)
                writeTVar df (aesupdate aesd)
  withAsync watchk \s -> do
    link s
    traceIO "withcxfromsocket: about to call 'cont'"
    handle
      do \(e :: SomeException) -> do liftIO $ traceIO $ printf "with..et caught: %s" (displayException e); throwIO e
      do
        cont
          Connection
            { cxkey = k,
              cxcompth = th,
              cxinput = i2,
              cxoutput = o2
            }

-- compatibility for socketToStreams from System.IO.Streams.Network
-- that uses "network" for networking instead of "winasyncsocket"
socketToStreams ::
  Socket ->
  IO (InputStream ByteString, OutputStream ByteString)
socketToStreams sk = do
  i <- makeInputStream do
    c <- recv sk 2048
    pure
      if B.null c
        then Nothing
        else Just c
  o <- makeOutputStream \case
    Nothing -> pure () -- leave open; conventional in io-streams
    Just x -> sendall sk x
  pure (i, o)
