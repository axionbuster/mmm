-- |
-- Module: M.IO.Internal.Socket
-- Description: Socket connection handling for Minecraft protocol
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Implements reliable duplex stream connections for the Java Minecraft protocol,
-- handling encryption and compression.
module M.IO.Internal.Socket (Connection (..), withcxfromsocket) where

import Data.ByteString qualified as B
import M.Crypto
import M.IO.Internal.Datagram
import M.IO.Obs
import Network.SocketA
import System.IO.Streams
import UnliftIO

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
withcxfromsocket :: (MonadUnliftIO m) => Socket -> (Connection -> m a) -> m a
withcxfromsocket sk cont = do
  th <- newTVarIO (-1) -- compression off by default
  (i0, o0) <- liftIO (socketToStreams sk)
  (ef, df) <- liftA2 (,) (newTVarIO pure) (newTVarIO pure)
  (i1, o1) <-
    liftA2
      (,)
      (liftIO $ makedecrypting df i0)
      (liftIO $ makeencrypting ef o0)
  (i2, o2) <-
    liftA2
      (,)
      (liftIO $ makepacketstreami th i1)
      (liftIO $ makepacketstreamo th o1)
  k <- newTVarIO Nothing
  let watchk = liftIO $ obs
        do k -- target
        do const pure -- transform (accept new value)
        do
          const \case
            -- what to do upon a change in 'k'
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
    handle
      do \(e :: SomeException) -> throwIO e
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
