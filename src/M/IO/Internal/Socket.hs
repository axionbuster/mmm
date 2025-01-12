-- | Bringing the Java Minecraft protocol to reliable duplex streams.
module M.IO.Internal.Socket (Connection (..), withcxfromsocket) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Data.ByteString
import M.Crypto
import M.IO.Internal.Datagram
import Network.Socket
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.Network (socketToStreams)

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
  th <- newTVarIO (-1) -- compression off by default
  (i0, o0) <- socketToStreams sk
  (ef, df) <- liftA2 (,) (newTVarIO pure) (newTVarIO pure)
  (i1, o1) <- liftA2 (,) (makedecrypting df i0) (makeencrypting ef o0)
  (i2, o2) <- liftA2 (,) (makepacketstreami th i1) (makepacketstreamo th o1)
  k <- newTVarIO Nothing
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
  withAsync watchk \_ ->
    cont
      Connection
        { cxkey = k,
          cxcompth = th,
          cxinput = i2,
          cxoutput = o2
        }
