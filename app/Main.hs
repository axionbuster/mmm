module Main (main) where

import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BB
import Data.Int
import Data.Semigroup
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Unboxed qualified as VU
import Data.Word
import Debug.Trace
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.NonDet
import Effectful.State.Dynamic
import M.Chunk.Code
import M.Chunk.Net
import M.IO
import M.NBT
import M.Pack
import M.V769.C qualified as C
import M.V769.H qualified as H
import M.V769.I qualified as I
import M.V769.L qualified as L
import M.V769.P qualified as P
import M.V769.Reg
import M.V769.S qualified as S
import Network.SocketA qualified as Network
import Text.Printf

reifybuilder :: Builder -> ByteString
reifybuilder = B.toStrict . BB.toLazyByteString

basiccolumn :: Builder
basiccolumn =
  let (enc, _) = mkcscodec (ChunkSectionEncoding (shift 1 15) (shift 1 6))
      cempty = ChunkSection 0 (VU.replicate 4096 0) (VU.replicate 64 0)
      cstone = ChunkSection 4096 (VU.replicate 4096 1) (VU.replicate 64 0)
      eempty = enc (cempty :: ChunkSection Word16 Word64)
      estone = enc (cstone :: ChunkSection Word16 Word64)
   in stimes (7 :: Int) eempty <> estone <> stimes (16 :: Int) eempty

chunk0 :: Int32 -> Int32 -> Builder
chunk0 x y =
  let height =
        Compound
          [ ("MOTION_BLOCKING", LongArray []),
            ("WORLD_SURFACE", LongArray [])
          ]
      packet = P.ChunkDataAndUpdateLight x y cdata ldata
      cdata = ChunkData height (reifybuilder basiccolumn) []
      bszero = Bitset 0
      ldata = LightData bszero bszero bszero bszero [] []
   in pack packet

greeting :: (IOE :> es, Error Text :> es, Talking' es) => Eff es ()
greeting = do
  -- handshake
  liftIO $ traceIO "starting"
  do
    hs <- hear @H.HandshakePacket Eventually
    liftIO $ traceIO $ printf "hs = %s" (show hs)
    unless (hs.protocolversion == 769) do
      liftIO $ traceIO "a!"
      throwError $ T.pack "unsupported protocol version"
    unless (hs.nextstate == 2 {- LOGIN -}) do
      liftIO $ traceIO "b!"
      throwError $ T.pack "state not LOGIN"
  liftIO $ putStrLn "i'm here"

server :: (IOE :> es, Error Text :> es, Talking' es) => Eff es ()
server = catchError greeting \_ e -> do
  liftIO $ traceIO $ T.unpack e
  server

main :: IO ()
main =
  let action = do
        Network.startup
        runEff
          . runNonDet OnEmptyKeep
          . runConcurrent
          . runError @Text
          . evalStateShared (forserver handshake)
          $ do
            withUnliftStrategy
              do ConcUnlift Persistent Unlimited
              ( withtalkingserver
                  do Just "127.0.0.1"
                  do "25565"
                  do server
              )
   in void action
