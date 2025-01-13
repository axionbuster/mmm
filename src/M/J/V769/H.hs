{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module M.J.V769.H where

import Control.DeepSeq
import Data.Data
import Data.Int
import Data.Serde.QQ
import Data.Text (Text)
import Data.Word
import GHC.Generics
import M.J.V769.I
import M.LEB
import M.Pack

[serde|
.derive
  Show Read Data Typeable

-- Handshaking
data HandshakePacket
  protocolversion :: Int32 via VarInt
  serveraddress :: Text 
  serverport :: Word16
  nextstate :: Int32 via VarInt
 |]

runusercoercion
  borrowderivepackunpack
  properderivepackunpack
  -- preparations for shadow types
  [ ''Generic,
    ''NFData,
    ''Eq,
    ''Ord
  ]
