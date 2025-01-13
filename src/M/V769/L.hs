{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module M.V769.L where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Data
import Data.Int
import Data.Serde.QQ
import Data.Text (Text)
import Data.UUID.Types (UUID)
import Data.Vector qualified as V
import GHC.Generics
import M.LEB
import M.Pack
import M.TODO
import M.V769.I

[serde|
.derive
  Show Read Data Typeable

data LoginDisconnect
  reason :: TextComponent

data EncryptionRequest
  serverid :: Text
  publickey :: ByteString
  verifytoken :: ByteString
  shouldauthenticate :: Bool

data LoginSuccess
  uuid :: UUID
  username :: Text
  properties :: V.Vector LoginSuccess_Property

data LoginSuccess_Property
  name :: Text
  value :: Text
  signature :: Maybe Text

data LoginPluginRequest
  messageid :: Int32 via LEB Int32
  channel :: Text
  data_ :: ByteString via TakeRest -- if Coercible, ok

data LoginPluginResponse
  messageid :: Int32 via VarInt
  successful :: Bool
  data_ :: Maybe ByteString

data LoginStart
  name :: Text
  uuid :: UUID

data LoginAcknowledged

data EncryptionResponse
  sharedsecret :: ByteString
  verifytoken :: ByteString
  |]

-- provided by "th-serde": Data.Serde.QQ
runusercoercion
  -- provided by M.Pack
  borrowderivepackunpack
  properderivepackunpack
  -- preparations for shadow types
  [ ''Generic,
    ''NFData,
    ''Eq,
    ''Ord
  ]
