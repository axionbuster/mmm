{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: M.V769.L
-- Description: Protocol login state packets (AI-generated)
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Defines packet structures for the login phase of protocol version 769,
-- handling authentication, encryption setup, and session establishment
-- between client and server.
module M.V769.L where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Data
import Data.Int
import Data.Text (Text)
import Data.UUID.Types (UUID)
import Data.Vector qualified as V
import M.LEB
import M.Pack
import M.PkMacro
import M.TODO

setdefaultderives
addproperderives [''NFData, ''Typeable, ''Show, ''Eq]

[pkmacro|
data LoginDisconnect {
  reason :: TextComponent,
}

data EncryptionRequest {
  serverid :: Text,
  publickey :: ByteString,
  verifytoken :: ByteString,
  shouldauthenticate :: Bool,
}

data LoginSuccess {
  uuid :: UUID,
  username :: Text,
  properties :: V.Vector LoginSuccess_Property,
}

data LoginSuccess_Property {
  name :: Text,
  value :: Text,
  signature :: Maybe Text,
}

data LoginPluginRequest {
  messageid :: Int32 via LEB Int32,
  channel :: Text,
  data_ :: ByteString via TakeRest,
}

data LoginPluginResponse {
  messageid :: Int32 via VarInt,
  successful :: Bool,
  data_ :: Maybe ByteString,
}

data LoginStart {
  name :: Text,
  uuid :: UUID,
}

data LoginAcknowledged {}

data EncryptionResponse {
  sharedsecret :: ByteString,
  verifytoken :: ByteString,
}
|]
