{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module M.V769.C where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Data
import Data.Int
import Data.Serde.QQ
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word
import GHC.Generics
import M.LEB
import M.Pack
import M.V769.I
import Prelude hiding (id)

[serde|
.derive
  Show Read Data Typeable

-- Clientbound packets
data CookieRequest
  key :: Text

data ClientboundPluginMessage
  channel :: Text
  data_ :: ByteString

data Disconnect
  reason :: Text

data FinishConfiguration -- Empty packet

data ClientboundKeepAlive
  id :: Int64

data Ping
  id :: Int64

data ResetChat -- Empty packet

data RegistryData
  data_ :: ByteString

data RemoveResourcePack -- Empty packet

data AddResourcePack
  url :: Text
  hash :: Text

data StoreCookie
  key :: Text
  value :: Text

data Transfer
  serveraddress :: Text
  serverport :: Word16

data FeatureFlags
  flags :: V.Vector Text via V.Vector Identifier

data UpdateTags
  tags :: ByteString via TakeRest

data ClientboundKnownPacks
  packs :: ByteString via TakeRest

data CustomReportDetails
  details :: ByteString via TakeRest

data ServerLinks
  links :: V.Vector Text via V.Vector Identifier

-- Serverbound packets
data ClientInformationConfiguration
  locale :: Text
  viewdistance :: Int8
  chatmode :: ChatMode via EnumIndex VarInt ChatMode
  chatcolors :: Bool
  displayedskinparts :: DisplayedSkinParts via Bitwise Word8 DisplayedSkinParts
  mainhandright :: Bool
  enabletextfiltering :: Bool
  allowserverlistings :: Bool
  particlestatus :: ParticleStatus via EnumIndex VarInt ParticleStatus

data CookieResponse
  key :: Text
  value :: Text

data ServerboundPluginMessage
  channel :: Text
  data_ :: ByteString

data AcknowledgeFinishConfiguration -- Empty packet

data ServerboundKeepAlive
  id :: Int64

data Pong
  id :: Int64

data ResourcePackResponse
  status :: VarInt

data ServerboundKnownPacks
  packs :: ByteString via TakeRest
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
