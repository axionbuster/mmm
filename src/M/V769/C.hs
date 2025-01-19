{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: M.V769.C
-- Description: Protocol configuration state packets (AI-generated)
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Defines packet structures for the configuration state of protocol version 769,
-- including client and server capabilities negotiation, resource pack management,
-- and server feature flags.
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

data Disconnect
  reason :: Text

data FinishConfiguration -- Empty packet

data ResetChat -- Empty packet

data RegistryData
  data_ :: ByteString

data RemoveResourcePack -- Empty packet

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

data KnownPacks
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

data AcknowledgeFinishConfiguration -- Empty packet

data Pong
  id :: Int64

data ResourcePackResponse
  status :: VarInt
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
