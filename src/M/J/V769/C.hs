{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module M.J.V769.C where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Data
import Data.Int
import Data.Serde.QQ
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word
import GHC.Generics
import M.J.TODO
import M.J.V769.I
import M.LEB
import M.Pack
import Prelude hiding (id)

[serde|
.derive
  Show Read Data Typeable
data FinishConfiguration -- Empty packet

data FeatureFlags
  flags :: V.Vector Text via V.Vector Identifier

data ServerData
  motd :: TextComponent
  icon :: Maybe Text
  enforcesecurechat :: Bool

data ServerLinks
  links :: V.Vector Text via V.Vector Identifier

data CustomReportDetails
  details :: ByteString via TakeRest

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
