{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module M.J.V769.P where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Data
import Data.Int
import Data.Serde.QQ
import Data.Text (Text)
import Data.UUID.Types (UUID)
import Data.Vector qualified as V
import Data.Word
import GHC.Generics
import Linear (V2, V3)
import M.J.Chunk.Net
import M.J.NBT (Tg)
import M.J.Position (Position)
import M.J.TODO
import M.J.V769.I
import M.LEB
import M.Pack
import Prelude hiding (id, length, sequence)

[serde|
.derive
  Show Read Data Typeable

data BundleDelimiter -- empty packet

data SpawnEntity
  entityid :: Int32 via VarInt
  entityuuid :: UUID
  type_ :: Int32 via VarInt
  position :: V3 Double
  rotation :: V2 Int8 via V2 Int8Angle
  data_ :: Int32 via VarInt
  velocity :: V3 Double via V3 (Fixed' Int16 SetEntityVelocityRes Double)

data SpawnExperienceOrb
  entityid :: Int32 via VarInt
  position :: V3 Double
  count :: Int16

data EntityAnimation
  entityid :: Int32 via VarInt
  animation :: Word8

data AwardStatistics
  stats :: V.Vector AwardStatistics_Entry

data AwardStatistics_Entry
  categoryid :: Int32 via VarInt
  statisticid :: Int32 via VarInt
  value :: Int32 via VarInt

data AcknowledgeBlockChange
  sequence :: Int32 via VarInt

data SetBlockDestroyStage
  entityid :: Int32 via VarInt
  location :: Position
  destroystage :: Word8

data BlockEntityData
  location :: Position
  type_ :: Int32 via VarInt
  data_ :: Tg -- NBT data

data BlockAction
  location :: Position
  actionid :: Word8
  actionparam :: Word8
  blocktype :: Int32 via VarInt

data BlockUpdate
  location :: Position 
  blockid :: Int32 via VarInt

data BossBar
  uuid :: UUID
  action :: BossBarAction

data ChangeDifficulty
  difficulty :: Word8
  locked :: Bool

data ChunkBatchFinished
  batchsize :: Int32 via VarInt

data ChunkBatchStarted

data ChunkBiomes
  x :: Int32
  z :: Int32
  data_ :: ChunkData

data ClearTitles
  reset :: Bool

data CommandSuggestionsResponse
  id :: Int32 via VarInt
  start :: Int32 via VarInt
  length :: Int32 via VarInt
  matches :: V.Vector CommandMatch

data CommandMatch
  match :: Text
  tooltip :: Maybe TextComponent

data Commands
  nodes :: V.Vector CommandNode  -- CommandNode type defined elsewhere
  rootindex :: Int32 via VarInt

data CloseContainer
  windowid :: Int32 via VarInt

data SetContainerContent
  windowid :: Int32 via VarInt
  stateid :: Int32 via VarInt
  slots :: V.Vector Slot  -- Slot type defined elsewhere
  carrieditem :: Slot

data SetContainerProperty
  windowid :: Int32 via VarInt
  property :: Int16
  value :: Int16

data SetContainerSlot
  windowid :: Int32 via VarInt
  stateid :: Int32 via VarInt
  slot :: Int16
  data_ :: Slot

data DisguisedChatMessage
  message :: TextComponent
  chattype :: Int32 via VarInt
  sendername :: TextComponent
  targetname :: Maybe TextComponent

data WorldEvent  
  event :: Int32
  location :: Position
  data_ :: Int32
  disablerelativevolume :: Bool

data ParticleEffect
  particleid :: Int32 via VarInt
  longdistance :: Bool
  position :: V3 Double
  offset :: V3 Float
  maxspeed :: Float
  count :: Int32
  data_ :: ParticleData -- Type defined elsewhere

data UpdateLight
  chunkx :: Int32 via VarInt
  chunkz :: Int32 via VarInt
  data_ :: LightData

data SystemChatMessage
  content :: TextComponent
  overlay :: Bool

data InitializeWorldBorder
  x :: Double
  z :: Double
  oldradius :: Double
  newradius :: Double
  speed :: Int64 via VarLong
  portalteleportboundary :: Int32 via VarInt
  warningtime :: Int32 via VarInt
  warningblocks :: Int32 via VarInt

data ConfirmTeleportation
  teleportid :: Int32 via VarInt

data QueryBlockEntityTag
  transactionid :: Int32 via VarInt
  location :: Position

data ChatCommand
  command :: Text
  timestamp :: Int64
  salt :: Int64
  signatures :: V.Vector ByteString
  signedpreview :: Bool
  lastseenmessages :: V.Vector UUID

data ClientStatus
  action :: Int32 via VarInt

data ClientTickEnd

data CommandSuggestionsRequest
  id :: Int32 via VarInt
  text :: Text
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
