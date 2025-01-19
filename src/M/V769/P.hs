{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: M.V769.P
-- Description: Protocol play state packets (AI-generated)
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Implements the extensive set of packet structures for the play state
-- of protocol version 769, including entity management, world interaction,
-- inventory handling, and all other gameplay-related communication.
module M.V769.P where

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
import M.Chunk.Net
import M.LEB
import M.NBT (Tg)
import M.Pack
import M.Position (Position)
import M.TODO
import M.V769.I
import Prelude hiding (id, length, sequence)

[serde|
.derive
  Show Read Data Typeable

-- Clientbound packets
data BundleDelimiter -- Empty packet

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
  data_ :: Tg

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

data ChunkBatchStarted -- Empty packet

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
  nodes :: V.Vector CommandNode
  rootindex :: Int32 via VarInt

data CloseContainer
  windowid :: Int32 via VarInt

data SetContainerContent
  windowid :: Int32 via VarInt
  stateid :: Int32 via VarInt
  slots :: V.Vector Slot
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

data CookieRequest
  key :: Text

data SetCooldown
  itemid :: Int32 via VarInt
  cooldown :: Int32 via VarInt

data ChatSuggestions
  id :: Int32 via VarInt
  suggestions :: V.Vector Text

data DamageEvent
  entityid :: Int32 via VarInt
  sourceid :: Int32 via VarInt
  type_ :: Int32 via VarInt
  amount :: Float

data DebugSample
  data_ :: ByteString via TakeRest

data DeleteMessage
  messageid :: UUID

data Disconnect
  reason :: Text

data DisguisedChatMessage
  message :: TextComponent
  chattype :: Int32 via VarInt
  sendername :: TextComponent
  targetname :: Maybe TextComponent

data EntityEvent
  entityid :: Int32 via VarInt
  event :: Word8

data TeleportEntity
  entityid :: Int32 via VarInt
  position :: V3 Double
  rotation :: V2 Int8 via V2 Int8Angle
  onground :: Bool

data Explosion
  position :: V3 Float
  radius :: Float
  affectedblocks :: V.Vector Position
  playermotion :: V3 Float

data UnloadChunk
  chunkx :: Int32
  chunkz :: Int32

data GameEvent
  event :: Int32 via VarInt
  data_ :: Float

data OpenHorseScreen
  windowid :: Word8
  slotcount :: Int32 via VarInt
  entityid :: Int32

data HurtAnimation
  entityid :: Int32 via VarInt
  damagedirection :: Float

data InitializeWorldBorder
  x :: Double
  z :: Double
  oldradius :: Double
  newradius :: Double
  speed :: Int64 via VarLong
  portalteleportboundary :: Int32 via VarInt
  warningtime :: Int32 via VarInt
  warningblocks :: Int32 via VarInt

data ChunkDataAndUpdateLight
  chunkx :: Int32
  chunkz :: Int32
  data_ :: ChunkData
  lightdata_ :: LightData

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
  data_ :: ParticleData

data UpdateLight
  chunkx :: Int32 via VarInt
  chunkz :: Int32 via VarInt
  data_ :: LightData

data Login
  entityid :: Int32
  ishardcore :: Bool
  gamemode :: Word8
  previousgamemode :: Int8
  worldnames :: V.Vector Text
  dimensioncodec :: Tg
  dimension :: Tg
  worldname :: Text
  hashedseed :: Int64
  maxplayers :: Int32 via VarInt
  viewdistance :: Int32 via VarInt
  simulationdistance :: Int32 via VarInt
  reduceddebuginfo :: Bool
  enablerespawnscreen :: Bool
  isdebug :: Bool
  isflat :: Bool

data MapData
  mapid :: Int32 via VarInt
  scale :: Int8
  trackingposition :: Bool
  locked :: Bool
  icons :: V.Vector MapIcon
  columns :: Int8
  rows :: Int8
  x :: Int8
  z :: Int8
  data_ :: ByteString

data MerchantOffers
  windowid :: Word8
  offers :: V.Vector MerchantOffer
  villagerlevel :: Int32 via VarInt
  experience :: Int32 via VarInt
  isregularvillager :: Bool
  canrestock :: Bool

data UpdateEntityPosition
  entityid :: Int32 via VarInt
  deltax :: Int16
  deltay :: Int16
  deltaz :: Int16
  onground :: Bool

data UpdateEntityPositionAndRotation
  entityid :: Int32 via VarInt
  deltax :: Int16
  deltay :: Int16
  deltaz :: Int16
  rotation :: V2 Int8 via V2 Int8Angle
  onground :: Bool

data MoveMinecartAlongTrack
  entityid :: Int32 via VarInt
  location :: Position

data UpdateEntityRotation
  entityid :: Int32 via VarInt
  rotation :: V2 Int8 via V2 Int8Angle
  onground :: Bool

data MoveVehicle
  position :: V3 Double
  rotation :: V2 Float

data OpenBook
  hand :: Int32 via VarInt

data OpenScreen
  windowid :: Int32 via VarInt
  type_ :: Text
  title :: TextComponent

data OpenSignEditor
  location :: Position

data PlayPing
  id :: Int64

data PlayPingResponse
  id :: Int64

data PlaceGhostRecipe
  windowid :: Word8
  recipe :: Text

data ClientboundPlayerAbilities
  flags :: Word8
  flyingspeed :: Float
  walkingspeed :: Float

data PlayerChatMessage
  sender :: UUID
  message :: Text
  timestamp :: Int64
  salt :: Int64
  signature :: ByteString
  signedpreview :: Bool
  lastseenmessages :: V.Vector UUID

data EndCombat
  duration :: Int32 via VarInt
  entityid :: Int32

data EnterCombat -- Empty packet

data CombatDeath
  playerid :: Int32 via VarInt
  entityid :: Int32 via VarInt
  message :: Text

data PlayerInfoRemove
  players :: V.Vector UUID

data PlayerInfoUpdate
  action :: Int32 via VarInt -- incorrect, but we don't know the correct type
  data_ :: ByteString via TakeRest

data LookAt
  target :: Int32 via VarInt
  position :: V3 Double
  isentity :: Bool

data SynchronizePlayerPosition
  position :: V3 Double
  rotation :: V2 Float
  flags :: Word8
  teleportid :: Int32 via VarInt

data PlayerRotation
  rotation :: V2 Float
  onground :: Bool

data RecipeBookAdd
  recipe :: Text

data RecipeBookRemove
  recipe :: Text

data RecipeBookSettings
  bookid :: Int32 via VarInt
  bookopen :: Bool
  filteractive :: Bool

data RemoveEntities
  entityids :: V.Vector Int32 via V.Vector VarInt

data RemoveEntityEffect
  entityid :: Int32 via VarInt
  effectid :: Word8

data ResetScore
  objectivename :: Text
  playername :: Text
  action :: Word8

data RemoveResourcePack -- Empty packet

data AddResourcePack
  url :: Text
  hash :: Text

data Respawn
  dimension :: Tg
  worldname :: Text
  hashedseed :: Int64
  gamemode :: Word8
  previousgamemode :: Int8
  isdebug :: Bool
  isflat :: Bool
  copymetadata_ :: Bool

data SetHeadRotation
  entityid :: Int32 via VarInt
  headyaw :: Int8 via Int8Angle

data UpdateSectionBlocks
  sectionposition :: Int64
  trustedges :: Bool
  blocks :: V.Vector Int64 via V.Vector VarLong

data SelectAdvancementsTab
  identifier :: Text

data ServerData
  motd :: TextComponent
  icon :: Text
  enforcesecurechat :: Bool

data SetActionBarText
  text :: TextComponent

data SetBorderCenter
  x :: Double
  z :: Double

data SetBorderLerpSize
  oldsize :: Double
  newsize :: Double
  speed :: Int64 via VarLong

data SetBorderSize
  size :: Double

data SetBorderWarningDelay
  warningtime :: Int32 via VarInt

data SetBorderWarningDistance
  warningblocks :: Int32 via VarInt

data SetCamera
  cameraid :: Int32 via VarInt

data SetCenterChunk
  chunkx :: Int32 via VarInt
  chunkz :: Int32 via VarInt

data SetRenderDistance
  viewdistance :: Int32 via VarInt

data SetCursorItem
  slotdata_ :: Slot

data SetDefaultSpawnPosition
  location :: Position
  angle :: Float

data DisplayObjective
  position :: Word8
  name :: Text

data SetEntityMetadata
  entityid :: Int32 via VarInt
  metadata_ :: ByteString via TakeRest

data LinkEntities
  vehicleid :: Int32 via VarInt
  passengerids :: V.Vector Int32 via V.Vector VarInt

data SetEntityVelocity
  entityid :: Int32 via VarInt
  velocity :: V3 Int16

data SetEquipment
  entityid :: Int32 via VarInt
  equipment :: V.Vector Slot

data SetExperience
  experiencebar :: Float
  level :: Int32 via VarInt
  totalexperience :: Int32 via VarInt

data SetHealth
  health :: Float
  food :: Int32 via VarInt
  foodsaturation :: Float

data SetHeldItem
  slot :: Word8

data UpdateObjectives
  name :: Text
  mode :: Word8
  displayname :: Text
  type_ :: Int32 via VarInt

data SetPassengers
  entityid :: Int32 via VarInt
  passengerids :: V.Vector Int32 via V.Vector VarInt

data SetPlayerInventorySlot
  slot :: Int16
  slotdata_ :: Slot

data UpdateTeams
  teamname :: Text
  mode :: Word8
  displayname :: Text
  prefix :: Text
  suffix :: Text
  flags :: Word8
  nametagvisibility :: Text
  collisionrule :: Text
  color :: Int32 via VarInt
  players :: V.Vector Text

data UpdateScore
  entityname :: Text
  action :: Word8
  objectivename :: Text
  value :: Int32 via VarInt

data SetSimulationDistance
  simulationdistance :: Int32 via VarInt

data SetSubtitleText
  text :: TextComponent

data UpdateTime
  worldage :: Int64
  timeofday :: Int64

data SetTitleText
  text :: TextComponent

data SetTitleAnimationTimes
  fadein :: Int32
  stay :: Int32
  fadeout :: Int32

data EntitySoundEffect
  soundid :: Int32 via VarInt
  soundcategory :: Int32 via VarInt
  entityid :: Int32 via VarInt
  volume :: Float
  pitch :: Float

data SoundEffect
  soundid :: Int32 via VarInt
  soundcategory :: Int32 via VarInt
  position :: V3 Int32
  volume :: Float
  pitch :: Float

data StartConfiguration -- Empty packet

data StopSound
  flags :: Word8
  source :: Int32 via VarInt
  sound :: Int32 via VarInt

data StoreCookie
  key :: Text
  value :: Text

data SetTabListHeaderAndFooter
  header :: TextComponent
  footer :: TextComponent

-- Serverbound

data ConfirmTeleportation
  teleportid :: Int32 via VarInt

data QueryBlockEntityTag
  transactionid :: Int32 via VarInt
  location :: Position

data BundleItemSelected
  slot :: Int32 via VarInt

data ServerboundChangeDifficulty
  difficulty :: Word8 -- UByte

data AcknowledgeMessage
  messageid :: UUID

data ServerboundChatCommand
  command :: Text
  timestamp :: Int64
  salt :: Int64
  argumentsignatures :: ByteString
  signedpreview :: Bool
  lastseenmessages :: V.Vector UUID

data SignedChatCommand
  command :: Text
  timestamp :: Int64
  salt :: Int64
  argumentsignatures :: ByteString
  signedpreview :: Bool
  lastseenmessages :: V.Vector UUID

data ChatMessage
  message :: Text
  timestamp :: Int64
  salt :: Int64
  signature :: ByteString
  signedpreview :: Bool
  lastseenmessages :: V.Vector UUID

data PlayerSession
  sessionid :: UUID
  publickey :: ByteString
  signature :: ByteString

data ChunkBatchReceived -- Empty packet

data ClientStatus
  actionid :: Int32 via VarInt

data ClientTickEnd -- Empty packet

data ClientInformationPlay
  locale :: Text
  viewdistance :: Int8
  chatmode :: Int32 via VarInt 
  chatcolors :: Bool
  displayedskinparts :: Word8
  mainhand :: Int32 via VarInt
  enabletextfiltering :: Bool
  allowserverlistings :: Bool

data CommandSuggestionsRequest
  id :: Int32 via VarInt
  text :: Text

data AcknowledgeConfiguration -- Empty packet

data ClickContainerButton
  windowid :: Word8
  buttonid :: Word8

data ClickContainer
  windowid :: Word8
  stateid :: Int32 via VarInt
  slot :: Int16
  button :: Int8
  mode :: Int32 via VarInt
  slots :: V.Vector Slot

data CloseContainerServerbound
  windowid :: Word8

data ChangeContainerSlotState
  windowid :: Word8
  slot :: Int16
  state :: Int8

data CookieResponsePlay
  key :: Text
  value :: Text

data DebugSampleSubscription
  data_ :: ByteString via TakeRest

data EditBook
  hand :: Int32 via VarInt
  pages :: V.Vector Text
  title :: Text

data QueryEntityTag
  transactionid :: Int32 via VarInt
  entityid :: Int32 via VarInt

data Interact
  entityid :: Int32 via VarInt
  type_ :: Int32 via VarInt
  target :: V3 Float
  hand :: Int32 via VarInt
  sneaking :: Bool

data JigsawGenerate
  location :: Position
  levels :: Int32 via VarInt
  keepjigsaws :: Bool

data LockDifficulty
  locked :: Bool

data SetPlayerPosition
  position :: V3 Double
  rotation :: V2 Float
  flags :: Word8
  teleportid :: Int32 via VarInt

data SetPlayerPositionAndRotation
  position :: V3 Double
  rotation :: V2 Float
  flags :: Word8
  teleportid :: Int32 via VarInt

data SetPlayerRotation
  rotation :: V2 Float
  onground :: Bool

data SetPlayerMovementFlags
  flags :: Word8

data MoveVehicleServerbound
  position :: V3 Double
  rotation :: V2 Float

data PaddleBoat
  leftpaddleturning :: Bool
  rightpaddleturning :: Bool

data PickItemFromBlock
  location :: Position

data PickItemFromEntity
  entityid :: Int32 via VarInt

data PingRequestPlay
  id :: Int64

data PlaceRecipe
  windowid :: Word8
  recipe :: Text
  makeall :: Bool

data ServerboundPlayerAbilities
  flags :: Word8

data PlayerAction
  actionid :: Int32 via VarInt
  location :: Position
  face :: Int8
  sequence :: Int32 via VarInt

data PlayerCommand
  entityid :: Int32 via VarInt
  actionid :: Int32 via VarInt
  data_ :: Int32

data PlayerInput
  sideways :: Float
  forward :: Float
  flags :: Word8

data PlayerLoaded -- Empty packet

data PongPlay
  id :: Int64

data ChangeRecipeBookSettings
  bookid :: Int32 via VarInt
  bookopen :: Bool
  filteractive :: Bool

data SetSeenRecipe
  recipe :: Text

data RenameItem
  name :: Text

data ResourcePackResponsePlay
  status :: Int32 via VarInt

data SeenAdvancements
  action :: Int32 via VarInt
  tabid :: Text

data SelectTrade
  slot :: Int32 via VarInt

data SetBeaconEffect
  primaryeffect :: Int32 via VarInt
  secondaryeffect :: Int32 via VarInt

data SetHeldItemServerbound
  slot :: Word8

data ProgramCommandBlock
  location :: Position
  command :: Text
  mode :: Int32 via VarInt
  flags :: Word8

data ProgramCommandBlockMinecart
  entityid :: Int32 via VarInt
  command :: Text
  trackoutput :: Bool

data SetCreativeModeSlot
  slot :: Int16
  slotdata_ :: Slot

data ProgramJigsawBlock
  location :: Position
  name :: Text
  target :: Text
  pool :: Text
  finalstate :: Text
  jointtype :: Text

data ProgramStructureBlock
  location :: Position
  action :: Int32 via VarInt
  mode :: Int32 via VarInt
  name :: Text
  offset :: V3 Int8
  size :: V3 Int8
  mirror :: Int32 via VarInt
  rotation :: Int32 via VarInt
  metadata_ :: Text
  integrity :: Float
  seed :: Int64 via VarLong
  flags :: Word8

data UpdateSign
  location :: Position
  isfronttext :: Bool
  lines :: V.Vector Text

data SwingArm
  hand :: Int32 via VarInt

data TeleportToEntity
  targetentityid :: Int32 via VarInt

data UseItemOn
  hand :: Int32 via VarInt
  location :: Position
  face :: Int32 via VarInt
  cursor :: V3 Float
  insideblock :: Bool

data UseItem
  hand :: Int32 via VarInt
  sequence :: Int32 via VarInt
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
