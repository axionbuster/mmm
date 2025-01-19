-- |
-- Module: M.V769.Reg
-- Description: Protocol version 769 packet state registrations
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Defines packet state registrations for protocol version 769 using Template Haskell.
-- This module maps packet types to their numeric codes for different connection states:
-- 'handshake', 'status', 'login', 'configuration' and 'play'.
--
-- == States
--
-- * Handshake - Initial connection handshake
-- * Status - Server status/ping
-- * Login - Authentication and encryption
-- * Configuration - Server/client capability exchange
-- * Play - Main game state
--
-- Each state defines bidirectional packet mappings using the 'states' quasi-quoter.
-- Packet definitions include receive (client->server) and send (server->client) codes.
module M.V769.Reg
  ( handshake,
    status,
    login,
    configuration,
    play,
    ParserStates (..),
  )
where

import M.IO.TH
import M.V769.C qualified as C
import M.V769.H qualified as H
import M.V769.I qualified as I
import M.V769.L qualified as L
import M.V769.P qualified as P
import M.V769.S qualified as S

[states|
  -- name
  handshake
  -- name:hex recv:hex send
  H.HandshakePacket::0
  |]

[states|
  -- name
  status
  -- name:hex recv:hex send
  S.StatusRequest:0:
  S.StatusResponse::0
  I.KeepAlive::1  -- Now using shared KeepAlive type
  |]

[states|
  -- name
  login
  -- name:hex recv:hex send
  L.LoginStart:0:
  L.LoginDisconnect::0
  L.EncryptionRequest::1
  L.LoginSuccess::2
  L.LoginPluginRequest::3
  L.LoginPluginResponse:2:
  L.LoginAcknowledged:3:
  L.EncryptionResponse:1:
  |]

[states|
  -- name
  configuration
  -- name:hex recv:hex send
  C.CookieRequest::0
  I.PluginMessage:2:1
  C.Disconnect::2
  C.FinishConfiguration::3
  I.KeepAlive:4:4  -- Using shared KeepAlive
  I.Ping:5:5       -- Using shared Ping type
  C.ResetChat::6
  C.RegistryData::7
  C.RemoveResourcePack::8
  I.ResourcePack::9
  C.StoreCookie::a
  C.Transfer::b
  C.FeatureFlags::c
  C.UpdateTags::d
  C.KnownPacks:7:e
  C.CustomReportDetails::f
  C.ServerLinks::10

  C.ClientInformationConfiguration:0:
  C.CookieResponse:1:
  C.AcknowledgeFinishConfiguration:3:
  C.ResourcePackResponse:6:
  |]

[states|
  -- name
  play
  -- name:hex recv:hex send
  P.BundleDelimiter::0
  P.SpawnEntity::1
  P.SpawnExperienceOrb::2
  P.EntityAnimation::3
  P.AwardStatistics::4
  P.AcknowledgeBlockChange::5
  P.SetBlockDestroyStage::6
  P.BlockEntityData::7
  P.BlockAction::8
  P.BlockUpdate::9
  P.BossBar::a
  P.ChangeDifficulty::b
  P.ChunkBatchFinished::c
  P.ChunkBatchStarted::d
  P.ChunkBiomes::e
  P.ClearTitles::f
  P.CommandSuggestionsResponse::10
  P.Commands::11
  P.CloseContainer::12
  P.SetContainerContent::13
  P.SetContainerProperty::14
  P.SetContainerSlot::15
  P.CookieRequest::16
  P.SetCooldown::17
  P.ChatSuggestions::18
  I.PluginMessage:14:19
  P.DamageEvent::1a
  P.DebugSample::1b
  P.DeleteMessage::1c
  P.Disconnect::1d
  P.DisguisedChatMessage::1e
  P.EntityEvent::1f
  P.TeleportEntity::20
  P.Explosion::21
  P.UnloadChunk::22
  P.GameEvent::23
  P.OpenHorseScreen::24
  P.HurtAnimation::25
  P.InitializeWorldBorder::26
  I.KeepAlive:1a:27
  P.ChunkDataAndUpdateLight::28
  P.WorldEvent::29
  P.ParticleEffect::2a
  P.UpdateLight::2b
  P.Login::2c
  P.MapData::2d
  P.MerchantOffers::2e
  P.UpdateEntityPosition::2f
  P.UpdateEntityPositionAndRotation::30
  P.MoveMinecartAlongTrack::31
  P.UpdateEntityRotation::32
  P.MoveVehicle::33
  P.OpenBook::34
  P.OpenScreen::35
  P.OpenSignEditor::36
  P.PlayPing::37
  P.PlayPingResponse::38
  P.PlaceGhostRecipe::39
  P.ClientboundPlayerAbilities::3a
  P.PlayerChatMessage::3b
  P.EndCombat::3c
  P.EnterCombat::3d
  P.CombatDeath::3e
  P.PlayerInfoRemove::3f
  P.PlayerInfoUpdate::40
  P.LookAt::41
  P.SynchronizePlayerPosition::42
  P.PlayerRotation::43
  P.RecipeBookAdd::44
  P.RecipeBookRemove::45
  P.RecipeBookSettings::46
  P.RemoveEntities::47
  P.RemoveEntityEffect::48
  P.ResetScore::49
  P.RemoveResourcePack::4a
  P.AddResourcePack::4b
  P.Respawn::4c
  P.SetHeadRotation::4d
  P.UpdateSectionBlocks::4e
  P.SelectAdvancementsTab::4f
  P.ServerData::50
  P.SetActionBarText::51
  P.SetBorderCenter::52
  P.SetBorderLerpSize::53
  P.SetBorderSize::54
  P.SetBorderWarningDelay::55
  P.SetBorderWarningDistance::56
  P.SetCamera::57
  P.SetCenterChunk::58
  P.SetRenderDistance::59
  P.SetCursorItem::5a
  P.SetDefaultSpawnPosition::5b
  P.DisplayObjective::5c
  P.SetEntityMetadata::5d
  P.LinkEntities::5e
  P.SetEntityVelocity::5f
  P.SetEquipment::60
  P.SetExperience::61
  P.SetHealth::62
  P.SetHeldItem::63
  P.UpdateObjectives::64
  P.SetPassengers::65
  P.SetPlayerInventorySlot::66
  P.UpdateTeams::67
  P.UpdateScore::68
  P.SetSimulationDistance::69
  P.SetSubtitleText::6a
  P.UpdateTime::6b
  P.SetTitleText::6c
  P.SetTitleAnimationTimes::6d
  P.EntitySoundEffect::6e
  P.SoundEffect::6f
  P.StartConfiguration::70
  P.StopSound::71
  P.StoreCookie::72
  P.SetTabListHeaderAndFooter::73

  P.ConfirmTeleportation:0:
  P.QueryBlockEntityTag:1:
  P.BundleItemSelected:2:
  P.ServerboundChangeDifficulty:3:
  P.AcknowledgeMessage:4:
  P.ServerboundChatCommand:5:
  P.SignedChatCommand:6:
  P.ChatMessage:7:
  P.PlayerSession:8:
  P.ChunkBatchReceived:9:
  P.ClientStatus:a:
  P.ClientTickEnd:b:
  P.ClientInformationPlay:c:
  P.CommandSuggestionsRequest:d:
  P.AcknowledgeConfiguration:e:
  P.ClickContainerButton:f:
  P.ClickContainer:10:
  P.CloseContainerServerbound:11:
  P.ChangeContainerSlotState:12:
  P.CookieResponsePlay:13:
  P.DebugSampleSubscription:15:
  P.EditBook:16:
  P.QueryEntityTag:17:
  P.Interact:18:
  P.JigsawGenerate:19:
  P.LockDifficulty:1b:
  P.SetPlayerPosition:1c:
  P.SetPlayerPositionAndRotation:1d:
  P.SetPlayerRotation:1e:
  P.SetPlayerMovementFlags:1f:
  P.MoveVehicleServerbound:20:
  P.PaddleBoat:21:
  P.PickItemFromBlock:22:
  P.PickItemFromEntity:23:
  P.PingRequestPlay:24:
  P.PlaceRecipe:25:
  P.ServerboundPlayerAbilities:26:
  P.PlayerAction:27:
  P.PlayerCommand:28:
  P.PlayerInput:29:
  P.PlayerLoaded:2a:
  P.PongPlay:2b:
  P.ChangeRecipeBookSettings:2c:
  P.SetSeenRecipe:2d:
  P.RenameItem:2e:
  P.ResourcePackResponsePlay:2f:
  P.SeenAdvancements:30:
  P.SelectTrade:31:
  P.SetBeaconEffect:32:
  P.SetHeldItemServerbound:33:
  P.ProgramCommandBlock:34:
  P.ProgramCommandBlockMinecart:35:
  P.SetCreativeModeSlot:36:
  P.ProgramJigsawBlock:37:
  P.ProgramStructureBlock:38:
  P.UpdateSign:39:
  P.SwingArm:3a:
  P.TeleportToEntity:3b:
  P.UseItemOn:3c:
  P.UseItem:3d:
  |]
