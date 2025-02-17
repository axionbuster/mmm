-- |
-- Module: M.Misc
-- Description: Common miscellaneous types for Minecraft protocol
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Defines common types used across the Minecraft protocol implementation,
-- including teleportation flags and sound events.
module M.Misc (TeleportFlags (..), SoundEvent (..)) where

import Control.DeepSeq
import Data.Bits
import Data.Data
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.Pack
import M.PkMacro

-- | flags for teleporting an entity
--
-- 16+ bits
data TeleportFlags = TeleportFlags
  { tprelx :: Bool,
    tprely :: Bool,
    tprelz :: Bool,
    tprelyaw :: Bool,
    tprelpitch :: Bool,
    tprelvelx :: Bool,
    tprelvely :: Bool,
    tprelvelz :: Bool,
    tprelvelyaw :: Bool,
    -- | from The Minecraft Wiki: \"Rotate velocity according to the change in
    -- rotation, before applying the velocity change in this packet. Combining
    -- this with absolute rotation works as expected—the difference in rotation
    -- is still used.\"
    tprotvelfirst :: Bool
  }
  deriving stock (Eq, Ord, Show, Read, Generic, Lift, Data, Typeable)
  deriving anyclass (Hashable, NFData)

-- | do NOT use 8-bit packing for this type.
-- use a type that is at least 16 bits wide
instance (Bits i, Integral i, Pack i, Unpack i) => Bitreppable i TeleportFlags

setdefaultderives
addproperderives [''NFData, ''Typeable, ''Show, ''Eq]

[pkmacro|
  data SoundEvent {
    soundname :: Text via Identifier,
    fixedrange :: Maybe Float,
  }
  |]
