-- | Define some common miscellaneous types.
module M.J.Misc (TeleportFlags (..), SoundEvent (..)) where

import Control.DeepSeq
import Data.Bits
import Data.Data
import Data.Hashable
import Data.Serde.QQ
import Data.Text (Text)
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.Pack

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

-- TODO for th-serde:
--  support doc comments

[serde|
.derive
  Eq Ord Show Read Hashable NFData
  Generic Lift Data Typeable

data SoundEvent
  soundname :: Text via Identifier
  fixedrange :: Maybe Float
  |]

-- 1st argument: TH declarations generator
-- 2nd argument: preparation derivations for shadow type (SoundEvent__)

-- in English:
--  first, derive the Pack, Unpack, and Generic instances for SoundEvent__
--  (the shadow type)
--
--  then, derive the Pack, Unpack, and Generic instances for SoundEvent
--  by borrowing the shadow type's instances
runusercoercion derivepackunpack [''Pack, ''Unpack, ''Generic]
