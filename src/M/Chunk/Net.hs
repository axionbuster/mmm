-- |
-- Module: M.Chunk.Net
-- Description: Network serialization types for chunk data
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Defines data types for chunk and lighting data used in network serialization.
-- Includes block entities and packed coordinate representations.
module M.Chunk.Net (ChunkData (..), BlockEntity (..), LightData (..)) where

import Control.DeepSeq
import Data.Bits
import Data.ByteString (ByteString)
import Data.Data
import Data.Functor
import Data.Hashable
import Data.Int
import Data.Vector qualified as V
import Data.Word
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import Linear
import M.LEB
import M.NBT
import M.Pack
import M.PkMacro

-- only for internal use
newtype PackedXZ = PackedXZ {getpackedxz :: V2 Word8}
  deriving stock (Generic, Typeable, Data, Lift)
  deriving newtype (Eq, Ord, Show, Read, Hashable, NFData)

pxzx :: (Num a) => PackedXZ -> a
pxzx (PackedXZ (V2 x _)) = fromIntegral x
{-# INLINE pxzx #-}

pxzz :: (Num a) => PackedXZ -> a
pxzz (PackedXZ (V2 _ z)) = fromIntegral z
{-# INLINE pxzz #-}

instance Pack PackedXZ where
  pack p = pack @Word8 $ ((pxzx p .&. 15) .<<. 4) .|. (pxzz p .&. 15)
  {-# INLINEABLE pack #-}

instance Unpack PackedXZ where
  unpack = unpack @Word8 <&> \w -> PackedXZ (V2 (w .>>. 4) (w .&. 15))
  {-# INLINEABLE unpack #-}

setdefaultderives
addproperderives [''NFData, ''Typeable, ''Show, ''Eq]

[pkmacro|
data BlockEntity {
  bexz :: V2 Word8 via PackedXZ,
  berelheight :: Int16,
  betype :: Int32 via VarInt,
  bedata :: Tg,
}

data ChunkData {
  cdhmaps :: Tg,
  cddata :: ByteString,
  cdblockentities :: V.Vector BlockEntity,
}

data LightData {
  ldskymask :: Bitset,
  ldblockmask :: Bitset,
  ld0skymask :: Bitset,
  ld0blockmask :: Bitset,
  -- inner ByteStrings are always 2048 bytes long by contract
  ldskylights :: V.Vector ByteString,
  ldblocklights :: V.Vector ByteString,
}
 |]
