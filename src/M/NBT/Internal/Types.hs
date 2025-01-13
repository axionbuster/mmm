-- |
-- Module: M.NBT.Internal.Types
-- Description: Core NBT type definitions
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Internal type definitions for Named Binary Tag (NBT) format used in
-- Minecraft, including tag types and container types.
module M.NBT.Internal.Types (Ty (..), Tg (..), getty) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Data
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.Int
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.Pack

-- | NBT tag types
data Ty
  = -- | special type for the end of a compound tag
    TEnd
  | TByte
  | TShort
  | TInt
  | TLong
  | TFloat
  | TDouble
  | TByteArray
  | TString
  | TList
  | TCompound
  | TIntArray
  | TLongArray
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data, Lift)
  deriving stock (Enum, Bounded)
  deriving anyclass (Hashable, NFData)

-- tag type serialization / deserialization is defined here

instance Pack Ty where
  pack = pack @Word8 . fromIntegral . fromEnum
  {-# INLINE pack #-}

instance Unpack Ty where
  unpack = enumindex <$> unpack @(EnumIndex Word8 Ty)
  {-# INLINE unpack #-}

-- | NBT tag
data Tg where
  -- | used for internal purposes only
  End :: Tg
  Byte :: Int8 %1 -> Tg
  Short :: Int16 %1 -> Tg
  Int :: Int32 %1 -> Tg
  Long :: Int64 %1 -> Tg
  Float :: Float %1 -> Tg
  Double :: Double %1 -> Tg
  ByteArray :: ByteString %1 -> Tg
  String :: Text %1 -> Tg
  -- | a homogeneous list of tags
  List :: Ty %1 -> (V.Vector Tg) %1 -> Tg
  Compound :: HashMap Text Tg %1 -> Tg
  IntArray :: (VU.Vector Int32) %1 -> Tg
  LongArray :: (VU.Vector Int64) %1 -> Tg
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data)
  deriving anyclass (NFData)

-- | get the type of a tag
getty :: Tg -> Ty
getty End = TEnd
getty (Byte _) = TByte
getty (Short _) = TShort
getty (Int _) = TInt
getty (Long _) = TLong
getty (Float _) = TFloat
getty (Double _) = TDouble
getty (ByteArray _) = TByteArray
getty (String _) = TString
getty (List _ _) = TList
getty (Compound _) = TCompound
getty (IntArray _) = TIntArray
getty (LongArray _) = TLongArray
{-# INLINE getty #-}
