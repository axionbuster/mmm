-- | This module contains types that are not implemented yet.
module M.TODO
  ( Slot,
    BossBarAction,
    CommandNode,
    ParticleData,
    TextComponent,
  )
where

import Control.DeepSeq
import Data.Data
import Data.Hashable
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.Pack
import Text.Printf

-- | internal helper type for types that are not implemented yet
newtype ErrorOnPackUnpack a = ErrorOnPackUnpack a
  deriving newtype (Eq, Ord, Show, Read)
  deriving stock (Generic, Typeable, Data, Lift)
  deriving anyclass (NFData, Hashable)

-- | throws an error saying that the type is not implemented yet
instance (Typeable a) => Pack (ErrorOnPackUnpack a) where
  pack _ =
    error $
      printf
        "pack: not implemented for type %s"
        (show $ typeRep (Proxy @a))
  {-# INLINE pack #-}

-- | throws an error saying that the type is not implemented yet
instance (Typeable a) => Unpack (ErrorOnPackUnpack a) where
  unpack =
    error $
      printf
        "unpack: not implemented for type %s"
        (show $ typeRep (Proxy @a))
  {-# INLINE unpack #-}

data BossBarAction
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data)
  deriving anyclass (NFData, Hashable)
  deriving (Pack, Unpack) via ErrorOnPackUnpack BossBarAction

data CommandNode
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data)
  deriving anyclass (NFData, Hashable)
  deriving (Pack, Unpack) via ErrorOnPackUnpack CommandNode

data Slot
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data)
  deriving anyclass (NFData, Hashable)
  deriving (Pack, Unpack) via ErrorOnPackUnpack Slot

data ParticleData
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data)
  deriving anyclass (NFData, Hashable)
  deriving (Pack, Unpack) via ErrorOnPackUnpack ParticleData

data TextComponent
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data)
  deriving anyclass (NFData, Hashable)
  deriving (Pack, Unpack) via ErrorOnPackUnpack TextComponent

data MapIcon
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data)
  deriving anyclass (NFData, Hashable)
  deriving (Pack, Unpack) via ErrorOnPackUnpack MapIcon

data MerchantOffer
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data)
  deriving anyclass (NFData, Hashable)
  deriving (Pack, Unpack) via ErrorOnPackUnpack MerchantOffer
