-- | dummy for Slot data (not implemented yet)
module M.J.Slot (Slot (..)) where

import Control.DeepSeq
import Data.Data
import Data.Hashable
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.Pack

-- | WARNING: not implemented yet
data Slot = Slot
  deriving stock (Eq, Ord, Show, Generic, Typeable, Data, Lift)
  deriving anyclass (NFData, Hashable)

-- | throws an error saying that Slot is not implemented yet
instance Pack Slot where
  pack = error "Slot.pack: not implemented"
  {-# INLINE pack #-}

-- | throws an error saying that Slot is not implemented yet
instance Unpack Slot where
  unpack = error "Slot.unpack: not implemented"
  {-# INLINE unpack #-}
