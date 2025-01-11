-- | dummy Text Component newtype (not implemented yet)
module M.J.TextComponent (TextComponent (..)) where

import Control.DeepSeq
import Data.Data
import GHC.Generics
import M.J.NBT
import M.Pack

-- | WARNING: not implemented yet, but is still usable as a placeholder
-- because formally a \"text component\" is just an NBT tag
--
-- see also: 'TextComponentJSON'
newtype TextComponent = TextComponent {untextcomponent :: Tg}
  deriving stock (Generic, Typeable, Data)
  deriving newtype (Eq, Ord, Show, Pack, Unpack, NFData)

-- | WARNING: not implemented yet, but is still usable as a placeholder
newtype TextComponentJSON = TextComponentJSON
  {untextcomponentjson :: TextComponent}
  deriving stock (Generic, Typeable, Data)
  deriving newtype (Eq, Ord, Show, NFData)

-- | throws an error saying that 'TextComponentJSON' is not implemented yet
instance Pack TextComponentJSON where
  pack = error "TextComponentJSON.pack: not implemented"
  {-# INLINE pack #-}

-- | throws an error saying that 'TextComponentJSON' is not implemented yet
instance Unpack TextComponentJSON where
  unpack = error "TextComponentJSON.unpack: not implemented"
  {-# INLINE unpack #-}
