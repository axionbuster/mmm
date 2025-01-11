-- | dummy Text Component newtype (not implemented yet)
module M.J.TextComponent (TextComponent (..)) where

import Control.DeepSeq
import Data.Data
import GHC.Generics
import M.J.NBT
import M.Pack

-- | WARNING: not implemented yet, but is still usable as a placeholder
-- because formally a \"text component\" is just an NBT tag
newtype TextComponent = TextComponent {untextcomponent :: Tg}
  deriving stock (Eq, Ord, Show, Generic, Typeable, Data)
  deriving newtype (Pack, Unpack, NFData)
