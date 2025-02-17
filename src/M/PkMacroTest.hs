module M.PkMacroTest where

import Data.Int
import GHC.Generics
import M.PkMacro
import M.Pack

newtype AAA = AAA Int32
  deriving stock (Generic)
  deriving newtype (Pack, Unpack)

[pkmacro|
data A {
  f1 :: Int32,
  f2 :: Int32 via AAA,
  deriving (Generic)
    and shadow deriving (Pack, Unpack)
    with (Generic, Pack, Unpack)
}

data B {
  f3 :: Int32,
  deriving (Generic)
}
  |]
