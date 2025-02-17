module M.PkMacroTest where

import Data.Int
import GHC.Generics
import M.PkMacro
import M.Pack

newtype AAA = AAA Int32
  deriving stock (Generic)
  deriving newtype (Pack, Unpack)

setdefaultderives

[pkmacro|
data A {
  f1 :: Int32,
  f2 :: Int32 via AAA,
}

data B {
  f3 :: Int32,
}

data C {}
  |]
