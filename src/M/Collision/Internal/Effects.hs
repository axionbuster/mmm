module M.Collision.Internal.Effects
  ( GetBlock (..),
    getblock,
    Resolve (..),
    _resdis,
    _respos,
    _restou,
    NewlyTouchingGround (..),
    updonground,
    islanding,
    istakingoff,
  )
where

import Control.Lens
import Data.Coerce
import Data.Data
import Data.Hashable
import Data.Kind
import Effectful
import Effectful.Dispatch.Dynamic
import GHC.Generics (Generic)
import Linear

-- | collision resolution data type
data Resolve a = Resolve
  { -- | final position
    respos :: !(V3 a),
    -- | remaining displacement
    resdis :: !(V3 a),
    -- | what to do with the on-ground status
    restou :: !NewlyTouchingGround
  }
  deriving (Show, Eq, Generic, Typeable, Functor, Hashable, Data)

-- | lens for 'Resolve' position
_respos :: Lens' (Resolve a) (V3 a)
_respos = lens respos \x y -> x {respos = y}
{-# INLINE _respos #-}

-- | lens for 'Resolve' displacement
_resdis :: Lens' (Resolve a) (V3 a)
_resdis = lens resdis \x y -> x {resdis = y}
{-# INLINE _resdis #-}

-- | lens for 'Resolve' newly touching ground
_restou :: Lens' (Resolve a) NewlyTouchingGround
_restou = lens restou \x y -> x {restou = y}
{-# INLINE _restou #-}

-- | newly touching ground?
newtype NewlyTouchingGround = NewlyTouchingGround {newonground :: Ordering}
  deriving newtype (Show, Eq, Ord, Enum, Bounded, Hashable)
  deriving stock (Data, Generic, Typeable)

-- | get a block's shape at integer coordinates (dynamic effect)
data GetBlock (f :: Type -> Type) a :: Effect where
  -- | get a block's shape at integer coordinates
  GetBlock :: !(V3 Int) %1 -> GetBlock f a m (Maybe (f a))

type instance DispatchOf (GetBlock f a) = Dynamic

-- | get a block's shape at integer coordinates
getblock ::
  (HasCallStack, GetBlock f a :> ef) =>
  V3 Int ->
  Eff ef (Maybe (f a))
getblock = send . GetBlock
{-# INLINE getblock #-}

-- | is it landing?
islanding :: NewlyTouchingGround -> Bool
islanding = (== GT) . newonground
{-# INLINE islanding #-}

-- | is it taking off?
istakingoff :: NewlyTouchingGround -> Bool
istakingoff = (== LT) . newonground
{-# INLINE istakingoff #-}

-- \'upgrade\' a boolean: find @y@ as in @y CMP x || y == x@
boolupgr ::
  Ordering ->
  Bool ->
  Bool
boolupgr LT True = False
boolupgr LT False = False
boolupgr EQ x = x
boolupgr GT True = True
boolupgr GT False = True
{-# INLINE boolupgr #-}

-- | \'update\' the on-ground status
updonground :: NewlyTouchingGround -> Bool -> Bool
updonground = coerce boolupgr
{-# INLINE updonground #-}
