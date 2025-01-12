-- | "effectful" collision detection and resolution
--
-- = Parts
--
-- 1. The 'GetBlock' effect
-- 2. The 'Resolve' data type and 'resolve' function (the main part)
-- 3. The 'NewlyTouchingGround' data type and 'updonground' function
--
-- = Usage
--
-- 1. Use 'getblock' to get a block's shape at integer coordinates
-- 2. Use 'resolve' to detect and resolve collision
-- 3. Use 'updonground' to update the on-ground status (from \#2)
module M.Collision.Effectful
  ( GetBlock (..),
    getblock,
    Resolve (..),
    resolve,
    _respos,
    _resdis,
    _restou,
    NewlyTouchingGround (..),
    updonground,
    islanding,
    istakingoff,
  )
where

import Control.Lens hiding (index)
import Control.Monad
import Control.Monad.Fix
import Data.Bits
import Data.Coerce
import Data.Data
import Data.Foldable
import Data.Functor.Rep
import Data.Hashable
import Data.Kind
import Data.List (nub)
import Data.Semigroup
import Data.Traversable
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Exception
import Effectful.Reader.Static
import GHC.Generics (Generic)
import Linear
import M.Collision.Internal.Face
import M.Collision.Internal.March
import M.Collision.Pure
import Prelude hiding (break)

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

-- | \'upgrade\' a boolean: find @y@ as in @y CMP x || y == x@
-- but if @CMP@ is not 'EQ', prefer @y /= x@
--
-- used to implement 'updonground'
boolupgr ::
  -- | comparison (@CMP@)
  Ordering ->
  -- | the @x@
  Bool ->
  -- | left-hand side of the @CMP@ comparison
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

-- internal control-flow exception because i don't think i can use call/cc
newtype EarlyExit a = EarlyExit (Resolve a)
  deriving stock (Generic, Typeable)
  deriving anyclass (Exception)

instance Show (EarlyExit a) where
  show _ = "EarlyExit"

-- | newly touching ground?
--
-- - 'LT' means it taking off from the ground
-- - 'EQ' means it should maintain the previous state
-- - 'GT' means it landing on the ground
newtype NewlyTouchingGround = NewlyTouchingGround {newonground :: Ordering}
  deriving newtype (Show, Eq, Ord, Enum, Bounded, Hashable)
  deriving stock (Data, Generic, Typeable)

-- | is it landing?
islanding :: NewlyTouchingGround -> Bool
islanding = (== GT) . newonground
{-# INLINE islanding #-}

-- | is it taking off?
istakingoff :: NewlyTouchingGround -> Bool
istakingoff = (== LT) . newonground
{-# INLINE istakingoff #-}

-- | get a block's shape at integer coordinates (dynamic effect)
data GetBlock (f :: Type -> Type) a :: Effect where
  -- | get a block's shape at integer coordinates
  GetBlock :: !(V3 Int) %1 -> GetBlock f a m (Maybe (f a))

type instance DispatchOf (GetBlock f a) = Dynamic

-- | get a block's shape at integer coordinates
getblock ::
  (HasCallStack, GetBlock f a :> ef) =>
  -- | integer coordinates
  V3 Int ->
  -- | if (relevant) block exists, return its shape
  --
  -- what block is \"relevant\" is up to the implementation
  Eff ef (Maybe (f a))
getblock = send . GetBlock
{-# INLINE getblock #-}

-- | detect and resolve collision
resolve ::
  ( Shape s,
    RealFloat n,
    Epsilon n,
    Typeable n,
    GetBlock s n :> ef
  ) =>
  -- | shape of the object who is moving
  s n ->
  -- | attempted displacement
  V3 n ->
  -- | new resolution
  --
  -- unless it got stuck, the new displacement should be zero
  Eff ef (Resolve n)
resolve myself disp =
  let res0 = Resolve (scenter myself) disp (NewlyTouchingGround EQ)
      fps =
        fmap ((slocorner myself +) . fmap fromIntegral) do
          facepoints
            (ceiling <$> sdimensions myself)
            (round . signum <$> disp)
   in catch
        do
          if nearZero disp
            then pure res0
            else runReader fps . resolve' myself $ res0
        do \(EarlyExit res1) -> pure res1

-- the actual implementation of 'resolve'
resolve' ::
  forall s n ef.
  ( Shape s,
    RealFloat n,
    Epsilon n,
    Typeable n,
    GetBlock s n :> ef,
    -- face points
    Reader [V3 n] :> ef
  ) =>
  s n ->
  Resolve n ->
  Eff ef (Resolve n)
resolve' =
  -- this is a loop that will run until the displacement is resolved
  -- (i.e., until it stops moving due to resolution or being blocked)
  fix \cont myself resolution -> do
    -- sample some points off the relevant faces of the object
    -- we will grid march along the rays (of the displacement) shot
    -- from these points
    let disp = resdis resolution
        gofast =
          -- if the assumptions of the fast core algorithm are met,
          -- yeah, apply the fast core algorithm
          quadrance disp <= 1
            || ( abs (disp ^. _x) == 1
                   && abs (disp ^. _z) == 1
                   && abs (disp ^. _y) <= 1
               )
        core
          | gofast = fastcore
          | otherwise = slowcore
    fps <- ask @[V3 n] -- retrieve the face points
    -- if i'm currently in contact with something, i can freely
    -- move in the direction of the displacement, as part of the
    -- game physics; this is so i can unstuck myself out
    for_ (fmap floor <$> fps) do
      getblock >=> \case
        Just block
          | intersecting myself block ->
              -- i am indeed stuck, so i will just exit early
              throwIO $ EarlyExit resolution
        _ -> pure ()
    -- compute the times ("hits") at which the object will hit a block
    -- and then find the earliest hit
    mearliest <- core resolution myself
    -- feed back (or stop)
    case coerce mearliest of
      earliest
        | hitin01 earliest ->
            -- now correct the displacement; advance position
            let (!) = index
                delta = hittime earliest *^ disp
                collided = (/= 0) <$> hitnorm earliest
                resdis =
                  tabulate \i ->
                    if collided ! i
                      then 0 -- collision cancels out the displacement
                      else (1 - hittime earliest) * (disp ! i)
                respos = scenter myself + delta
                restou
                  | hitnorm earliest ^. _y > 0 = coerce GT -- hit the ground
                  | disp ^. _y > 0 = coerce LT -- on-ground becomes False
                  | otherwise = coerce EQ
                res = Resolve {respos, resdis, restou}
             in if or collided
                  && not (and collided)
                  && not (nearZero resdis)
                  then cont (translate delta myself) res
                  else pure $ res & _resdis .~ zero
      _ ->
        -- no collision, so apply the displacement
        pure
          Resolve
            { respos = respos resolution + disp,
              resdis = zero,
              restou =
                if disp ^. _y > 0
                  then coerce LT -- on-ground becomes False
                  else restou resolution -- (inherit previous decision)
            }

-- slow process ... use ray marching from each face point
-- collect all hits found from all rays
slowcore ::
  forall s n ef.
  ( Shape s,
    Epsilon n,
    RealFloat n,
    GetBlock s n :> ef,
    Reader [V3 n] :> ef
  ) =>
  Resolve n -> s n -> Eff ef (Min (Hit' n))
slowcore res myself =
  ask @[V3 n] >>= \fps ->
    mconcat <$> for fps \fp ->
      -- ray marching algorithm:
      --   1. shoot ray from each face point (fp)
      --   2. track ray's path through grid cubes
      --   3. stop at first hit or when ray length > displacement
      --
      -- grid traversal details:
      --   - ray can enter multiple cubes simultaneously:
      --     * 1 cube: through face
      --     * 2 cubes: through edge (if not parallel to axis)
      --     * 3 cubes: through corner
      --   - cubes are checked for collision
      --   - early exit on first collision
      --
      -- note: behavior unspecified when ray travels exactly
      -- along grid edge or plane, but will return something
      -- for sake of completeness
      (March 0 undefined [floor <$> fp] : march fp (resdis res))
        & fix \contrm ->
          \case
            -- some improper displacements can cause termination
            -- of ray marching (which should normally be infinite)
            [] -> pure mempty
            -- no hit
            March t _ _ : _ | t > 1 -> pure mempty
            -- entering (a) grid cube(s), are there any blocks in them?
            March _ _ cubes : rm ->
              cubes & fix \contcb -> \case
                -- ran out of grid cubes, so, no;
                -- need to go one step further along the ray
                [] -> contrm rm
                -- let's check the block at the grid cube
                cb : cb' ->
                  chkcol
                    (hitting (resdis res) myself)
                    (contcb cb')
                    cb

-- fast collision detection for small movements (length <= 1 or
-- diagonal |x|=|z|=1)
--
-- key idea: instead of ray marching, we check potential collision blocks
-- directly by considering the movement box - the space swept by the object
-- during movement
--
-- algorithm:
--   for each face point of the moving object:
--     1. find start (bef) and end (aft) positions
--     2. generate test points at corners of movement box:
--        - movement box is the space between bef and aft positions
--        - use binary counting (0-7) to pick coordinates:
--          * bit 0 = x: choose between bef.x (0) or aft.x (1)
--          * bit 1 = y: choose between bef.y (0) or aft.y (1)
--          * bit 2 = z: choose between bef.z (0) or aft.z (1)
--        - skip coordinates where movement is negligible
--     3. for each test point:
--        - get block at that position
--        - test if moving object would hit that block
--        - collect all hits found
--
-- example: moving +X+Y (need = <True,True,False>)
--   we only check 4 corners instead of 8 since Z movement = 0:
--   * 000 -> (bef.x, bef.y, bef.z)
--   * 001 -> (aft.x, bef.y, bef.z)
--   * 010 -> (bef.x, aft.y, bef.z)
--   * 011 -> (aft.x, aft.y, bef.z)
fastcore ::
  forall s n ef.
  ( Shape s,
    Epsilon n,
    RealFloat n,
    GetBlock s n :> ef,
    Reader [V3 n] :> ef
  ) =>
  Resolve n -> s n -> Eff ef (Min (Hit' n))
fastcore res myself =
  ask @[V3 n] -- face points (world coordinates)
    >>= fmap (mconcat . concat) <$> traverse \bef ->
      let dis = resdis res
          aft = bef + dis -- before (bef) and after (aft) positions
          -- moving in {x,y,z} directions?
          need = not . nearZero <$> dis
          -- note: chkcol uses continuation-passing to collect hits
          -- we use 'pure mempty' as the final continuation
          hits = for targets $ chkcol (hitting dis myself) (pure mempty)
            where
              (!) = index
              -- select position component (bef/aft) based on bit
              pos n i
                | need ! i && testBit n (V3 0 1 2 ! i) = aft ! i
                | otherwise = bef ! i
              -- see \#2 in the algorithm description
              targets = nub $ fmap floor . tabulate . pos <$> [0 .. 7 :: Int]
       in hits

-- internal helper function for 'resolve'
-- check if i hit a block at the grid cube (and check below for tall blocks)
chkcol ::
  (GetBlock s n :> ef, Shape s, Fractional n, Ord n) =>
  -- check for hit given block shape (shape has absolute coordinates)
  (s n -> Hit n) ->
  -- continuation for continuing to next grid cube
  Eff ef (Min (Hit' n)) ->
  -- where (block coordinates)
  V3 Int ->
  Eff ef (Min (Hit' n))
chkcol chkhit continue cb = do
  let chkbelow =
        -- go below and check too
        getblock (cb & _y -~ 1) <&> \case
          Just blockbelow
            | hit <- chkhit blockbelow,
              hitin01 hit ->
                (coerce hit <>)
          _ -> id
      {-# INLINE chkbelow #-}
      True ? action = action
      _ ? _ = pure id
      short b = shicorner b ^. _y < 0.5
  -- check if a block exists at the grid cube & is solid
  -- also just in case a tall block (like a fence)
  -- is there, we check the block below it
  getblock cb >>= \case
    Just block
      | hit <- chkhit block,
        hitin01 hit ->
          -- oh, we hit something
          (short block ? chkbelow) <*> ((coerce hit <>) <$> continue)
      | otherwise ->
          -- a block is there but we don't hit it
          (short block ? chkbelow) <*> continue
    -- no block at the grid cube
    Nothing -> chkbelow <*> continue
