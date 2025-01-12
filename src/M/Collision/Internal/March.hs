-- | march along a ray, finding all intersections with grid points
module M.Collision.Internal.March (March (..), march) where

import Control.Lens hiding (index)
import Control.Monad.Fix
import Control.Monad.ST.Lazy
import Data.Foldable
import Data.Functor
import Data.Functor.Rep
import Data.STRef.Lazy
import Linear hiding (trace)
import Prelude hiding (read)

-- | convert negative zero to positive zero
nonegzero :: (RealFloat a) => a -> a
nonegzero x | isNegativeZero x = 0 -- positive zero
nonegzero x = x

isfinite :: (RealFloat a) => a -> Bool
isfinite x = not (isNaN x || isInfinite x)

-- | apply Kahan's compensated sum to two numbers
add ::
  (Num a) =>
  -- | x
  a ->
  -- | y
  a ->
  -- | compensator
  a ->
  -- | (x + y, compensator)
  (a, a)
add x y c =
  let y' = y - c
      u = x + y'
      c' = u - x - y'
   in (u, c')

-- | intermediate data structure for 'march'
data I f a = I
  { -- | delta time to next intersection
    itim :: !a,
    -- | new current position
    icur :: !(f a),
    -- | compensator for 'icur'
    icom :: !(f a),
    -- | grid points intersected
    igrid :: ![f Int]
  }

-- | 'march' data structure
data March f a = March
  { -- | total time
    mtot :: a,
    -- | grid intersection (lies on boundaries of grid cells)
    --
    -- \'pct\' is for \'punctum\', which is Latin for point
    mpct :: f a,
    -- | grid points (e.g., cubes, squares) intersected
    mict :: [f Int]
  }

-- | march along a line segment, finding all intersections
-- with grid squares or cubes (depending on the dimensionality)
-- as well as the time it takes to reach each intersection
-- and the cubes that are intersected
--
-- the cubes are represented by their low corner coordinates
--
-- in 2D, when a point is intersected, the two squares about
-- the point that the line (that extends rhe ray) does NOT
-- intersect will be included. it's because this routine is used
-- for collision detection
--
-- in 3D, there are many edge cases, but generally only the cubes
-- needed for collision detection are returned. so about
-- a corner, three cubes will be returned; abour an edge,
-- two (assuming ray is not parallel to a coordinate plane)
--
-- a compensated sum is used to reduce floating point error.
-- the compensation applies to the coordinates and times
--
-- the returned list being infinite, it is recommended to
-- use 'take' to limit the number of points to be computed
--
-- the starting point is not included in the list unless it
-- happens to be a grid intersection
--
-- if the direction is (near) zero, or if any component of the
-- direction is not finite, then the function will return an empty list
march ::
  forall f a.
  ( Foldable f,
    Representable f,
    Rep f ~ E f,
    RealFloat a,
    Epsilon a
  ) =>
  -- | starting point. use either f ~ 'V2' or f ~ 'V3' or other 'Representable'
  -- vector types where 'fmap f x' agrees with
  --
  -- @'tabulate' \\i -> f ('index' x i))@
  f a ->
  -- | direction (no need to be normalized)
  f a ->
  -- | list of (total time, point, [grid point]) pairs
  [March f a]
march _ direction | (not . all isfinite) direction = []
march _ direction | all nearZero direction = []
march start (fmap nonegzero -> direction) = runST do
  let fi = fromIntegral :: Int -> a
      (!) = index
      new = newSTRef
      read = readSTRef
      write = writeSTRef
      modify = modifySTRef
      lift2 f x y = tabulate @f \i -> f (x ! i) (y ! i)
      minimum_ = foldr1 \a b ->
        if
          | isNaN a -> b
          | isNaN b -> a
          | otherwise -> min a b -- if both are NaN, then pick either
      computesig d = f . floor . signum <$> d
        where
          -- for difficult-to-explain reasons, you need to give a stationary (0)
          -- displacement component a fake signum of +1 in order to avoid
          -- getting -Infinity when it shouldn't
          -- \^ FIXME: this explanation could be outdated
          f 0 = 1
          f x = x
      -- round toward opposite direction of a signum component
      round_ (-1) = ceiling
      round_ 1 = floor
      round_ _ = error "signum neither -1 nor 1"
      -- vector of functions that each generate a grid point
      -- (specialized for each dimension). confused yet? yeah, it's
      -- really hard to explain
      gengridpoints = tabulate \i sig v ->
        -- grid point (compute from later-determined cur value)
        -- this was the hardest part to figure out
        --
        -- 1
        -- on (round_ (-(sig ! j))) ... you need a greatest integer
        -- less than (or least integer greater than) the current
        -- coordinate, which is either ((subtract 1) . ceiling)
        -- or ((+ 1) . floor), depending on the OPPOSITE side of the
        -- signum of the direction. the subtract 1 vs. + 1 will be done
        -- when we subtract the sig component from the grid point
        -- later on
        --
        -- 2
        -- on (max 0 <$> sig) ... if you flip the coordinate system
        -- by some axis, the grid points are still numbered by the
        -- bottom left (& etc) corner, so you need to subtract 1
        -- from the grid point if the direction is negative. this
        -- means to add 1 to the sig component -> hence max 0
        let roundedv = tabulate \j -> round_ (-(sig ! j)) (v ! j)
         in lift2 (-) roundedv (max 0 <$> sig) & el i +~ sig ! i
      -- 'inter' is the main loop logic
      inter sig dir com cur =
        -- mechanism:
        -- using the parametric equation of the line
        -- find the closest intersection with the grid -> get 'time' value
        -- then use the 'time' to get the coordinates of the intersection
        let times = tabulate \i ->
              let r = round_ $ sig ! i
                  u = fi (r (cur ! i) + sig ! i) - cur ! i
               in (u / dir ! i, (gengridpoints ! i) sig)
            t = minimum_ $ filter (> 0) $ map fst $ toList times
            -- funcs to compute grid coordinates at the time of intersection
            -- if many intersected simultaneously, return all of them
            eqtim = nearZero . subtract t
            gridcoordsf = fmap snd $ filter (eqtim . fst) $ toList times
            -- elementwise error-compensated vector addition
            vadd v w = tabulate \i -> add (v ! i) (w ! i) (com ! i)
            -- update current position and compensator
            s = vadd cur $ dir <&> (* t)
            icur_ = fst <$> s
            icom = snd <$> s
            -- properly round coordinates meant to be integers
            icur = tabulate \i ->
              let n = icur_ ! i
               in if eqtim $ fst $ times ! i
                    then fi $ round n
                    else n
         in I {itim = t, icur, icom, igrid = gridcoordsf <&> ($ icur)}
  cur <- new start -- current position
  com <- new $ tabulate $ const 0 -- Kahan sum compensator for cur
  tot <- new (0, 0) -- (total time, compensator)
  -- IMPORTANT NOTE:
  -- at start, we go BACKWARD and find the first intersection
  -- and then go forward from there to resume the normal process
  -- this extremely hacky way of doing things is necessary
  -- to generate the grid coordinates of the starting point in a
  -- way that is consistent with the rest of the function
  do
    -- go backward and set cur, com and tot, but
    -- don't append output to the returned list
    let d = (* (-1)) <$> direction
    I {itim, icur, icom} <- inter (computesig d) d <$> read com <*> read cur
    write tot (-itim, 0)
    write cur icur
    write com icom
  -- loop
  fix \this -> do
    let sig = computesig direction
    I {itim, icur, icom, igrid} <- inter sig direction <$> read com <*> read cur
    (t, _) <- modify tot (uncurry (add itim)) *> read tot
    write cur icur
    write com icom
    (March t icur igrid :) <$> this
{-# INLINEABLE march #-}
{-# SPECIALIZE march :: V3 Double -> V3 Double -> [March V3 Double] #-}
{-# SPECIALIZE march :: V3 Float -> V3 Float -> [March V3 Float] #-}
