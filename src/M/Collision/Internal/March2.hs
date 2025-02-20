-- |
-- Module: M.Collision.Internal.March2
-- Description: Grid-based ray marching implementation using digital differential analyzer
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Alternative implementation of ray marching algorithm focused on performance.
module M.Collision.Internal.March2
  ( VHit (..),
    isnotahit,
    isahit,
    march,
  )
where

import Control.Lens
import Data.Int
import Debug.Trace
import Linear
import Text.Printf

-- | voxel hit data structure. may encode hit or no hit.
data VHit i a = VHit
  { -- | finite nonnegative float on hit; otherwise
    -- will be positive infinity and there will be
    -- no hit (and the other fields will be
    -- defined, but meaningless and unspecified)
    vhittim :: !a,
    -- | integer coordinate location of where it hit
    vhitloc :: !(V3 i),
    -- | the normal vectors (signum; opposite to
    -- the displacement)
    vhitnor :: !(V3 i)
  }
  deriving (Eq, Show)

-- | decide if something is not a hit
isnotahit :: (RealFloat a) => VHit i a -> Bool
isnotahit = isInfinite . vhittim

-- | decide if something is a hit
isahit :: (RealFloat a) => VHit i a -> Bool
isahit = not . isnotahit

-- | march along an integer grid using a digital differential
-- analyzer (DDA)-based algorithm
march ::
  (RealFloat a, Integral i, Monad m) =>
  -- | test for stoppage ('True' to stop)
  (V3 i -> m Bool) ->
  -- | direction, any physical dimension
  V3 a ->
  -- | initial position, dimensionless
  V3 a ->
  -- | number of iterations (maximum)
  Int ->
  -- | hit information (success or failure)
  m (VHit i a)
march test ray pos0 = go dis0 (floor <$> pos0) 0 0
  where
    -- prevent NaNs. for the comparison in finding 'closest'
    -- NaNs need to have no influence and therefore actually
    -- must be Infinity.
    mul1 p q =
      [ if isInfinite a || isInfinite b
          then 1 / 0
          else a * b
      | a <- p
      | b <- q
      ]
    -- ////// dimensional analysis //////
    --
    -- . dimensions of "pos" and "pos0"
    --
    -- by comparing these three expressions:
    --  dis0 = rcp `mul1` ... pos0 ...
    --       = [X] * [L]
    --  rcp  = [X]
    --  dis' = dis + [1] `mul1` rcp
    --       = [X] * [L] + [X]
    -- we can deduce that
    --  [X] = [X] * [L]
    -- which has two solutions:
    --  [X] = [0] (meaningless)
    --  [L] = [1]
    -- which makes sense given that our algorithm
    -- works on 1x1x1 voxels.
    --
    -- . dimensions of "ray"
    --
    -- if [L] = [1] then we are free in our choice
    -- of [X], the dimensions of rcp. and the definition
    -- of rcp is
    --  rcp = recip ray
    -- so ray has [1]/[X] dimensions. thus ray is
    -- compatible with any physical dimensions.
    dis0 =
      rcp
        `mul1` abs
          ( fmap (fromIntegral @Int64 . floor) pos0
              - pos0
              + signum ray
          )
    rcp = recip ray
    sgn = floor <$> signum ray
    go dis pos closest0 time0 iter
      | iter >= 0 = do
          let time1 = time0 + minimum dis
          t <- test pos
          if t
            then pure $ VHit time1 pos (-sgn * closest0)
            else
              -- for each of x, y, and z coordinates,
              -- decide if it's the "closest" one among
              -- the other coordinates to the respective
              -- next voxel boundary. there could be many
              -- simultaneous advancements, though, which
              -- is handled smoothly with no issue.
              let closest =
                    [ fromIntegral $ fromEnum (a <= b && a <= c)
                    | a <- dis
                    | b <- dis ^. _yzx
                    | c <- dis ^. _zxy
                    ]
                  fclosest = fromIntegral <$> closest
               in go
                    (dis + fclosest `mul1` rcp)
                    (pos + closest * sgn)
                    closest
                    time1
                    (iter - 1)
      | otherwise = pure $ VHit (1 / 0) 0 0

-- debug

_march2tester :: V3 Int -> IO Bool
_march2tester v = False <$ traceIO (printf "v = %s" (show v))

_march2tester_distance :: V3 Int -> Double -> V3 Int -> IO Bool
_march2tester_distance home dst v =
  let h = fromIntegral <$> home
      w = fromIntegral <$> v
   in pure (distance h w >= dst)
