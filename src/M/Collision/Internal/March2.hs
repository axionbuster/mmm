module M.Collision.Internal.March2 (march) where

import Control.Lens
import Data.Int
import Debug.Trace
import Linear
import Text.Printf

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
  -- | number of iterations left or -1 for failure
  m Int
march test ray pos0 = go dis0 (floor <$> pos0)
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
    go dis pos iter
      | iter >= 0 = do
          t <- test pos
          if t
            then pure iter
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
                    (iter - 1)
      | otherwise = pure (-1)
