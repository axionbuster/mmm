{-# LANGUAGE MonoLocalBinds #-}

-- | pure collision detection
module M.Collision.Pure
  ( Shape (..),
    SomeShape1 (..),
    Hit (..),
    Hit' (..),
    Box (.., Box'),
    ManyBoxes (..),
    ManyBoxes_,
    _dimensions,
    _center,
    _lcorner,
    _hcorner,
    hitin01,
    infhit,
    boxfromcorners,
    castshape1,
    boxzero,
    hicorner,
    hicorner',
    locorner,
    locorner',
    shicorner,
    slocorner,
  )
where

import Control.Lens hiding (index)
import Control.Monad.Zip
import Data.Data
import Data.Foldable
import Data.Functor.Rep
import Data.Hashable
import Data.Ord
import Data.Semigroup
import GHC.Generics (Generic)
import Linear

-- | a collision resolution data type
--
-- no hit is represented by a hit at infinity (other fields are unspecified)
data Hit a = Hit
  { -- | proportion of move completed in [0, 1]
    hittime :: !a,
    -- | the point of collision
    --
    -- if you're using 'Box', this is the center of the box
    hitwhere :: !(V3 a),
    -- | normal vector of the surface hit
    --
    -- a signum vector, so each component is either -1, 0, or 1
    hitnorm :: !(V3 a)
  }
  deriving (Show, Eq, Generic, Typeable, Hashable, Functor, Data)

posinf :: (Fractional a) => a
posinf = 1 / 0
{-# INLINE posinf #-}

-- | check if the hit time is in [0, 1]
hitin01 :: (Num a, Ord a) => Hit a -> Bool
hitin01 Hit {hittime} = hittime >= 0 && hittime <= 1
{-# INLINE hitin01 #-}

-- | a hit at infinity
infhit :: (Fractional a) => Hit a
infhit = Hit posinf zero zero
{-# INLINE infhit #-}

-- | internal newtype used with 'Data.Semigroup.Min' to find the closest hit
newtype Hit' a = Hit' {unHit' :: Hit a}
  deriving newtype (Show, Eq, Hashable)

instance (Ord a) => Ord (Hit' a) where
  compare = comparing (hittime . unHit')
  {-# INLINE compare #-}

instance (Fractional a, Num a) => Bounded (Hit' a) where
  minBound = Hit' $ Hit 0 zero zero -- no negative time
  maxBound = Hit' $ Hit posinf zero zero -- hit at infinity
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}

-- | existential 'Shape' type but where numeric type is erased
--
-- see also: 'castshape1'
data SomeShape1 a
  = forall s.
    ( Typeable (s a),
      Show (s a),
      Shape s
    ) =>
    SomeShape1 (s a)
  deriving (Typeable)

instance Show (SomeShape1 a) where
  show (SomeShape1 s) = show s

instance Shape SomeShape1 where
  crossing v d (SomeShape1 s) = crossing v d s
  intersecting (SomeShape1 s1) (SomeShape1 s2)
    | Just s3 <- cast s2 = intersecting s1 s3
    | otherwise = intersecting (tomanyboxes s1) (tomanyboxes s2)
  hitting v (SomeShape1 s1) (SomeShape1 s2)
    | Just s3 <- cast s2 = hitting v s1 s3
    | otherwise = hitting v (tomanyboxes s1) (tomanyboxes s2)
  translate v (SomeShape1 s) = SomeShape1 (translate v s)
  corners (SomeShape1 s) = corners s
  tomanyboxes (SomeShape1 s) = tomanyboxes s

-- | cast a 'SomeShape1' to a specific type
castshape1 :: (Typeable b) => SomeShape1 a -> Maybe b
castshape1 (SomeShape1 s) = cast s

-- | an AABB type class used for collision detection and resolution
class Shape s where
  -- | check if two shapes intersect
  intersecting :: (Fractional a, Ord a) => s a -> s a -> Bool

  -- | check if a ray will hit the shape and return the hit data
  crossing :: (RealFloat a) => V3 a -> V3 a -> s a -> Hit a

  -- | check if the first shape will collide into the second shape
  -- if it moves with the given displacement
  hitting :: (RealFloat a) => V3 a -> s a -> s a -> Hit a

  -- | translate the shape by the given displacement
  translate :: (Num a) => V3 a -> s a -> s a

  -- | the locations of the lower and higher corners of the shape
  -- respectively
  corners :: (Fractional a, Ord a) => s a -> V2 (V3 a)

  -- | convert a 'Shape' to a 'ManyBoxes' of 'Box'es with a list container,
  -- which is a canonical form for 'ManyBoxes'
  tomanyboxes :: s a -> ManyBoxes [] a

  -- | the center of the shape
  scenter :: (Fractional a, Ord a) => s a -> V3 a
  scenter s = (sum . corners $ s) <&> (/ 2) -- not robust to large numbers
  {-# INLINE scenter #-}

  -- | the dimensions of the shape
  sdimensions :: (Fractional a, Ord a) => s a -> V3 a
  sdimensions s = let V2 l h = corners s in h - l
  {-# INLINE sdimensions #-}

v2fst :: V2 a -> a
v2fst (V2 a _) = a

v2snd :: V2 a -> a
v2snd (V2 _ b) = b

-- | the upper corner of a shape
shicorner :: (Shape s, Fractional a, Ord a) => s a -> V3 a
shicorner = v2snd . corners
{-# INLINE shicorner #-}

-- | the lower corner of a shape
slocorner :: (Shape s, Fractional a, Ord a) => s a -> V3 a
slocorner = v2fst . corners
{-# INLINE slocorner #-}

-- | a box in 3D space, located either relatively or absolutely
data Box a = Box
  { -- | the dimensions of the box
    dimensions :: !(V3 a),
    -- | the center of the box
    center :: !(V3 a)
  }
  deriving stock (Show, Eq, Generic, Typeable, Functor, Data)
  deriving anyclass (Hashable)

instance Applicative Box where
  pure x = Box (pure x) (pure x)
  {-# INLINE pure #-}
  Box d1 c1 <*> Box d2 c2 = Box (d1 <*> d2) (c1 <*> c2)
  {-# INLINE (<*>) #-}

-- used to define the Box' pattern
-- the expression is kind of nonsense, but it just needs to
-- work in a formal way so we can use the pattern
chgbox_ :: (Fractional a) => Box a -> Box a
chgbox_ b = Box (locorner b) (hicorner b)

-- | bidrectional pattern for 'Box' but with corner locations (low to high)
--
-- you can use the 'locorner'' and 'hicorner'' patterns to extract the corners,
-- respectively
pattern Box' :: (Fractional a) => V3 a -> V3 a -> Box a
pattern Box' {locorner', hicorner'} <- (chgbox_ -> Box locorner' hicorner')
  where
    Box' l h = Box (h - l) ((h + l) ^/ 2)

{-# COMPLETE Box' #-}

-- | a box from the low and high corners
boxfromcorners ::
  (Fractional a) =>
  -- | low corner
  V3 a ->
  -- | high corner
  V3 a ->
  -- | the box
  Box a
boxfromcorners l h = Box (h - l) ((h + l) ^/ 2)

-- | a newtype over a 'Foldable' 'Functor' container of 'Box'es
--
-- the low and high corners are those of the smallest bounding box
newtype ManyBoxes f a = ManyBoxes (f (Box a))
  deriving stock (Generic, Typeable)

-- | a type alias for a list of 'Box'es
-- (canonical form for 'ManyBoxes')
type ManyBoxes_ a = ManyBoxes [] a

deriving stock instance
  (Typeable f, Typeable a, Data (f (Box a))) =>
  Data (ManyBoxes f a)

instance (Eq (f (Box a))) => Eq (ManyBoxes f a) where
  ManyBoxes a == ManyBoxes b = a == b
  {-# INLINE (==) #-}

instance (Ord (f (Box a))) => Ord (ManyBoxes f a) where
  compare (ManyBoxes a) (ManyBoxes b) = compare a b
  {-# INLINE compare #-}

instance (Functor f) => Functor (ManyBoxes f) where
  fmap f (ManyBoxes boxes) = ManyBoxes $ fmap g boxes
    where
      g (Box d c) = Box (f <$> d) (f <$> c)
  {-# INLINE fmap #-}

instance (Hashable (f (Box a))) => Hashable (ManyBoxes f a) where
  hashWithSalt s (ManyBoxes boxes) = hashWithSalt s boxes
  {-# INLINE hashWithSalt #-}

-- | Lens for the dimensions of the box
_dimensions :: Lens' (Box a) (V3 a)
_dimensions = lens dimensions \b d -> b {dimensions = d}
{-# INLINE _dimensions #-}

-- | Lens for the center of the box
_center :: Lens' (Box a) (V3 a)
_center = lens center \b c -> b {center = c}
{-# INLINE _center #-}

-- | Lens for the lower corner of the box
_lcorner :: (Fractional a) => Lens' (Box a) (V3 a)
_lcorner = lens locorner \b l -> b {center = l + dimensions b ^/ 2}
{-# INLINE _lcorner #-}

-- | Lens for the higher corner of the box
_hcorner :: (Fractional a) => Lens' (Box a) (V3 a)
_hcorner = lens hicorner \b h -> b {center = h - dimensions b ^/ 2}
{-# INLINE _hcorner #-}

instance Shape Box where
  crossing origin displacement shape =
    let v2sort (V2 x y)
          | x < y = V2 x y
          | otherwise = V2 y x
        (!) = index
        times = tabulate \i ->
          let l = locorner shape
              h = hicorner shape
              x = (l ! i - origin ! i) / displacement ! i
              y = (h ! i - origin ! i) / displacement ! i
           in v2sort $ V2 x y
        nonans = not . any isNaN
        -- sequenceA = transpose; sequenceA :: V3 (V2 a) -> V2 (V3 a)
        -- vector upgrades a V3 to V4 but sets fourth component to 0
        -- point does the same but sets fourth component to 1
        V2 (vector -> tenter) (point -> tleave) = sequenceA times
        t = maximum tenter
     in if nonans tenter && nonans tleave && t < minimum tleave
          then
            Hit
              { hittime = t,
                hitwhere = origin + t *^ displacement,
                hitnorm =
                  let p1m1 True = 1
                      p1m1 _ = -1
                   in tabulate \i ->
                        if t == (tenter ^. _xyz) ! i
                          then p1m1 $ displacement ! i < 0
                          else 0
              }
          else infhit

  -- moving = displacement from t = 0 to t = 1
  -- 'this' is the box that is moving
  -- into 'that' box
  hitting moving this that =
    let l = locorner that
        h = hicorner that
        d = dimensions this
        -- reduce box-box collision to ray-box collision
        --  1. shrink 'this' box into a point
        --  2. expand 'that' box by the same amount in each direction
        (l', h') = (l - d ^/ 2, h + d ^/ 2)
     in crossing
          do center this
          do moving
          do boxfromcorners l' h'
  intersecting this that =
    let lotest = and $ mzipWith (<) (locorner this) (hicorner that)
        hitest = and $ mzipWith (>) (hicorner this) (locorner that)
     in lotest && hitest
  translate displacement box = box {center = displacement + center box}
  corners box = V2 (locorner box) (hicorner box)
  tomanyboxes = ManyBoxes . pure
  scenter = center
  sdimensions = dimensions

-- | a box with zero dimensions and center
boxzero :: (Num a) => Box a
boxzero = Box zero zero

-- | the location of the lower corner of the box
locorner :: (Fractional a) => Box a -> V3 a
locorner (Box d c) = c - d ^/ 2

-- | the location of the higher corner of the box
hicorner :: (Fractional a) => Box a -> V3 a
hicorner (Box d c) = c + d ^/ 2

instance (Show (f (Box a))) => Show (ManyBoxes f a) where
  show (ManyBoxes boxes) = show boxes

-- | this one implements 'Bounded' as well
newtype Arg' a b = Arg' (Arg a b)
  deriving newtype (Eq, Ord, Generic, Typeable, Hashable, Functor)

instance (Fractional a, Bounded b) => Bounded (Arg' a b) where
  minBound = Arg' (Arg 0 minBound)
  maxBound = Arg' (Arg posinf maxBound)
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}

unarg :: Arg' a b -> b
unarg (Arg' (Arg _ b)) = b
{-# INLINE unarg #-}

-- if a ~ 'Box _', the find the box with the smallest hit time
arghitminboxf ::
  (Foldable f, Ord b, Fractional b) =>
  (a -> Hit b) -> f a -> Hit b
arghitminboxf f =
  let g (f -> h) = Min $ Arg' $ Arg (hittime h) (Hit' h)
   in unHit' . unarg . getMin . foldMap' g
{-# INLINE arghitminboxf #-}

instance (Functor f, Foldable f) => Shape (ManyBoxes f) where
  crossing origin displacement (ManyBoxes boxes) =
    arghitminboxf (crossing origin displacement) boxes

  -- find the first hitting collision
  hitting moving (ManyBoxes these) (ManyBoxes those) =
    let firsthit boxes box = arghitminboxf (hitting moving box) boxes
     in arghitminboxf (firsthit those) these

  -- check if any of the boxes intersect
  intersecting (ManyBoxes these) (ManyBoxes those) =
    foldr
      ( \this r ->
          foldr
            do \that s -> intersecting this that || s
            do False
            do those
            || r
      )
      do False
      do these

  -- translate all the boxes
  translate displacement (ManyBoxes boxes) =
    ManyBoxes $
      translate displacement <$> boxes

  -- find the corners of the smallest bounding box
  corners (ManyBoxes boxes) =
    let low = foldl' (flip $ liftA2 min . locorner) (pure posinf) boxes
        high = foldl' (flip $ liftA2 max . hicorner) (pure (-posinf)) boxes
     in V2 low high

  tomanyboxes (ManyBoxes boxes) = ManyBoxes $ toList boxes
