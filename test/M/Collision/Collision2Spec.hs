module M.Collision.Collision2Spec (spec) where

import Data.Coerce
import Data.Map.Lazy qualified as M
import Effectful
import Linear
import M.Collision.BlockModel1
import M.Collision.Effectful2
import M.Collision.Internal.Face
import M.Collision.Pure
import Test.Hspec

-- | given low coordinates return unit cube
genericcube :: V3 Double -> Box Double
genericcube l = Box (pure 1) (l + pure 0.5) -- dim, cent

-- | given low coordinates return zombie
genericzombie :: V3 Double -> Box Double
genericzombie = (`translate` Box (V3 0.6 1.95 0.6) (V3 0.3 0.975 0.3))

zombiebycenter :: V3 Double -> Box Double
zombiebycenter = Box (V3 0.6 1.95 0.6)

-- | get the center of a zombie with the given low coordinates
zomtr :: V3 Double -> V3 Double
zomtr = (+ V3 0.3 0.975 0.3)

-- | given a list of coordinates return a model of stones
stones :: [V3 Int] -> Model Box Double
stones = Model . M.fromList . fmap \v -> (v, genericcube (fromIntegral <$> v))

-- | given a list of coordinates & boxes return a model
--
-- no validation is done
boxes :: [(V3 Int, Box Double)] -> Model Box Double
boxes = Model . M.fromList

-- | like 'boxes' but with many boxes
boxes' :: [(V3 Int, ManyBoxes [] Double)] -> Model (ManyBoxes []) Double
boxes' = Model . M.fromList

-- | like 'zombiebycenter' but with many boxes
zombiebycenter' :: V3 Double -> ManyBoxes [] Double
zombiebycenter' = ManyBoxes . pure . zombiebycenter

-- | run an effect with a model
run :: Model f n -> Eff (GetBlock f n : '[]) a -> a
run = (runPureEff .) . runBlockModel

-- | check if two resolutions are near equal
resolveneareq :: Resolve Double -> Resolve Double -> Expectation
resolveneareq a_ b = shouldSatisfy a_ \a ->
  nearZero (respos a - respos b)
    && nearZero (resdis a - resdis b)
    && restou a == restou b

-- | lay an infinite ray of blocks of a certain shape along an axis
layray ::
  -- | length of the ray
  Int ->
  -- | a point chosen on the line
  V3 Int ->
  -- | a direction vector
  --
  -- try using a signum vector; this function does NOT use
  -- a proper line-drawing algorithm
  V3 Int ->
  -- | a shape to lay
  (V3 Int -> Box Double) ->
  -- | model
  Model Box Double
layray n p d f =
  Model $
    M.fromList $
      take
        n
        ((\i -> (i, f i)) <$> iterate (+ d) p)

{-
layline :: Int -> V3 Int -> V3 Int -> (V3 Int -> Box Double) -> Model Box Double
layline n p d f =
  let inter = fix \z (x : xs) (y : ys) -> x : y : z xs ys
      g i = (i, f i)
   in Model $
        M.fromList $
          take n $
            inter
              (g <$> iterate (+ d) p)
              (g <$> iterate (subtract d) p)
-}

spec :: Spec
spec = do
  describe "Collision2" do
    describe "hitting" do
      it "lets zombie slide on top of block" do
        let zombie = genericzombie (V3 0 0.5 0)
            block = genericcube (V3 0 (-1) 1)
            disp = V3 0 0 1
         in hitting disp zombie block `shouldSatisfy` not . hitin01
      it "handles the stairs case (passing)" do
        let zombie = zombiebycenter' (V3 (-1) 1.478 0.8)
            stairs =
              ManyBoxes
                [ Box (V3 1 0.5 1) (V3 0.5 0.25 0.5),
                  Box (V3 1 0.5 0.5) (V3 0.5 0.75 0.25)
                ]
            disp = V3 10 0 0
         in hitting disp zombie stairs `shouldSatisfy` not . hitin01
      it "handles the stairs case (blocking)" do
        let zombie = zombiebycenter' (V3 (-1) 1.478 0.7)
            stairs =
              ManyBoxes
                [ Box (V3 1 0.5 1) (V3 0.5 0.25 0.5),
                  Box (V3 1 0.5 0.5) (V3 0.5 0.75 0.25)
                ]
            disp = V3 10 0 0
         in hitting disp zombie stairs `shouldSatisfy` hitin01
    describe "ManyBoxes" do
      describe "corners" do
        it "of zombie (ManyBoxes) are identical to Box version" do
          let zombie' = zombiebycenter' (V3 (-1) 1.478 0.7)
              zombie = zombiebycenter (V3 (-1) 1.478 0.7)
           in corners zombie `shouldBe` corners zombie'
      describe "sdimensions" do
        it "of zombie (ManyBoxes) are identical to Box version" do
          let zombie' = zombiebycenter' (V3 (-1) 1.478 0.7)
              zombie = zombiebycenter (V3 (-1) 1.478 0.7)
           in sdimensions zombie `shouldSatisfy` \d ->
                nearZero $ sdimensions zombie' - d
    describe "Face" do
      it "correctly computes face points of ManyBoxes version of zombie" do
        let zombie' = zombiebycenter' (V3 (-1) 1.478 0.7)
            zombie = zombiebycenter (V3 (-1) 1.478 0.7)
            fp0 = facepoints (ceiling <$> sdimensions zombie) (V3 10 0 0)
            fp1 = facepoints (ceiling <$> sdimensions zombie') (V3 10 0 0)
         in fp1 `shouldBe` fp0
  describe "Collision2" do
    describe "resolve" do
      it "blocks zombie from sliding right" do
        let model = stones [V3 0 43 1]
            zombie = genericzombie (V3 0 42 0)
            disp = V3 0 0 10
         in run model (resolve zombie disp)
              `resolveneareq` Resolve
                { -- the zombie tries to slide to the right but gets
                  -- blocked by the stone
                  respos = zomtr $ V3 0 42 0.4,
                  resdis = zero,
                  restou = NewlyTouchingGround {newonground = EQ}
                }
      it "detects when stuck and overlapping" do
        let model = stones [V3 0 42 1]
            zombie = genericzombie (V3 0 42 0.9)
            disp = V3 0 0 1.3
         in run model (resolve zombie disp)
              `resolveneareq` Resolve
                { -- zombie is stuck because it is overlapping with the stone
                  respos = zomtr $ V3 0 42 0.9,
                  resdis = disp, -- so it can unstuck itself
                  restou = NewlyTouchingGround {newonground = EQ}
                }
      it "does well with slabs" do
        let model = boxes [(V3 0 (-1) 0, Box (V3 1 0.5 1) (V3 0.5 0.25 0.5))]
            zombie = genericzombie (V3 0 1.5 0)
            disp = V3 0 (-2) 0
         in run model (resolve zombie disp)
              `resolveneareq` Resolve
                { respos = zomtr $ V3 0 0.5 0,
                  resdis = zero,
                  -- it touches the ground so it should say GT
                  restou = NewlyTouchingGround {newonground = GT}
                }
      it "slams and slides" do
        let model = stones [V3 0 0 0]
            zombie = zombiebycenter (V3 1.112 1.998 0)
            disp = V3 0.273 (-0.0784) 0
         in run model (resolve zombie disp)
              `resolveneareq` Resolve
                { respos = V3 1.385 1.975 0,
                  resdis = zero,
                  restou = NewlyTouchingGround {newonground = GT}
                }
      it "detects a fence below" do
        let model = boxes [(V3 0 0 0, Box (V3 0.25 1.5 0.25) (V3 0.5 0.75 0.5))]
            zombie = zombiebycenter (V3 0.8 2.475 0.8)
            disp = V3 0 (-0.25) 0
            resu = run model (resolve zombie disp)
         in resu `shouldSatisfy` \_ ->
              nearZero (respos resu - V3 0.8 2.475 0.8)
                && nearZero (resdis resu)
                && restou resu /= NewlyTouchingGround {newonground = LT}
      it "jumps" do
        let model = stones [V3 0 0 0]
            zombie = zombiebycenter (V3 0.5 1.975 0.5)
            disp = V3 0 0.5 0
            resu = run model (resolve zombie disp)
         in resu `shouldSatisfy` \_ ->
              nearZero (respos resu - V3 0.5 2.475 0.5)
                && nearZero (resdis resu)
                && restou resu == NewlyTouchingGround {newonground = LT}
      it "detects a fence below a carpet" do
        let model =
              boxes
                [ (V3 0 (-1) 0, Box (V3 1 0.0625 1) (V3 0.5 (-0.96875) 0.5)),
                  (V3 0 (-2) 0, Box (V3 0.25 1.5 0.25) (V3 0.5 (-1.25) 0.5))
                ]
            zombie = zombiebycenter (V3 0.3 0.975 0.3)
            disp = V3 0 (-10) 0
            resu = run model (resolve zombie disp)
         in resu
              `resolveneareq` Resolve
                { respos = V3 0.3 0.475 0.3,
                  resdis = zero,
                  restou = NewlyTouchingGround {newonground = GT}
                }
      it "lets a zombie pass through the gap of stairs" do
        let model =
              boxes'
                [ ( V3 0 0 0,
                    ManyBoxes
                      [ Box (V3 1 0.5 1) (V3 0.5 0.25 0.5),
                        Box (V3 1 0.5 0.5) (V3 0.5 0.75 0.25)
                      ]
                  )
                ]
            zombie = zombiebycenter' (V3 (-1) 1.478 0.8)
            disp = V3 10 0 0
         in run model (resolve zombie disp)
              `resolveneareq` Resolve
                { respos = V3 9 1.478 0.8,
                  resdis = zero,
                  restou = NewlyTouchingGround {newonground = EQ}
                }
      it "blocks zombie by stairs" do
        let model =
              boxes'
                [ ( V3 0 0 0,
                    ManyBoxes
                      [ Box (V3 1 0.5 1) (V3 0.5 0.25 0.5),
                        Box (V3 1 0.5 0.5) (V3 0.5 0.75 0.25)
                      ]
                  )
                ]
            zombie = zombiebycenter' (V3 (-1) 1.478 0.7)
            disp = V3 10 0 0
         in run model (resolve zombie disp)
              `resolveneareq` Resolve
                { respos = V3 (-0.3) 1.478 0.7,
                  resdis = zero,
                  restou = NewlyTouchingGround {newonground = EQ}
                }
      it "allows sliding along a line" do
        let model =
              layray 10 (V3 0 0 0) (V3 1 0 0) $
                genericcube . fmap fromIntegral
            zombie = genericzombie (V3 0 1 0)
            disp = V3 10 (-1) 1
         in run model (resolve zombie disp)
              `shouldSatisfy` \m ->
                nearZero (respos m - zomtr (V3 10 1 1))
                  && nearZero (resdis m)
                  && restou m /= NewlyTouchingGround {newonground = LT}
      it "blocks when moving diagonally" do
        let cub = stones [V3 (-1) 0 (-1), V3 0 0 (-2)]
            zombie = genericzombie (V3 1 0 1)
            disp = V3 (-2) 0 (-2)
         in run cub (resolve zombie disp)
              `resolveneareq` Resolve
                { respos = zomtr $ V3 0 0 0,
                  resdis = zero,
                  restou = NewlyTouchingGround {newonground = EQ}
                }
      it "blocks when moving diagonally (|x| = |z| = 1)" do
        let cub = stones [V3 (-1) 0 (-1), V3 0 0 (-2)]
            zombie = genericzombie (V3 0.5 0 0.5)
            disp = V3 (-1) 0 (-1)
         in run cub (resolve zombie disp)
              `resolveneareq` Resolve
                { respos = zomtr $ V3 0 0 0,
                  resdis = zero,
                  restou = NewlyTouchingGround {newonground = EQ}
                }
    describe "updonground" do
      it "upgrades F to T correctly" do
        updonground (coerce GT) False `shouldBe` True
