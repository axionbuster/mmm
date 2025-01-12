module M.Collision.BoxSpec (spec) where

import Linear
import M.Collision.Pure
import Test.Hspec

boxd :: V3 Double -> V3 Double -> Box Double
boxd = Box

spec :: Spec
spec = do
  describe "Box" do
    describe "hitting" do
      it "passes case 1" do
        let vel = V3 (-6.8) 1.4 1.23
            box1 = boxd (pure 1) (V3 0.5 0.5 (-1.5))
            box2 = boxd (pure 1) (V3 (-2.5) 1.5 0.5)
         in hitting vel box1 box2 `shouldSatisfy` not . hitin01
      it "passes case 1(a)" do
        let vel = V3 (-6.8) 1.4 1.23
            box1 = boxd (pure 1) (V3 0.5 0.5 (-1.5))
            box2 = boxd (V3 2 2 3) (V3 (-3) 1 (-0.5))
         in hitting vel box1 box2 `shouldSatisfy` hitin01
      it "passes case 1(b)" do
        let vel = V3 (-0.8) 1.4 1.23
            box1 = boxd (pure 1) (V3 0.5 0.5 (-1.5))
            box2 = boxd (V3 2 2 3) (V3 (-3) 1 (-0.5))
         in hitting vel box2 box1 `shouldSatisfy` not . hitin01
      it "passes case 2" do
        let vel = V3 4 5 0
            box1 = boxd (V3 0.5 1 2) (V3 (-3.75) (-3.5) 0)
            box2 = boxd (V3 1 0.5 0.25) (V3 (-1.5) 0.25 0.125)
         in hitting vel box1 box2 `shouldSatisfy` hitin01
      it "passes case 3" do
        let vel = V3 0 0 5
            box1 = boxd (pure 1) (V3 0 0 (-5))
            box2 = boxd (pure 1) (V3 0 0 (-1))
         in hitting vel box1 box2 `shouldSatisfy` hitin01
      it "passes case 3(b)" do
        let vel = V3 0 0 3
            box1 = boxd (pure 1) (V3 0 0 (-5))
            box2 = boxd (pure 1) (V3 0 0 (-1))
         in -- we decided that barely touching is not hitting
            hitting vel box1 box2 `shouldSatisfy` not . hitin01
