import Control.Exception (evaluate)
import Test.QuickCheck

import Data.Set (fromList)

import Lib (isAlongAxis, getPoints, parse, getDangerousPoints, isAlongAxis)
import Test.Hspec

main :: IO ()
main = hspec $ do
  let testinput = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"
  describe "Segments" $ do
    it "along axis" $ do
      isAlongAxis ((2,9),(2,9)) `shouldBe` True
      isAlongAxis ((1,9),(2,8)) `shouldBe` False
      isAlongAxis ((2,9),(0,9)) `shouldBe` True
      isAlongAxis ((2,7),(2,9)) `shouldBe` True
    it "gets points" $ do
      getPoints ((0,9),(2,9)) `shouldBe` [(0,9), (1,9), (2,9)]
      getPoints ((2,9),(0,9)) `shouldBe` [(2,9), (1,9), (0,9)]
      getPoints ((0,0),(0,0)) `shouldBe` [(0,0)]
      getPoints ((2,1),(2,4)) `shouldBe` [(2,1), (2,2), (2,3), (2,4)]
    it "gets points diagonal" $ do
      getPoints ((0,2),(2,0)) `shouldBe` [(0,2), (1,1), (2,0)]
      getPoints ((2,0),(0,2)) `shouldBe` [(2,0), (1,1), (0,2)]
  describe "Test 1" $ do
    it "should get testanswer" $ do
      length (getDangerousPoints isAlongAxis $ parse $ lines testinput) `shouldBe` 5
  describe "QuickCheck" $ do
    it "set of getPoints should be invariant to direction of Segment" $ do
      property prop_LengthInvariantToDirection

prop_LengthInvariantToDirection ((x1,y1), (x2,y2)) = fromList (getPoints ((x1,y1), (x2,y2))) == fromList (getPoints ((x2,y2), (x1,y1)))