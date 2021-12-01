import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Lib (getIncrements, slidingWindows, getSlidingIncrements, getIncrements', getIncrements'')
import Test.Hspec (describe)

main :: IO ()
main = hspec $ do
  describe "getIncrements: count increases in list of numbers" $ do
    it "sorted list" $ do
      getIncrements [1,2,3,4] `shouldBe` 3
    it "empty list" $ do
      getIncrements [] `shouldBe` 0
    it "constant list" $ do
      getIncrements [1,1,1,1] `shouldBe` 0
    it "general list" $ do
      getIncrements [5, 1,1,2,1] `shouldBe` 1
  describe "slidingWindows: create a list of sliding windows from a list" $ do
    it "empty list" $ do
      slidingWindows  1 ([] :: [Int]) `shouldBe` []
    it "general list" $ do
      slidingWindows 2 [1,2,3,4] `shouldBe` [[1,2], [2,3], [3,4]]
    it "too short list" $ do
      slidingWindows 2 [1] `shouldBe` []
  describe "all three implementations of getIncrements deliver same answer" $ do
    it "sorted list" $ do
      getIncrements [1,2,3,4] `shouldBe` getIncrements' 1 [1,2,3,4] 
      getIncrements [1,2,3,4] `shouldBe` getIncrements'' 1 [1,2,3,4]
    it "empty list" $ do
      getIncrements [] `shouldBe` getIncrements' 1 []
      getIncrements [] `shouldBe` getIncrements'' 1 []
    it "constant list" $ do
      getIncrements [1,1,1,1] `shouldBe` getIncrements' 1 [1,1,1,1]
      getIncrements [1,1,1,1] `shouldBe` getIncrements'' 1 [1,1,1,1]
    it "general list" $ do
      getIncrements [5, 1,1,2,1] `shouldBe` getIncrements' 1 [5, 1,1,2,1]
      getIncrements [5, 1,1,2,1] `shouldBe` getIncrements'' 1 [5, 1,1,2,1]
    it "sliding" $ do
      getSlidingIncrements 3 [5, 1,1,2,1] `shouldBe` getIncrements' 3 [5, 1,1,2,1]
      getSlidingIncrements 3 [5, 1,1,2,1] `shouldBe` getIncrements'' 3 [5, 1,1,2,1]
