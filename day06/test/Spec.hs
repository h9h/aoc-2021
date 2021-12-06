import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Lib ( initialFish ,growFish, parse )

main :: IO ()
main = do
  testinput <- readFile "test/input-test.txt"
  hspec $ do
    describe "Testdata" $ do
      it "should parse" $ do
        initialFish (parse testinput) `shouldBe` [0,1,1,2,1,0,0,0,0]
      it "should fit day 18" $ do
        sum (iterate growFish [0,1,1,2,1,0,0,0,0] !! 18) `shouldBe` 26
      it "should fit day 80" $ do
        sum (iterate growFish [0,1,1,2,1,0,0,0,0] !! 80) `shouldBe` 5934
    describe "QuickCheck" $ do
      it "Population grows" $ do
        property prop_population_increases

prop_population_increases :: Int -> Bool
prop_population_increases n = population n >= population n-1
  where population n
          | n > 0 = sum $ iterate growFish [0,1,1,2,1,0,0,0,0] !! n
          | otherwise = 0