import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Lib (parse, deltaY, deltaY2, solve1, solve2)

main :: IO ()
main = do
  testinput <- readFile "test/input-test.txt"
  let xs = parse testinput
  hspec $ do
    describe "Part 1" $ do
      it "should match example 1" $ do
        deltaY 2 xs `shouldBe` 37
      it "should match example 2" $ do
        deltaY 1 xs `shouldBe` 41
    describe "Part 2" $ do
      it "should match example 1" $ do
        deltaY2 2 xs `shouldBe` 206
      it "should match example 2" $ do
        deltaY2 5 xs `shouldBe` 168
