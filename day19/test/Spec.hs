import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Lib ()

main :: IO ()
main = do
  testinput <- readFile "test/input-test.txt"
  hspec $ do
    describe "function" $ do
      it "should" $ do
        True `shouldBe` True
    describe "QuickCheck" $ do
      it "property" $ do
        property prop_LengthInvariantToDirection

prop_Trivial :: Int -> Int -> Bool
prop_Trivial x y = x + y == y + x