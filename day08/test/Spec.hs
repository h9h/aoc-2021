import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Lib ( parseLine )

main :: IO ()
main = do
  testinput <- lines <$> readFile "test/input-test.txt"
  let code1 = head testinput
  hspec $ do
    describe "function" $ do
      it "parse into tuple" $ do
        parseLine code1 `shouldBe` (["be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"],["fdgacbe", "cefdb", "cefbgd", "gcbe"])
    describe "QuickCheck" $ do
      it "property" $ do
        property prop_Trivial

prop_Trivial :: Int -> Int -> Bool
prop_Trivial x y = x + y == y + x