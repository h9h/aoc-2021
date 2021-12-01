import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Lib

main :: IO ()
main = hspec $ do
  describe "xxx" $ do
    it "xxx" $ do
      3 `shouldBe` 3
