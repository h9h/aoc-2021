import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Lib ()
import Test.Hspec (describe)

main :: IO ()
main = hspec $ do
  describe "function" $ do
    it "should" $ do
      True `shouldBe` True
