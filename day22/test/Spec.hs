import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as S
import Lib ( parseInstruction, cuboid )

main :: IO ()
main = do
  testinput <- readFile "test/input-test.txt"
  hspec $ do
    describe "Parser" $ do
      it "Parse Instruction" $ do
        (length $ cuboid S.empty $ parseInstruction "on x=10..12,y=10..12,z=10..12") `shouldBe` 27
