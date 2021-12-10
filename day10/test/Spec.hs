import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Lib ( 
        CheckResult(..), 
        categorise, 
        removePairs, 
        removeOpenBrackets, 
        part1,
        costCorrect,
        part2
        )

main :: IO ()
main = do
  testinput <- lines <$> readFile "test/input-test.txt"
  hspec $ do
    describe "part1" $ do
      it "removePairs Blank" $ do
        removePairs "" `shouldBe` ""
      it "removePairs OK" $ do
        removePairs "<[(<>{})][<><>][{()()}]>" `shouldBe` ""
      it "removePairs Corrupted" $ do
        removePairs "<[(<>{})][<(><)>][{()()}]>" `shouldBe` "<[<(><)>]>"
      it "removePairs Incomplete" $ do
        removePairs "[({(<(())[]>[[{[]{<()<>>" `shouldBe` "[({([[{{"
      it "removeOpenBrackets" $ do
        removeOpenBrackets "<[(<>{})][<(><)>][{()()}]>" `shouldBe` ">{})][<(><)>][{()()}]>"
      it "check testinput" $ do
        map categorise testinput `shouldBe` 
          [
            Incomplete "[({([[{{",
            Incomplete "({[<{(",
            Corrupted '}',
            Incomplete "((((<{<{{",
            Corrupted ')',
            Corrupted ']',
            Incomplete "<{[{[{{[[",
            Corrupted ')',
            Corrupted '>',
            Incomplete "<{(["
          ]
      it "should fit testdata part 1" $ do
        part1 testinput `shouldBe` 26397
    describe "part2" $ do
      it "cost to correct" $ do
        costCorrect 0 "[({([[{{" `shouldBe` 182193
      it "should fit testdata part 2" $ do
        part2 testinput `shouldBe` 288957
      
