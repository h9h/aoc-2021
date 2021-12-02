import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

--import Lib (runCommands, move2, getCommands, State, State(...))
import Lib

main :: IO ()
main = hspec $ do
  describe "Testcase Part2" $ do
    it "should result in Pos 15 Depth 60" $ do
      let commands = getCommands "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
      runCommands move2 commands `shouldBe` State 15 60 10
