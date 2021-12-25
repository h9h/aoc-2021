import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Lib ( makeGrid, solve, moveDirection, move )
import qualified Data.Map.Strict as M

checkEast a b = M.filter (== '>') a `shouldBe` M.filter (== '>') b
checkSouth a b = M.filter (== 'v') a `shouldBe` M.filter (== 'v') b

main :: IO ()
main = do
  testinput <- lines <$> readFile "test/input-test.txt"
  let cs = makeGrid testinput

  hspec $
    describe "move" $ do
    it "should move east" $ do
      checkEast (moveDirection '>' 9 cs) (makeGrid $ lines "....>.>v.>\nv.v>.>v.v.\n>v>>..>v..\n>>v>v>.>.v\n.>v.v...v.\nv>>.>vvv..\n..v...>>..\nvv...>>vv.\n>.v.v..v.v")
    it "should move south" $ do
      checkSouth (moveDirection 'v' 8 $ moveDirection '>' 9 cs) (makeGrid $ lines "....>.>v.>\nv.v>.>v.v.\n>v>>..>v..\n>>v>v>.>.v\n.>v.v...v.\nv>>.>vvv..\n..v...>>..\nvv...>>vv.\n>.v.v..v.v")
    it "should move" $ do
      move 8 9 cs `shouldBe` makeGrid (lines "....>.>v.>\nv.v>.>v.v.\n>v>>..>v..\n>>v>v>.>.v\n.>v.v...v.\nv>>.>vvv..\n..v...>>..\nvv...>>vv.\n>.v.v..v.v")