import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Lib ( Grid, makeGrid, solve)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  testinput <- readFile "test/input-test.txt"
  hspec $ do
    let grid = makeGrid 10 1 $ lines testinput
    let grid2 = makeGrid 10 5 $ lines testinput
    let grid3 = makeGrid 100 5 $ lines testinput
    describe "makeGrid" $ do
      it "read correct small grid" $ do
        map (\n -> grid M.! (n,n)) [0 .. 9]   `shouldBe` [1,3,3,4,4,2,2,6,2,1]
      it "read correct big grid small indices" $ do
        map (\n -> grid2 M.! (n,n)) [0 .. 9]   `shouldBe` [1,3,3,4,4,2,2,6,2,1]
      it "read correct big grid big indices" $ do
        map (\n -> grid2 M.! (n,n)) [40 .. 49]   `shouldBe` [9,2,2,3,3,1,1,5,1,9]
      it "read correct big grid small indices other diagonal" $ do
        map (\n -> grid2 M.! (9-n,n)) [0 .. 9]   `shouldBe` [2,2,2,9,1,1,1,3,7,2]
      it "read correct big grid big indices other diagonal" $ do
        map (\n -> grid2 M.! (89-n,n)) [40 .. 49]   `shouldBe` [1,1,1,8,9,9,9,2,6,1]
    describe "solve" $ do
      it "Part1 should be 40 for test" $ do
        f (solve grid) `shouldBe` 40
      it "Part2 should be 315 for test" $ do
        f (solve grid2) `shouldBe` 315
        
f :: Maybe (Int, [(Int, Int)]) -> Int
f x = case x of
  Just (n, _) -> n
  Nothing     -> error "fail"
