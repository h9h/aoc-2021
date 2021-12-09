{-# LANGUAGE TypeApplications #-}

import Control.Exception (evaluate)
import Test.Hspec

import Lib ( getAllLowPoints, part1, collectBasin, part2, collectAllBasins )

main :: IO ()
main = do
  area <- map (map (\x -> read @Int [x])) . lines <$> readFile "test/input-test.txt"
  hspec $ do
    describe "part 1" $ do
      it "find all low points" $ do
        getAllLowPoints area `shouldBe` [(0,1), (0,9), (2,2), (4,6)]
      it "part 1 should fit" $ do
        part1 area `shouldBe` 15
    describe "part 2" $ do
      it "find basin (0,0)" $ do
        collectBasin area (0,0) `shouldBe` [(0,1), (1,0), (0,0)]
      it "part 2 should fit" $ do
        part2 area `shouldBe` 1134
