import Control.Exception (evaluate)
import Test.Hspec ( hspec, describe, it, shouldBe )
import Test.QuickCheck ()

import Data.List (nub)
import Lib ( parseInput, overlap, alignWithReference, computeAllPositions )

main :: IO ()
main = do
  scanners <- parseInput <$> readFile "test/input-test.txt"
  let s n = scanners !! n
  hspec $ do
    describe "Overlap" $ do
      it "no overlap (without orientations)" $ do
        overlap (s 0) (s 1)  `shouldBe` []
      it "does overlap with orientations 0-1" $ do
        snd ( head $ alignWithReference (s 0) (s 1)) `shouldBe` [68,-1246,-43]
      it "does overlap with orientations 4-1" $ do
        snd ( head $ alignWithReference (s 1) (s 4)) `shouldBe` [88,113,-1104]
    describe "Allignment" $ do
      it "should be 79 beacons" $ do
        let x:xs = scanners
        (length . nub . concatMap fst $ computeAllPositions [(x, [0, 0, 0])] [x] xs) `shouldBe` 79