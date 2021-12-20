import Control.Exception (evaluate)
import Test.Hspec ( hspec, describe, it, shouldBe )
import Test.QuickCheck ()

import qualified Data.Map.Strict as M
import Data.List (sortBy, groupBy)
import Data.Ord ( comparing )
import Lib (parseInput, neighbors, enhance, iterateEnhance)

main :: IO ()
main = do
  testinput <- readFile "test/input-test.txt"
  let (algorithm, image) = parseInput testinput
  print $ "Light on/off " ++ show (head algorithm) ++ " , " ++ show (algorithm !! 511)
  print "The testcase fails with switching according to the algorithm string."
  print "The testcase works, without switching on infinity."
  putStrLn $ render image
  print "-----------------------------"
  let (_, image1) = enhance algorithm (1, image)
  putStrLn $ render image1
  let (_, image2) = enhance algorithm (2, image1)
  print "-----------------------------"
  putStrLn $ render image2
  hspec $ do
    describe "neighbors" $ do
      it "neighbors at (5,10)" $ do
        neighbors (5,10) `shouldBe` [(4,9), (4,10), (4,11), (5,9), (5,10), (5,11), (6,9), (6,10), (6,11)]
    describe "solve 1" $ do
      it "should count test image" $ do
        iterateEnhance 2 algorithm image `shouldBe` 35

render :: M.Map (Int, Int) Int -> String
render img =
  unlines
    $ map renderRow
    $ groupBy (\a b -> fst (fst a) == fst (fst b))
    $ sortBy (comparing fst)
    $ M.toList img

renderRow :: [((Int, Int), Int)] -> String
renderRow = map ((\p -> if p==1 then '#' else '.') . snd)