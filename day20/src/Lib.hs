module Lib where

import qualified Data.Map.Strict as M
import           Data.Set ( fromList, toList )
import           Data.List ( foldl' )
import           Data.List.Split ( splitOn )
import           Control.Arrow ( Arrow((&&&)) )
-- import Debug.Trace ( trace )

puzzle :: IO ()
puzzle = do
    input <- readFile "src/input.txt"
    let (algorithm, image) = parseInput input
    print $ "Part 1: " ++ show (iterateEnhance 2 algorithm image)
    print $ "Part 2: " ++ show (iterateEnhance 50 algorithm image)

--
-- Parse Input
--

type Light = Int
type Algorithm = [Light]
type Point = (Int, Int)
type Image = M.Map Point Light

parseInput :: String -> (Algorithm, Image)
parseInput = (parseAlgorithm . head &&& parseImage . tail) . splitOn "\n\n"

bit :: Char -> Light
bit c = if c == '#' then 1 else 0

parseAlgorithm :: String -> Algorithm
parseAlgorithm = map bit


parseImage :: [String] -> Image
parseImage s = M.fromList [((r,c), bit n)
               | (r, row) <- zip [0..] (lines $ head s)
               , (c, n)   <- zip [0..] row
               ]

--
-- Image manipulation
--

neighbors :: Point -> [Point]
neighbors (r,c) =  [ (r + i, c + j) | i <- [-1..1], j <- [-1..1]]

toDec :: [Light] -> Int
toDec = foldl' (\acc x -> acc * 2 + x) 0

pointState :: Algorithm -> Light -> Image -> Point -> Light
pointState algorithm def image =
  (algorithm !!) . toDec . map (\p -> M.findWithDefault def p image) . neighbors

enhance :: Algorithm -> (Int, Image) -> (Int, Image)
enhance algorithm (step, image) = (step+1, newImage)
 where
  def      = [head algorithm, algorithm !! 511] !! (step `mod` 2)
  points   = toList $ fromList $ concatMap neighbors (M.keys image)
  newImage = M.fromList $ map (id &&& pointState algorithm def image) points

iterateEnhance :: Int -> Algorithm -> Image -> Int
iterateEnhance n algorithm image =
  sum $ snd $ iterate enhancer (1, image) !! n
  where enhancer = enhance algorithm
