{-# LANGUAGE TypeApplications #-}
module Lib where

import Data.List.Split ( wordsBy )
import Data.Bits (Bits(xor))

type X = Int

puzzle :: IO ()
puzzle = do
    input <- readFile "src/input.txt"
    let xxx = parse input
    print $ take 1 xxx
    print $ "Part 1: " ++ show (solve1 xxx)
    print $ "Part 2: " ++ show (solve2 xxx)

--
-- Parse Input
--

--
-- Parse Input
--
parse :: String -> [Int]
parse line = map (read @Int) $ wordsBy ( == ',') line

--
-- Calculate fuel
--
dist :: Num a => a -> a -> a
dist y x = abs $ y - x

deltaY :: Int -> [Int] -> Int
deltaY y = sum <$> map (dist y)

mins :: [Int] -> Int
mins = foldr min maxBound

maxs :: [Int] -> Int
maxs = foldr max minBound

solve1 xs = mins $ flip deltaY xs <$> range'
  where range' = [mins xs..maxs xs]

-- Part 2

dist2 :: (Eq p, Num p, Enum p) => p -> p -> p
dist2 y x
  | y == x = 0
  | otherwise = sum [0..abs $ y - x]

deltaY2 :: Int -> [Int] -> Int 
deltaY2 y = sum <$> map (dist2 y)

solve2 xs = mins $ flip deltaY2 xs <$> range'
  where range' = [mins xs..maxs xs]

{-
>>> deltaY2 2 [16,1,2,0,4,2,7,1,2,14]
206
>>> solve1 [16,1,2,0,4,2,7,1,2,14]
37
-}
