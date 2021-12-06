{-# LANGUAGE TypeApplications #-}
module Lib where

import Data.List.Split ( wordsBy )
import Data.List ( group, sort, replicate, repeat )

type Idx = Int

puzzle :: IO ()
puzzle = do
    input <- readFile "src/input.txt"
    let fishes = parse input
    print $ "Input: " ++ show fishes
    let initialPopulation = initialFish fishes
    print $ "Initial Population: " ++ show initialPopulation
    print $ "Part 1: " ++ show (sum $ iterate growFish initialPopulation !! 80)
    print $ "Part 2: " ++ show (sum $ iterate growFish initialPopulation !! 256)

--
-- Parse Input
--
parse :: String -> [Int]
parse line = map (read @Int) $ wordsBy ( == ',') line

initialFish :: [Int] -> [Int]
initialFish = foldl incr' (replicate 9 0)

--
-- "Modifying" a list (is a pain)
--   which means, I'm probably not using the right data structure
--
add' :: Int -> Idx -> [Int] -> [Int]
add' n index = zipWith (+) (addend n index)
    where
      addend :: Int -> Idx -> [Int]
      addend n index
        | index <= 0 = n : repeat 0
        | otherwise =  replicate index 0 ++ n : repeat 0

incr' :: [Int] -> Idx -> [Int]
incr' xs index = add' 1 index xs

rotate :: [a] -> [a]
rotate = tail <> take 1

--
-- evolve the fish population
--

growFish :: [Int] -> [Int]
growFish xs = let n = head xs in
    add' n 6 $ rotate xs

{-
>>>iterate growFish [0,1,1,2,1,0,0,0,0] !! 1
[1,1,2,1,0,0,0,0,0]
>>>iterate growFish [0,1,1,2,1,0,0,0,0] !! 2
[1,2,1,0,0,0,1,0,1]

>>>iterate growFish [0,1,1,2,1,0,0,0,0] !! 3
[2,1,0,0,0,1,1,1,1]
-}

--
-- a better alternative
--
growFish' :: [Int] -> [Int]
growFish' fish = setAtIndex <$> [0..8]
  where setAtIndex 8 = head fish
        setAtIndex 6 = head fish + fish !! 7
        setAtIndex n = fish !! (n + 1)

{-
>>> growFish' [0,1,1,2,1,0,0,0,0]
[1,1,2,1,0,0,0,0,0]

>>> iterate growFish' [0,1,1,2,1,0,0,0,0] !! 80
[424,729,558,790,739,762,991,370,571]

-}
