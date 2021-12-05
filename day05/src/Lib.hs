{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module Lib where

import Data.Function ( on )
import Control.Arrow ( Arrow((&&&)) )
import Data.List ( group, sort, sortOn, sortBy )
import Data.List.Split (wordsBy)
import Data.Ord ( comparing, Down(..) )
import Data.Char (isDigit)
import Data.Bool (bool)

type Coordinate = (Int, Int)
type Segment = (Coordinate, Coordinate)

day05 :: IO ()
day05 = do
    input <- lines <$> readFile "src/input-05.txt"
    let segments = parse input
    print $ take 1 segments
    print $ "Part 1: " ++ show (length $ getDangerousPoints isAlongAxis segments)
    print $ "Part 2: " ++ show (length $ getDangerousPoints isAlongAxisOrDiagonal segments)
    print $ "end"

--
-- Parse Input
--

{-
>>> parseLine "35,968 -> 974,29"
((35,968),(974,29))
-}
parseLine :: String -> Segment
parseLine = makeSegment . map (read @Int) . wordsBy (not . isDigit)
    where
        makeSegment :: [Int] -> Segment
        makeSegment [a,b,c,d] = ((a,b), (c,d))
        makeSegment _ = error "Invalid input for segment"

parse :: [String] -> [Segment]
parse = map parseLine

-- 
-- work on segments
--
isAlongAxis :: Segment -> Bool
isAlongAxis ((x1,y1), (x2,y2)) = x1 == x2 || y1 == y2

{-
>>> isAlongAxisOrDiagonal ((1,2), (3,4))
True
-}
isAlongAxisOrDiagonal :: Segment -> Bool
isAlongAxisOrDiagonal s@((x1,y1), (x2,y2)) = isAlongAxis s || abs (x1-x2) == abs (y1-y2)

{-
>>> getPoints ((1,2), (3,4))
[(1,2),(2,3),(3,4)]
-}
getPoints :: Segment -> [Coordinate]
getPoints ((x1,y1), (x2,y2))
  | x1 == x2 = (x1,) <$> range y1 y2
  | y1 == y2 = (,y1) <$> range x1 x2
  | abs (x1-x2) == abs (y1-y2) = zip (range x1 x2) (range y1 y2)
  | otherwise = []
  where
      range :: Int -> Int -> [Int]
      range a b = bool [a, a-1..b] [a..b] (a < b)

getAllPoints :: [Segment] -> [Coordinate]
getAllPoints = concatMap getPoints

{-
>>> frequency [(1,2), (1,2), (2,3)]
[(2,(1,2)),(1,(2,3))]
-}
frequency :: [Coordinate] -> [(Int,Coordinate)]
frequency = map (length &&& head) . group . sort

getDangerousPoints :: (Segment -> Bool) -> [Segment] -> [(Int, Coordinate)]
getDangerousPoints selector = filter ((> 1) . fst) . frequency . getAllPoints . filter selector
