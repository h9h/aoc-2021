{-# LANGUAGE TypeApplications #-}
module Lib where

import Data.Ix ( range )
import Data.List ( sort )

type Area = [[Int]]
type Point = (Int, Int)

puzzle :: IO ()
puzzle = do
    area <- map (map (\x -> read @Int [x])) . lines <$> readFile "src/input.txt"
    print $ value' area (2, 3)
    print $ "Part 1: " ++ show (part1 area)
    print $ "Part 2: " ++ show (part2 area)

value' :: Area -> Point -> Int
value' a (x,y)
    | x < 0 || y < 0 || x >= lCol || y >= lRow = 9
    | otherwise = a !! x !! y
    where lRow = length $ head a
          lCol = length a

axis :: Point -> [Point]
axis (x,y) = [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

neighbors:: Point -> [Point]
neighbors (x,y) = axis (x,y) ++ [(x-1,y-1), (x-1,y+1), (x+1,y-1), (x+1,y+1)]

isLowPoint :: Area -> Point -> Bool
isLowPoint a p = all (> val) $ value' a <$> neighbors p
    where val = value' a p

getAllLowPoints :: Area -> [Point]
getAllLowPoints a = filter (isLowPoint a) $ range ((0,0), (lCol-1, lRow-1))
    where lRow = length $ head a
          lCol = length a

part1 :: Area -> Int
part1 a = sum . map ((+1) . value' a) $ getAllLowPoints a

{-
Flood-fill (node):
1. Start inside (here: at a low point)
2. Set the node (here: Collect)
3. Perform Flood-fill for each axis:
    - one step to the south of node.
    - one step to the north of node
    - one step to the west of node
    - one step to the east of node
7. Return.
-}

collectBasin :: Area -> Point -> [Point]
collectBasin a p = collect' a [] p
    where collect' a acc p
            | value' a p < 9 && notElem p acc = foldl (collect' a) (p:acc) (axis p)
            | otherwise = acc

collectAllBasins :: Area -> [[Point]]
collectAllBasins a = map (collectBasin a) $ getAllLowPoints a

part2 a = (product . take 3 . reverse . sort) . map length $ collectAllBasins a