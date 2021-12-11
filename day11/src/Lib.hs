module Lib where

import Data.List ( takeWhile )

type Value = Int
type Point = (Int, Int)
type Grid = [(Point, Value)]

puzzle :: IO ()
puzzle = do
  input <- lines <$> readFile "src/input.txt"
  let grid = makeGrid input
  print $ show (makeGrid input)
  print $ "Part1 " ++ show (part1 grid)
  print $ "Part2 " ++ show (part2 grid)


makeGrid :: [String] -> Grid
makeGrid input = [((r, c), read [n]) |
    (r, row) <- zip [0 ..] input,
    (c, n) <- zip [0 ..] row
  ]

neighbors :: Point -> [Point]
neighbors (x, y) = [ (x + i, y + j) | i <- [-1..1], j <- [-1..1], (i, j) /= (0, 0)]

{-
First, the energy level of each octopus increases by 1.

Then, any octopus with an energy level greater than 9 flashes. 
This increases the energy level of all adjacent octopuses by 1, 
including octopuses that are diagonally adjacent. If this causes 
an octopus to have an energy level greater than 9, it also flashes. 
This process continues as long as new octopuses keep having their 
energy level increased beyond 9. 
(An octopus can only flash at most once per step.)

Finally, any octopus that flashed during this step has its energy 
level set to 0, as it used all of its energy to flash.
-}
increaseEnergy :: Grid -> Grid
increaseEnergy = map (\(p,v) -> (p,v+1))

flashPoints :: Grid -> [Point]
flashPoints = concatMap (\(p, v) -> [p | v > 9])

neighboringPoints :: [Point] -> [Point]
neighboringPoints = concatMap neighbors

propagate :: [Point] -> Grid -> Grid
propagate neighs = map newEnergy
  where
    newEnergy (p, v) = if v == 0 then (p,v) else (p, v + increase' p neighs)
    increase' p = length . filter (==p)

setZero :: Grid -> Grid
setZero = map (\(p,v) -> (p, if v > 9 then 0 else v))

innerStep :: ([Point], Grid) -> ([Point], Grid)
innerStep (flashes, grid) = (flashes', grid')
  where flashes' = neighboringPoints $ flashPoints grid
        grid' = propagate flashes' (setZero grid)

step :: Grid -> Grid
step grid = snd 
              $ last 
              $ takeWhile ((/=[]) . fst) 
              $ iterate innerStep ([(-1, -1)], increaseEnergy grid)

sumStep :: (Int, Grid) -> (Int, Grid)
sumStep (s, grid) = (s', g')
  where g' = step grid
        s' = s + length (filter (\(p,v) -> v == 0) g')

part1 :: Grid -> Int
part1 grid = fst $ iterate sumStep (0, grid) !! 100

allFlashStep :: (Bool, Grid) -> (Bool, Grid)
allFlashStep (_, grid) = (s', g')
  where g' = step grid
        s' = all ((==0) . snd) g'

part2 :: Grid -> Int
part2 grid = length $ takeWhile ((==False) . fst) $ iterate allFlashStep (False, grid)

{- 
====================================================== 
                      Testdaten
====================================================== 
-}
testGrid = makeGrid $ lines "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"

{-

>>> flashPoints $ increaseEnergy $ increaseEnergy testGrid
[(0,2),(1,4),(4,5),(4,9),(6,4),(7,1),(7,2),(7,4),(7,5),(8,1),(8,4),(8,6),(9,2)]

>>> neighboringPoints $ flashPoints $ increaseEnergy $ increaseEnergy testGrid
[(-1,1),(-1,2),(-1,3),(0,1),(0,3),(1,1),(1,2),(1,3),(0,3),(0,4),(0,5),(1,3),(1,5),(2,3),(2,4),(2,5),(3,4),(3,5),(3,6),(4,4),(4,6),(5,4),(5,5),(5,6),(3,8),(3,9),(3,10),(4,8),(4,10),(5,8),(5,9),(5,10),(5,3),(5,4),(5,5),(6,3),(6,5),(7,3),(7,4),(7,5),(6,0),(6,1),(6,2),(7,0),(7,2),(8,0),(8,1),(8,2),(6,1),(6,2),(6,3),(7,1),(7,3),(8,1),(8,2),(8,3),(6,3),(6,4),(6,5),(7,3),(7,5),(8,3),(8,4),(8,5),(6,4),(6,5),(6,6),(7,4),(7,6),(8,4),(8,5),(8,6),(7,0),(7,1),(7,2),(8,0),(8,2),(9,0),(9,1),(9,2),(7,3),(7,4),(7,5),(8,3),(8,5),(9,3),(9,4),(9,5),(7,5),(7,6),(7,7),(8,5),(8,7),(9,5),(9,6),(9,7),(8,1),(8,2),(8,3),(9,1),(9,3),(10,1),(10,2),(10,3)]

>>> step testGrid
[((0,0),6),((0,1),5),((0,2),9),((0,3),4),((0,4),2),((0,5),5),((0,6),4),((0,7),3),((0,8),3),((0,9),4),((1,0),3),((1,1),8),((1,2),5),((1,3),6),((1,4),9),((1,5),6),((1,6),5),((1,7),8),((1,8),2),((1,9),2),((2,0),6),((2,1),3),((2,2),7),((2,3),5),((2,4),6),((2,5),6),((2,6),7),((2,7),2),((2,8),8),((2,9),4),((3,0),7),((3,1),2),((3,2),5),((3,3),2),((3,4),4),((3,5),4),((3,6),7),((3,7),2),((3,8),5),((3,9),7),((4,0),7),((4,1),4),((4,2),6),((4,3),8),((4,4),4),((4,5),9),((4,6),6),((4,7),5),((4,8),8),((4,9),9),((5,0),5),((5,1),2),((5,2),7),((5,3),8),((5,4),6),((5,5),3),((5,6),5),((5,7),7),((5,8),5),((5,9),6),((6,0),3),((6,1),2),((6,2),8),((6,3),7),((6,4),9),((6,5),5),((6,6),2),((6,7),8),((6,8),3),((6,9),2),((7,0),7),((7,1),9),((7,2),9),((7,3),3),((7,4),9),((7,5),9),((7,6),2),((7,7),2),((7,8),4),((7,9),5),((8,0),5),((8,1),9),((8,2),5),((8,3),7),((8,4),9),((8,5),5),((8,6),9),((8,7),6),((8,8),6),((8,9),5),((9,0),6),((9,1),3),((9,2),9),((9,3),4),((9,4),8),((9,5),6),((9,6),2),((9,7),6),((9,8),3),((9,9),7)]

>>> iterate step testGrid !! 100
[((0,0),0),((0,1),3),((0,2),9),((0,3),7),((0,4),6),((0,5),6),((0,6),6),((0,7),8),((0,8),6),((0,9),6),((1,0),0),((1,1),7),((1,2),4),((1,3),9),((1,4),7),((1,5),6),((1,6),6),((1,7),9),((1,8),1),((1,9),8),((2,0),0),((2,1),0),((2,2),5),((2,3),3),((2,4),9),((2,5),7),((2,6),6),((2,7),9),((2,8),3),((2,9),3),((3,0),0),((3,1),0),((3,2),0),((3,3),4),((3,4),2),((3,5),9),((3,6),7),((3,7),8),((3,8),2),((3,9),2),((4,0),0),((4,1),0),((4,2),0),((4,3),4),((4,4),2),((4,5),2),((4,6),9),((4,7),8),((4,8),9),((4,9),2),((5,0),0),((5,1),0),((5,2),5),((5,3),3),((5,4),2),((5,5),2),((5,6),2),((5,7),8),((5,8),7),((5,9),7),((6,0),0),((6,1),5),((6,2),3),((6,3),2),((6,4),2),((6,5),2),((6,6),2),((6,7),9),((6,8),6),((6,9),6),((7,0),9),((7,1),3),((7,2),2),((7,3),2),((7,4),2),((7,5),2),((7,6),8),((7,7),9),((7,8),6),((7,9),6),((8,0),7),((8,1),9),((8,2),2),((8,3),2),((8,4),2),((8,5),8),((8,6),6),((8,7),8),((8,8),6),((8,9),6),((9,0),6),((9,1),7),((9,2),8),((9,3),9),((9,4),9),((9,5),9),((9,6),8),((9,7),7),((9,8),6),((9,9),6)]
-}
