module Lib where

import Data.List ( maximum )

type TargetArea = ((Int, Int), (Int, Int))
type Velocity = (Int, Int)
type Point = (Int, Int) 
type Probe = (Point, Velocity)
type Path = [Probe]

target = ((288,330),(-96,-50))

puzzle :: IO ()
puzzle = do
    let paths = potentialPaths target
    print $ "Part 1: " ++ show (maximum . map (\((_,y), _) -> y) . concat $ paths)
    print $ "Part 2: " ++ show (length paths)

move :: Probe -> Probe
move ((x, y), (dx, dy)) = ((x + dx, y + dy), (dx - signum dx,dy - 1))

targetHit :: TargetArea -> Path -> Bool
targetHit ((x0,x1), (y0,y1)) = any  (\((x,y), _) -> x0 <= x && x <= x1 
                                                 && y0 <= y && y <= y1)

path :: TargetArea -> Velocity -> Path
path (_, (y0,_)) (dx, dy) = takeWhile (\((x,y), _) -> y >= y0) 
                                 $ iterate move ((0,0), (dx, dy))

potentialPaths :: TargetArea -> [Path]
potentialPaths ta@((x0,x1), (y0,_)) = 
    [ p 
    | dx <- [1..x1]
    , dx >= floor(sqrt (2.0 * fromIntegral x0))
    , dy <- [y0..abs y0]
    , let p = path ta (dx,dy)
    , targetHit ta p
    ]

{-
>>> path target (25,5)
[((0,0),(25,5)),((25,5),(24,4)),((49,9),(23,3)),((72,12),(22,2)),((94,14),(21,1)),((115,15),(20,0)),((135,15),(19,-1)),((154,14),(18,-2)),((172,12),(17,-3)),((189,9),(16,-4)),((205,5),(15,-5)),((220,0),(14,-6)),((234,-6),(13,-7)),((247,-13),(12,-8)),((259,-21),(11,-9)),((270,-30),(10,-10)),((280,-40),(9,-11)),((289,-51),(8,-12)),((297,-63),(7,-13)),((304,-76),(6,-14)),((310,-90),(5,-15))]

>>> targetHit target $ path target (25,5)
True
-}
