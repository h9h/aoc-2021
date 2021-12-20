{-# LANGUAGE TupleSections #-}
module Lib where

{-
Adapted from https://gitlab.com/sakisan/adventofcode/-/blob/2021/Haskell/Day19.hs
-}   

import Data.Either (partitionEithers)
import Data.List (nub, permutations, transpose)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Arrow ( Arrow((&&&)) )

type Position = [Int]
type Scanner  = [Position]

puzzle :: IO ()
puzzle = do
    scanners <- parseInput <$> readFile "src/input.txt"
    let x:xs = scanners
    let aligned = computeAllPositions [(x, [0, 0, 0])] [x] xs
    print $ "Part 1: " ++ show (length . nub . concatMap fst $ aligned)
    print $ "Part 2: " ++ show (maximum . map (uncurry manhattan) . pick2 . map snd $ aligned)

--
-- Parse Input
--

parsePoint :: String -> Position
parsePoint = map read  . splitOn ","

parseScannerNr :: String -> Int
parseScannerNr s = read $ splitOn " " s !! 2

parseScanner :: String -> Scanner
parseScanner = map parsePoint . tail . lines

parseInput :: String -> [Scanner]
parseInput = map parseScanner  . splitOn "\n\n"

{-|
The first parameter is the result list, that is being recursively build.
The second parameter contains the scanners, that have been found.
The third parameter is the list of scanners yet to be positioned.

Each time around we try all yet unfound scanners against our current reference and 
partition those into "found scanners" and "unknown scanners".
Unless the puzzle was broken, there must be at least one newly found scanner (as
long as there are still unknown scanners). Hence we are safe to take the first
of those newly found scanners and add it to the results.
Since we partitioned into "found" and "not found", we can be certain that the number
of unfound scanners is decreasing.

Pattern:
--------

f accumulator []        = accumulator
f accumulator workqueue = f (result ++ accumulator) newWorkqueue
  where (result, newWorkqueue) = ...

-}
computeAllPositions :: [(Scanner, Position)] -> [Scanner] -> [Scanner] -> [(Scanner, Position)]
computeAllPositions accumulator _ [] = accumulator
computeAllPositions accumulator (ref:refs) scanners 
  = computeAllPositions (found ++ accumulator) (map fst found ++ refs) notFound
    where (found, notFound) = partitionByFound ref scanners
computeAllPositions _ _ _ = error "in align"

{-| We partition into positioned scanners (fst) and those still unknown (snd).

    allignWithReference returns a potentially empty list of positioned scanners.
    List comprehension on result:
      Case non-empty: return Left positioned scanner
      Case empty: <Nothing> gets the default value Right scanner
    partitionEithers splits the Right's and Left's into a tuple.
-}
partitionByFound :: Scanner -> [Scanner] -> ([(Scanner, Position)], [Scanner])
partitionByFound ref scanners = partitionEithers
          [ maybe (Right scanner) Left . safeHead $ alignWithReference ref scanner
          | scanner <- scanners
          ]

{-| A list of positioned scanners, or the empty list 
-}
alignWithReference :: Scanner -> Scanner -> [(Scanner, Position)]
alignWithReference reference scanner = [(map (add pos) o, pos) 
                           | o <- orientations scanner
                           , pos <- overlap reference o
                           ]

{-| Calculate relative position. Return it, if enough beacons in overlap.
-}
overlap :: Scanner -> Scanner -> [Position]
overlap as bs = Map.keys . Map.filter (>= 12) . Map.fromListWith (+) . map (, 1) $ diff <$> as <*> bs

orientations :: Scanner -> [Scanner]
orientations ps = transpose $ map orientationsPos ps

orientationsPos :: Position -> [Position]
orientationsPos p = scanl (flip ($)) p steps
  -- https://stackoverflow.com/questions/16452383/how-to-get-all-24-rotations-of-a-3-dimensional-array
  where steps = [r,t,t,t,r,t,t,t,r,t,t,t, r.t.r.r, t,t,t,r,t,t,t,r,t,t,t]
        r [x,y,z] = [x,z,-y]
        r _ = error "in roll"
        t [x,y,z] = [-y,x,z]
        t _ = error "in turn"

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

pick2 :: [a] -> [(a,a)]
pick2 [] = []
pick2 (x:xs) = map (x ,) xs ++ pick2 xs

manhattan :: Position -> Position -> Int
manhattan a b = sum $ map abs $ diff a b

diff :: Position -> Position -> Position
diff = zipWith (-)

add :: Position -> Position -> Position
add = zipWith (+)

