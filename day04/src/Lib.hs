module Lib where

import Data.List( maximumBy, minimumBy, transpose )
import Data.Ord ( comparing )

type Draw = Int
type Board = [[(Int, Bool )]]
type Result = Int
type Idx = Int

day04 :: IO ()
day04 = do
    input <- lines <$> readFile "src/input-04.txt"
    let (draws, boards) = parse input
    print $ "Part 1: " ++ show (calculateResult draws $ getFirstWinner boards draws)
    print $ "Part 2: " ++ show (calculateResult draws $ getLastWinner boards draws)

-- parse input into draws (from first line) and boards
parse :: [String] -> ([Draw], [Board])
parse input = (parseDraws $ head input, parseBoards $ tail input)
  where
      -- first line: substitute blank for comma and split with words:
      parseDraws l = read <$> words [if c == ',' then ' ' else c|c <- l]  :: [Int]
      -- subsequent lines: group in 6, drop blank first line, split lines with words
      -- and map String to Int and make tuple (Int, Bool):
      parseBoards ls = map (map (map (\i -> (read i :: Int, False)))) b
          where b = map (map words . drop 1) (groupN 6 ls)

groupN :: Int -> [String] -> [[String]]
groupN _ [] = []
groupN n l = take n l : groupN n (drop n l)

-- cross off the drawn number on bingo board
applyDraw :: Board -> Draw -> Board
applyDraw board draw = map (map (\(n, b) -> if n == draw then (n, True) else (n, b))) board

-- do that for all draws and return the sukzessivly changed board
applyDraws :: Board -> [Draw] -> [Board]
applyDraws = scanl applyDraw 

isWinner :: Board -> Bool
isWinner board = any (all snd) board || any (all snd) (transpose board)

{- To a given board and the draws return a tuple of the index into draws and the winning board.

   We apply all draws to the board, into an intermediate list of (idx, board)
   and then filter this list with isWinner, returning the first tuple of this filtered list
   We return the index, so we can determin which boards won first or last.

   The index needs to start at -1, because the first tuple in applyDraws contains the starting board,
   i.e. the starting value in scanl

   scanl f z [x1, x2, ..] == [z, f z x1, f (f z x1) x2]  -- here z = starting board
-}
getWinningBoard :: Board -> [Draw] -> (Idx, Board)
getWinningBoard board = head . filter (isWinner . snd) . zip [-1 ..] . applyDraws board

getAllWinningBoards :: [Board] -> [Draw] -> [(Idx, Board)]
getAllWinningBoards boards draws = map (`getWinningBoard` draws) boards

getFirstWinner :: [Board] -> [Draw] -> (Idx, Board)
getFirstWinner boards draws = minimumBy (comparing fst) $ getAllWinningBoards boards draws 

getLastWinner :: [Board] -> [Draw] -> (Idx, Board)
getLastWinner boards draws = maximumBy (comparing fst) $ getAllWinningBoards boards draws

sumBoard :: Board -> Int
sumBoard b = sum . map fst . filter (not . snd) $ concat b

calculateResult :: [Draw] -> (Idx, Board) -> Result
calculateResult draws (idx, board) = draws !! idx * sumBoard board
