module Lib where

import Data.List ( sort, elemIndex )
import Control.Applicative ( liftA )

data CheckResult = Ok | Incomplete [Char] | Corrupted Char deriving (Show, Eq)

puzzle :: IO ()
puzzle = do
    input <- lines <$> readFile "src/input.txt"
    print $ head input
    print $ "Part 1: " ++ show (part1 input)
    print $ "Part 2: " ++ show (part2 input)

openBrackets = "([{<"
closeBrackets = ")]}>"

{-
Idea:
- push char onto stack
- if next closes top of stack pop stack and continue with stack
    else push next onto stack
- remaining rest is either incomplete or corrupt
-}
removePairs :: [Char] -> [Char]
removePairs = reverse . foldl removePair []
            where
                removePair ('(':ls) ')' = ls
                removePair ('{':ls) '}' = ls
                removePair ('[':ls) ']' = ls
                removePair ('<':ls) '>' = ls
                removePair ls c = c:ls


removeOpenBrackets :: [Char] -> [Char]
removeOpenBrackets [] = []
removeOpenBrackets (c:xs)
    | c `elem` openBrackets = removeOpenBrackets xs
    | otherwise = c:xs

categorise :: String -> CheckResult
categorise = f . removePairs
    where f s
            | s == "" = Ok
            | all (`notElem` s) closeBrackets = Incomplete s
            | otherwise = Corrupted $ head $ removeOpenBrackets s

costFalse :: Char -> Int
costFalse c = case elemIndex c closeBrackets of
    Just i -> [3, 57, 1197, 25137] !! i
    Nothing -> 0

part1 :: [String] -> Int
part1 input = sum $ [costFalse c | (Corrupted c) <- map categorise input]

costCorrect :: Int -> [Char] -> Int
costCorrect m [] = m
costCorrect m (c:cs) = case elemIndex c openBrackets of
    Just i -> costCorrect (m * 5 + (i + 1)) cs
    Nothing -> m

part2 :: [String] -> Int
part2 input = let l = [reverse x | (Incomplete x) <- map categorise input]
          in  sort (map (costCorrect 0) l) !! (length l `div` 2)
