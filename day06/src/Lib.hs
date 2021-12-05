module Lib where

type X = Int

puzzle :: IO ()
puzzle = do
    input <- lines <$> readFile "src/input.txt"
    let xxx = parse input
    print $ take 1 xxx
    print $ "Part 1: " ++ "todo"
    print $ "Part 2: " ++ "todo"

--
-- Parse Input
--

parseLine :: String -> X
parseLine _ = 1

parse :: [String] -> [X]
parse = map parseLine
