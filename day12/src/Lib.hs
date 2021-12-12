module Lib where

import Data.List ( elem, notElem )
import Data.List.Split ( wordsBy )
import Data.Char ( isLower )

type Node = String
type Edge = (Node, Node)

puzzle :: IO ()
puzzle = do
    input <- lines <$> readFile "src/input.txt"
    let edges = parse input
    print $ edges
    print $ "Part 1: " ++ show (traverseEdges edges [] False "start")
    print $ "Part 2: " ++ show (traverseEdges edges [] True "start")

testInput :: [Edge]
testInput = parse $ lines "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"

--
-- Parse Input
--

parseline :: String -> Edge
parseline line = f $ take 2 $ wordsBy ( == '-') line
    where f [x,y] = (x,y)
          f _ = error "Failed parsing"

parse :: [String] -> [Edge]
parse = map parseline

isSmallCave :: Node -> Bool
isSmallCave = all isLower

targets :: [Edge] -> Node -> [Node]
targets edges node = 
    map (\(a,b) -> if node == a then b else a) $ 
    filter (\(a,b) -> node == a || node == b) edges

traverseEdges :: [Edge] -> [Node] -> Bool -> Node -> Int
traverseEdges edges seen canTwice node 
    | node == "end" = 1
    | node `notElem` seen = sum [traverseEdges edges (node : seen) canTwice n | n <- targets edges node]
    | node `elem` seen  = case () of
        ()  | node == "start"  -> 0
            | isSmallCave node && not canTwice -> 0
            | isSmallCave node -> sum [traverseEdges edges (node : seen) False n | n <- targets edges node]
            | otherwise -> sum [traverseEdges edges (node : seen) canTwice n | n <- targets edges node]
    | otherwise = error "Fail"
