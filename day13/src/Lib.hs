module Lib where

import Data.List ( elem )
import Data.List.Split ( wordsBy, splitOn )
import Data.Set ( toList, fromList )

type Dot = (Int, Int)
type Instruction = (String, Int)

puzzle :: IO ()
puzzle = do
    input <- readFile "src/input.txt"
    let (dots, instructions) = parse input
    print $ take 1 dots
    print instructions
    print $ "Part 1: " ++ show (length $ fold dots (head instructions))
    print "Part 2: " 
    mapM_ putStrLn $ showCode (foldl fold dots instructions)

--
-- Parse Input
--

parse :: String -> ([Dot], [Instruction])
parse input = (parseDots, parseInstructions)
    where
        [dots, instructions] = map lines $ splitOn "\n\n" input
        parseDots = map ((\[x,y] -> (x,y)) . map read . splitOn ",") dots
        parseInstructions = map ((\[direction, n] -> (direction, read n)) . splitOn "=" . last . words) instructions

fold :: [Dot] -> Instruction -> [Dot]
fold dots instr = toList $ fromList $ f instr dots
    where f instr dots
            | fst instr == "x" = map (\(x,y) -> let n = snd instr in (n - abs(x - n), y)) dots
            | fst instr == "y" = map (\(x,y) -> let n = snd instr in (x, n - abs(y - n))) dots
            | otherwise  = dots

showCode :: [Dot] -> [String]
showCode points = [ 
                       [ if (x, y) `elem` points then '█' else ' ' | x <- [0 .. h] ] | y <- [0 .. w]
                   ]
    where (h, w) = (maximum $ map fst points, maximum $ map snd points)

{-
 ##  #    ###  #### #  # #### #  # #  #  
#  # #    #  # #    # #  #    # #  #  #  
#  # #    #  # ###  ##   ###  ##   #  #  
#### #    ###  #    # #  #    # #  #  #  
#  # #    # #  #    # #  #    # #  #  #  
#  # #### #  # #### #  # #    #  #  ##   
-}