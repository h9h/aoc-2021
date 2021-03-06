module Lib where

import Control.Arrow ( Arrow((&&&)) )
import Data.Maybe ( fromMaybe )
import Data.List ( group, sort, isInfixOf, elemIndex )

type Input = String
type Output = String

puzzle :: IO ()
puzzle = do
    input <- lines <$> readFile "src/input.txt"
    let signals = parse input
    print $ take 1 signals
    print $ "Part 1: " ++ show (part1 $ map snd signals)
    print $ "Part 2: " ++ show (decodeOutputs signals)
    print $ "Part 2: " ++ show (sum $ decodeOutputs signals)

--
-- Parse Input
--

parseLine :: String -> ([Input], [Output])
parseLine =  (take 10 &&& drop 11) . words

parse :: [String] -> [([Input], [Output])]
parse = map parseLine

-- Part1 
easyDigit :: Output -> Bool
easyDigit string = length string `elem` [2,3,4,7]

part1 :: [[Output]] -> Int
part1 = length . concatMap (filter easyDigit)

-- Part2:

{-
    0  1  2  3  4  5  6
    a  b  c  d  e  f  g
0:  1  1  1  0  1  1  1         len 6
1:  0  0  1  0  0  1  0  len 2
2:  1  0  1  1  1  0  1         len 5
3:  1  0  1  1  0  1  1         len 5
4:  0  1  1  1  0  1  0  len 4
5:  1  1  0  1  0  1  1         len 5
6:  1  1  0  1  1  1  1         len 6
7:  1  0  1  0  0  1  0  len 3
8:  1  1  1  1  1  1  1  len 8
9;  1  1  1  1  0  1  1         len 6

c      6        4  9    (count occurrence)
    8     8  7        7

>>> getMapping ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"]
[('a','c'),('b','f'),('c','g'),('d','a'),('e','b'),('f','d'),('g','e')]

>>> translate [('a','c'),('b','f'),('c','g'),('d','a'),('e','b'),('f','d'),('g','e')] "aceg"
"bceg"

>>> mapStringToDigit $ translate [('a','c'),('b','f'),('c','g'),('d','a'),('e','b'),('f','d'),('g','e')] "fbcad"
Just 3

-}

-- Die eigentlich Logik, alles andere ist nur mappen
getMapping :: [Input] -> [(Char,Char)]
getMapping ins = map (f . (\ x -> (head x, length x))) $ group . sort $ concat ins
  where len2 = head $ filter ((==2) . length) ins
        len4 = head $ filter ((==4) . length) ins
        f (c, 4) = (c, 'e')
        f (c, 6) = (c, 'b')
        f (c, 9) = (c, 'f')
        f (c, 7) = if [c] `isInfixOf` len4 then (c, 'd') else (c, 'g')
        f (c, 8) = if [c] `isInfixOf` len2 then (c, 'c') else (c, 'a')
        f (c, _) = error "Kann nicht sein"

translate :: [(Char, Char)] -> String -> String
translate mapping = sort . map (snd . head . (\c -> filter ((==c) . fst) mapping))

mapStringToDigit :: String -> Maybe Int
mapStringToDigit = flip elemIndex codes
    where codes = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

translateInputToPutput :: ([Input], [Output]) -> [Maybe Int]
translateInputToPutput (ins, outs) = map (mapStringToDigit . translate (getMapping ins)) outs

decodeOutputs :: [([Input], [Output])] -> [Int]
decodeOutputs = map (read . concatMap (show . fromMaybe 0) <$> translateInputToPutput)
