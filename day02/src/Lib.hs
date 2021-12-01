module Lib where

day02 :: IO ()
day02 = do
    contents <- readFile "src/input-01.txt"
    let numbers = getNumbers contents
    print numbers
    print "Part 1:"
    print $ "to be done"
    print "Part 2:"
    print $ "to be done"

-- read lines of numberstring into list of Ints
getNumbers :: String -> [Int]
getNumbers = map read . lines

