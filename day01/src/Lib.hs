module Lib
    ( day01
    ) where

day01 :: IO ()
day01 = do
    contents <- readFile "src/input-01.txt"
    let numbers = getNumbers contents
    print "Part 1:"
    print $ getIncrements numbers
    print "Part 2:"
    print $ getSlidingIncrements 3 numbers

getNumbers :: String -> [Int]
getNumbers str = read <$> lines str

countIncrease :: Int -> Int -> Int 
countIncrease a b = if b > a then 1 else 0

getIncrements :: [Int] -> Int
getIncrements numbers = sum $ zipWith countIncrease numbers (tail numbers)

windowed :: Int -> [a] -> [[a]]
windowed size ls =
    case ls of
        [] -> []
        x:xs ->
            if length ls >= size then
                take size ls : windowed size xs
            else windowed size xs

getSlidingIncrements :: Int -> [Int] -> Int 
getSlidingIncrements size numbers = getIncrements $ sum <$> windowed size numbers
