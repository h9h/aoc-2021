module Lib where

day01 :: IO ()
day01 = do
    contents <- readFile "src/input-01.txt"
    let numbers = getNumbers contents
    print "Part 1:"
    print $ getIncrements numbers
    print "Part 2:"
    print $ getSlidingIncrements 3 numbers

-- read lines of numberstring into list of Ints
getNumbers :: String -> [Int]
getNumbers = map read . lines

-- count the number of increments within a list
getIncrements :: [Int] -> Int
getIncrements numbers = sum $ zipWith countIncrease numbers (tail numbers)
    where countIncrease a b = max 0 . signum $ b-a

{- Alternative:
getIncrements numbers = sum $ countIncrease <$> (zip <*> tail $ numbers)
    where countIncrease (a, b) = max 0 . signum $ b-a
-}

-- build a list of sliding windows with given size from a list
slidingWindows :: Int -> [a] -> [[a]]
slidingWindows _ [] = []
slidingWindows size ls@(x:xs) = filter longEnough $ take size ls : slidingWindows size xs
    where longEnough xs = length xs >= size

-- count the number of sliding increments within a list
getSlidingIncrements :: Int -> [Int] -> Int 
getSlidingIncrements size numbers = getIncrements $ sum <$> slidingWindows size numbers

{- 
Here's an arguably nicer alternative, since it catches both cases:
It hinges on the observation, that the difference between adjacent sliding windows is 
determined only by the *non-overlapping* elements:
   [1,2,3] --> [2,3,4] share [2,3], 
so only the difference between the first element of the first window and the last element
of the second window matter.
-}
getIncrements' :: Int -> [Int] -> Int
getIncrements' size numbers = length $ filter (== True) $ zipWith (<) numbers (drop size numbers)

-- or a similar one (here point-free):
getIncrements'' :: Int -> [Int] -> Int 
getIncrements'' size = length . filter (> 0) . (zipWith (-) <$> drop size <*> id)

{-
Note also a common pattern:

    `zip <*> tail $ xs` is equivalent to `zip xs (tail xs)`

so zip <*> tail $ [1, 2, 3] = [[1,2], [2,3]]
-}

