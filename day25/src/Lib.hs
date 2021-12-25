{-# LANGUAGE NamedFieldPuns #-}
module Lib where

import qualified Data.Map.Strict as M
import Control.Monad ( when )
import Control.Monad.State ( State, put, modify, get, gets, execState, evalState )
import Data.Tuple ( swap ) 
import Debug.Trace ( trace )

type Point = (Int, Int)
type Cucumbers = M.Map Point Char

data Seafloor = Seafloor
  { cucumbers :: Cucumbers
  , height :: Int
  , width :: Int
  , count :: Int
  , changed :: Bool
  }
  deriving (Show)

makeGrid :: [String] -> Cucumbers
makeGrid rows = M.fromList
                [ ((r, c), n)
                | (r, row) <- zip [0 ..] rows
                , (c, n  ) <- zip [0 ..] row
                , n == '>' || n == 'v'
                ]

move :: Int -> Int -> Cucumbers -> Cucumbers
move h w = moveDirection 'v' h . moveDirection '>' w

moveDirection :: Char -> Int -> Cucumbers -> Cucumbers
moveDirection typ l cs = newCs
  where newCs = M.fromList $ as ++ bs
        coord = if typ == 'v' then swap else id
        as    =  [(coord (r, c'), typ)
                 | (a,b) <- M.keys $ M.filter (== typ) cs
                 , let (r,c) = coord (a,b)
                 , let c''= (c + 1) `mod` (l + 1)
                 , let c' = if M.member (coord (r, c'')) cs then c else c''
                 ]
        bs    = M.toList $ M.filter (/= typ) cs

while :: (Show s) => (s -> Bool) -> State s () -> State s ()
while test body =
  do
    st <- get
    let continue = test st -- trace ("State " ++ show st) (test st)
    when continue $ do modify (execState body)
                       while test body

solve :: State Seafloor Int
solve = do
    while changed
            (do
              sf <- get
              let Seafloor {cucumbers, height, width, count} = sf
              let cucumbers' = move height width cucumbers
              put $ sf {cucumbers = cucumbers', count = 1 + count, changed = cucumbers' /= cucumbers})
    gets count

runSolve :: Cucumbers -> Int
runSolve cs = evalState solve $ Seafloor cs height width 0 True
  where (height, width) = (fst . M.findMax) cs

puzzle :: IO ()
puzzle = do
  input <- lines <$> readFile "src/input.txt"
  let cucumbers = makeGrid input
  let n = runSolve cucumbers
  print $ "Part 1: " ++ show n
