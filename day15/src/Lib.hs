module Lib where

import qualified Data.Map.Strict as M
import           Algorithm.Search  ( dijkstra, aStar )

type Cost  = Int
type State = (Int, Int)
type Grid  = M.Map State Cost

output :: Maybe (Int, [(Int, Int)]) -> String
output x = case x of
  Just (n, _) -> show n
  Nothing     -> error "fail"

puzzle :: IO ()
puzzle = do
    input <- lines <$> readFile "src/input.txt"
    let grid = makeGrid 100 1 input
    print $ "Part 1           : " ++ output (solve grid)
    let grid2 = makeGrid 100 5 input
    print $ "Part 2 - dijkstra: " ++ output (solve grid2)
    print $ "Part 2 - a*      : " ++ output (solve2 grid2)


makeGrid :: Int -> Int -> [String] -> Grid
makeGrid step copies rows = M.fromList
  [ ((r + i * step, c + j * step), iterate addWrap(read [n]) !! (i+j))
  | i        <- [0 .. copies-1]
  , j        <- [0 .. copies-1]
  , (r, row) <- zip [0 ..] rows
  , (c, n  ) <- zip [0 ..] row
  ]
  where addWrap n = if n > 8 then 1 else n + 1

{-
Tatsächlich kann es günstiger sein auch mal einen Schritt zurückzutreten...
(für meine Daten ergeben sich ansonsten (falsche) Kosten von 2816 statt 2809 im Teil 2)
-}
neighbors :: Grid -> State -> [State]
neighbors grid (x, y) = filter (`M.member` grid) [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

cost :: Grid -> State -> State -> Cost
cost grid _ state = grid M.! state

finished :: Grid -> State -> Bool
finished grid state = state == (fst . M.findMax) grid

solve :: Grid -> Maybe (Cost, [State])
solve grid = dijkstra (neighbors grid) (cost grid) (finished grid) (0,0)

{-
Alternativ statt Dijkstra mit a*
-}

estimate :: Grid -> State -> Cost
estimate grid state = dx + dy
  where (h, w) = (fst . M.findMax) grid
        (x, y) = state
        (dx, dy) = (h-x, w-y)

solve2 :: Grid -> Maybe (Cost, [State])
solve2 grid = aStar (neighbors grid) (cost grid) (estimate grid) (finished grid) (0,0)
