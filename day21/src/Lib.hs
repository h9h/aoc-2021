module Lib where

import Control.Monad ( replicateM )
import Data.Array ( (!), listArray, range )
import Data.Foldable (Foldable(toList))

type Die    = Int 
type Pos    = Int
type Score  = Int
type Player = (Pos, Score)

puzzle :: IO ()
puzzle = do
  let input = (7, 10)
  print $ "Part 1: " ++ show (solve1 input)
  print $ "Part 2: " ++ show (solve2 input)  


modDie :: Int -> Int
modDie n = (n - 1) `mod` 100 + 1

modPos :: Int -> Int
modPos n = (n - 1) `mod` 10 + 1

{- 
-------------------------------  Part 1  -------------------------------
-}
solve1 :: (Pos, Pos) -> Int
solve1 (p1, p2) = head
  [ n * score1
  | (n, ((_, score1), (_, score2), _)) <- zip [0, 3..] $ iterate turns ((p1, 0), (p2, 0), 1)
  , score2 >= 1000
  ]


{-| Players take turns, so flip players around
-}
turns :: (Player, Player, Die) -> (Player, Player, Die)
turns ((p1, s1), p, d) = (p, (p1', s1 + p1'), modDie (d + 3) ) 
  where
    p1' = modPos (p1 + rolls)
    rolls = modDie d + modDie (d + 1) + modDie (d + 2)


{- 
-------------------------------  Part 2  -------------------------------
-}

-- relative occurrence of sum of three Dirac Dice throws: just once for sum 3 etc.
allThrows :: [(Die, Int)]
allThrows = [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]


{-| All possible incompleted states of this Game are:
    - for each player: Position 1..10, Score 0..20
    - so we cumulate number of possible universes in an (10,20,10,20) Array
      - remember: "when you roll it, the universe splits into multiple copies, 
                  one copy for each possible outcome of the die."
    - Since haskell lazily unravels the paths to wins, the final number of
      winning games is for each player at Position 0 to be found.
-}
bounds :: ((Pos, Score, Pos, Score), (Pos, Score, Pos, Score))
bounds = ((1, 0, 1, 0), (10, 20, 10, 20))

solve2 :: (Int, Int) -> Int
solve2 (p1, p2) = uncurry max $ scores ! (p1, 0, p2, 0)
  where
    scores = listArray bounds
      [ foldr addTuple (0, 0)
          -- if we end, we end with n wins
          -- else this state gives rise to n times the instances to this state  
          -- Since we add as absolutenumber only the wins, those are the only
          -- paths that contribute to the value at this position (since n * 0 == 0).
          [ if s1 + p'   >= 21 then (n, 0) else multTuple n (u2, u1)
          | (rolls, n)   <- allThrows
          , let p'       = modPos (p1 + rolls)
          , let (u1, u2) = scores ! (p2, s2, p', s1 + p')
          ]
      | (p1, s1, p2, s2) <- range bounds
      ]
    multTuple  n    (a, b) = (n * a, n * b)  
    addTuple (a, b) (c, d) = (a + c, b + d)
