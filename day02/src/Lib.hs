module Lib where

import Data.Foldable (Foldable(foldl'))

day02 :: IO ()
day02 = do
    contents <- readFile "src/input-02.txt"
    let commands = getCommands contents
    print $ take 3 commands
    print "Part 1:"
    let state1 = runCommands move commands
    print state1
    print $ "Answer 1 = " ++ show (position state1 * depth state1)
    print "Part 2:"
    let state2 = runCommands move2 commands
    print state2
    print $ "Answer 2 = " ++ show (position state2 * depth state2)

-- is it worth it, defining these types?
data Command = Forward Int | Up Int | Down Int deriving (Show)
data State = State { position :: Int, depth :: Int, aim :: Int } deriving (Eq, Show)

-- read lines of numberstring into list of commands
getCommands :: String -> [Command]
getCommands = map parseCommand . lines
  where
  -- parse string representation of command into structure
  --
  -- In a "real" program, we wouldn't want to crash on invalid input
  -- The return type would rather be a Maybe Command
  parseCommand :: String -> Command
  parseCommand line =
    case words line of
      ["forward", x] -> Forward (read x)
      ["up", x] -> Up (read x)
      ["down", x] -> Down (read x)
      _ -> error $ "Invalid command: " ++ line

-- Sequence a list of commands over a starting state to return the final state
--
-- `engine` knows how to transform a state with a given command into a new state
runCommands :: (State -> Command -> State) -> [Command] -> State
runCommands engine = foldl' engine (State 0 0 0)

-- Calculation effect of command on state for part 1
move :: State -> Command -> State
move state command = case command of
    Forward x -> State {position = position state + x, depth = depth state, aim=0}
    Up x -> State {position = position state, depth = depth state - x, aim=0}
    Down x -> State {position = position state, depth = depth state + x, aim=0}

-- Calculation effect of command on state for part 2
move2 :: State -> Command -> State
move2 state command = case command of
    Forward x -> State {position = position state + x, depth = depth state + aim state * x, aim = aim state}
    Up x -> State {position = position state, depth = depth state, aim = aim state - x}
    Down x -> State {position = position state, depth = depth state, aim = aim state + x}

{-
With GHC 9.2 there will be a more elegant way of updating fields in a record:

    Forward x -> State {position = position + x, depth = depth + aim * x}
    Up x -> State {aim = aim - x}
    Down x -> State {aim = aim + x}

Unchanged fields do not have to be mentioned.
-}