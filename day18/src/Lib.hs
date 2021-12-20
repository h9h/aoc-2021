module Lib where

import Data.Char                  ( isDigit )
import Data.List                  ( maximum )
import Text.Megaparsec            ( parse, (<|>), Parsec, MonadParsec(try) )
import Text.Megaparsec.Char       ( char )
import Text.Megaparsec.Char.Lexer ( decimal )
import Data.Void                  ( Void )
import Control.Monad              ( (>=>) )

type Parser = Parsec Void String

data SnailNr = Regular Int | Pair SnailNr SnailNr deriving (Show, Eq)

--
-- Parse Input
--

parseSnailNr :: String -> SnailNr
parseSnailNr s = let Right nr = parse snail "" s in nr
 where
  snail :: Parser SnailNr
  snail = try regular <|> pair
  regular = Regular <$> decimal
  pair = Pair <$> (char '[' *> snail <* char ',') <*> snail <* char ']'

parseInput :: [String] -> [SnailNr]
parseInput = map parseSnailNr

--
-- Puzzle
--

puzzle :: IO ()
puzzle = do
    input <- lines <$> readFile "src/input.txt"
    let snails = parseInput input
    print $ "Part 1: " ++ show (solve1 snails)
    print $ "Part 2: " ++ show (solve2 snails)

solve1 :: [SnailNr] -> Int
solve1 snails = magnitude $ foldl add (head snails) snails

solve2 :: [SnailNr] -> Int
solve2 snails = maximum $ map magnitude $ [add s1 s2 | s1 <- snails, s2 <- snails, s1 /= s2]

--
-- Logik
--

add :: SnailNr -> SnailNr -> SnailNr
add s1 s2 = reduction $ Pair s1 s2

addLeft :: Int -> SnailNr -> SnailNr
addLeft n (Regular m) = Regular (n+m)
addLeft n (Pair a b) = Pair (addLeft n a) b

addRight :: SnailNr -> Int -> SnailNr
addRight (Regular m) n = Regular (n+m)
addRight (Pair a b) n = Pair a (addRight b n)

{-| To reduce a snailfish number, you must repeatedly do the first action in this
    list that applies to the snailfish number:

    If any pair is nested inside four pairs, the leftmost such pair explodes.
    If any regular number is 10 or greater, the leftmost such regular number splits.
    Once no action in the above list applies, the snailfish number is reduced.

    During reduction, at most one action applies, after which the process returns 
    to the top of the list of actions. For example, if split produces a pair that 
    meets the explode criteria, that pair explodes before other splits occur.
-}
reduction :: SnailNr -> SnailNr
reduction = either reduction id . (explode >=> split)

{-| To explode a pair, the pair's left value is added to the first regular
    number to the left of the exploding pair (if any), and the pair's right 
    value is added to the first regular number to the right of the exploding 
    pair (if any). Exploding pairs will always consist of two regular numbers. 
    Then, the entire exploding pair is replaced with the regular number 0.
-}
explode :: SnailNr -> Either SnailNr SnailNr
explode = either (\(_, s, _) -> Left s) Right . helper 0
    where
        helper :: Int -> SnailNr -> Either (Int, SnailNr, Int) SnailNr
        helper _ (Regular n) = Right (Regular n)
        helper depth (Pair (Regular n) (Regular m)) | depth >= 4 = Left (n, Regular 0, m)
        helper depth (Pair l r) = case helper (depth + 1) l of
            Left (a, l, b) -> Left (a, Pair l (addLeft b r), 0)
            _              -> case helper (depth + 1) r of
                Left (a, r, b) -> Left (0, Pair (addRight l a) r, b)
                _              -> Right (Pair l r)

{-| To split a regular number, replace it with a pair; the left element of the 
    pair should be the regular number divided by two and rounded down, while the 
    right element of the pair should be the regular number divided by two and 
    rounded up. 
    For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes [6,6], and so on.
-}
split :: SnailNr -> Either SnailNr SnailNr
split = helper
    where
        helper (Regular n)
            | n >= 10   = Left $ Pair (Regular $ n `quot` 2) (Regular $ (n + 1) `quot` 2)
            | otherwise = Right (Regular n)
        helper (Pair l r) = case helper l of
            Left l' -> Left (Pair l' r)
            _       -> case helper r of
                Left r' -> Left (Pair l r')
                _       -> Right (Pair l r)

magnitude :: SnailNr -> Int
magnitude (Regular n) = n
magnitude (Pair s t) = 3 * magnitude s + 2 * magnitude t


