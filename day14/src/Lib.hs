module Lib where

import Data.List ( minimum, maximum)
import Data.List.Split ( splitOn )
import qualified Data.Map.Strict as M

{-
Idee: 

Uns interessiert eigentlich nicht die gesamte Zeichenkette,
sondern nur die Anzahl der verschiedenen Buchstabenpaare,
die vorkommen.

Dafür bauen wir eine entsprechende Map auf, mit dem Buchstabenpaar
als Tupel als Schlüssel. Als Wert zählen wir das Vorkommen des Paars.

Template     N   N   C   B
              \ / \ / \ /
Step 1         C   B   H

d.h. N - N - C - B wird zur diagonalen Linie N \ C / N \ B / C \ H / B "intercalated"
d.h. Anzahl Paare --> 2 * Anzahl Paare pro Schritt
aber nicht alle Paare sind disjunkt, daher einfach Paare zählen.

Für die Lösung: Statt der Zeichenfolge haben wir dann Paare, d.h. wir können 
nicht einfach zählen:

NNCB -> NN NC CB

n Buchstaben --> n-1 Paare, 
d.h. m Paare -> (m+1)/2 Buchstaben
-}

type Polymer = M.Map (Char, Char) Integer
type Rules = M.Map (Char, Char) Char

testRules :: Rules
testPolymer :: Polymer
(testPolymer, testRules) = parse "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"

puzzle :: IO ()
puzzle = do
    input <- readFile "src/input.txt"
    let (polymer, rules) = parse input
    print $ "Part 1: " ++ show (solve rules polymer 10)
    print $ "Part 2: " ++ show (solve rules polymer 40)

--
-- Parse Input
--
addOccurence :: (Ord a) => a -> Integer -> M.Map a Integer -> M.Map a Integer
addOccurence = M.insertWith  (+)

parse :: String -> (Polymer, Rules)
parse input = (polymer, rules)
  where [[template], insertions] = map lines $ splitOn "\n\n" input
        polymer = foldr (`addOccurence` 1) M.empty $ (zip <*> tail) template
        rules = M.fromList$ map ((\[[x, y], [c]] -> ((x, y), c)) . splitOn " -> ") insertions

--
-- Logic
--

step :: Rules -> Polymer -> Polymer
step rules = M.foldrWithKey addPairs M.empty
  where addPairs (x, y) n = addOccurence (x, p) n . addOccurence (p, y) n
          where p = rules M.! (x, y)

frequency :: Polymer -> M.Map Char Integer
frequency = M.map (\n -> (n + 1) `div` 2) . M.foldrWithKey addOccurenceChars M.empty
  where addOccurenceChars (x, y) n = addOccurence x n . addOccurence y n

solve :: Rules -> Polymer -> Int -> Integer
solve rules polymer nrSteps = maximum ns - minimum ns
  where ns = frequency $ iterate (step rules) polymer !! nrSteps

{-
>>> frequency testPolymer
fromList [('B',1),('C',1),('N',2)]

>>> frequency $ (iterate (step testRules) testPolymer) !! 1
fromList [('B',2),('C',2),('H',1),('N',2)]

>>> frequency $ (iterate (step testRules) testPolymer) !! 2
fromList [('B',6),('C',4),('H',1),('N',2)]

>>> frequency $ (iterate (step testRules) testPolymer) !! 3
fromList [('B',11),('C',5),('H',4),('N',5)]
-}
