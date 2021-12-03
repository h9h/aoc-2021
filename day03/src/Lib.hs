module Lib where
import Data.Char (digitToInt)
import Data.List (transpose, sort, maximumBy, group, minimumBy)
import Data.Ord (comparing)

day03 :: IO ()
day03 = do
    input <- readFile "src/input-03.txt"
    let bits = parse $ lines input
    print $ "a few Bits: " ++ show (take 5 bits)
    -- Part 1
    let mostC = toDecimal $ map (findCandidateByFrequency maximumBy) . transpose $ bits
    let leastC = toDecimal $ map (findCandidateByFrequency minimumBy) . transpose $ bits
    print $ "Most common: " ++ show mostC
    print $ "Least common: " ++ show leastC
    print $ "Part 1: " ++ show (mostC * leastC)
    -- Part 2
    let oxy = rate (findCandidateByFrequency maximumBy) bits 0
    let co2 = rate (findCandidateByFrequency minimumBy) bits 0
    print $ "Oxy: " ++ show oxy
    print $ "CO2: " ++ show co2
    print $ "Part 2: " ++ show (oxy * co2)


parse :: [String] -> [[Int]]
parse = map (map digitToInt)

-- wandle List of Ints als bits in Decimalzahl um
-- [1,0,1] --> 5
toDecimal :: [Int] -> Int
toDecimal = sum . zipWith (\pow n -> n * 2 ^ pow) [0 ..] . reverse

-- Wir sortieren und gruppieren die Spalte: group . sort
-- aus den Gruppen suchen wir die längste/kürzeste heraus: maximumBy/minimumBy (comparing length)
-- Ein Vertreter der längsten Gruppe verrät uns den Kandidaten in der Gruppe: head
findCandidateByFrequency minOrMaxBy = head . minOrMaxBy (comparing length) . group . sort

-- Part 2
rate :: ([Int] -> Int) -> [[Int]] -> Int -> Int
rate mostOrLeastCommonCandidateSelector candidates pos =
  if length candidates == 1
    -- entweder fertig
    then toDecimal $ head candidates
    -- oder gehe zur nächsten Spalte (oder über das Ende hinaus :-( )
    else rate mostOrLeastCommonCandidateSelector candidates' (pos + 1)
  where
    -- selektiere die häufigste/seltenste Ziffer in der Spalte pos
    b = mostOrLeastCommonCandidateSelector . map (!! pos) $ candidates 
    -- filtere die Liste auf die Einträge, die b an der Stelle pos haben
    candidates' = filter ((==) b . (!! pos)) candidates