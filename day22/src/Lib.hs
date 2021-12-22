{-# LANGUAGE TupleSections #-}
module Lib where

import Text.Megaparsec            ( parseTest, parse, (<|>), Parsec, MonadParsec(try), skipSome, empty )
import Text.Megaparsec.Char       ( char, string, spaceChar )
import Text.Megaparsec.Char.Lexer ( lexeme, signed, decimal, space )
import Data.Void                  ( Void )
import Data.Maybe                 ( mapMaybe )
import qualified Data.Set as S

type Parser = Parsec Void String

type Range        = (Int, Int)
type Point        = (Int, Int, Int)
type Instruction  = (Bool, Box)
type Box          = (Point, Point)
type Multiplicity = Int

puzzle :: IO ()
puzzle = do
    input <- lines <$> readFile "src/input.txt"
    let instructions = parseInput input
    let instructionsPart1 = executeInstructions $ take 20 instructions
    print $ "Boxes to sum in Part 1: " ++ show (length instructionsPart1)
    print $ "Part 1                : " ++ show (solve instructionsPart1)
    let instructionsPart2 = executeInstructions instructions
    print $ "Boxes to sum in Part 2: " ++ show (length instructionsPart2)
    print $ "Part 2                : " ++ show (solve instructionsPart2)

--
-- Parse Input
--

parseInput :: [String] -> [Instruction]
parseInput = map parseInstruction

-- on x=10..12,y=10..12,z=10..12
parseInstruction :: String -> Instruction
parseInstruction s = case parse instruction "" s of
  Right instr -> instr
  Left _ -> error "in parse instructions"

instruction :: Parser Instruction
instruction = (,) <$> onOff <*> coordRanges

onOff :: Parser Bool
onOff = (== "on") <$> try (string "on" <|> string "off")

sc :: Parser ()
sc = space (skipSome spaceChar) empty empty

signedInteger :: Parser Int
signedInteger = signed sc decimal

coordRange :: Parser Range
coordRange = do
  d1 <- signedInteger
  string ".."
  d2 <- signedInteger
  return (d1, d2)

separator :: Parser [Char]
separator = try (string " x=" <|> string ",y=" <|> string ",z=")

coordRanges :: Parser Box
coordRanges = do
   separator
   r1 <- coordRange
   separator
   r2 <- coordRange
   separator
   r3 <- coordRange
   return ((fst r1, fst r2, fst r3), (snd r1, snd r2, snd r3))

--
-- Geometry
--

intersection :: Box -> Box -> Maybe Box
intersection ((x1, y1, z1), (x1', y1', z1')) ((x2, y2, z2), (x2', y2', z2'))
 | x2 > x1' || x1 > x2' = Nothing
 | y2 > y1' || y1 > y2' = Nothing
 | z2 > z1' || z1 > z2' = Nothing
 | otherwise = Just ((max x1 x2, max y1 y2, max z1 z2), (min x1' x2', min y1' y2', min z1' z2'))

volume :: Box -> Int
volume ((x, y, z), (x', y', z')) = (x' - x + 1) * (y' - y + 1) * (z' - z + 1)

{-
_____________________________________
| 1                      ___________|____
|                        | -1       :   |
-------------------------| .........:   |
                         |            0 |
                         ----------------
-}
flipOff :: [(Box, Multiplicity)] -> Box -> [(Box, Multiplicity)]
flipOff boxes box = boxes ++ mapMaybe subtractIntersection boxes
    where subtractIntersection :: (Box, Int) -> Maybe (Box, Int)
          subtractIntersection (box', multiplicity)
              = (, negate multiplicity) <$> intersection box box'

{-
Die Schnittmenge müssen wir neutralisieren, damit die Schnittmenge
nicht doppelt gezählt wird.
_____________________________________
| 1                      ___________|____
|                        | -1       :   |
-------------------------| .........:   |
                         |            1 |
                         ----------------
-}
flipOn :: [(Box, Multiplicity)] -> Box -> [(Box, Multiplicity)]
flipOn boxes box = (box, 1) : flipOff boxes box

executeInstructions :: [Instruction] -> [(Box, Multiplicity)]
executeInstructions = foldl exec []
  where exec :: [(Box, Multiplicity)] -> (Bool, Box) -> [(Box, Multiplicity)]
        exec boxes (True,  box) = flipOn  boxes box
        exec boxes (False, box) = flipOff boxes box

solve :: [(Box, Multiplicity)] -> Int
solve = sum . map (\(box, multiplicity) -> multiplicity * volume box)
