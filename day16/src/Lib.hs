--
-- Failed with Haskell on day 16! Solved my problem with JS.
-- This here is the solution from https://github.com/teekookoo/advent-2021/blob/main/app/Day16.hs
--
-- Tutorial on Megaparsec here https://markkarpov.com/tutorial/megaparsec.html
--

{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad        ( when )
import Data.Bifunctor       ( first, second )
import Numeric              ( showIntAtBase )
import Data.Char            ( intToDigit, digitToInt )
import Data.List            ( foldl' )
import Data.Text            ( Text, pack )
import Data.Void            ( Void )
import Data.Function        ( (&) )
import Text.Megaparsec      ( oneOf,
                              single,
                              count,
                              many,
                              optional,
                              (<|>),
                              parse,
                              parseTest,
                              runParser,
                              errorBundlePretty,
                              Parsec,
                              MonadParsec(getParserState),
                              State(stateOffset) )
import Text.Megaparsec.Char ( eol )
import qualified Data.Text as T

type Version = Int
type Type    = Int
type Value   = Int

data Packet = Literal Version Value
            | Sum     Version [Packet]
            | Product Version [Packet]
            | Minimum Version [Packet]
            | Maximum Version [Packet]
            | Greater Version Packet Packet
            | Less    Version Packet Packet
            | Equal   Version Packet Packet
            deriving (Show, Eq)

data LengthInfo = Bits    Int
                | Packets Int
                deriving (Show, Eq)

puzzle :: IO ()
puzzle = do
    input <- readFile "src/input.txt"
    let bincode = readInput input
    let packet = parse' parser bincode
    print $ "Part 1: " ++ show (solve1 packet)
    print $ "Part 2: " ++ show (solve2 packet)

--
-- Parse Input
--

readInput :: String -> Text
readInput = pack . concatMap hexToBits

intToBits :: Int -> String
intToBits n = drop (length s) $ "0000" ++ s
  where s = showIntAtBase 2 intToDigit n ""

hexToBits :: Char -> String
hexToBits c
  | c `elem` ['0'..'9'] = intToBits $ read [c]
  | c `elem` ['A'..'F'] = intToBits $ fromEnum c - 55
  | otherwise = error $ "Invalid Char " ++ [c]

bitsToInt :: String -> Int
bitsToInt = foldl' (\num digit -> 2*num + digit) 0 . map digitToInt

{-
----- Parser ----- 
-}

type Parser  = Parsec Void Text

parse' :: Parser c -> Text -> c
parse' p t = either (error . errorBundlePretty) id $ parse p "(source)" t

{- The BITS transmission contains a single packet at its outermost layer 
which itself contains many other packets.-}

packet :: Parser Packet
packet  = (&) <$> version <*> (typeID >>= partialPacket)

{- The hexadecimal representation of this packet might encode a few extra 0 bits
 at the end; these are not part of the transmission and should be ignored.-}

parser :: Parser Packet
parser = packet <* many (single '0') <* optional eol

{- Every packet begins with a standard header: the first three bits encode the 
packet version, and the next three bits encode the packet type ID. These two values
are numbers; all numbers encoded in any packet are represented as binary with the 
most significant bit first. For example, a version encoded as the binary sequence 
100 represents the number 4.-}

version :: Parser Int
version = bitsToInt <$> bits 3

typeID :: Parser Int
typeID  = bitsToInt <$> bits 3

bits :: Int -> Parser String
bits n = count n $ oneOf ['0', '1']

{- Packets with type ID 4 represent a literal value. Literal value packets encode
a single binary number. To do this, the binary number is padded with leading zeroes 
until its length is a multiple of four bits, and then it is broken into groups of 
four bits. Each group is prefixed by a 1 bit except the last group, which is prefixed 
by a 0 bit. These groups of five bits immediately follow the packet header.-}

literalValue :: Parser Value
literalValue = (\gs g -> bitsToInt $ foldr (++) g gs)
  <$> many (single '1' *> bits 4) <* single '0' <*> bits 4

{- Every other type of packet (any packet with a type ID other than 4) represent an 
operator that performs some calculation on one or more sub-packets contained within.

An operator packet contains one or more packets. To indicate which subsequent binary 
data represents its sub-packets, an operator packet can use one of two modes indicated 
by the bit immediately after the packet header; this is called the length type ID:

If the length type ID is 0, then the next 15 bits are a number that represents the 
total length in bits of the sub-packets contained by this packet.
If the length type ID is 1, then the next 11 bits are a number that represents the 
number of sub-packets immediately contained by this packet.-}

lengthInfo :: Parser LengthInfo
lengthInfo = single '0' *> (Bits    . bitsToInt <$> bits 15) <|>
             single '1' *> (Packets . bitsToInt <$> bits 11)

{- Finally, after the length type ID bit and the 15-bit or 11-bit field, the sub-packets 
appear.-}

subpackets :: LengthInfo -> Parser [Packet]
subpackets (Bits    n) =
  packetsUntilAt . (+ n) . stateOffset =<< getParserState
subpackets (Packets n) = count n packet

packetsUntilAt :: Int -> Parser [Packet]
packetsUntilAt i = do
  j <- stateOffset <$> getParserState
  when (i < j) $ fail "Went past bit count"
  if i == j
    then return []
    else (:) <$> packet <*> packetsUntilAt i


partialPacket :: Type -> Parser (Value -> Packet)
partialPacket typ
  | typ == 4  = flip Literal <$> literalValue
  | typ == 0  = flip Sum     <$> (lengthInfo >>= subpackets)
  | typ == 1  = flip Product <$> (lengthInfo >>= subpackets)
  | typ == 2  = flip Minimum <$> (lengthInfo >>= subpackets)
  | typ == 3  = flip Maximum <$> (lengthInfo >>= subpackets)
  | typ == 5  = f    Greater <$  lengthInfo <*> packet <*> packet
  | typ == 6  = f    Less    <$  lengthInfo <*> packet <*> packet
  | typ == 7  = f    Equal   <$  lengthInfo <*> packet <*> packet
  | otherwise = error "Failed to parse Packet"
  where f pack a b v = pack v a b

{-
----- Solver ----- 
-}

solve1 :: Packet -> Int
solve1 = versionSum
  where
    versionSum (Literal v _)     = v
    versionSum (Sum     v ps)    = v + sum (map versionSum ps)
    versionSum (Product v ps)    = v + sum (map versionSum ps)
    versionSum (Minimum v ps)    = v + sum (map versionSum ps)
    versionSum (Maximum v ps)    = v + sum (map versionSum ps)
    versionSum (Greater v p1 p2) = v + versionSum p1 + versionSum p2
    versionSum (Less    v p1 p2) = v + versionSum p1 + versionSum p2
    versionSum (Equal   v p1 p2) = v + versionSum p1 + versionSum p2

solve2 :: Packet -> Int
solve2 = eval

-- Logic

eval :: Packet -> Value
eval (Literal _ x )                         = x
eval (Sum     _ ps)                         = sum     $ map eval ps
eval (Product _ ps)                         = product $ map eval ps
eval (Minimum _ ps)                         = minimum $ map eval ps
eval (Maximum _ ps)                         = maximum $ map eval ps
eval (Greater _ p1 p2) | eval p1 >  eval p2 = 1
                       | otherwise          = 0
eval (Less    _ p1 p2) | eval p1 <  eval p2 = 1
                       | otherwise          = 0
eval (Equal   _ p1 p2) | eval p1 == eval p2 = 1
                       | otherwise          = 0