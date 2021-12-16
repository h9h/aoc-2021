import Control.Exception (evaluate)
import Control.Applicative hiding (some)
import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Lib (  readInput, 
              version,
              typeID,
              lengthInfo,
              literalValue,
              subpackets,
              Parser,
              LengthInfo(..),
              Packet(..)
            )


main :: IO ()
main = do
  hspec $ do
    describe "Literal Type" $ do
      let in1 = readInput "D2FE28"
      it "should parse version" $ do
        parse version "" in1 `shouldParse` 6
      it "should parse typeID" $ do
        parse (version *> typeID) "" in1 `shouldParse` 4
      it "should parse Literal" $ do
        parse (version *> typeID *> literalValue) "" in1 `shouldParse` 2021
    describe "Operators" $ do
      let in2 = readInput "38006F45291200"
      let in3 = readInput "EE00D40C823060"
      it "should parse version" $ do
        parse version "" in2 `shouldParse` 1
      it "should parse typeID" $ do
        parse (version *> typeID) "" in2 `shouldParse` 6
      it "should parse lengthInfo type 0" $ do
        parse (version *> typeID *> lengthInfo) "" in2 `shouldParse` Bits 27
      it "should parse lengthInfo type 1" $ do
        parse (version *> typeID *> lengthInfo) "" in3 `shouldParse` Packets 3
      it "should parse v,t,l type 1" $ do
        parse ((,,) <$> version <*> typeID <*> lengthInfo) "" in3 `shouldParse` (7, 3, Packets 3)
      it "should parse subpackets type 1" $ do
        parse (version *> typeID *> lengthInfo *> (subpackets (Packets 3))) "" in3 `shouldParse` [Literal 2 1, Literal 4 2, Literal 1 3]
