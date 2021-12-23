import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Lib ( costMove, reachable, firstDifference, replace, neighbors )

main :: IO ()
main = do
  hspec $ do
    describe "costMove" $ do
      it "Room 2 B out" $ do
        (costMove 2 (["BA", "CD", "BC", "DA"],".......") 2 2) `shouldBe` 40
      it "Room 1 C out" $ do
        (costMove 2 (["BA", "CD", "C", "DA"],"..B....") 1 3) `shouldBe` 200
      it "Room 2 C in" $ do
        (costMove 2 (["BA", "D", "C", "DA"],"..BC...") 1 3) `shouldBe` 200
      it "Room 1 D out" $ do
        (costMove 2 (["BA", "D", "C", "DA"],"..B....") 1 3) `shouldBe` 3000
      it "Room 1 B in" $ do
        (costMove 2 (["BA", "", "C", "DA"],"..BD...") 1 2) `shouldBe` 30
    describe "reachable" $ do
      it "....... 1 1" $ do
        (reachable "......." 1 1) `shouldBe` True
      it "..D.... 1 1" $ do
        (reachable "..D...." 1 1) `shouldBe` False
      it "..D.... 1 3" $ do
        (reachable "..D...." 1 3) `shouldBe` True
      it "..D.... 0 5" $ do
        (reachable "..D...." 0 5) `shouldBe` False
      it "......D 0 6" $ do
        (reachable "......D" 0 6) `shouldBe` False
    describe "firstDifference" $ do
      it "Rooms" $ do
        (firstDifference ["AB", "A", "CD", "DB"] ["AB", "AC", "CD", "DB"]) `shouldBe` 1
      it "Hall" $ do
        (firstDifference "......." "...D...") `shouldBe` 3
    describe "replace" $ do
      it "Rooms" $ do
        (replace ["AB", "A", "CD", "DB"] 1 "AC") `shouldBe` ["AB","AC","CD","DB"]
      it "Hall" $ do
        (replace "...D..." 3 '.') `shouldBe` "......."
    describe "neighbors" $ do
      it "Move out" $ do
        (neighbors (["AD", "CA", "BD", "CB"], "..D....")) `shouldBe` [(["D","CA","BD","CB"],"A.D...."),
                                                                      (["D","CA","BD","CB"],".AD...."),
                                                                      (["AD","A","BD","CB"],"..DC..."),
                                                                      (["AD","A","BD","CB"],"..D.C.."),
                                                                      (["AD","A","BD","CB"],"..D..C."),
                                                                      (["AD","A","BD","CB"],"..D...C"),
                                                                      (["AD","CA","D","CB"],"..DB..."),
                                                                      (["AD","CA","D","CB"],"..D.B.."),
                                                                      (["AD","CA","D","CB"],"..D..B."),
                                                                      (["AD","CA","D","CB"],"..D...B"),
                                                                      (["AD","CA","BD","B"],"..DC..."),
                                                                      (["AD","CA","BD","B"],"..D.C.."),
                                                                      (["AD","CA","BD","B"],"..D..C."),
                                                                      (["AD","CA","BD","B"],"..D...C")]
      it "Move in" $ do
        (neighbors (["AA", "BB", "C", "D"], "C.D....")) `shouldBe` [(["AA","BB","C","DD"],"C......")]
