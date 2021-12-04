import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Lib
import Test.Hspec (describe)

main :: IO ()
main = hspec $ do
  let draws1 = [1,2,3]
  let board1 = [[(1,False),(2,False)],[(2,False),(3,False)],[(3,False),(4,False)],[(4,False),(5,False)],[(5,False),(6,False)]]
  describe "Parse input" $ do
    it "should get draws and boards" $ do
      parse ["1,2,3", "", "1 2", "2 3", "3 4", "4 5", "5 6"] `shouldBe` (draws1, [board1])
  describe "Apply draws" $ do
    it "should apply a single draw" $ do
      applyDraw board1 1 `shouldBe` [[(1,True),(2,False)],[(2,False),(3,False)],[(3,False),(4,False)],[(4,False),(5,False)],[(5,False),(6,False)]]
    it "should apply all draws" $ do
      applyDraws board1 draws1 `shouldBe` [
        board1,
        [[(1,True),(2,False)],[(2,False),(3,False)],[(3,False),(4,False)],[(4,False),(5,False)],[(5,False),(6,False)]],
        [[(1,True),(2,True)],[(2,True),(3,False)],[(3,False),(4,False)],[(4,False),(5,False)],[(5,False),(6,False)]],
        [[(1,True),(2,True)],[(2,True),(3,True)],[(3,True),(4,False)],[(4,False),(5,False)],[(5,False),(6,False)]]
                                          ]
  describe "Bingo" $ do
    it "should get Bingo board" $ do
      getBingoBoard board1 [1,2] `shouldBe` 
        (1, [[(1,True),(2,True)],[(2,True),(3,False)],[(3,False),(4,False)],[(4,False),(5,False)],[(5,False),(6,False)]])
        