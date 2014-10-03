module Lab5

where
import Data.List
import Test.Hspec
import Test.QuickCheck
import Week5


--consistent :: Sudoku -> Bool
--consistent s = and $
--               [ rowInjective s r |  r <- positions ]
--                ++
--               [ colInjective s c |  c <- positions ]
--                ++
--               [ subgridInjective s (r,c) |
--                    r <- [1,4,7], c <- [1,4,7]]


lenSudoku :: Grid x-> Int
lenSudoku [] = 0
lenSudoku (x:xs)= length(x) + lenSudoku( xs )



main2 :: IO ()
main2 = hspec $ do
  describe "Specification of the Sudoku Game\n" $ do
    it "Checks if there are 9 lists in a grid" $ do
        length(example1) == 9
    it "Checks if there are 81 elements in the 9 lists" $ do
        lenSudoku(example1) == 81 -- is nog niet correct
    it "Checks if the row is consistent" $ do
        and [rowInjective (grid2sud example1) c |  c <- positions ] `shouldBe` True
    it "Checks if the column is consistent" $ do
        and [colInjective (grid2sud example1) c |  c <- positions ] `shouldBe` True
    it "Checks if the subgrid is consistent" $ do
        and [subgridInjective (grid2sud example1) (r,c) |  r <- [1,4,7], c <- [1,4,7] ] `shouldBe` True