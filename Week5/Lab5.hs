module Lab5

where
import System.Random
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

lenSudoku :: Grid -> Int
lenSudoku [] = 0
lenSudoku (x:xs)= length(x) + lenSudoku( xs )


-- Question 1

main2 :: IO ()
main2 = hspec $ do
  describe "Specification of the Sudoku Game\n" $ do
    it "Checks if there are 9 lists in a grid" $ do
        length(example1) == 9
    it "Checks if there are 81 elements in the 9 lists" $ do
        lenSudoku(example1) == 81
    it "Checks if the row is consistent" $ do
        and [rowInjective (grid2sud example1) c |  c <- positions ] `shouldBe` True
    it "Checks if the column is consistent" $ do
        and [colInjective (grid2sud example1) c |  c <- positions ] `shouldBe` True
    it "Checks if the subgrid is consistent" $ do
        and [subgridInjective (grid2sud example1) (r,c) |  r <- [1,4,7], c <- [1,4,7] ] `shouldBe` True


-- Question 2

mainq2 :: IO()
mainq2 = hspec $ do
    describe "Minimal sudoku" $ do
        it "The given minimum sudoku is really minimum" $do
            [r] <- rsolveNs [emptyN]
            showNode r
            s  <- genProblem r
            showNode s

-- Question 3
genProblem3 :: Node -> IO Node
genProblem3 n = do ys <- randomize xs
                   print(xs)
                   print(ys)
                   return (minimalize n xs)
   where xs = filledPositions (fst n)

-- Question 4

-- definition of blocks:
-- any block n is located in quadrant:
-- row_pos = row_size - (n % row_size)
-- col_pos = col_size - (n % col_size)

--genTuple :: Int -> Int -> ( Row, Column )
--genTuple x y = ( x, y )

--genBlock :: Int -> [(Row, Column)]
--genBlock x = map genTuple x [1..9]

-- we should change randomize to define the points to delete (on whole blocks)

genProblem4 :: Node -> IO Node
genProblem4 n = do ys <- return(getBlocks 3)
                   return (minimalize n ys)
   where xs = filledPositions (fst n)

input4 = rsolveNs [emptyN]
rows = bl 8
cols = bl ( (mod 8 3) * 3 )

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

blockCoords :: Int -> [Int] -> [(Row, Column)]
blockCoords n [] = []
blockCoords n [x] = [( n, x )]
blockCoords n (x:xs) = ( n, x ) : (blockCoords n xs)

-- get the column positions for 1 block
blockCols :: Int -> [Int]
blockCols n | (mod n divider) /= 0 = bl ((mod n divider) * divider)
            | otherwise = bl 9
            where divider = length blocks

-- example:
-- getBlock 8 
-- [(7,4),(7,5),(7,6),(8,4),(8,5),(8,6),(9,4),(9,5),(9,6)]

getBlock :: Int -> [(Row, Column)]
getBlock blockno = do 
   ( blockCoords r cols ) ++ (blockCoords s cols ) ++ (blockCoords t cols )
    where [r,s,t] = bl blockno
          cols = blockCols blockno

getRandomRInt :: Int -> Int -> IO Int
getRandomRInt l h = randomRIO (l,h)

-- getRandomInts :: Int -> IO Int
-- getRandomInts n = getRandomRInt 1 9

getRandomInts :: Int -> IO [Int]
getRandomInts 0 = return([])
getRandomInts n = do x <- getRandomRInt 1 9
                     xs <- getRandomInts (n-1)
                     return([x]++xs)

-- getRandomInts' :: Int -> IO [Int]
-- getRandomInts' 0 = return []
-- getRandomInts' n = getRandomInts' 0 ++ getRandomRInt 1 9 ++ getRandomInts' (n-1)

getBlocks :: Int -> [(Row, Column)]
getBlocks 0 = []
getBlocks n = getBlock 1 ++ getBlock 5 ++ getBlock 9

-- we can use the map getBlock [list...] to obtain a list of positions to clear
q4 :: IO ()
q4 = do [r] <- rsolveNs [emptyN]
        showNode r
        s  <- genProblem4 r
        showNode s

-- Question 6
{-
Sudoku puzzles fall in the catagory of Constraint satisfaction problems (CSP).
Difficulty is correlates with human performance (measured by time).
"The first level of complexity is defined by the complexity of the 
individual steps (logic operation) involved in solving the problem"
"The second criteria determining the complexity is the structure of dependency
among individual steps, whether steps are independent, e.g. parallel branching 
in the search tree or are dependent (sequential)"

(chapter 2.2)
The backtracking search is a basic approach solving CPSs. It tries to find a
solution by assigning values to variables one by one. This is time consuming, it 
continues till a violation is made and backtracks to the last "valid" contraint.
This systemetich approach is time consuming and hard for humans.

The Constraint Propagation(CP) technique determines values for a variable by reasoning
about a candidate set meeting the constraints. This is much simpler than the backtracking
approach and can be solved with 2 steps:
- Naked single technique
- Hidden single technique
Sudoku problems solvable using CP are ranked as "easy" or "mild" are simple Sudokus.

Based on these criteria we could write an algorithm to determine the level of our Sudoku

-}