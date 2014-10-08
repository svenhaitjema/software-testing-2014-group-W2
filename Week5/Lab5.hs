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

-- such a test could be to take a problem and put for test of minimalizm all the parent nodes with one filled position less,
-- and then check their children for uniqueness.

isMinimal :: (Sudoku, [Constraint]) -> Bool
isMinimal node = let
   f = (filledPositions (fst node)) in
   and(map (not.uniqueSol) (callErase node f))

callErase :: Node -> [(Row, Column)] -> [Node]
callErase node x = eraseOne node x (length(x)-1)

eraseOne :: Node -> [(Row, Column)] -> Int -> [Node]
eraseOne node [] n = error "please give a non-empty sudoku problem"
eraseOne node (x:xs) n | n == 0 = [eraseN node x]
   | otherwise = [eraseN node ((x:xs)!!n)] ++ (eraseOne node (x:xs) (n-1)) --  [eraseN n (r,c)]


generatePositiveMinimalTest :: IO Bool
generatePositiveMinimalTest = do 
  sud <- genRandomSudoku
  node <- genProblem sud
  return (isMinimal node)


-- Question 3
genProblem3 :: Node -> IO Node
genProblem3 n = do ys <- randomize xs
                   print(xs)
                   print(ys)
                   return (minimalize n xs)
   where xs = filledPositions (fst n)



generate3Free :: IO()
generate3Free = do
  name <- genRandomSudoku
  let 
    a = (([(i,j)|i<-[1..3],j<-[1..3]])) 
    b = (([(i,j)|i<-[4..6],j<-[4..6]])) 
    c = (([(i,j)|i<-[7..9],j<-[7..9]])) 
    m1 = ((minimalize name) (a++b++c)) in
      if (and(map(\x -> elem x (openPositions(fst m1))) a) 
        && and(map(\x -> elem x (openPositions(fst m1))) b) 
        && and(map(\x -> elem x (openPositions(fst m1))) c))  
      then showNode m1
      else generate3Free
      
      
generate4Free :: IO()
generate4Free = do
  name <- genRandomSudoku
  let 
    a = (([(i,j)|i<-[4..6],j<-[1..3]]))
    b = (([(i,j)|i<-[1..3],j<-[4..6]]))
    c = (([(i,j)|i<-[7..9],j<-[4..6]]))
    d = (([(i,j)|i<-[4..6],j<-[7..9]]))
    m1 = ((minimalize name) (a++b++c++d)) in
      if and(map(\x -> elem x (openPositions(fst m1))) a)
        then 
          if and(map(\x -> elem x (openPositions(fst m1))) b) 
            then
              if and(map(\x -> elem x (openPositions(fst m1))) c) 
                then
                  if and(map(\x -> elem x (openPositions(fst m1))) d)
                    then showNode m1
                    else generate4Free
               else generate4Free
            else generate4Free
        else generate4Free
      
-- the example4 grid is an example of a sudoku problem with 4 blocks empty


generate5Free = do
  name <- genRandomSudoku
  let 
    a = (([(i,j)|i<-[4..6],j<-[1..3]]))
    b = (([(i,j)|i<-[1..3],j<-[4..6]]))
    c = (([(i,j)|i<-[7..9],j<-[4..6]]))
    d = (([(i,j)|i<-[4..6],j<-[7..9]]))
    e = (([(i,j)|i<-[1..3],j<-[7..9]]))
    m1 = ((minimalize name) (a++b++c++d++e)) in
      if and(map(\x -> elem x (openPositions(fst m1))) a)
        then 
          if and(map(\x -> elem x (openPositions(fst m1))) b) 
            then
              if and(map(\x -> elem x (openPositions(fst m1))) c) 
                then
                  if and(map(\x -> elem x (openPositions(fst m1))) d)
                    then
                      if and(map(\x -> elem x (openPositions(fst m1))) e)
                        then showNode m1
                        else generate5Free
                    else generate5Free
                 else generate5Free
              else generate5Free
          else generate5Free

-- the example5 grid is an example of a sudoku problem with 5 (actually 6) blocks empty. It takes very long time to either 
-- create or solve such.



-- Question 4

{-| Override in week5.hs
prBlockPositions :: [[Int]]
prBlockPositions = [[2..4],[6..8]]

prBlock :: Int -> [Int]
prBlock x = concat $ filter (elem x) prBlockPositions     

prSubBlock:: Sudoku -> (Row,Column) -> [Value]
prSubBlock s (r,c) = 
  [ s (r',c') | r' <- prBlock r, c' <- prBlock c ]

freeInPrSubBlock :: Sudoku -> (Row,Column) -> [Value]
freeInPrSubBlock s (r,c) = freeInSeq (prSubBlock s (r,c))

prSubBlockInjective :: Sudoku -> (Row,Column) -> Bool
prSubBlockInjective s (r,c) = injective vs where 
   vs = filter (/= 0) (prSubBlock s (r,c))
   
isSamePrBlock :: (Row,Column) -> (Row,Column) -> Bool
isSamePrBlock (r,c) (x,y) = prBlock r == prBlock x && prBlock c == prBlock y 

consistent :: Sudoku -> Bool
consistent s = and $
               [ rowInjective s r |  r <- positions ]
                ++
               [ colInjective s c |  c <- positions ]
                ++
               [ subgridInjective s (r,c) | 
                    r <- [1,4,7], c <- [1,4,7]]
                ++
               [ prSubBlockInjective s (r,c) | 
                    r <- [2,6], c <- [2,6]]
                    
prune :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune _ [] = []
prune (r,c,v) ((x,y,zs):rest)
  | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
  | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
  | sameblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune (r,c,v) rest
  | isSamePrBlock (r,c) (x,y) =
        (x,y,zs\\[v]) : prune (r,c,v) rest    
  | otherwise = (x,y,zs) : prune (r,c,v) rest

-}

-- Question 5
genPrSudoku ::  IO()
genPrSudoku = do
	[sudoku] <- rsolveNs [emptyN]
	showNode sudoku
	s  <- genProblem sudoku
	showNode s

{-| Result
+-------+-------+-------+
| 2 1 5 | 3 8 9 | 4 6 7 |
| 4 3 7 | 5 2 6 | 1 9 8 |
| 6 8 9 | 1 4 7 | 5 3 2 |
+-------+-------+-------+
| 9 7 2 | 4 3 1 | 6 8 5 |
| 8 5 3 | 6 7 2 | 9 4 1 |
| 1 6 4 | 9 5 8 | 7 2 3 |
+-------+-------+-------+
+-------+-------+-------+
|   2 1 |       | 3   6 |
| 3     |       | 2     |
|     6 |       |   7 9 |
+-------+-------+-------+
|       |     9 |       |
| 4     |       |     8 |
| 6   9 | 1     |       |
+-------+-------+-------+
|       |   3   | 6     |
|     3 | 6     |       |
|       |       |       |
+-------+-------+-------+
-}



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








-- question 6.

-- there are various known techniques that humans use to solve sudokus that can be used to classify the 
-- difficul of a specific sudoku problem. Besides luck, one of the main influners on the hardness of a problem is 
-- the height of branching factors that underly a specific problem. 
-- having branching factor 1 means that the problem can be solved straight forward, without having to evaluate the evolution 
-- of the solution based on assumptions.
-- A sudoku problem having branching factor 2 or higher requires making such assumption and mental manipulation of the 
-- number which requires remembering more numbers at each step and is considered hard.
-- The computation complexity increases as well as the branching factor increases.
-- rough classification of the hardness of a sudoku problem is thus based on the hardness of the techniques used to "traverse" 
-- problem trees of higher branching factors, which we here roughly categorize in branching factor up to 1, 2 and 3+

checkDifficulty :: Node -> [Char]
checkDifficulty n | filter (>1) (branchingFactors(grow succNode n)) == [] = "easy"
  | filter (>2) (branchingFactors(grow succNode n)) == [] = "medium"
  | otherwise = "hard"

branchingFactors :: Tree Node -> [Int]
branchingFactors (T t ts) = [length(succNode(t))]  ++ concat(map branchingFactors ts) 
