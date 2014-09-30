module Assignment

where
import Test.QuickCheck
import System.Random
import Data.List
import SetOrd

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

--Deze versie werkt dus niet.
--getRandomSet :: Int -> Set -> IO(Set Int)
--getRandomSet i s | isEmpty(s) = do
--                                    p <- getRandomInt(i)
--                                    return getRandomSet(p (insertSet p emptySet))
--                 | length(s) < 10 =  do
--                                        h <- getRandomInt(i)
--                                        return getRandomSet(h (insertSet h s))
--                 | otherwise = s

-- version with random range with both barriers default smaller than 300
randomSet :: IO (Set Int)
randomSet = do
    n <- getRandomInt 15;   -- random set length
    g <- getRandomInt 100;  -- random randomgenerator
    a <- getRandomInt 300;  -- first boundary
    b <- getRandomInt 300;  -- second boundry
    return (list2set(take n (randomRs (a,b) (mkStdGen g))))

-- version with random range given as given in input
randomSet2 :: Int -> Int -> IO (Set Int)
randomSet2 a b = do
    n <- getRandomInt 15;   -- random set length
    g <- getRandomInt 100;  -- random randomgenerator
--  a <- getRandomInt 300;
--  b <- getRandomInt 300;
    return (list2set(take n (randomRs (a,b) (mkStdGen g))))

--Opdracht 4.
-- INTERSECTION

setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set []) (Set s) = Set []
setIntersection (Set (x:xs)) (Set s) | inSet x (Set (sort s)) = setUnion (Set [x]) (setIntersection (Set xs) (Set s))
    | otherwise = setIntersection (Set xs) (Set s)

--UNION
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set c ) (Set s) = list2set(nub(c ++ s))

--DIFFERENCE
setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set []) (Set s) = Set []
setDifference (Set (x:xs)) (Set s) | inSet x (Set (sort s)) = setDifference (Set xs) (Set s)
    | otherwise = setUnion (Set [x]) (setDifference (Set xs) (Set s))

ioSetIntersection :: Set Int -> Set Int -> IO (Set Int)
ioSetIntersection n p = do
        return (setIntersection (n) (p))       
       
lenSet :: (Ord a) => Set a-> Int
lenSet (Set []) = 0
lenSet (Set (x:xs))= 1 + lenSet (Set xs)       
       
doTest :: Int -> IO (Bool)
doTest n = do
        fs <- (randomSet2 0 n)
        putStr "First generated set: "
        print(fs)
        ss <- (randomSet2 0 n)
        putStr "Second generated set: "
        print(ss)
        iss <- (ioSetIntersection fs ss)
        putStr "Intersection"
        print(iss)
        return ((subSet iss fs) && (subSet iss fs) && ((lenSet iss) <= (lenSet fs)) && ((lenSet iss) <= (lenSet ss)) )
       
doLotsOfTests :: Int -> Int -> IO ()
doLotsOfTests n p = do
    if (p==0) then print("All tests passed")
        else do
        t <- doTest n  
        if (t) then do
                print("Test Passed")
                (doLotsOfTests n (p-1))
        else print("Test failed")



--OPDRACHT 5.
type Rel a = [(a,a)]
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => [(a,a)] -> [(a,a)]
trClos x | x == nub(x ++ (x @@ x)) = sort(x)
	| otherwise = trClos (nub (x ++ (x @@ x)))

--Opdracht 6
-- See Tests.hs and run with:
-- $ runhaskell Tests.hs
main = hspec $ do
  describe "trClos" $ do
    it "gives the transitive closure of binary relations" $ do 
      property $ \x -> 
        trClos x == giveTrC (x)
    it "includes all elements from input in the output" $ do
      property $ \x -> 
      	all (==True) (verif (nub(x)))
    it "returns output with more or same amount of unique elements than the input" $ do
      property $ \x ->
        length(nub(x)) <= length(giveTrC (x))


verif :: Rel Int -> [Bool] -- Rel Int
verif [] = []
verif (x:xs) = [elem x (trClos(x:xs))] ++ (verif xs)

--Opdracht 7

-- The random verification method used in the HSpec

=============================================================================================================================
please set extentions -XFlexibleInstances, -XFlexibleContexts and -XOverlappingInstances on 
in order to use instance Arbitrary (Rel Int)
=============================================================================================================================

instance Arbitrary (Rel Int) where
	arbitrary = do
		x <- choose (0,5)
		y <- choose (0,5)
		return [(x,y)|i<-[0..x]] 


quickVerif :: Rel Int -> Property -- consumable by quickCheck
quickVerif x =
	property $ -- not(elem False 
		all (==True) (verif (nub(x)))-- ) -- == False

lenSet :: (Ord a) => Set a-> Int
lenSet (Set []) = 0
lenSet (Set (x:xs))= 1 + lenSet (Set xs)

-- quickCheck quickVerif

-- +++ OK, passed 100 tests
