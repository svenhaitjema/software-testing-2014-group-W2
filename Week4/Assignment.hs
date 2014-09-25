module Assignment

where

import Test.QuickCheck
import System.Random
import Data.List
import SetOrd

--1.
--Sinan:
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

getRandomSet :: Int -> Set -> IO(Set Int)
getRandomSet i s | isEmpty(s) = do
                                    p <- getRandomInt(i)
                                    return getRandomSet(p (insertSet p emptySet))
                 | length(s) < 10 =  do
                                        h <- getRandomInt(i)
                                        return getRandomSet(h (insertSet h s))
                 | otherwise = s


--       Jongens, bekijk even de code wat ik hierhoven heb. Volgens mij zit ik er heel dichtbij, maar is dit nog niet helemaal goed.
--	Amir: Sinan, het lijkt me dat t lastig wordt om een IO (a) te manipuleren op zo'n manier. Er is een speciale library 
-- 	dat veel aanbiedt mbt randoms: kijk hier - http://learnyouahaskell.com/input-and-output#randomness

--Amir:
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
--	a <- getRandomInt 300;
--	b <- getRandomInt 300;
	return (list2set(take n (randomRs (a,b) (mkStdGen g))))
--Sven:
--    -
--    -
--    -
--Wai Yi:
--    -
--    -
--    -

--2.
--Sinan:
--Amir: 
  --set union:
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set c ) (Set s) = list2set(nub(c ++ s))

setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set []) set2 = Set []
setDifference (Set (x:xs)) set2 | inSet x set2 = setDifference (Set xs) set2
    | otherwise = setUnion (Set [x]) (setDifference (Set xs) (set2))

--Sven:
--Wai Yi:

--3.
