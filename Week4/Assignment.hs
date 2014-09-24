--1.
--Sinan:
--    -
--    -
--    -
--Amir:
--    -
--    -
--    -
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
--Sven:
--Wai Yi:

--3.

module Assignment

where

import Test.QuickCheck
import System.Random
import Data.List
import SetOrd

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


                 Jongens, bekijk even de code wat ik hierhoven heb. Volgens mij zit ik er heel dichtbij, maar is dit nog niet helemaal goed.