{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Lab4 where 

import Test.Hspec
import Test.QuickCheck
import System.Random
import Data.List
import System.Random
import SetOrd
import Assignment

fp :: Eq a => (a -> a) -> a -> a
fp f = \ x -> if x == f x then x else fp f (f x)


-- getRandomInt :: Int -> IO Int
-- getRandomInt n = getStdRandom (randomR (0,n))


-- randomSet' :: Int -> [Int]
-- randomSet' x = [x]

-- randomSet :: Int -> Set a
-- randomSet n = take n $ getStdRandom (randomR (0,n))


-- A thin monadic skin layer
getList :: IO [Char]
getList = fmap take5 getContents
 
-- The actual worker
take5 :: [Char] -> [Char]
take5 = take 5 . filter (`elem` ['a'..'e'])


instance Arbitrary (Rel Int) where
 arbitrary = do
  x <- choose (0,5)
  y <- choose (0,5)
  return [(x,y)|i<-[0..x]]