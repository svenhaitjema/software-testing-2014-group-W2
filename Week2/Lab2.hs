module Week2 where

import Data.List
import Data.Eq
import System.Random

data Shape = NoTriangle | Equilateral
            | Isosceles | Rectangular | Scalene deriving (Eq,Show)

-- 1.
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  | a <= 0 = NoTriangle
                | a + b < c = NoTriangle
                | a + b == c = Scalene
                | a==c = Equilateral
                | (a^2) + (b^2) == c^2 = Rectangular
                | a == b || b == c = Isosceles
                | otherwise = Scalene


sortTriangle :: Integer -> Integer -> Integer -> Shape

sortTriangle x y z = triangle (s!!0) (s!!1) (s!!2) where s = sort [x,y,z]

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation x y | length(x) /= length(y) = False
                  | otherwise = permuCheck x y

permuCheck :: Eq a => [a] -> [a] -> Bool
permuCheck [] [] = True
permuCheck (x:xs) (y:ys) | (x:xs) == (y:ys) = True
                         | elem x (y:ys) == False = False
                         | elem x (y:ys) == True = permuCheck xs (removeItem x (y:ys))
                         | otherwise = False

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem x (y:ys) | x == y = removeItem x ys
                    | otherwise = y : removeItem x ys
                    
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs  = [ y:ys | y <- xs, ys <- perms (xs\\[y]) ]

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys) | x == y = False
	| otherwise = isDerangement xs ys

testDeran1 :: isDerangement [] [] == True
testDeran2 = isDerangement [1,2] [2] == False
testDeran3 = isDerangement [2] [1,2] == False
testDeran4 = isDerangement [2] [1,1,1] == False
testDeran5 = isDerangement [1,1,2] [2,1,1] == False
testDeran5 = isDerangement [1,2,3] [2,1,3] == True
testDeran6 = isDerangement [1,2,3] [3,2,1] == isPermutation [1,2,3] [3,2,1]


--deran (Enum a, Eq a, Num a) => a -> [[a]]
deran n = [j|j<-xs,and(zipWith(/=) x j)] where (x:xs) = perms[0..n-1]

-- another alternative:
deran' :: Eq a => [a] -> [a] -> [[a]]
deran' [] [] = [[]]
deran' (x:xs) ys = [d:ds | d <- ys, d /= x, ds <- deran' xs (ys\\[d])]

                   
