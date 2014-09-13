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
                    
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement (x:xs) (y:ys) | x == y = False
	| otherwise = isDerangement xs ys
                    
