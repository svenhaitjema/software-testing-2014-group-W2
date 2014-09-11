module Lab2 where

import Data.List
import System.Random

data Shape = NoTriangle | Equilateral
            | Isosceles | Rectangular | Scalene deriving (Eq,Show)


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