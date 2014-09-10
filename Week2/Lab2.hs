module Lab2 where

import Data.List
import System.Random

data Shape = NoTriangle | Equilateral
            | Isosceles | Rectangular | Scalene deriving (Eq,Show)


triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c  | a <= 0 || b<= 0 || c<=0 = NoTriangle
                | a + b < c = NoTriangle
                | a + b == c = Scalene
                | a==b&&b==c = Equilateral
                | (a^2) + (b^2) == c^2 = Rectangular
                | a == b || b == c || c == a = Isosceles
                | otherwise = Scalene


trianglep :: Integer -> Integer -> Integer -> Shape
--sortlegs x y z  | sort [x,y,z] = (x,y,z)
trianglep x y z = triangle (s!!0) (s!!1) (s!!2) where s = sort [x,y,z]



--sortlegs1 :: Integer -> Integer -> Integer ->[Integer]
--sortlegs x y z  | sort [x,y,z] = (x,y,z)
--sortlegs1 x y z = [(s!!0), (s!!1), (s!!2)] where s = sort [x,y,z]


--triangle sort[x,y,z]
--s!!0 s!!1 s!!2
--trianglep :: Integer -> Integer -> Integer -> Shape
--trianglep a b c  | s!!0 + s!!1 <= s!!2 where s = sort[a,b,c] =