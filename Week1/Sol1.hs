module Sol1

where 

import GS
-- exercise 1.4:
-- no, because LD equaling to N is by definition not prime?

prime0 :: Integer -> Bool
prime0 n | n < 1 = error "not a positive integer"
		| n == 1 = False
		| otherwise = ld n == n

-- exercise 1.5
-- see prime.hs, copied from book

-- exercise 1.6
-- The type of `rem` should be infered as a Integer because of the equality sign `==`

-- exercise 1.7
-- *Sol1> :t divides 5
-- divides 5 :: Integer -> Bool
-- -- Tells me that it is expecting another Integer to getting a Bool result, so we do that in the next step
-- *Sol1> :t divides 5 7
-- divides 5 7 :: Bool
-- -- In this example, 5 and 7 are supplied to the divides function and it can return a Bool
-- -- Since we are executing this in the :t mode, we don't see the returned value.
-- *Sol1>

-- exercise 1.8


-- exercise 1.9
-- this function is based on the solution given in mnmInt
listMax :: [Integer] -> Integer
listMax [] = error "empty list"
listMax [x] = x
listMax (x:xs) = max x (listMax xs)



-- exercise 1.10


-- infered from the gchi:
-- *Sol1> :t removeFst 3 [1,4,6,7,8]
-- removeFst 3 [1,4,6,7,8] :: (Num t, Eq t) => [t]

removeFst :: Eq t => t ->  [t] -> [t]
removeFst m [] = []
removeFst m (x:xs) | m == x 	= xs
					| otherwise = x: (removeFst m xs)

-- exercise 1.11 -- from book

srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

srtInts' :: [Int] -> [Int]
srtInts' [] = []
srtInts' xs = let
				m = mnmInt xs
				in m : (srtInts' (removeFst m xs))
				
-- exercise 1.13
-- usage: count 'a' "Haaaaaskell"
count :: Char -> String -> Int
count n [] = 0
count n (x:xs) | n == x		= 1 + (count n xs)
				| otherwise = (count n xs)

-- exercise 1.14

rep :: Char -> Int -> String
rep c 0 = []
rep c n = c:(rep c (n-1))

blowup_n :: String -> Int -> String
blowup_n [] n = []
blowup_n (x:xs) n = (rep x n) ++ (blowup_n xs (n+1))

blowup :: String -> String
blowup [] = []
blowup (x:xs) = [x] ++ (blowup_n xs 2)

-- exercise 1.15
-- sorting of strings
-- Should be the same as srtInts, but this time we deal with Strings / [Char]
-- We could transform each char to Ord? (sort by ascii value?)


-- http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html#v:sort
-- ordVal can be anything, this is taken care by Ord a
ordVal :: Ord a => [a] -> a
ordVal [] = error "cannot sort empty"
ordVal [x] = x
ordVal (x:xs) = min x (ordVal xs)

srtString :: [String] -> [String]
srtString [] = []
srtString xs = lowval : ( srtString (removeFst lowval xs) ) where lowval = ordVal xs


-- exercise 1.17
-- following the rules:
-- xs is shorter or equal length of ys
-- 1: if xs is a prefix of ys, xs is a substring of ys,
-- 2: if ys equals y:ys’ and xs is a substring of ys’, xs is a substring of ys,
-- 3: nothing else is a substring of ys.

-- test with:
-- substring "sch" "schaap"
-- substring "sch" "blaatschaap"
-- substring "sch" "sc"

substring :: String -> String -> Bool
substring [] [] = True
substring [] (y:ys) = False
substring (x:xs) [] = False
substring (x:xs) (y:ys) = (x==y) && (prefix xs ys) || (substring (x:xs) ys)


