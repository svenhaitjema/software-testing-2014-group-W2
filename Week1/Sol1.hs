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

removeFst