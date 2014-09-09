module Sol2

where 

import GS
import TAMO


--2.2
-- P | Q | P XOR Q 
-- f | f | f
-- f | t | t
-- t | f | t
-- t | t | f
-- 


--2.4
-- P | Q | <=> | -
-- f | f | t   | f
-- f | t | f   | t
-- t | f | f   | t
-- t | t | t   | f

--2.9
-- P | Q | P XOR Q | (P XOR Q) XOR Q
-- f | f |    f    | t 
-- f | t |    t    | f 
-- t | f |    t    | t 
-- t | t |    f    | f 

--2.13
-- test1a = logEquiv1 (\ p -> not True) (\ p -> False)
-- test1b = logEquiv1 (\ p -> not False) (\ p -> True)
-- test2  = logEquiv1 (\ p -> p ==> False) (\ p -> not p)
-- test3a = logEquiv1 (\ p -> p || True) (const True)
-- test3b = logEquiv1 (\ p -> p && False) (const False)
-- test4a = logEquiv1 (\ p -> p || False) (\ p -> p)
-- test4b = logEquiv1 (\ p -> p && True) (\ p -> p)
-- test5  = logEquiv1 (\ p -> p || not p) (const True)
-- test6  = logEquiv1 (\ p -> p && not p) (const False)


-- 2.15
contrad1 :: (Bool -> Bool) -> Bool 
contrad1 bf =  not (bf True) && not (bf False)

contrad2 :: (Bool -> Bool -> Bool)  -> Bool
contrad2 bf = and [not (bf p q) | p <- [True,False], q <- [True,False]]

contrad3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
contrad3 bf = and [ not (bf p q r) | p <- [True,False], 
                                     q <- [True,False], 
                                     r <- [True,False]]
									 
									 
-- 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p = parity . map p
