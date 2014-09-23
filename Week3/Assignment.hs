module Assignment

where

import Week3

--ASSIGNMENT 1

contradiction :: Form -> Bool
contradiction f | not(satisfiable f) = True
                | otherwise = False

tautology :: Form -> Bool
tautology f | all (\ v -> eval v f) (allVals f) = True
            | otherwise = False

entails :: Form -> Form -> Bool
entails a b = tautology (Impl a b)

equiv ::  Form -> Form -> Bool
equiv a b | entails a b && entails b a = True
          | otherwise = False

--Description of checking the definitions
--Before the implementation we checked per definition what the meaning was.
--We did this by reading the logic in action book and the wiki of the definitions.
--After the implementation of the definitions we manually checked them in the terminal by making new formulas as shown below.

cont1 = Cnj[Neg p, p]       -- contradiction cont1 returns True
taut1 = Dsj[Neg p, p]       -- tautology taut 1 returns True

ent1 = Cnj[Neg p, q]        -- entails ent1 ent2 returns True
ent2 = Neg(Cnj[p, Neg q])   -- entails ent2 ent1 returns False

equi1 = Dsj[Neg p, q]       -- equiv equi1 equi2 returns True
equi2 = Neg(Cnj[p, Neg q])  -- equiv equi2 equi1 returns True

--Avg time spend 3 hours.

--ASSIGNMENT 2

callcnf :: Form -> Form
callcnf a = cnf (nnf (arrowfree (a)))

cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg y) = Neg (cnf y)
cnf (Cnj (x:xs)) = Cnj((cnf x):(map cnf xs))
cnf (Dsj (y:ys)) | length ys > 0 = (dist (cnf y) (cnf (Dsj ys)))
                 | otherwise = cnf y

dist :: Form -> Form -> Form
dist (Cnj (x:xs)) y = Cnj((dist x y):(map (dist y) xs))
dist x (Cnj (y:ys)) = Cnj((dist y x):(map (dist x) ys))
dist x y = Dsj[x,y]

--Avg time spend 4 hours.

-- Assignment 3

testCNF :: Int -> IO ()
testCNF n = testForms n (\f -> equiv f  (callcnf f))



-- Assignment 4
-- avg time spent: 6 hrs.

type Clause = [Int]
type Clauses = [Clause]

inv :: Form -> Int
inv (Prop x) = x
int (Neg x) = inv x

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [ [x] ] 
cnf2cls (Neg x) = [ [(inv(x) `div` (-1))] ]

cnf2cls (Cnj (x:xs)) | length(xs) > 0 = cnf2cls(x)  ++ cnf2cls(Cnj(xs))
	     | otherwise = cnf2cls(x)

cnf2cls (Dsj (x:xs)) | length(xs) > 0 = [concat(cnf2cls(x) ++ cnf2cls(Dsj(xs)))]
	     | otherwise = cnf2cls(x)


-- Assignment 4
-- avg time spent: 6 hrs.

-- First thing to tackle is: taking in a Property x, single element
-- should return a base Clauses with just 1 Clause in the list

-- Second: convert the Form into Int 
-- The Prop on valuation, gives back a Int
-- The Neg does the same, this is achieved by calling the Prop of it.

-- Lastly, the multi element Forms like Cnj and Dsj
-- This sounds like a recursive function which traverses till the last node of
-- the tree, we map the elements from the Cnj and Dsj functions and 
-- examine each of the elements on its own.
-- 

-- Testing the `cnf2cls` function
-- During development, the following base test were defined:

-- cont1 = Cnj[Neg p, p]
-- ent1 = Cnj[Neg p, q]
cnj_pq = Cnj[p, q, p]
djj = Dsj[q, Neg r,q]

t1 = cnf (Neg (cnf p))
t2 = cnf q
t3 = cnf cont1
t4 = cnf cnj_pq
t5 = djj

-- Testing procedure:
-- cnf2cls was called using the following, if it didn't complain about
-- not able to run, the test was successfull.
-- Return values were checked manually

cnf2cls t1
cnf2cls t2
cnf2cls t3
cnf2cls t4
cnf2cls t5
cnf2cls( callcnf(Cnj[Dsj[p,q],q,r]) )











