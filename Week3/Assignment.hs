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
callcnf a =  cnf (nnf (arrowfree (a)))

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