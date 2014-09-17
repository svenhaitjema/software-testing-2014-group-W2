module Assignment

where

import Week3

contradiction :: Form -> Bool
contradiction f | not(satisfiable f) = True
                | otherwise = False

tautology :: Form -> Bool
tautology f | all (\ v -> eval v f) (allVals f) = True
            | otherwise = False


--entails :: Form -> Form -> Bool


--equiv ::  Form -> Form -> Bool

