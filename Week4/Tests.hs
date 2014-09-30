module Tests where

import Data.List
import SetOrd
import System.Random
import Test.Hspec
import Test.QuickCheck
import Assignment

main = hspec testtrClos

testtrClos = describe "trClos" $ do
    it "Following assignment" $ do
        (trClos [(1,2),(2,3),(3,4)] == [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)])
--    it "Empty set" $ do
--        (trClos [] == [])
    it "Single item in set, should not give transitive set" $ do
        (trClos [(0,1)] == [(0,1)])
    it "Simple" $ do
        (trClos [(0,1),(1,0)] == [(0,0),(0,1),(1,0),(1,1)])
    it "Indirect" $ do
        (trClos [(0,1),(1,2),(2,1)] == [(0,1),(0,2),(1,1),(1,2),(2,1),(2,2)])
    it "includes all elements from input in the output" $ do
        property $ \x -> 
            all (==True) (verif (nub(x)))