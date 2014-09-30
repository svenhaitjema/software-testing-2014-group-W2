module Tests where

import Test.Hspec
import Assignment

main = hspec testtrClos

testtrClos = describe "trClos" $ do
    it "Following assignment" $ do
        (trClos [(1,2),(2,3),(3,4)] == [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)])
    it "Empty set closure" $ do
        (trClos [] == [])
    it "Simple" $ do
        (trClos [(0,1),(1,0)] == [(0,0),(0,1),(1,0),(1,1)])
