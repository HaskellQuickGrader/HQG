{-# LANGUAGE DeriveGeneric #-}

{- Homework 1 Tests -}
module Hwk1Tests where

import Test.QuickCheck
import Grader

import Prelude hiding (all, foldr)
import Hwk1Sol

test1 :: QTest 
test1 = (5 , property appendProp)
 where
   appendProp :: [Int] -> [Int] -> [Int] -> Bool
   appendProp l1 l2 l3 = (l1 `append` l2) `append` l3 == l1 `append` (l2 `append` l3)

test2 :: QTest 
test2 = (3 , property allProp)
 where
   allProp :: [Bool] -> [Bool] -> Bool                      
   allProp l1 l2 = all (l1 ++ l2) == ((all l1) && all l2)

test3 :: QTest 
test3 = (5 , property foldrProp)
 where
   foldrProp :: [[Int]] -> [[Int]] -> Bool
   foldrProp l1 l2 = foldr (++) [] (l1 ++ l2) == (foldr (++) [] l1) ++ (foldr (++) [] l2)

tests :: [QTest]
tests = [test1, test2, test3]
