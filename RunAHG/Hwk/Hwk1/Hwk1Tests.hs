{-# LANGUAGE DeriveGeneric #-}

{- Homework 1 Tests -}
module Hwk.Hwk1.Hwk1Tests (tests, Test) where

import Test.QuickCheck

import Prelude hiding (all, foldr)
import Hwk.Hwk1.Hwk1

-- Type of tests: Double is the point value, and Property is the actual
-- test case.
type Test = (Double, Property)

test1 :: Test 
test1 = (5 , property appendProp)
 where
   appendProp :: [Int] -> [Int] -> [Int] -> Bool
   appendProp l1 l2 l3 = (l1 `append` l2) `append` l3 == l1 `append` (l2 `append` l3)

test2 :: Test 
test2 = (3 , property allProp)
 where
   allProp :: [Bool] -> [Bool] -> Bool                      
   allProp l1 l2 = all (l1 ++ l2) == ((all l1) && all l2)

test3 :: Test 
test3 = (5 , property foldrProp)
 where
   foldrProp :: [[Int]] -> [[Int]] -> Bool
   foldrProp l1 l2 = foldr (++) [] (l1 ++ l2) == (foldr (++) [] l1) ++ (foldr (++) [] l2)

tests :: [Test]
tests = [test1, test2, test3]
