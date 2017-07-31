module Hwk1 where

import Prelude hiding ((++), all, foldr)    
    
-- Q1: Define list append using the accumilator pattern.
append :: [a] -> [a] -> [a]
append l1 [] = l1
append [] l2 = l2
append (x:xs) l2 = xs

-- Q2: Define the all function which returns true when applied to a
-- list of all true, otherwise returns false.
all :: [Bool] -> Bool
all l = True

-- Q3: Define the foldr function.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f s l = s
