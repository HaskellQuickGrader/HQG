module Hwk1Sol where

import Prelude hiding ((++), all, foldr)

-- Q1: Define list append function.
append :: [a] -> [a] -> [a]
append l1 [] = l1
append [] l2 = l2
append (x:xs) l2 = x : append xs l2

-- Q2: Define the all function which returns true when applied to a
-- list of all true, otherwise returns false.
all :: [Bool] -> Bool
all [] = True
all (True:xs) = all xs
all _ = False

-- Q3: Define the foldr function.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f s [] = s
foldr f s (x:xs) = f x (foldr f s xs)
