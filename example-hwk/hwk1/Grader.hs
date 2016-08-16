module Grader where

import Test.QuickCheck
    
-- Type of tests: Double is the point value, and Property is the actual
-- test case.
type QTest = (Double, Property)

-- The double in the return type is the total score, and the list of
-- results are the results after running each test in the input list.
runTests :: [QTest] -> (Double, [Result])
runTests = undefined
