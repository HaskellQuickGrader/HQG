module AHG where

import Control.Monad

import Test.QuickCheck
import Hwk1Tests
    


-- The double in the return type is the total score, and the list of
-- results are the results after running each test in the input list.
gradeHomework :: IO (Double, [Result])
gradeHomework = runTests tests 0.0 []

--foldl (\(pointsTotal, results) test@(points, prop) -> runQuickCheck prop results points pointsTotal)  (0.0,[]) tests
   
runTests :: [Test] -> Double -> [Result] -> IO (Double, [Result])
runTests [] totalPts results = return (totalPts,results) -- runQuickCheck prop points
runTests (test:tests) totalPts results= do
    (pts, result) <- runQuickCheck test
    runTests tests (pts + totalPts) (result:results)
            
   
   
runQuickCheck :: Test -> IO (Double, Result)
runQuickCheck test@(points, prop) = do
    result <- quickCheckResult prop
    case result of
        success@(Success numTests labels output) -> return (points, success)
        gaveUp@(GaveUp numTests labels output) -> return (0.0, gaveUp)
        failure@(Failure numTests _ _ _ _ usedSize reason _ labels output failingTestCase) -> return (0.0, failure)
        _ -> undefined
                    -- if(isSuccess result)
                        -- then ((pointsTotal + testPts),result:results)
                        -- else (pointsTotal,result:results)

isSuccess :: IO Result -> Bool
isSuccess result = undefined
-- do
                    -- b <- Success result
                    ---b
                    
checkForSuccess :: Result -> Bool
checkForSuccess result = undefined