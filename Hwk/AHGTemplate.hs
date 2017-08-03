import Control.Monad
import Test.QuickCheck

import Hwk{{HwkNum}}.Hwk{{HwkNum}}Tests
import GradeReport


-- Overview of steps:
-- Make sure Hwk1 folder is empty of previous grade report and student's solution
-- Copy student's Hwk1.hs file to Hwk1 folder
-- Make grade report and copy it back to student's report
-- Delete student's solution and grade report in Hwk1 folder to ready it for next student

checkFolder :: IO ()
checkFolder = undefined


makeGradeReport :: IO ()
makeGradeReport = do
    (grade, results) <- gradeHomework
    let folder = "Hwk{{HwkNum}}"
    makeReport (show grade) results folder
    

-- The double in the return type is the total score, and the list of
-- results are the results after running each test in the input list.
gradeHomework :: IO (Double, [Result])
gradeHomework = runTests tests 0.0 []

  
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
        
main = do
    makeGradeReport