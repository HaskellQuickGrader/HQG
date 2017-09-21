{-# OPTIONS_GHC -fno-warn-tabs #-}

import Test.QuickCheck
import System.Directory
import System.Environment
import CGI_Modules.TransferData

import {{Name}}.Hwk{{HwkNum}}Tests  -- 1.Hwk1Tests
import GradingModules.GradeReport


-- Overview of steps:
-- Make sure Hwk1 folder is empty of previous grade report and student's solution
-- Copy student's Hwk1.hs file to Hwk1 folder
-- Make grade report and copy it back to student's report
-- Delete student's solution and grade report in Hwk1 folder to ready it for next student


makeGradeReport :: String -> IO ()
makeGradeReport folder = do
    _ <- begin.show $ "Making grade report"
    (grade, results) <- gradeHomework
    makeReport (show grade) results folder
    
    

-- The double in the return type is the total score, and the list of
-- results are the results after running each test in the input list.
gradeHomework :: IO (Double, [Result])
gradeHomework = runTests tests 0.0 []

  
runTests :: [Test] -> Double -> [Result] -> IO (Double, [Result])
runTests [] totalPts results = return (totalPts,results) -- runQuickCheck prop points
runTests (test:tests) totalPts results = do
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
        
moveReportToRepo :: String -> String -> IO ()
moveReportToRepo repoDir workingDir = do
		 	 _ <- begin.show $ "About to move grade report to repo."
		 	 _ <- begin.show $ "Repo folder: "++repoDir
			 _ <- begin.show $ "Working Dir: "++workingDir
			 copyFile (workingDir++"GradeReport.txt") (repoDir++"GradeReport.txt")
			 return ()

moveSolutionFromRepo :: String -> String -> IO ()
moveSolutionFromRepo solutionRepoPath solutionWorkingPath = do
		         _ <- begin.show $ "About to move solution from repo."
			 _ <- begin.show $ "Solution Repo Path: "++solutionRepoPath
			 _ <- begin.show $ "Moving solution to path: "++solutionWorkingPath
			 copyFile solutionRepoPath solutionWorkingPath
			 return ()
        
main = do
    (x:y:xs) <- getArgs
    let repoFolder = x
    let workingDir = y
    let hwkName = "Hwk{{HwkNum}}"  -- Report folder and student's solution file have same name
    currentDir <- getCurrentDirectory
    _ <- begin.show $ "From AHG_Hwk1 - Current directory: "++currentDir
    _ <- begin.show $ "Repo Dir: "++repoFolder
    _ <- begin.show $ "Report Folder: "++workingDir
    moveSolutionFromRepo (repoFolder++hwkName++".hs") (workingDir++hwkName++".hs")
    _ <- begin.show $ "Solution moved from repo, making grade report"
    makeGradeReport $ workingDir
    _ <- begin.show $ "Grade report made, moving back to repo"
    moveReportToRepo repoFolder workingDir