module GradeReport where

import System.IO
import Test.QuickCheck
import GHwkTransferData

import Data.Char(toUpper)


makeReport :: String -> [Result] -> String -> IO ()
makeReport grade results folder = do
    _ <- begin.show $ "Path for grade report. / add to end: "++folder
    reportHandle <- openFile (folder++"/GradeReport.txt") WriteMode
    hPutStrLn reportHandle $ "Your total score is: "++grade++"."
    writeToReport results 0 reportHandle
    hClose reportHandle

writeToReport :: [Result] -> Int -> Handle -> IO ()
writeToReport [] _ _ = return ()
writeToReport (result:results) testNum reportHandle = do
    hPutStrLn reportHandle $ "Test "++show testNum
    hPutStrLn reportHandle $ "    Result: "++parseGradeReport result
    hPutStrLn reportHandle ""
    writeToReport results (testNum + 1) reportHandle
       
       
       
parseGradeReport :: Result -> String
parseGradeReport result = case result of
                            success@(Success numTests labels output) -> "Success!"
                            gaveUp@(GaveUp numTests labels output) -> "Gave up. Could not determine result."
                            failure@(Failure numTests _ _ _ _ usedSize reason _ labels output failingTestCase) -> "Failed on test case: "++show failingTestCase
                            _ -> undefined
