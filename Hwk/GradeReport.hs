module GradeReport where

import System.IO

import Data.Char(toUpper)



writeToReport :: String -> IO ()
writeToReport inpStr = do
       outh <- openFile "GradeReport.txt" AppendMode
       hPutStrLn outh inpStr
       hClose outh
