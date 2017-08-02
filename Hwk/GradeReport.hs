module GradeReport where

import System.IO

import Data.Char(toUpper)



writeToReport :: String -> String -> IO ()
writeToReport inpStr folder= do
       outh <- openFile (folder++"\\GradeReport.txt") AppendMode
       hPutStrLn outh inpStr
       hClose outh
