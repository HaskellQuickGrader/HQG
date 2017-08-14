import System.IO
import Data.List.Utils
import System.Environment
import System.Directory
import System.Process
import Control.Monad
import System.Exit

-- command line arguments must follow this order:
-- First argument is the Homework Number
-- Second argument is the path to the student's homework repo folder, this is used
-- for copying the grade report into that folder, which is then added to their repo for them to see

-- Need to move student's solution in this module and not in AHGTemplate, because AHGTemplate imports 
-- Hwk1.Hwk1Tests, which imports Hwk1 (student's solutions)
 
main = do
    args@(x:y:xs) <- getArgs
    let ahgHwk = "AHG_Hwk"++x++".hs"
    -- let ahgHwkExe = "./AHG_Hwk"++x   -- for Linux usage
    let ahgHwkExe = "AHG_Hwk"++x++".exe" -- for Windows usage
    --exists <- doesFileExist ahgHwkExe
    let reportFolder = "Hwk"++x  -- Report folder and student's solution file have same name
    currentDir <- getCurrentDirectory
    let reportFolderPath = currentDir++"\\"++reportFolder
    clearFolder reportFolder reportFolderPath
    getStudentHwk y reportFolder
    makeAHG x ahgHwk
    print "About to make executable"
    makeExe ahgHwk
    print "Made executable, about to run it"
    runExe ahgHwkExe y
    -- if(exists)
        -- then runExe ahgHwkExe y
        -- else do
              
    
makeAHG :: String -> String -> IO ()
makeAHG hwkNum  ahgHwk = do
    readHandle <- openFile "AHGTemplate.hs" ReadMode
    writeHandle <- openFile ahgHwk WriteMode
    contents <- hGetContents readHandle
    let newLine = replace "{{HwkNum}}" hwkNum contents
    hPutStrLn writeHandle newLine
    hClose writeHandle
    hClose readHandle

runExe :: String -> String -> IO ()
runExe ahgHwkExe repoDir = do 
                currentDir <- getCurrentDirectory
                (exitCode,stdOut,stdErr) <- readProcessWithExitCode (currentDir++"\\"++ahgHwkExe) [repoDir] ""
                case exitCode of
                    ExitSuccess -> return ()
                    _ -> do
                            print stdOut
                            print stdErr
                    
makeExe :: String -> IO ()
makeExe file = do 
                (exitCode,stdOut,stdErr) <- readProcessWithExitCode ("ghc") ["--make",file] ""
                case exitCode of
                    ExitSuccess -> return ()
                    _ -> do
                        error $"Standard out: "++ stdOut++ "   Standard error: "++stdErr
                        
                        
getStudentHwk :: String -> String -> IO ()
getStudentHwk repoFolder hwkName = do
    currentDir <- getCurrentDirectory
    copyFile (repoFolder++"\\"++hwkName++".hs") $ currentDir++"\\"++hwkName++"\\"++hwkName++".hs"
    
clearFolder :: String -> String -> IO ()
clearFolder solutionName reportFolder = do
    let reportPath = reportFolder++"\\GradeReport.txt"
    let solutionPath = reportFolder++"\\"++solutionName++".hs"
    print $ "report Path: "++ reportPath
    print $ "solution path: "++ solutionPath
    reportExists <- doesFileExist reportPath
    solutionExists <- doesFileExist solutionPath
    if(reportExists)
     then removeFile $ reportFolder++"\\GradeReport.txt"
     else return ()
    if(solutionExists)
     then removeFile solutionPath
     else return ()