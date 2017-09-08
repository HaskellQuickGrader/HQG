import System.IO
import Data.List.Utils
import System.Environment
import System.Directory
import System.Process
import Control.Monad
import System.Exit
import Filesystem.Path

import GradeHomework.TransferData

-- command line arguments must follow this order:
-- First argument is the Homework Number
-- Second argument is the path to the student's homework repo folder, this is used
-- for copying the grade report into that folder, which is then added to their repo for them to see

-- Need to move student's solution in this module and not in AHGTemplate, because AHGTemplate imports 
-- Hwk1.Hwk1Tests, which imports Hwk1 (student's solutions)
 
main = do
    args@(x:y:xs) <- getArgs
    _ <- begin.show $ " Homework number: "++x++", student's repo path: "++y
    listUser
    let ahgHwk = "AHG_Hwk"++x++".hs"
    let ahgHwkExe = "./AHG_Hwk"++x   -- for Linux usage
    -- let ahgHwkExe = "AHG_Hwk"++x++".exe" -- for Windows usage
    --exists <- doesFileExist ahgHwkExe
    let reportFolder = "Hwk"++x  -- Report folder and student's solution file have same name
    dir <- getCurrentDirectory
    let currentDir = dir++"/Hwk"
    -- _ <- begin.show $ "Current Directory just set to : "++(show dir)
    -- dir <- getCurrentDirectory
    -- let parentDir = show $ parent dir
    -- let currentDir = parentDir++"Hwk"
    --let currentDir = "/usr/lib/cgi-bin/AHG/Hwk"
    let reportFolderPath = currentDir++"/"++reportFolder
    clearFolder reportFolder reportFolderPath
    getStudentHwk y reportFolder currentDir
    makeAHG x ahgHwk currentDir
    makeExe ahgHwk currentDir
    runExe ahgHwkExe y currentDir
    -- if(exists)
        -- then runExe ahgHwkExe y
        -- else do
              
    
makeAHG :: String -> String -> String -> IO ()
makeAHG hwkNum  ahgHwk currentDir = do
    _ <- begin.show $ "starting to make AHG"
    readHandle <- openFile (currentDir++"/AHGTemplate.hs") ReadMode
    writeHandle <- openFile (currentDir++"/"++ahgHwk) WriteMode
    contents <- hGetContents readHandle
    let newLine = replace "{{HwkNum}}" hwkNum contents
    hPutStrLn writeHandle newLine
    hClose writeHandle
    hClose readHandle
    _ <- begin.show $ "Finished making AHG"
    return ()

runExe :: String -> String -> String -> IO ()
runExe ahgHwkExe repoDir currentDir = do 
                -- currentDir <- getCurrentDirectory
		_ <- begin.show $ "running executable"
		_ <- begin.show $ "Passing repo dir to AHGTemplate: "++repoDir
                (exitCode,stdOut,stdErr) <- readProcessWithExitCode (currentDir++"/"++ahgHwkExe) [repoDir] ""
                case exitCode of
                    ExitSuccess -> do
		    		   _ <- begin.show $ "Successfully ran executable"
		    		   return ()
                    _ -> do
		      	   _ <- begin.show $ "Unsuccessfully ran exectuable"
                           _ <- begin.show $ "Standard out: "++ (show stdOut)
                           _ <- begin. show $ "Standard error: "++(show stdErr)
			   return ()

listUser :: IO ()
listUser = do
	 (extCode,stdOut,stdErr) <- readProcessWithExitCode "whoami" [] ""
	 case extCode of
	   ExitSuccess -> do
	   	       _ <- begin.show $ "Current user in SetupAHG: "++stdOut
		       return ()
	   _ -> do
	     	_ <- begin.show $ "Standard Error in getting current user: "++stdErr
		return ()
                    
makeExe :: String -> String -> IO ()
makeExe file currentDir = do
	        _ <- begin.show $ "making executable"
		_ <- begin.show $ "File being made into executable: "++currentDir++"/"++file
                (exitCode,stdOut,stdErr) <- readProcessWithExitCode ("ghc") ["--make",(currentDir++"/"++file)] ""
                case exitCode of
                    ExitSuccess -> do
		    		   _ <- begin.show $ "Executable made successfully"
				   return ()
                    _ -> do
		        _ <- begin.show $ "Exectuable NOT made successfully"
                        error $"Standard out: "++ stdOut++ "   Standard error: "++stdErr
		
                        
                        
getStudentHwk :: String -> String -> String -> IO ()
getStudentHwk repoFolder hwkName currentDir = do
    -- currentDir <- getCurrentDirectory
    _ <- begin.show $ "getting student's homework"
    let copyFrom = repoFolder++hwkName++".hs"
    let copyTo = currentDir++"/"++hwkName++"/"++hwkName++".hs"
    _ <- begin.show $ "Copy from: "++copyFrom++", copy to: "++copyTo
    copyFile copyFrom copyTo
    
clearFolder :: String -> String -> IO ()
clearFolder solutionName reportFolder = do
    let reportPath = reportFolder++"/GradeReport.txt"
    let solutionPath = reportFolder++"/"++solutionName++".hs"
    _ <- begin.show $ "report Path: "++ reportPath
    _ <- begin.show $ "solution path: "++ solutionPath
    reportExists <- doesFileExist reportPath
    solutionExists <- doesFileExist solutionPath
    if(reportExists)
     then removeFile $ reportFolder++"/GradeReport.txt"
     else return ()
    if(solutionExists)
     then removeFile solutionPath
     else return ()