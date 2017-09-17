{-# OPTIONS_GHC -fno-warn-tabs #-}

import System.IO
import Data.List.Utils
import System.Environment
import System.Directory
import System.Process
import Control.Monad
import System.Exit


import CGI_Modules.TransferData

-- command line arguments must follow this order:
-- First argument is the Homework Number
-- Second argument is the path to the student's homework repo folder, this is used
-- for copying the grade report into that folder, which is then added to their repo for them to see
-- Third argument is the student's name whose homework is being graded

-- Need to move student's solution in this module and not in AHGTemplate, because AHGTemplate imports 
-- Hwk1.Hwk1Tests, which imports Hwk1 (student's solutions)
 
main = do
    args@(hwkNum:repoPath:studentName:xs) <- getArgs
    _ <- begin.show $ " Homework number: "++hwkNum++", "++studentName++"'s repo path: "++repoPath
    
    -- Set up local variables
    let ahgHwk = "AHG_Hwk"++hwkNum++".hs"
    let ahgHwkExe = "./AHG_Hwk"++hwkNum
    let homeworkName = "Hwk"++hwkNum
    let fullRepoPath = repoPath++"/"++"Hwk_"++hwkNum
    let reportFolder = "/Hwk/"++homeworkName
    let reportFolderPath = currentDir++"/"++reportFolder
    let workingDir = currentDir++"/"++studentName++"/"++homeworkName++"/"
    
    currentDir <- getCurrentDirectory
    setupWorkingDir studentName currentDir homeworkName
    
    getStudentHwk fullRepoPath homeworkName reportFolderPath
    makeAHG hwkNum ahgHwk currentDir
    makeExe ahgHwk currentDir
    runExe ahgHwkExe fullRepoPath currentDir
    -- if(exists)
        -- then runExe ahgHwkExe y
        -- else do


setupWorkingDir :: String -> String -> String -> IO ()
setupWorkingDir studentName currentDir hwkFolder = do
    let workingDir = currentDir++"/"++studentName
    let hwkTemplateFolder = currentDir++"/"++"Homeworks/"++hwkFolder
    doesExist <- doesDirectoryExist workingDir  -- Should be false
    if(doesExist == False)
        then do
            (exitCode,stdOut,stdErr) <- readProcessWithExitCode "cp" [hwkTemplateFolder, workingDir] ""
                case exitCode of
                    ExitSuccess -> do
		    		   _ <- begin.show $ "Successfully created working directory"
                    _ -> do
                            _ <- begin.show $ "Unsuccessfully created working directory"
                            _ <- begin.show $ "Standard out: "++ (show stdOut)
                            _ <- begin.show $ "Standard error: "++(show stdErr)
        else begin.show $ "Working directory already exists"
        
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
getStudentHwk repoFolder hwkName reportPath = do
    _ <- begin.show $ "getting student's homework"
    let copyFrom = repoFolder++hwkName++".hs"
    let copyTo = reportPath++"/"++hwkName++".hs"
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