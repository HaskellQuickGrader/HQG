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
    let fullRepoPath = repoPath++"Hwk_"++hwkNum++"/"
    currentDir <- getCurrentDirectory
    let workingDir = currentDir++"/"++studentName++"/"
    
    -- Customize Homework files
    let hwkPath = workingDir++homeworkName++".hs"
    let hwkTestsPath = workingDir++homeworkName++"Tests.hs"
    customizeFile studentName "{{Name}}" hwkPath
    customizeFile studentName "{{Name}}" hwkTestsPath
    
    setupWorkingDir studentName currentDir homeworkName
    getStudentHwk fullRepoPath homeworkName workingDir
    customAHG <- makeAHG hwkNum ahgHwk currentDir workingDir
    customizeFile studentName "{{Name}}" customAHG
    makeExe ahgHwk currentDir
    runExe ahgHwkExe fullRepoPath workingDir


setupWorkingDir :: String -> String -> String -> IO ()
setupWorkingDir studentName currentDir hwkFolder = do
    let workingDir = currentDir++"/"++studentName++"/"
    let hwkTemplateFolder = currentDir++"/"++"Homeworks/"++hwkFolder++"/"
    doesExist <- doesDirectoryExist workingDir  -- Should be false
    if(doesExist == False)
        then do
            (exitCode,stdOut,stdErr) <- readProcessWithExitCode "cp" ["-R", hwkTemplateFolder, workingDir] ""
            case exitCode of
                ExitSuccess -> begin.show $ "Successfully created working directory"
                _ -> do
                        begin.show $ "Unsuccessfully created working directory"
                        begin.show $ "Standard out: "++ (show stdOut)
                        begin.show $ "Standard error: "++(show stdErr)
        else begin.show $ "Working directory already exists"
        
customizeFile :: String -> String -> String -> IO ()
customizeFile dataToInject replaceToken templateFile = do
        begin.show $ "Starting to make: "++templateFile
        contents <- readFile templateFile
        
        -- Force strict evaluation, otherwise handle is in semi-closed state so you 
        -- cannot write to the file
        length contents `seq` (return ())
        let newLine = replace replaceToken dataToInject contents
        writeFile templateFile newLine
        begin.show $ "Finished making file"
        
customizeAndCreateFile :: String -> String -> String -> String -> IO ()
customizeAndCreateFile dataToInject replaceToken templateFile customizedFile = do
        begin.show $ "Starting to make: "++customizedFile
        readHandle <- openFile templateFile ReadMode
        writeHandle <- openFile customizedFile WriteMode
        contents <- hGetContents readHandle
        let newLine = replace replaceToken dataToInject contents
        hPutStrLn writeHandle newLine
        hClose writeHandle
        hClose readHandle
        begin.show $ "Finished making file"
        -- return customizedFile
    
        
makeAHG :: String -> String -> String -> String -> IO String
makeAHG hwkNum  ahgHwk currentDir workingDir = do
    let templateFile = currentDir++"/AHGTemplate.hs"
    let customFile = workingDir++"/"++ahgHwk
    let replaceToken = "{{HwkNum}}"
    customizeAndCreateFile hwkNum replaceToken templateFile customFile
    return customFile
    

runExe :: String -> String -> String -> IO ()
runExe ahgHwkExe repoDir workingDir = do 
                -- currentDir <- getCurrentDirectory
		_ <- begin.show $ "running executable"
		_ <- begin.show $ "Passing repo dir to AHGTemplate: "++repoDir
                (exitCode,stdOut,stdErr) <- readProcessWithExitCode (workingDir++"/"++ahgHwkExe) [repoDir] ""
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
makeExe file workingDir = do
	        _ <- begin.show $ "making executable"
		_ <- begin.show $ "File being made into executable: "++workingDir++"/"++file
                (exitCode,stdOut,stdErr) <- readProcessWithExitCode ("ghc") ["--make",(workingDir++"/"++file)] ""
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
    let copyTo = reportPath++hwkName++".hs"
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