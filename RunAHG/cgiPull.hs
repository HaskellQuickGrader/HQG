{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE DuplicateRecordFields #-}
import Network.CGI
import qualified Network.CGI.Protocol as NCP
import System.Process
import qualified System.Directory as SD
import System.Exit
import Data.List.Split
import qualified Data.Time.LocalTime as LT
import Control.Monad.Trans.Class
import CGI_Modules.TransferData
import CGI_Modules.ParseUserInfo
import CGI_Modules.GitPush
import qualified Data.ByteString.Lazy.Char8 as B


cgiMain :: CGI CGIResult
cgiMain = do
        -- Place date at top of log file 
        localTime <- liftIO $ LT.getZonedTime
        liftIO.begin.show $ ""
        liftIO.begin.show $ "NEW LOG ENTRY: "++show localTime
        
        --get header and check for secret token authorization
        header <- requestHeader "X-Gitlab-Token"
        case header of
            Nothing -> error "Error no header."
            Just h -> do
            if(h == "eNbbFFBqgBq5TSGdUtWr9gw4WXptmKbKQKp3P8bPAksYyKvx")
                then do
                    -- Get info from git push
                    listUser
                    inputs <- getBody
                    user <- parseJSON $ B.pack inputs
                    
                    -- The branch name that was just pushed is not expressly given
                    -- instead we have to parse it out from "ref"
                    let branchRef = ref user         
                    let fullBranch = getBranchName branchRef 0
                    
                    -- Homework number is not simply parsed from data received when 
                    -- the student pushes, instead it is nested in the branch name
                    -- The branch naming convention is as follows:
                    -- Hwk-<Number>-solution
                    let (hwkNum, branch) = getHwkNumber $ fullBranch
                    liftIO.begin.show $ "Homework number: "++ show hwkNum
                    liftIO.begin.show $ "Pulling on branch name: "++fullBranch
                    let url = git_http_url ((repository user) :: Repo)
                                        
                    -- Repo folder structure:
                    -- Repos/<ClassName>/<StudentName>/Hwk_<Number>/Hwk<Number>.hs
                    -- Compose Repo path
                    let studentName = name ((project user) :: Project)
                    let className = namespace ((project user) :: Project)
                    let repoBase = "/usr/lib/cgi-bin/Repos/"
                    let classRepo = repoBase++className++"/"
                    let studentRepo = classRepo++studentName++"/"
                    let hwkRepoFolder = studentRepo++"Hwk_"++hwkNum
                    
                    liftIO.begin.show $ "Student's repo path: "++studentRepo
                    
                    if(branch == "solution" && hwkNum /= (-1))    -- only pull and grade on "solution" branch
                      then do 
                            -- Pull student's solution and put it in their repo
                            
                            reportExists <- liftIO $ checkForGradeReport hwkRepoFolder
                            liftIO.begin.show $ "Homework number: "++show hwkNum
                            if(not reportExists)
                              then 
                                do
                                 gitStudentRepo studentRepo fullBranch
                                 runAHGSetup url (show hwkNum) studentRepo studentName
                              else
                                do
                                liftIO.begin.show $ "student pushed homework but gradereport already exists."
                                output ""
                      else output ""
                    output ""
            else do
                _ <- liftIO.begin.show $ "You are not authenticated."
                output ""

-- This prevents students from pushing their homeowork multiple times                
checkForGradeReport :: String -> IO Bool
checkForGradeReport repo = do
    let gradeReportPath = repo++"GradeReport.txt"
    SD.doesFileExist gradeReportPath
    
                
getHwkNumber :: String -> (Int, String)
getHwkNumber branch = do
    let (hwkNum, solutionBranch) = getNum branch
    (read hwkNum :: Int, solutionBranch)
 where 
    getNum :: String -> (String,String)
    getNum branch = do
        let tokens = splitOn "-" branch
        if (length tokens == 3)
            then (tokens !! 1, tokens !! 2)
            else ("-1", tokens !! 2)
                
gitStudentRepo :: String -> String ->  CGI CGIResult
gitStudentRepo repoPath branch = do
    liftIO.begin.show $ "Calling bash script"
    (extCode,stndOut,stndErr) <- liftIO $ readProcessWithExitCode "CGI_Modules/./GitStudentRepo.sh" [repoPath, branch] ""
    case extCode of
       ExitSuccess -> do 
                   liftIO.begin.show $ "Bash script finished"
                   output ""
       _ -> do
             liftIO.begin.show $ "Standard out:"++stndOut
             liftIO.begin.show $ "Standard error:"++stndErr
             output ""  
                 
                
runAHGSetup :: String -> String -> String -> String -> CGI CGIResult
runAHGSetup url hwkNum repoFolder studentName = do
    liftIO.begin.show $ "Running AHG Setup"
    liftIO.begin.show $ "Repo folder used for git add, commit, and push: "++repoFolder
    (extCode,stndOut,stndErr) <- liftIO $ readProcessWithExitCode "/usr/lib/cgi-bin/AHG/RunAHG/./SetupAHG" [hwkNum, repoFolder, studentName] ""
    case extCode of
       ExitSuccess -> do 
                   liftIO.begin.show $ "Finished grading homework, pushing grade report to repo"
                   let gitUrl = getGitUrlWithCreds "root" "password" url 0
                   liftIO $ runGitPush gitUrl repoFolder
                   output ""
       _ -> do
             liftIO.begin.show $ stndOut
             liftIO.begin.show $ stndErr
             output ""
  
                                   
parseHwkNum :: String -> String
parseHwkNum [] = []
parseHwkNum (x:xs) = if (x == '_')
                        then xs
                        else parseHwkNum xs
                        
getBranchName :: String -> Int -> String
getBranchName [] _ = []
getBranchName (x:xs) slashCount | x == '/' = if(slashCount == 1)
                                                then xs
                                                else getBranchName xs (slashCount + 1)
                                | otherwise = getBranchName xs slashCount

listUser :: CGI CGIResult
listUser = do
	 (exitCode, stnOut, stdErr) <- liftIO $ readProcessWithExitCode "whoami" [] ""
	 case exitCode of
	   ExitSuccess -> do
	   	       liftIO.begin.show $ "Current user: "++stnOut
		       output ""
	   _ -> do
	     liftIO.begin.show $ "standard error: "++stdErr
	     output ""
                               

main :: IO ()
main = runCGI (handleErrors cgiMain)