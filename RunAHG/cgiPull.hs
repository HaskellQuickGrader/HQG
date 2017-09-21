{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE DuplicateRecordFields #-}
import Network.CGI
import qualified Network.CGI.Protocol as NCP
import System.Process
import System.Exit
import Control.Monad.Trans.Class
import CGI_Modules.TransferData
import CGI_Modules.ParseUserInfo
import CGI_Modules.GitPush
import qualified Data.ByteString.Lazy.Char8 as B


cgiMain :: CGI CGIResult
cgiMain = do
        --get header and check for secret token authorization
        header <- requestHeader "X-Gitlab-Token"
        case header of
            Nothing -> error "Error no header."
            Just h -> do
            if(h == "eNbbFFBqgBq5TSGdUtWr9gw4WXptmKbKQKp3P8bPAksYyKvx")
                then do
                    -- Get info from git push
                    _ <- listUser
                    inputs <- getBody
                    user <- parseJSON $ B.pack inputs
                    
                    -- The branch name that was just pushed is not expressly given
                    -- instead we have to parse it out from "ref"
                    let branchRef = ref user         
                    let branch = getBranchName branchRef 0 
                    _ <- liftIO.begin.show $ "Pulling on branch name: "++branch
                    let url = git_http_url ((repository user) :: Repo)
                                        
                    -- Repo folder structure:
                    -- Repos/<ClassName>/<StudentName>/Hwk_<Number>/Hwk<Number>.hs
                    -- Compose Repo path
                    let studentName = name ((project user) :: Project)
                    let className = namespace ((project user) :: Project)
                    let repoBase = "/usr/lib/cgi-bin/Repos/"
                    let classRepo = repoBase++className++"/"
                    let studentRepo = classRepo++studentName++"/"
                    
                    -- Homework number is not simply parsed from data received when 
                    -- student pushes, instead commit message must only contain homework number
                    let hwkNum = getHwkNumber $ commits user
                    _ <- liftIO.begin.show $ "Homework number: "++ show hwkNum
                    _ <- liftIO.begin.show $ "Student's repo path: "++studentRepo
                    
                    if(branch == "solution" && hwkNum /= (-1))    -- only pull and grade on "solution" branch
                      then do 
                            -- Pull student's solution and put it in their repo
			    _ <- liftIO.begin.show $ hwkNum
                            gitStudentRepo studentRepo
                            runAHGSetup url (show hwkNum) studentRepo studentName
                      else output ""
                    output ""
            else do
                _ <- liftIO.begin.show $ "You are not authenticated."
                output ""
                
getHwkNumber :: [Commit] -> Int
getHwkNumber [] = (-1)    -- indicates there was no homework number as a commit
getHwkNumber (c:cts) = case NCP.maybeRead (message c) :: Maybe Int of
                Just num -> num
                Nothing -> getHwkNumber cts
                
                
gitStudentRepo :: String -> CGI CGIResult
gitStudentRepo repoPath = do
    _ <- liftIO.begin.show $ "Calling bash script"
    (extCode,stndOut,stndErr) <- liftIO $ readProcessWithExitCode "CGI_Modules/./GitStudentRepo.sh" [repoPath] ""
    case extCode of
       ExitSuccess -> do 
                   _ <- liftIO.begin.show $ "Bash script finished"
                   output ""
       _ -> do
             _ <- liftIO.begin.show $ "Standard out:"++stndOut
             _ <- liftIO.begin.show $ "Standard error:"++stndErr
             output ""  
                 
                
runAHGSetup :: String -> String -> String -> String -> CGI CGIResult
runAHGSetup url hwkNum repoFolder studentName = do
    _ <- liftIO.begin.show $ "Running AHG Setup"
    _ <- liftIO.begin.show $ "Repo folder used for git add, commit, and push: "++repoFolder
    (extCode,stndOut,stndErr) <- liftIO $ readProcessWithExitCode "/usr/lib/cgi-bin/AHG/RunAHG/./SetupAHG" [hwkNum, repoFolder, studentName] ""
    case extCode of
       ExitSuccess -> do 
                   _ <- liftIO.begin.show $ "Finished grading homework, pushing grade report to repo"
                   let gitUrl = getGitUrlWithCreds "root" "password" url 0
                   _ <- liftIO $ runGitPush gitUrl repoFolder
                   -- _ <- liftIO.gitAddGradeReport $ repoFolder
                   -- _ <- liftIO $ gitCommit  "Pushing grade report." repoFolder
                   
                   -- _ <- liftIO.begin.show $ "Git url for pushing repo: "++gitUrl
                   -- _ <- liftIO $ gitPushGradeReport url repoFolder
                   output ""
       _ -> do
             _ <- liftIO.begin.show $ stndOut
             _ <- liftIO.begin.show $ stndErr
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
	   	       _ <- liftIO.begin.show $ "Current user: "++stnOut
		       output ""
	   _ -> do
	     _ <- liftIO.begin.show $ "standard error: "++stdErr
	     output ""
                               

main :: IO ()
main = runCGI (handleErrors cgiMain)