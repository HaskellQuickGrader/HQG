{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE DuplicateRecordFields#-}
import Network.CGI 
import System.Process
import System.Exit
import Control.Monad.Trans.Class
import TransferData
import ParseUserInfo
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
                    inputs <- getBody
                    user <- parseJSON $ B.pack inputs
                    let branchRef = ref user       -- used for getting branch name  
                    let branch = getBranchName branchRef 0 
                    _ <- liftIO.begin.show $ "Pulling on branch name: "++branch
                    let url = git_http_url ((repository user) :: Repo)
                    let repoName = name ((project user) :: Project)
                    let hwkNum = parseHwkNum repoName
                    _ <- liftIO.begin.show $ "Homework number: "++hwkNum
                    let repoFolder = "/usr/lib/cgi-bin/Repos/Hwk_1"
                    if(branch == "solution")    -- only pull and grade on "solution" branch
                      then do 
                            (eCode,stdOut,stdErr) <- liftIO $ readProcessWithExitCode "/usr/bin/git" ["-C",("/usr/lib/cgi-bin/Repos/"++repoName),"pull", "--all"] ""        -- Pull all branches
                            case eCode of
                                ExitSuccess -> switchBranch repoName hwkNum
                                _ -> do
                                    _ <- liftIO.begin.show $ stdOut
                                    _ <- liftIO.begin.show $ stdErr
                                    output ""
                      else output ""
            else do
                _ <- liftIO.begin.show $ "You are not authenticated."
                output ""
                
                
runAHGSetup :: String -> String -> CGI CGIResult
runAHGSetup hwkNum repoFolder = do
    _ <- liftIO.begin.show $ "Running AHG Setup"
    (extCode,stndOut,stndErr) <- liftIO $ readProcessWithExitCode "/usr/lib/cgi-bin/AHG/Hwk" ["./SetupAHG", hwkNum, repoFolder] ""
    case extCode of
       ExitSuccess -> do 
                   _ <- liftIO.begin.show $ "Finished grading homework"
                   output ""
       _ -> do
             _ <- liftIO.begin.show $ stndOut
             _ <- liftIO.begin.show $ stndErr
             output ""
               
-- Switch to "Solution" branch               
switchBranch :: String -> String -> CGI CGIResult
switchBranch repoName hwkNum = do
        _ <- liftIO.begin.show $ "switching branch"
        (exitCode,standardOut,standardErr) <- liftIO $ readProcessWithExitCode "/usr/bin/git" ["-C",("/usr/lib/cgi-bin/Repos/"++repoName),"checkout", "solution"] ""
        case exitCode of
          ExitSuccess -> runAHGSetup hwkNum $ "/usr/lib/cgi-bin/Repos/"++repoName
          _ -> do
                _ <- liftIO.begin.show $ standardOut
                _ <- liftIO.begin.show $ standardErr
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
                               

main :: IO ()
main = runCGI (handleErrors cgiMain)