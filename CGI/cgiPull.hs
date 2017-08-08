{-# OPTIONS_GHC -fno-warn-tabs #-}
import Network.CGI 
import System.Process
import System.Exit
import Control.Monad.Trans.Class
import TransferData
import ParseUserInfo
import qualified Data.ByteString.Lazy.Char8 as B

-- Hack to get repo name
getRepoName :: String -> Int -> String
getRepoName [] countSlash = []
getRepoName (x:xs) countSlash | x == '/' = if(countSlash == 3)
                                                then xs
                                                else getRepoName xs (countSlash + 1)
                              | otherwise = getRepoName xs countSlash

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
                    let branch = getBranchName (ref user) 0
                    _ <- liftIO.begin.show $ "Pulling on branch name: "++branch
                    let url = git_http_url (repository user)
                    let repoName = getRepoName (homepage (repository user)) 0
                    let hwkNum = parseHwkNum repoName
                    _ <- liftIO.begin.show $ "Homework number: "++hwkNum
                    let repoFolder = "/usr/lib/cgi-bin/Repos/Hwk_1"
                    if(branch == "solution")    -- only pull and grade on "solution" branch
                      then do 
                            (eCode,stdOut,stdErr) <- liftIO $ readProcessWithExitCode "/usr/bin/git" ["-C",("/usr/lib/cgi-bin/Repos/"++repoName),"pull"] ""
                            case eCode of
                                ExitSuccess -> do
                                                (extCode,stndOut,stndErr) <- liftIO $ readProcessWithExitCode "/usr/lib/cgi-bin/AHG/Hwk" ["SetupAHG.exe", hwkNum, repoFolder] ""
                                                case extCode of
                                                    ExitSuccess -> do 
                                                                    _ <- liftIO.begin.show $ "Finished grading homework"
                                                                    output ""
                                                    _ -> do
                                                            _ <- liftIO.begin.show $ stdOut
                                                            _ <- liftIO.begin.show $ stdErr
                                                            output ""
                                _ -> do
                                    _ <- liftIO.begin.show $ stdOut
                                    _ <- liftIO.begin.show $ stdErr
                                    output ""
                      else output ""
            else do
                _ <- liftIO.begin.show $ "You are not authenticated."
                output ""
                
                
getBranchName :: String -> Int -> String
getBranchName [] _ = []
getBranchName (x:xs) slashCount | x == '/' = if (slashCount == 2)
                                                then xs
                                                else getBranchName xs (slashCount + 1)
                                | otherwise = getBranchName xs (slashCount)
                                
parseHwkNum :: String -> String
parseHwkNum [] = []
parseHwkNum (x:xs) = if (x == '_')
                        then xs
                        else parseHwkNum xs
                                

main :: IO ()
main = runCGI (handleErrors cgiMain)