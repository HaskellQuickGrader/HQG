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
getRepoName (x:xs) countSlash | x == '/' = if(countSlash == 2)
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
                    _ <- liftIO.begin.show $ map email (map author (commits user))
                    let url = git_http_url (repository user)
                    let repoName = getRepoName (homepage (repository user)) 0
                    _ <- liftIO.begin.show $ "url: "++url
                    (eCode,stdOut,stdErr) <- liftIO $ readProcessWithExitCode "/usr/bin/git" ["-C",("/usr/lib/cgi-bin/Repos/"++repoName),"pull"] ""
                    case eCode of
                        ExitSuccess -> output ""
                        _ -> do
			    _ <- liftIO.begin.show $ stdOut
                            _ <- liftIO.begin.show $ stdErr
			    output ""
                    
            else do
                _ <- liftIO.begin.show $ "You are not authenticated."
                output ""

main :: IO ()
main = runCGI (handleErrors cgiMain)