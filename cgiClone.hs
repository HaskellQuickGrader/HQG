{-# OPTIONS_GHC -fno-warn-tabs #-}
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
            if(h == "eNbbFFBqgBq5TSGdUtWr9gw4WXptmKbKQKp3P8bPAksYyKvx") -- make sure secret token is present
                then do
                    inputs <- getBody                                   -- Get body of reponse
                    user <- parseJSON $ B.pack inputs
                    let eName = (event_name user)
                    _ <- liftIO.begin.show $ "event_name: "++eName
                    let cloneURL = git_http_url (repository user)
                    if (eName == "project_create")                      -- verify this is a project creation
                        then do
                            _ <- liftIO.begin.show $ "clone url: "++cloneURL
                            (eCode,stdOut,stdErr) <- liftIO $ readProcessWithExitCode "/usr/bin/git" ["-C","/usr/lib/cgi-bin/Repos",("clone "++cloneURL)] ""        -- Clone newly created repo
                            case eCode of
                                ExitSuccess -> output ""
                                _ -> do
                                    _ <- liftIO.begin.show $ stdOut         -- Log any output or errors
                                    _ <- liftIO.begin.show $ stdErr
                                    output ""
                    else
                        output ""
            else do
                _ <- liftIO.begin.show $ "You are not authenticated."
                output ""

main :: IO ()
main = runCGI (handleErrors cgiMain)