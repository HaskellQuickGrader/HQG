{-# OPTIONS_GHC -fno-warn-tabs #-}
import Network.CGI 
import System.Process
import System.Exit
import Control.Monad.Trans.Class
import TransferData
import ParseSystemEventInfo
import qualified Data.ByteString.Lazy.Char8 as B


composeCloneURL :: String -> Int -> Int -> String
composeCloneURL [] countColon countSlash= []
composeCloneURL (x:xs) countColon countSlash | x == ':' = if (countColon == 1)
                                                            then []     -- Reached the port number
                                                            else x:composeCloneURL xs (countColon +1) countSlash
                                             | x == '/' = if(countSlash == 1)   -- Need to add gitlab username and password
                                                            then x:"root:password@"++composeCloneURL xs countColon 2
                                                            else x:composeCloneURL xs countColon (countSlash+1)
                                             | otherwise = x:composeCloneURL xs countColon countSlash

cgiMain :: CGI CGIResult
cgiMain = do
        --get header and check for secret token authorization
        headerToken <- requestHeader "X-Gitlab-Token"
        case headerToken of
            Nothing -> error "Error no token header."
            Just ht -> do
            if(ht == "eNbbFFBqgBq5TSGdUtWr9gw4WXptmKbKQKp3P8bPAksYyKvx") -- make sure secret token is present
            -- Determine which type of event just occured
                then do
                    headerEvent <- requestHeader "X-Gitlab-Event"
                    case headerEvent of
                        Nothing -> error "Error: no event header"
                        Just ge -> do
                                    if(ge == "System Hook")
                                        then do 
                                            inputs <- getBody                                   -- Get body of reponse
                                            systemEvent <- parseJSON $ B.pack inputs
                                            let pathNameSpace = (path_with_namespace systemEvent)
                                            let eName = (event_name systemEvent)
                                            uri <- progURI
                                            let domain = composeCloneURL (show uri) 0 0
                                            let cloneURL = domain++"/"++pathNameSpace++".git"
                                            let cloneCmd = "clone "++cloneURL
                                            if (eName == "project_create")                      -- verify this is a project creation
                                                then do
                                                    _ <- liftIO.begin.show $ "clone command: "++cloneCmd
                                                    (eCode,stdOut,stdErr) <- liftIO $ readProcessWithExitCode "/usr/bin/git" ["-C","/usr/lib/cgi-bin/Repos","clone", cloneURL] ""        -- Clone newly created repo
                                                    case eCode of
                                                        ExitSuccess -> do
                                                                    _ <- liftIO.begin.show $ "Repo cloned successfully"
                                                                    output ""
                                                        _ -> do
                                                            _ <- liftIO.begin.show $ stdOut         -- Log any output or errors
                                                            _ <- liftIO.begin.show $ stdErr
                                                            output ""
                                                else
                                                    do
                                                    _ <- liftIO.begin.show $ "Event name not project_create"
                                                    output ""
                                            output ""
                                    else 
                                        output ""
            else do
                _ <- liftIO.begin.show $ "You are not authenticated."
                output ""

                
main :: IO ()
main = runCGI (handleErrors cgiMain)