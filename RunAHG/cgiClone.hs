{-# OPTIONS_GHC -fno-warn-tabs #-}
import Network.CGI 
import System.Process
import System.Exit
import System.Directory
import qualified Data.Time.LocalTime as LT
import CGI_Modules.TransferData
import CGI_Modules.ParseSystemEventInfo
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
-- Place date at top of log file 
        localTime <- liftIO $ LT.getZonedTime
        _ <- liftIO.begin.show $ ""
        _ <- liftIO.begin.show $ "NEW LOG ENTRY: "++show localTime
        
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

					    -- Make sure class folder in Repos directory exists
                                            let className = (owner_name systemEvent)
                                            let repoFolderBase = "/usr/lib/cgi-bin/Repos/"
                                            let repoFolder = repoFolderBase++className
                                            let eName = (event_name systemEvent)
                                            uri <- progURI
                                            let domain = composeCloneURL (show uri) 0 0
                                            let cloneURL = domain++"/"++pathNameSpace++".git"
                                            let cloneCmd = "clone "++cloneURL
                                            if (eName == "project_create")                      -- verify this is a project creation
                                                then do
                                                    _ <- liftIO.begin.show $ inputs
                                                    _ <- liftIO.begin.show $ "clone command: "++cloneCmd
						    _ <- liftIO $ checkClassDirectory repoFolder
                                                    (eCode,stdOut,stdErr) <- liftIO $ readProcessWithExitCode "/usr/bin/git" ["-C",repoFolder,"clone", cloneURL] ""        -- Clone newly created repo
                                                    case eCode of
                                                        ExitSuccess -> do
                                                                    _ <- liftIO.begin.show $ "Repo cloned successfully"
                                                                    output ""
                                                        _ -> do
                                                            _ <- liftIO.begin.show $ stdOut         -- Log any output or errors
                                                            _ <- liftIO.begin.show $ stdErr
                                                            output ""
                                                else
                                                   output ""
                                            output ""
                                    else 
                                        output ""
            else do
                _ <- liftIO.begin.show $ "You are not authenticated."
                output ""
                
checkClassDirectory :: String -> IO ()
checkClassDirectory classDir = do
    doesExist <- doesDirectoryExist classDir
    if(doesExist == False)
        then createDirectory classDir
        else return ()

                
main :: IO ()
main = runCGI (handleErrors cgiMain)