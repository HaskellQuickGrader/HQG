{-# OPTIONS_GHC -fno-warn-tabs #-}
import Network.CGI 
import System.Process
import System.Exit
import Control.Monad.Trans.Class
import TransferData
import ParseSystemEventInfo
import qualified Data.ByteString.Lazy.Char8 as B


getDomainFromURI :: String -> Int -> String
getDomainFromURI [] count = []
getDomainFromURI (x:xs) count = if (x == ':')
                                    then if (count == 1)
                                            then []
                                            else x:getDomainFromURI xs 1
                                    else 
                                        x:getDomainFromURI xs count


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
                                            _ <- liftIO.begin.show $ "path with namespace: "++pathNameSpace
                                            let eName = (event_name systemEvent)
                                            _ <- liftIO.begin.show $ "event_name: "++eName
                                            uri <- progURI
                                            let domain = getDomainFromURI (show uri) 0
                                            _ <- liftIO.begin.show $ "domain: "++domain
                                            let cloneURL = domain++"/"++pathNameSpace++".git"
                                            _ <- liftIO.begin.show $ cloneURL
                                            -- if (eName == "project_create")                      -- verify this is a project creation
                                                -- then do
                                                    -- -- _ <- liftIO.begin.show $ "clone url: "++cloneURL
                                                    -- -- (eCode,stdOut,stdErr) <- liftIO $ readProcessWithExitCode "/usr/bin/git" ["-C","/usr/lib/cgi-bin/Repos",("clone "++cloneURL)] ""        -- Clone newly created repo
                                                    -- -- case eCode of
                                                        -- -- ExitSuccess -> output ""
                                                        -- -- _ -> do
                                                            -- -- _ <- liftIO.begin.show $ stdOut         -- Log any output or errors
                                                            -- -- _ <- liftIO.begin.show $ stdErr
                                                            -- output ""
                                            -- else
                                                -- output ""
                                            output ""
                                    else 
                                        output ""
            else do
                _ <- liftIO.begin.show $ "You are not authenticated."
                output ""

                
main :: IO ()
main = runCGI (handleErrors cgiMain)