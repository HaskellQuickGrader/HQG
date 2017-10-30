module Main where

import Network.CGI 
import System.IO
import SystemHook.ProjectCreate
import Query

-- Debugging

logStr :: String -> IO ()
logStr inpStr = do outh <- openFile "clone-log.txt" AppendMode
                   hPutStrLn outh inpStr
                   hClose outh
-- End Debugging
    
secretKey :: String
secretKey = "GUC#Wwdk6x!5dtrrnb#8W$p%$wMgMd7xCvr$CNHmy#D%Vf&Ux6"

cgiMain :: CGI CGIResult
cgiMain = do      
        headerToken <- requestHeader "X-Gitlab-Token"
        case headerToken of
            Nothing -> error "Error no token header."
            Just ht -> do
              if(ht == secretKey) 
              then do headerEvent <- requestHeader "X-Gitlab-Event"
                      case headerEvent of
                        Nothing -> error "Error: no event header"
                        Just "System Hook" -> do inputs <- getBody
                                                 liftIO.logStr $ inputs
                                                 output ""
              else output ""

main :: IO ()
main = runCGI (handleErrors cgiMain)