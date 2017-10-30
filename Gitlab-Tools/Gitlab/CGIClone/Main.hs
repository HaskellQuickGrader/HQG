module CGIClone.Main where

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

processSystemHook :: String -> CGI CGIResult
processSystemHook inputs = do
  liftIO.logStr $ inputs
  output ""

processHeader :: Maybe String -> CGI CGIResult
processHeader Nothing = error "Error: no event header"
processHeader (Just "System Hook") = getBody >>= processSystemHook

cgiMain :: CGI CGIResult
cgiMain = do      
        headerToken <- requestHeader "X-Gitlab-Token"
        case headerToken of
            Nothing -> error "Error no token header."
            Just ht | ht == secretKey -> requestHeader "X-Gitlab-Event" >>= processHeader
                    | otherwise -> output ""

main :: IO ()
main = runCGI (handleErrors cgiMain)