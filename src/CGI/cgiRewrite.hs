module CGI.CGIRewrite where
import Network.CGI
import System.IO
import SystemHook
import Query
import qualified Data.ByteString.Lazy.Char8 as CH
import System.Directory
import System.Process
import CGI.Clone.Clone

import CGI.Clone.Clone

secretKey :: String
secretKey = "GUC#Wwdk6x!5dtrrnb#8W$p%$wMgMd7xCvr$CNHmy#D%Vf&Ux6"

-- Debugging

logStr :: String -> IO ()
logStr inpStr = do outh <- openFile "clone-log.txt" AppendMode
                   hPutStrLn outh inpStr
                   hClose outh
-- End Debugging


--Checks to see if system hook has a header associated with it
processHeader :: Maybe String -> CGI CGIResult
processHeader Nothing = error "Error: no event header"
processHeader (Just "System Hook") = getBody >>= processSystemHook                                


--Here I need to detemine what kind of event happend: Push, Move to Group, etc.
--Once determined make function call the handel action:Clone repo, run grade report, etc.
processSystemHook :: String -> CGI CGIResult
processSystemHook inputs = do
  liftIO.logStr $ inputs
  let d = sysHookDecoder $ CH.pack inputs
  case d of
    Left m -> output m
    Right (Left m) -> case m of
                       Left m -> undefined
                       Right m -> undefined
    
    Right (Right a) -> case a of
                      --For push actions (call grading module)
                      Injl (Injl (Injl (Injl (Injl (Injl (Injl (Injl (Injr p)))))))) -> do
                                                                                         liftIO.logStr $ "Push action"
                                                                                         liftIO.logStr $ show p
                                                                                         output ""
                      --Move user to group(call clone module)
                      Injl (Injl (Injl (Injl (Injl (Injl (Injr g)))))) -> do
                                                                           liftIO.logStr $ "Move user to group"
                                                                           liftIO.logStr $ show g
                                                                           let dontneed = clone g 
                                                                           output ""
                       
  liftIO.logStr.show $ d          
  output ""




cgiMain :: CGI CGIResult
cgiMain = do      
        headerToken <- requestHeader "X-Gitlab-Token"
        case headerToken of
            Nothing -> error "Error no token header."
            Just ht | ht == secretKey -> requestHeader "X-Gitlab-Event" >>= processHeader
                    | otherwise -> output ""


main :: IO ()
main = runCGI (handleErrors cgiMain)