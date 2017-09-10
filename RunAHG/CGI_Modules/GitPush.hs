{-# OPTIONS_GHC -fno-warn-tabs #-}
module CGI_Modules.GitPush where

import Network.CGI 
import System.Process
import System.Exit
import CGI_Modules.TransferData

runGitPush :: String -> String -> IO ()
runGitPush gitUrl repoFolder = do   
    (exitCode,standardOut,standardErr) <- readProcessWithExitCode "CGI_Modules/./PushToRepo.sh" [gitUrl] ""
    case exitCode of
      ExitSuccess -> do
                _ <- begin.show $ "Pushing grade report successful"
                return ()
      _ -> do
            _ <- begin.show $ standardOut
            _ <- begin.show $ standardErr
            return ()
 
                
getGitUrlWithCreds :: String -> String -> String -> Int -> String
getGitUrlWithCreds usrname pswd [] slashCount = []
getGitUrlWithCreds usrname pswd gitUrl@(x:xs) slashCount | x == '/' = if(slashCount == 1)
                                                                        then usrname++":"++pswd++"@"++xs
                                                                        else x:getGitUrlWithCreds usrname pswd xs (slashCount + 1)
                                                         | otherwise = x:getGitUrlWithCreds usrname pswd xs slashCount
                                  



 
