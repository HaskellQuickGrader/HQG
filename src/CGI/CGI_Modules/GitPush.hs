{-# OPTIONS_GHC -fno-warn-tabs #-}
module CGI.CGI_Modules.GitPush where

import Network.CGI 
import System.Process
import System.Exit
import CGI.CGI_Modules.TransferData

runGitPush :: String -> String -> IO ()
runGitPush gitUrl repoFolder = do   
    (exitCode,standardOut,standardErr) <- readProcessWithExitCode "CGI_Modules/./PushToRepo.sh" [gitUrl, repoFolder] ""
    case exitCode of
      ExitSuccess -> do
                _ <- begin.show $ "Pushing grade report successful"
                return ()
      _ -> do
            _ <- begin.show $ standardOut
            _ <- begin.show $ standardErr
            return ()
 
                
getGitUrlWithCreds :: String -> Int -> String
getGitUrlWithCreds [] slashCount = []
getGitUrlWithCreds gitUrl@(x:xs) slashCount | x == '/' = if(slashCount == 1)
                                                       	 then "git@"++xs
                                                         else getGitUrlWithCreds xs (slashCount + 1)
                                            | otherwise = getGitUrlWithCreds xs slashCount
                                  



 
