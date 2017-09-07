{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE DuplicateRecordFields#-}
import Network.CGI 
import System.Process
import System.Exit
import Control.Monad.Trans.Class


main :: IO ()
main = do
    (exitCode, stnOut, stdErr) <- liftIO $ readProcessWithExitCode "/bin/bash" ["./setGitConfig"] ""
    case exitCode of
	   ExitSuccess -> print $ "Current user: "++stnOut
	   _ -> print $ "standard error: "++stdErr