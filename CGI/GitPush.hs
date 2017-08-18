{-# OPTIONS_GHC -fno-warn-tabs #-}
import Network.CGI 
import System.Process
import System.Exit
import TransferData
 
 
gitCommit :: String -> String -> IO ()
gitCommit commitMessage pathToCommit = do
        (exitCode,standardOut,standardErr) <- readProcessWithExitCode "/usr/bin/git" ["-C",pathToCommit,"commit", "-a", "-m",commitMessage] ""
        case exitCode of
          ExitSuccess -> do
                    _ <- begin.show $ "Commiting grade report successful"
                    return ()
          _ -> do
                _ <- begin.show $ standardOut
                _ <- begin.show $ standardErr
                return ()
                
gitAddGradeReport :: String -> IO ()
gitAddGradeReport repoFolder = do
        (exitCode,standardOut,standardErr) <- readProcessWithExitCode "/usr/bin/git" ["-C",repoFolder,"add", "--all"] ""
        case exitCode of
          ExitSuccess -> do
                    _ <- begin.show $ "Git Add successful for student's repo folder"
                    return ()
          _ -> do
                _ <- begin.show $ standardOut
                _ <- begin.show $ standardErr
                return ()
                
gitPushGradeReport :: String -> String -> IO ()
gitPushGradeReport gitUrl repoFolder = do
        (exitCode,standardOut,standardErr) <- readProcessWithExitCode "/usr/bin/git" ["-C",repoFolder,"push"] ""
        case exitCode of
          ExitSuccess -> do
                    _ <- begin.show $ "Pushing grade report to repo successful"
                    return ()
          _ -> do
                _ <- begin.show $ standardOut
                _ <- begin.show $ standardErr
                return ()
