import System.IO
import Data.List.Utils
import System.Environment
import System.Directory
import System.Process
import Control.Monad
import System.Exit

-- command line arguments must follow this order:
-- First argument is the Homework Number
-- Second argument is the path to the student's homework repo folder, this is used
-- for copying the grade report into that folder, which is then added to their repo for them to see
main = do
    args@(x:y:xs) <- getArgs
    let ahgHwk = "AHG_Hwk"++x++".hs"
    let ahgHwkExe = "AHG_Hwk"++x++".exe"
    exists <- doesFileExist ahgHwkExe
    if(exists)
        then runExe ahgHwkExe y
        else do
              makeAHG x ahgHwk
              makeExe ahgHwk
              runExe ahgHwkExe y
    
makeAHG :: String -> String -> IO ()
makeAHG hwkNum  ahgHwk = do
    readHandle <- openFile "AHGTemplate.hs" ReadMode
    writeHandle <- openFile ahgHwk WriteMode
    contents <- hGetContents readHandle
    let newLine = replace "{{HwkNum}}" hwkNum contents
    hPutStrLn writeHandle newLine
    hClose writeHandle
    hClose readHandle

runExe :: String -> String -> IO ()
runExe ahgHwkExe repoDir = do 
                currentDir <- getCurrentDirectory
                (exitCode,stdOut,stdErr) <- readProcessWithExitCode (currentDir++"\\"++ahgHwkExe) [repoDir] ""
                case exitCode of
                    ExitSuccess -> return ()
                    _ -> do
                            print stdOut
                            print stdErr
                    
makeExe :: String -> IO ()
makeExe file = do 
                (exitCode,stdOut,stdErr) <- readProcessWithExitCode ("ghc") ["--make",file] ""
                case exitCode of
                    ExitSuccess -> return ()
                    _ -> do
                        print stdOut
                        print stdErr