import System.IO
import Data.List.Utils
import System.Environment
import System.Directory
import System.Process
import Control.Monad
import System.Exit

main = do
    args@(x:xs) <- getArgs
    -- let hwkNum = if(length args == 1)
                    -- then read x :: Int
                    -- else error "Only 1 command line argument allowed"
    let ahgHwk = "AHG_Hwk"++x++".hs"
    let ahgExe = "AHG_Hwk"++x++".exe"
    exists <- doesFileExist ahgExe
    if(exists)
        then runExe ahgExe x
        else do
              makeAHG x ahgHwk
              makeExe ahgHwk
              runExe ahgExe x
    
makeAHG :: String -> String -> IO ()
makeAHG hwkNum  ahgHwk = do
    readHandle <- openFile "AHG.hs" ReadMode
    writeHandle <- openFile ahgHwk WriteMode
    contents <- hGetContents readHandle
    --let f = replace "{{HwkNum}}" x
    -- hGetContents readHandle >>=(\contents -> interact f contents)
    let newLine = replace "{{HwkNum}}" hwkNum contents
    hPutStrLn writeHandle newLine
    hClose writeHandle
    hClose readHandle

runExe :: String -> String -> IO ()
runExe ahgExe arg = do 
                (exitCode,stdOut,stdErr) <- readProcessWithExitCode ("C:\\Development\\AHG\\Hwk\\"++ahgExe) [arg] ""
                case exitCode of
                    ExitSuccess -> return ()
                    _ -> error $ show stdOut
                    
makeExe :: String -> IO ()
makeExe file = do 
                (exitCode,stdOut,stdErr) <- readProcessWithExitCode ("ghc") ["--make",file] ""
                case exitCode of
                    ExitSuccess -> return ()
                    _ -> error $ show stdOut
    
-- main = getContents >>= putStr . replace "sourceString" "destinationString"