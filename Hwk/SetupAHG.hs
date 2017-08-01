import System.IO
import Data.List.Utils
import System.Environment

main = do
    args@(x:xs) <- getArgs
    let hwkNum = if(length args == 1)
                    then read x :: Int
                    else error "Only 1 command line argument allowed"
    readHandle <- openFile "AHG.hs" ReadMode
    writeHandle <- openFile "AHG_Modified.hs" WriteMode
    contents <- hGetContents readHandle
    --let f = replace "{{HwkNum}}" x
    -- hGetContents readHandle >>=(\contents -> interact f contents)
    let newLine = replace "{{HwkNum}}" x contents
    hPutStrLn writeHandle newLine
    hClose writeHandle
    hClose readHandle
    
-- main = getContents >>= putStr . replace "sourceString" "destinationString"