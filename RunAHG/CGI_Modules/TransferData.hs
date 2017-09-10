module CGI_Modules.TransferData where

import System.IO

import Data.Char(toUpper)



begin :: String -> IO ()

begin inpStr = do
       outh <- openFile "output.txt" AppendMode
       hPutStrLn outh inpStr
       hClose outh
