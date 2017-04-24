module TransferData where

import System.IO

import Data.Char(toUpper)



begin :: String -> IO ()

begin inpStr = do
       outh <- openFile "output.txt" WriteMode
       hPutStrLn outh inpStr
       hClose outh
