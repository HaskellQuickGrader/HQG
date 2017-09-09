module GradeHomework.GHwkTransferData where

import System.IO

import Data.Char(toUpper)

-- This is a duplicate file of TransferData, this is on purpose
-- SetupAHG.hs requres "GradeHomework" prepended on the name because of where it is called from
-- AHGTemplate.hs can't see the file with that same prefix so it is stripped off
-- otherwise the two files are the same


begin :: String -> IO ()

begin inpStr = do
       outh <- openFile "output.txt" AppendMode
       hPutStrLn outh inpStr
       hClose outh
