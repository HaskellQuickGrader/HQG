import Network.CGI
import System.IO
import SystemHook
import Query
import qualified Data.ByteString.Lazy.Char8 as CH
import System.Directory
import System.Process

secretKey :: String
secretKey = "GUC#Wwdk6x!5dtrrnb#8W$p%$wMgMd7xCvr$CNHmy#D%Vf&Ux6"

getBranchName :: String -> Int -> String
getBranchName [] _ = []
getBranchName (x:xs) slashCount | x == '/' = if(slashCount == 1)
                                                then xs
                                                else getBranchName xs (slashCount + 1)
                                | otherwise = getBranchName xs slashCount

--Checks to see if system hook has a header associated with it
processHeader :: Maybe String -> CGI CGIResult
processHeader Nothing = error "Error: no event header"
processHeader (Just "System Hook") = getBody >>= processSystemHook                                

processSystemHook :: String -> CGI CGIResult
processSystemHook inputs = do
  liftIO.logStr $ inputs
  let d = sysHookDecoder $ CH.pack inputs
  liftIO.logStr.show $ d          
  output ""

getHwkNumber :: String -> Int
getHwkNumber branch = 
    let hwkNum = getNum
        where
            getNum :: String -> String
            getNum branch =  

cgiMain :: CGI CGIResult
cgiMain = undefined 