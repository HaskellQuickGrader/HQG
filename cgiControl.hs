{-# OPTIONS_GHC -fno-warn-tabs #-}
import Network.CGI 
import System.Process
import Control.Monad.Trans.Class
import TransferData
import ParseUserInfo
import qualified Data.ByteString.Lazy.Char8 as B


cgiMain :: CGI CGIResult
cgiMain = do
        --get header and check for secret token authorization
        header <- requestHeader "X-Gitlab-Token"
        case header of
            Nothing -> error "Error no header."
            Just h -> do
            if(h == "eNbbFFBqgBq5TSGdUtWr9gw4WXptmKbKQKp3P8bPAksYyKvx")
                then do
                    inputs <- getBody
                    user <- parseJSON $ B.pack inputs
                    _ <- liftIO.begin.show $ map email (map author (commits user))
                    let url = git_http_url (repository user)
                    _ <- liftIO.begin.show $ "url: "++url
                    (_,outhdl,errhdl,_) <- liftIO.createProcess $ shell ("git clone "++url++" /ahg")
                    case errhdl of
                        Just err -> do
                            _ <- liftIO.begin $ "ERROR: "++(show err)
                            output ""
                        Nothing -> case outhdl of   
                                        Just message -> do
                                            _ <- liftIO.begin $ "cloning repo: "++(show url)
                                            output ""
                                        Nothing -> output ""
                    output ""
            else do
                _ <- liftIO.begin.show $ "You are not authenticated."
                output ""

main :: IO ()
main = runCGI (handleErrors cgiMain)