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
				Nothing -> error "You are not authenticated."
				Just h -> do 
							if(h == "eNbbFFBqgBq5TSGdUtWr9gw4WXptmKbKQKp3P8bPAksYyKvx")
								then do
									inputs <- getBody
									user <- parseJSON $ B.pack inputs
									_ <- liftIO.begin.show $ map name (map author (commits user))
									output ""
								else error "You are not authenticated."
			 
			

main :: IO ()
main = runCGI (handleErrors cgiMain)