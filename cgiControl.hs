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
		     Nothing -> error "Error."
		     Just h -> do
			  if(h == "eNbbFFBqgBq5TSGdUtWr9gw4WXptmKbKQKp3P8bPAksYyKvx")
			       then do
                    	       	    inputs <- getBody
				    user <- parseJSON $ B.pack inputs
                    		    _ <- liftIO.begin.show $ map email (map author (commits user))
				    url <- liftIO.begin.show $ git_http_url (repository user)
				    createProcess $ shell (url++" /AHG")
				    output ""
			  else do
				_ <- liftIO.begin.show $ "You are not authenticated."
				output ""
			 
			
>>>>>>> 29e835c1adda3c9c66d960fb15c47d99c04e6757

main :: IO ()
main = runCGI (handleErrors cgiMain)