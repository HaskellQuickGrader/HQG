{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Query where

import Network.URL
import Network.HTTP.Simple
import qualified Network.HTTP.Client as C
import Network.HTTP.Types.Method    
import System.Environment
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Text as T
import Data.ByteString.Lazy.Char8 as CH hiding (putStrLn)

privateTokenVar :: String
privateTokenVar = "GITLAB_API_PRIVATE_TOKEN"

apiEndpointVar :: String
apiEndpointVar = "GITLAB_API_ENDPOINT"

privateToken :: IO String
privateToken = getEnv privateTokenVar

apiEndpoint :: IO String
apiEndpoint = getEnv apiEndpointVar

requestURL :: (String -> a -> [(String,String)]) -- Private Key -> Add Data -> URL Parameters
           -> String                             -- Gitlab query string
           -> URL                                -- API URL
           -> String                             -- Private key
           -> a                                  -- Data to toP (first arg)
           -> String                             -- The full query URL
requestURL toP q u pt x = exportURL url
 where
   url :: URL
   url = u { url_params = toP pt x,
             url_path = url_path u ++ q }           

request :: (String -> a -> [(String,String)]) -- Private Key -> Add Data -> URL Parameters          
        -> String                             -- Gitlab query string
        -> a                                  -- Data to toP (first arg)
        -> IO Request                         -- The Gitlab request used with httpLBS
request toP q x = do
   pt <- privateToken
   gurl <- apiEndpoint  
   case (importURL gurl) of
     Just u -> do let url = requestURL toP q u pt x
                  putStrLn.show $ url
                  iRep <- parseRequest url
                  return (iRep {C.method = renderMethod (Right POST)})
     Nothing -> error "GITLAB_API_ENDPOINT not set correctly"       

-- Gitlab Message Response
data MResp = MResp {
  message :: String
} deriving (Generic,Show)
           
-- Gitlab Error Response
data EResp = EResp {
  err :: String
} deriving (Generic,Show)

type ErrorResp = Either EResp MResp

decodeMResp :: Either String Object -> Either String MResp
decodeMResp (Right r) = flip parseEither r $ (\o -> do
  m <- o .: "message"
  return (MResp m))
decodeMResp (Left e) = (Left e)

decodeEResp :: Either String Object -> Either String EResp
decodeEResp (Right r) = flip parseEither r $ (\o -> do
  e <- o .: "error"
  return (EResp e))
decodeEResp (Left e) = (Left e)
           
decodeGLResp :: (Either String Object -> Either String a) -- Decoder for a particular respose type
             -> ByteString                                -- JSON that needs decoding
             -> Either String (Either ErrorResp a)        -- Either an error message or the parsed JSON.
decodeGLResp aDecode s = case (aDecode eo) of
                           Right aR -> Right (Right aR)
                           Left e1 -> case (decodeMResp eo) of
                                        Right mR -> Right (Left (Right mR))
                                        Left e2 -> case (decodeEResp eo) of
                                                     Right eR -> Right (Left (Left eR))
                                                     Left e3 -> Left (e1 ++ "\n" ++ e2 ++ "\n" ++ e3)
 where
   eo = eitherDecode s :: Either String Object


defaultResp :: Either String (Either ErrorResp a) -> Either String a
defaultResp (Right (Right r)) = Right r
defaultResp (Right (Left (Right (MResp m)))) = Left m
defaultResp (Right (Left (Left (EResp e)))) = Left e
defaultResp (Left e) = Left e
