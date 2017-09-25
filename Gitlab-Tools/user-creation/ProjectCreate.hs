{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module ProjectCreate where

import Data.Aeson
import Data.Aeson.Types
import qualified Network.HTTP.Client as C
import Network.HTTP.Simple    
import Data.Text as T
import Data.ByteString.Lazy.Char8 as CH

import User   
import Query

-- Gitlab Success Response: (project_id,project_http_url)
type SResp = (Integer,String)

getProjRsp :: Either String Object -> Either String SResp
getProjRsp (Right r) = flip parseEither r $ (\o -> do
  id <- o .: "id"
  url <- o .: "http_url_to_repo"
  return (id,url))
getProjRsp (Left e) = (Left e)

decodeRsp :: ByteString -> Either String (Either ErrorResp SResp)
decodeRsp b = decodeGLResp getProjRsp b

-- TODO: Need to do a namespace lookup first to get the namespace_id.
userToParams :: String -> User -> [(String,String)]
userToParams pt (User _ _ _ _ _ _ _ _ uid pn ns ie v) =
    [("private_token",pt),
     ("name",pn),
     --("namespace_id",ns),
     ("issues_enabled",ie),
     ("visibility",v)]

response :: User -> IO (Either String (Either ErrorResp SResp))
response user = request userToParams "/projects" user
                   >>= httpLBS
                   >>= (\r -> return $ ((decodeRsp $ C.responseBody r) :: Either String (Either ErrorResp SResp)))

responses :: (Either String (Either ErrorResp SResp) -> a) -> [User] -> IO [a]
responses f [] = return []
responses f (u:urs) = do
  r <- response u
  rs <- responses f urs
  return $ (f r) : rs
