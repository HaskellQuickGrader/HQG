{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module UserCreate where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import System.Environment
import Network.URL
import Network.HTTP.Simple
import qualified Network.HTTP.Client as C
import Network.HTTP.Types.Method    

privateTokenVar :: String
privateTokenVar = "GITLAB_API_PRIVATE_TOKEN"

apiEndpointVar :: String
apiEndpointVar = "GITLAB_API_ENDPOINT"

privateToken :: IO String
privateToken = getEnv privateTokenVar

apiEndpoint :: IO String
apiEndpoint = getEnv apiEndpointVar

data User = User {
      email :: String,
      reset_password :: String,
      username :: String,
      name :: String,
      projects_limit :: String,
      admin :: String,
      can_create_group :: String,
      can_create_project :: String
} deriving (Generic, Show)

data Resp = Resp {
  message :: String
} deriving (Generic,Show)
           
instance FromJSON Resp

userToParams :: String -> User -> [(String,String)]
userToParams pt (User e rp u n pl a cg cp) =
    [("private_token",pt),
     ("email",e),
     ("reset_password",rp),
     ("username",u),
     ("name",n),
     ("projects_limit",pl),
     ("admin",a),
     ("can_create_group",cg),
     ("can_create_project",cp)]

requestURL :: URL -> String -> User -> String
requestURL u pt user = exportURL url
 where
   url :: URL
   url = u { url_params = userToParams pt user,
             url_path = url_path u ++ "/users" }           

request :: User -> IO Request
request user = do
   pt <- privateToken
   gurl <- apiEndpoint  
   case (importURL gurl) of
     Just u -> do let url = requestURL u pt user
                  iRep <- parseRequest url
                  return (iRep {C.method = renderMethod (Right POST)})
     Nothing -> error "GITLAB_API_ENDPOINT not set correctly"
          
response :: User -> IO (Maybe Resp)
response user = request user
                   >>= httpLBS
                   >>= (\r -> return $ ((decode $ C.responseBody r) :: Maybe Resp))

responses :: (String -> Maybe Resp -> a) -> [User] -> IO [a]
responses f [] = return []
responses f (u:urs) = do
  r <- response u
  rs <- responses f urs
  return $ (f (name u) r) : rs

respToStr :: String -> Maybe Resp -> String
respToStr n Nothing = n++": Successfully Created User"
respToStr _ (Just (Resp m)) = m


