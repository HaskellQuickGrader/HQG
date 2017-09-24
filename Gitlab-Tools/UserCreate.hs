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

requestURL :: User -> IO String
requestURL user = url >>= return.exportURL
 where
   url :: IO URL
   url = do     
     pt <- privateToken
     gurl <- apiEndpoint
     let iURL = importURL gurl
     return $ case iURL of
                Just u -> u { url_params = userToParams pt user,
                              url_path = url_path u ++ "/users"
                            }
                _ -> error "GITLAB_API_ENDPOINT not set"

request :: User -> IO Request
request user = do
   url <- requestURL user
   iRep <- parseRequest url
   return (iRep {C.method = renderMethod (Right POST)})
          
response :: User -> IO (Maybe Resp)
response user = request user
                   >>= httpLBS
                   >>= (\r -> return $ ((decode $ C.responseBody r) :: Maybe Resp))

responses :: (Maybe Resp -> a) -> [User] -> IO [a]
responses f [] = return []
responses f (u:urs) = do
  r <- response u
  rs <- responses f urs
  return $ (f r) : rs

respToStr :: Maybe Resp -> String
respToStr Nothing = "Successfully Created User"
respToStr (Just (Resp m)) = m


