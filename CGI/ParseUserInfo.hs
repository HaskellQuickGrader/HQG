{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedLabels #-}

module ParseUserInfo (Commit(..),Author(..),Repo(..),User(..), Project(..), parseJSON)where
    

import GHC.Generics
import System.IO
import Data.List
import Data.Aeson hiding (parseJSON)
import qualified Data.ByteString.Lazy as B

import Network.CGI 


data Commit = Commit {  id :: String,
                        message :: String,
                        timestamp :: String,
                        url :: String,
                        author :: Author,
                        added :: [String],
                        modified :: [String],
                        removed :: [String]
                     } deriving (GHC.Generics.Generic, Show)
             
data Author = Author {  name :: String,
                        email :: String
                     } deriving (GHC.Generics.Generic, Show)
             
data Repo = Repo {  name :: String,
                    url :: String,
                    description :: String,
                    homepage :: String,
                    git_http_url :: String,
                    git_ssh_url :: String,
                    visibility_level :: Int            
                 } deriving (GHC.Generics.Generic, Show)
                 
data Project = Project  {   name :: String,
                            description :: String,
                            web_url :: String,
                            -- avatar_url :: Object, -- Cannot parse this as it is always null
                            git_ssh_url :: String,
                            git_http_url :: String,
                            namespace :: String,
                            visibility_level :: Int,
                            path_with_namespace :: String,
                            default_branch :: String,
                            homepage :: String,
                            url :: String,
                            ssh_url :: String,
                            http_url :: String
                        } deriving (GHC.Generics.Generic, Show)
        
data User = User { object_kind :: String,
                    event_name :: String,
                    before :: String,
                    after :: String,
                    ref :: String,
                    checkout_sha :: String,
                    --message :: Object,
                    user_id :: Int,
                    user_name :: String,
                    user_email :: String,
                    user_avatar :: String,
                    project_id :: Int,
                    project :: Project,
                    commits :: [Commit],
                    total_commits_count :: Int,
                    repository :: Repo
                 } deriving (GHC.Generics.Generic, Show)

instance FromJSON Commit
instance FromJSON Author          
instance FromJSON Repo
instance FromJSON Project
instance FromJSON User



parseJSON :: B.ByteString -> CGI User

parseJSON json = case d of

    Left err -> error err

    Right u -> return u

 where

   d = eitherDecode json :: Either String User