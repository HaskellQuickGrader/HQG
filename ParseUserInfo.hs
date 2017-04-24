{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module ParseUserInfo where
    

import GHC.Generics
import System.IO
import Data.List
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Network.CGI 


data Commit = Commit {id :: String,
		      message :: String,
		      timestamp :: String,
		      url :: String,
		      author :: Author,
		      added :: [String],
		      modified :: [String],
		      removed :: [String]
		     } deriving GHC.Generics.Generic
	    
data Author = Author {	name :: String,
			email :: String
		     } deriving GHC.Generics.Generic
             
data Repo = Repo { name :: String,
            url :: String,
            description :: String,
            homepage :: String,
            git_http_url :: String,
            git_ssh_url :: String,
            visibility_level :: Int            
            } deriving GHC.Generics.Generic
        
data User = User {	object_kind :: String,
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
			project :: Object,
			commits :: [Commit],
			total_commits_count :: Int,
			repository :: Repo
		 } deriving GHC.Generics.Generic

instance FromJSON Commit
instance FromJSON Author          
instance FromJSON Repo
instance FromJSON User



parseJSON :: B.ByteString -> CGI User

parseJSON json = case d of

	  Left err -> error err

	  Right u -> return u

 where

   d = eitherDecode json :: Either String User
   
  
 