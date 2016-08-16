{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}
module ParseUserInfo where
    
--NB: run this in ghci first  :set -XDuplicateRecordFields

import GHC.Generics
import System.IO
import Data.List
import Data.List.Split
import Prelude hiding (catch)
import Data.Aeson

--import qualified Commit as C --import to avoid "Multiple declarations of 'message'" error since 'message' is found in Commit and User

data Commit = Commit {id :: String,
		      message :: String,
		      timestamp :: String,
		      url :: String,
		      author :: Author
		     } deriving GHC.Generics.Generic
	    
data Author = Author {	name :: String,
			email :: String,
			added :: [String],
			modified :: [String],
			removed :: [String]
		     } deriving GHC.Generics.Generic
        
data User = User {	object_kind :: String,
			event_name :: String,
			before :: String,
			after :: String,
			ref :: String,
			checkout_sha :: String,
			message :: String,
			user_id :: String,
			user_name :: String,
			user_email :: String,
			user_avatar :: String,
			project_id :: String,
			project :: Object,
			commits :: [Commit],
			total_commits_count :: String,
			repository :: Object
		 } deriving GHC.Generics.Generic

instance FromJSON Commit
instance FromJSON Author          
instance FromJSON User

-- parseJSON :: ByteString -> Maybe User
-- parseJSON jsonInfo = decode jsonInfo :: maybe User
  
 