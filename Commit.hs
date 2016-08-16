{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}
module Commit where

import GHC.Generics    
import Data.Aeson
    
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
						
instance FromJSON Commit
instance FromJSON Author

