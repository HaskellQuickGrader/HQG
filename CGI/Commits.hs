{-# LANGUAGE DuplicateRecordFields #-}
module Commits where

data Commit = Commit {	id :: String,
						message :: String,
						timestamp :: String,
						url :: String,
						author :: Author
						}deriving(Show)
						
data Author = Author {	name :: String,
						email :: String,
						added :: [String],
						modified :: [String],
						removed :: [String]
						}deriving(Show)
						
--instance FromJSON Commit
-- instance FromJSON Author

