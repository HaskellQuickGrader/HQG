{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedLabels #-}

module ParseSystemEventInfo (SystemEvent(..), parseJSON)where
    

import GHC.Generics
import System.IO
import Data.List
import Data.Aeson hiding (parseJSON)
import qualified Data.ByteString.Lazy as B

import Network.CGI 

data SystemEvent = SystemEvent {    event_name :: String,
                                    created_at :: String,
                                    update_at :: String,
                                    project_name :: String,
                                    project_path :: String,
                                    project_path_with_namespace :: String,
                                    project_id :: Int,
                                    user_username :: String,
                                    user_name :: String,
                                    user_email :: String,
                                    user_id :: Int,
                                    access_level :: String,
                                    project_visibility :: String
                                 } deriving GHC.Generics.Generic

instance FromJSON SystemEvent

parseJSON :: B.ByteString -> CGI SystemEvent

parseJSON json = case d of

    Left err -> error err

    Right u -> return u

 where

   d = eitherDecode json :: Either String SystemEvent