{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedLabels #-}

module ParseSystemEventInfo (SystemEvent(..), parseJSON)where
    

import GHC.Generics
import System.IO
import Data.List
import Data.Aeson hiding (parseJSON)
import qualified Data.ByteString.Lazy as B

import Network.CGI 

data SystemEvent = SystemEvent {    event_name :: Object,
                                    created_at :: Object,
                                    update_at :: Object,
                                    name :: Object,
                                    path :: Object,
                                    path_with_namespace :: Object,
                                    project_id :: Object,
                                    owner_name :: Object,
                                    owner_email :: Object,
                                    project_visibility :: Object
                                 } deriving (GHC.Generics.Generic, Show)

instance FromJSON SystemEvent

parseJSON :: B.ByteString -> CGI SystemEvent

parseJSON json = case d of

    Left err -> error err

    Right u -> return u

 where

   d = eitherDecode json :: Either String SystemEvent