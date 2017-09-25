module Main where

import System.Environment
import Data.List.Split

import User
import Query
import UserCreate
import UserParser

createUsers :: FilePath -> IO (String, [User])
createUsers file = do
         ls <- readFile file >>= return.lines
         let ls' = map ((splitOn "|").dropSpaces) ls
         users <- buildUsers ls'
         rsp <- responses defaultResp users
         let rsp' = processRsp users rsp
         return rsp'
 where
   -- Collects all the Gitlab error messages, and updates each
   -- succefully created user with their uid:
   processRsp :: [User] -> [Either String SResp] -> (String, [User])
   processRsp _ [] = ("",[])
   processRsp users ((Right (um,uid)):rest) =      
       case musers of
         Just u -> (e, (updateUserID u uid):us)
         -- Should never trigger:
         Nothing -> error "Somthing went wrong in processRsp."
    where
      musers = findUser users um
      (e,us) = processRsp users rest
   processRsp users ((Left e):rest) = (e++"\n"++e',us)
    where
      (e',us) = processRsp users rest

main :: IO ()
main = getArgs >>= process
 where
    process [] = error "A file path must be given."
    process (path:_) = do
          (e, users) <- createUsers path
          putStrLn e   
