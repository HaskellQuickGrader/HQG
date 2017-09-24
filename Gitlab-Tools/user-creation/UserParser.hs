{-# 
LANGUAGE 
  NoMonomorphismRestriction, 
  PackageImports, 
  TemplateHaskell, 
  FlexibleContexts 
#-}
module UserParser where

import Data.Char
import Data.List.Split
import Text.Parsec hiding (Empty)
import Text.Parsec.Token    
import Text.Email.Validate
import qualified Data.ByteString.Char8 as B
import Crypto.Password

import UserCreate
    
parseUsername = do
  a <- lower
  r <- many alphaNum
  char '$'
  return $ a : r

parseName = do
  last <- upper >>= (\c -> many (lower <|> char '-') >>= return.(c:))
  char ','
  first <- upper >>= (\c -> many (lower <|> char '-') >>= return.(c:))
  char '$'
  return $ first ++ " " ++ last

validateName n = do
  case (parse parseName "" n) of
    Left _ -> Nothing
    Right n -> Just n

validateUserame u = do
  case (parse parseUsername "" u) of
    Left _ -> False
    Right _ -> True
         
validateEmail = isValid.B.pack

dropSpaces :: String -> String
dropSpaces = filter (not.isSpace)

checkLength l@(a:b:c:[]) = l
checkLength _ = error "Parse Error: Every line must contain: name, username, and email."

genPass :: IO String
genPass = generatePassword pass_features
 where
   pass_features = [Length 8, Include Uppercase, Include Lowercase, Include Digit, Include Symbol]
                
buildUsers :: [[String]] -> IO [User]
buildUsers [] = return []
buildUsers ([name,username,email]:ls)
    | (validateUserame (username++"$")) && (validateEmail (email++"$"))
    = case (validateName (name++"$")) of
          Nothing -> error "Parse Error: Every line must contain: name, username, and email."
          Just name' -> do rest <- buildUsers ls
                           return $ usr name' : rest
 where
   usr nm = User {
           email = email,
           reset_password = "true",
           username = username,
           name = nm,
           projects_limit = "0",
           admin = "false",
           can_create_group = "false",
           can_create_project = "false"
         }

createUsers file = do
  ls <- readFile file >>= return.lines
  let ls' = map (checkLength.(splitOn "|").dropSpaces) ls
  users <- buildUsers ls'
  responses respToStr users
