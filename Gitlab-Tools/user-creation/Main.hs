module Main where

import UserCreate
import UserParser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  process args
 where
   process [] = error "A file path must be given."
   process (path:_) = do
         r <- createUsers path
         foldr (\s r -> putStrLn s >> r) (putStr "") r