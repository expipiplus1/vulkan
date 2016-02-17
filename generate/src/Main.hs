module Main 
  ( main
  ) where

import Parse

main :: IO ()
main = do specString <- getContents
          spec <- parseSpec specString
          print spec

