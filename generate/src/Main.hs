module Main 
  ( main
  ) where

import Parse.Spec
import Write.Spec
import System.IO(hPutStr, stderr)
import System.Exit

main :: IO ()
main = do specString <- getContents
          specMay <- parseSpec specString
          case specMay of 
            Nothing -> do hPutStr stderr "Failed to parse spec"
                          exitFailure
            Just spec -> putStrLn (haskellize spec)

