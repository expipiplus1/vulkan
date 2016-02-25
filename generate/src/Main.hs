module Main 
  ( main
  ) where

import Spec.StripExtensions
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
            Just spec -> let strippedSpec = stripWSIExtensions spec
                         in writeSpecModules "out" strippedSpec

