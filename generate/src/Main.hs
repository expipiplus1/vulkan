module Main
  ( main
  ) where

import           Parse.Spec
import           Spec.StripExtensions
import           System.Exit
import           System.IO            (hPutStr, stderr)
import           Write.Spec

main :: IO ()
main = do specString <- getContents
          specMay <- parseSpec specString
          case specMay of
            Nothing -> do hPutStr stderr "Failed to parse spec"
                          exitFailure
            Just spec -> let strippedSpec = stripWSIExtensions spec
                         in writeSpecModules "out" strippedSpec

