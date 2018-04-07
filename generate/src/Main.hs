module Main
  ( main
  ) where

import           Parse.Spec
-- import           Spec.StripExtensions
import           System.Exit
import           System.IO        (hPutStr, stderr)
import           Text.Show.Pretty
-- import           Write.Spec

main :: IO ()
main = do specString <- getContents
          specMay <- parseSpec specString
          case specMay of
            Nothing -> do hPutStr stderr "Failed to parse spec"
                          exitFailure
            Just spec ->
              putStrLn (ppShow spec)
              -- let strippedSpec = stripExtensions spec
              --            in writeSpecModules "out" strippedSpec

