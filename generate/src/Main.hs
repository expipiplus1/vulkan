module Main
  ( main
  ) where

import           Parse.Spec
import           System.Environment
import           System.Exit
import           System.IO          (hPutStr, stderr)
import           Write.Spec

main :: IO ()
main = do
  [outDir, xml]      <- getArgs
  specString <- readFile xml
  specMay    <- parseSpec specString
  case specMay of
    Nothing -> do
      hPutStr stderr "Failed to parse spec"
      exitFailure
    Just spec ->
      writeSpec outDir spec
