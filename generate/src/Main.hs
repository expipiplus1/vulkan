module Main
  ( main
  ) where

import           Parse.Spec
import           System.Environment
-- import           Spec.StripExtensions
import           System.Exit
import           System.IO          (hPutStr, stderr)
import           Text.Show.Pretty
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
      -- putStrLn (ppShow spec)
      writeSpec outDir spec
      -- let strippedSpec = stripExtensions spec
      --            in writeSpecModules "out" strippedSpec
