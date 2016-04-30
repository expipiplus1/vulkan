module Main
  ( main
  ) where

import           Parse.Spec
import           Spec.Pretty
import           Spec.StripExtensions
import           System.Exit
import           System.IO            (hPutStr, stderr)
import           Write.Spec

main :: IO ()
main = do specString <- readFile "Vulkan-Docs/src/spec/vk.xml"
          specMay <- parseSpec specString
          case specMay of
            Nothing -> do hPutStr stderr "Failed to parse spec"
                          exitFailure
            Just spec -> let strippedSpec = stripExtensions spec
                             prettySpec = prettifySpec strippedSpec
                         in writeSpecModules "out" prettySpec
