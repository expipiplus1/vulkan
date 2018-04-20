{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Text.Extra
import Data.Foldable
import           Say
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO          (hPutStr, stderr)

import           Documentation.All
import           Parse.Spec
import           Write.Spec
import           Spec.Savvy.Spec
import           Spec.Savvy.Error
import           Spec.Savvy.Extension

main :: IO ()
main = do
  [outDir, vkDir]      <- getArgs
  let xmlPath = vkDir </> "xml" </> "vk.xml"
      manPath = vkDir </> "man"
  sayErr ("Reading spec from" <+> pack xmlPath)
  specString <- readFile xmlPath
  specMay    <- parseSpec specString
  case specMay of
    Nothing -> do
      hPutStr stderr "Failed to parse spec"
      exitFailure
    Just parsedSpec ->
      case spec parsedSpec of
        Left e -> do
          traverse_ (sayErr . prettySpecError) e
          exitFailure
        Right s -> do
          let allExtensionNames = extName <$> sExtensions s
          documentation <- loadAllDocumentation allExtensionNames vkDir manPath
          writeSpec documentation outDir s
