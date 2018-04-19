{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Text.Extra
import           Say
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO          (hPutStr, stderr)

import           Documentation.All
import           Parse.Spec
import           Write.Spec

main :: IO ()
main = do
  [outDir, vkDir]      <- getArgs
  let xmlPath = vkDir </> "xml" </> "vk.xml"
      manPath = vkDir </> "man"
  sayErr ("Reading spec from" <+> pack xmlPath)
  specString <- readFile xmlPath
  specMay    <- parseSpec specString
  documentation <- loadAllDocumentation vkDir manPath
  case specMay of
    Nothing -> do
      hPutStr stderr "Failed to parse spec"
      exitFailure
    Just spec ->
      writeSpec documentation outDir spec
