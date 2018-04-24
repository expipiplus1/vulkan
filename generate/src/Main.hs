{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Data.Foldable
import           Data.List
import           Data.Text.Extra
import           Say
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO            (hPutStr, stderr)

import           Documentation.All
import           Parse.Spec
import           Spec.Savvy.Error
import           Spec.Savvy.Extension
import           Spec.Savvy.Spec
import           Write.Spec
import           Write.Wrapper

import qualified Data.Set             as Set
import           Spec.Savvy.Enum
import           Spec.Savvy.Handle

main :: IO ()
main = do
  [outDir, cabalPath, vkDir] <- getArgs
  let xmlPath = vkDir </> "xml" </> "vk.xml"
      manPath = vkDir </> "man"
  sayErr ("Reading spec from" <+> pack xmlPath)
  specString <- readFile xmlPath
  specMay    <- parseSpec specString
  case specMay of
    Nothing -> do
      hPutStr stderr "Failed to parse spec"
      exitFailure
    Just parsedSpec -> case spec parsedSpec of
      Left e -> do
        traverse_ (sayErr . prettySpecError) e
        exitFailure
      Right s -> do
        let isHandle = (`Set.member` Set.fromList (hName <$> sHandles s))
        let isBitmask =
              (`Set.member` Set.fromList
                [ n
                | Enum {..} <- sEnums s
                , n         <- eName : eAliases
                , eType == EnumTypeBitmask
                ]
              )
        let ss =
              sort (show . commandWrapper isHandle isBitmask <$> sCommands s)
        traverse_ (sayErrString . (++ "\n")) ss
        -- let allExtensionNames = extName <$> sExtensions s
        -- documentation <- loadAllDocumentation allExtensionNames vkDir manPath
        -- writeSpec documentation outDir cabalPath s
