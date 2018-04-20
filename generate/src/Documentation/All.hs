{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Documentation.All
  ( loadAllDocumentation
  ) where

import           Control.Arrow                ((&&&))
import           Control.Monad                (unless)
import           Control.Monad.Except
import           Data.Either
import           Data.Foldable                hiding (find)
import qualified Data.Map                     as Map
import           Data.Text.Extra              (Text, (<+>))
import qualified Data.Text.Extra              as T
import           Say
import           System.FilePath
import           System.FilePath.Find         (extension, find, (==?))
import           System.ProgressBar

import           Documentation
import           Documentation.RunAsciiDoctor

-- | Creat a function which can be used to query for documentation
-- Might take a few seconds to run, as vulkan has lots of documentation.
loadAllDocumentation
  :: [Text]
  -- ^ List of extensions
  -> FilePath
  -- ^ Path to the 'Vulkan-Docs' directory
  -> FilePath
  -- ^ Directory where the documentation ".txt" (asciidoc) files are located
  -> IO (Documentee -> Maybe Documentation)
loadAllDocumentation extensions vkDocs manDir = do
  let dontRecurse = pure True
      notDocs     = ["apispec.txt", "copyright-ccby.txt", "footer.txt"]
  allDocs <-
    filter ((`notElem` notDocs) . takeFileName)
      <$> find dontRecurse (extension ==? ".txt") manDir
  let numDocumentationThreads :: Int
      numDocumentationThreads = 16
  sayErr
    $   "Loading Documentation with"
    <+> T.tShow numDocumentationThreads
    <+> "threads"
  (errors, documentations) <-
    partitionEithers
      <$> withProgress numDocumentationThreads
                       (runExceptT . loadDocumentation extensions vkDocs)
                       allDocs
  unless (null errors) $ do
    sayErr "Errors while loading documentation:"
    traverse_ sayErr errors
  let allDocumentations = concat documentations
      docMap = Map.fromList ((dDocumentee &&& id) <$> allDocumentations)
  -- traverse_ sayShow (Map.assocs docMap)
  pure (`Map.lookup` docMap)

loadDocumentation
  :: [Text]
  -- ^ Extension names
  -> FilePath
  -- ^ Path to the 'Vulkan-Docs' directory
  -> FilePath
  -- ^ The asciidoc .txt file to load
  -> ExceptT Text IO [Documentation]
loadDocumentation extensions vkDocs doc = do
  docbook <- ExceptT $ manTxtToDocbook extensions vkDocs doc
  withExceptT (("Error while parsing documentation for" <+> T.tShow doc) <+>)
    . ExceptT
    . pure
    $ docBookToDocumentation docbook
