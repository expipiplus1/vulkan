module Documentation.All
  ( loadAllDocumentation
  ) where

import           Relude
import           Control.Monad.Except
import qualified Data.Map                      as Map
import           Data.Text.Extra                ( (<+>) )
import           Say
import           System.FilePath
import           System.Directory
import qualified Data.List                     as List
import           Control.Concurrent.Async.Pool
import           System.Console.AsciiProgress   ( Options(..)
                                                , displayConsoleRegions
                                                , def
                                                , newProgressBar
                                                , tick
                                                )
import           GHC.Conc                       ( numCapabilities )

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
  let notDocs = ["apispec.txt", "copyright-ccby.txt", "footer.txt"]
  allDocs <-
    filter ((`notElem` notDocs) . takeFileName)
    .   fmap (manDir </>)
    .   filter (".txt" `List.isSuffixOf`)
    <$> listDirectory manDir
  let numDocumentationThreads :: Int
      numDocumentationThreads = numCapabilities
  sayErr
    $   "Loading Documentation with"
    <+> show numDocumentationThreads
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
  withExceptT (("Error while parsing documentation for" <+> show doc) <+>)
    . ExceptT
    . pure
    $ docBookToDocumentation docbook

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- | Traverse concurrently with several threads while showing a progress bar
withProgress
  :: Traversable t
  => Int
  -- ^ Number of threads to run
  -> (a -> IO b)
  -> t a
  -> IO (t b)
withProgress numThreads f t = displayConsoleRegions $ do
  let numElements = length t
  pg <- newProgressBar def { pgTotal = fromIntegral numElements }
  withTaskGroup numThreads $ \g -> mapTasks g ((\x -> f x <* tick pg) <$> t)
