module Documentation.All
  ( loadAllDocumentation
  ) where

import           Control.Concurrent.Async.Pool
import           Control.Monad.Except
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.Text.Extra               as T
import           Data.Text.Extra                ( (<+>) )
import           GHC.Conc                       ( numCapabilities )
import           Relude
import           Say
import           System.Console.AsciiProgress   ( Options(..)
                                                , def
                                                , displayConsoleRegions
                                                , newProgressBar
                                                , tick
                                                )
import           System.Directory
import           System.FilePath

import           Documentation
import           Documentation.RunAsciiDoctor
import           Spec.Flavor

-- | Creat a function which can be used to query for documentation
-- Might take a few seconds to run, as vulkan has lots of documentation.
loadAllDocumentation
  :: SpecFlavor
  -> [Text]
  -- ^ List of extensions
  -> FilePath
  -- ^ Path to the 'Vulkan-Docs' directory
  -> FilePath
  -- ^ Directory where the documentation ".txt" or ".adoc" (asciidoc) files are
  -- located
  -> IO (Documentee -> Maybe Documentation)
loadAllDocumentation specFlavor extensions vkDocs manDir = do
  let notDocs = ["apispec.txt", "copyright-ccby.txt", "footer.txt"]
  allDocs <-
    filter ((`notElem` notDocs) . takeFileName)
    .   fmap (manDir </>)
    .   filter ((".txt" `List.isSuffixOf`) <||> (".adoc" `List.isSuffixOf`))
    <$> listDirectory manDir
  let numDocumentationThreads :: Int
      numDocumentationThreads = numCapabilities
  sayErr
    $   "Loading Documentation with"
    <+> show numDocumentationThreads
    <+> "threads"
  (errors, documentations) <-
    partitionEithers
      <$> withProgress
            numDocumentationThreads
            (runExceptT . loadDocumentation specFlavor extensions vkDocs)
            allDocs
  unless (null errors) $ do
    sayErr "Errors while loading documentation:"
    traverse_ sayErr errors
  let allDocumentations = concat documentations
      docMap = Map.fromList ((dDocumentee &&& id) <$> allDocumentations)
  pure (`Map.lookup` docMap)

loadDocumentation
  :: SpecFlavor
  -> [Text]
  -- ^ Extension names
  -> FilePath
  -- ^ Path to the 'Vulkan-Docs' directory
  -> FilePath
  -- ^ The asciidoc .txt file to load
  -> ExceptT Text IO [Documentation]
loadDocumentation specFlavor extensions vkDocs doc = do
  docbook <- ExceptT $ manTxtToDocbook specFlavor extensions vkDocs doc
  let name = takeBaseName doc
  withExceptT (("Error while parsing documentation for" <+> show doc) <+>)
    . ExceptT
    . pure
    $ docBookToDocumentation specFlavor docbook (T.pack name)

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

(<||>) :: ([Char] -> Bool) -> ([Char] -> Bool) -> [Char] -> Bool
(<||>) = liftA2 (||)
