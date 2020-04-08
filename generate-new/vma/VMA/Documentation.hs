{-# language RecursiveDo #-}
module VMA.Documentation
  where

import           Relude
import qualified Data.Map                      as Map
import qualified Data.Text.Extra               as T
import qualified Data.Text.IO                  as T
import           Say
import           System.FilePath
import           System.Directory
import qualified Data.List                     as List
import           Data.Traversable
import           Polysemy
import           Text.Pandoc

import           Documentation           hiding ( docBookToDocumentation
                                                , splitDocumentation
                                                )
import           Error
import           Spec.Name

loadAllDocumentation
  :: (HasErr r, Member (Embed IO) r)
  => FilePath
  -- ^ Path to the @VulkanMemoryAllocator/docs/docbook@ directory
  -> Sem r (Documentee -> Maybe Documentation)
loadAllDocumentation docbookDir = do
  allDocs <-
    liftIO
    $   filter (("struct" `isPrefixOf`) . takeFileName)
    .   fmap (docbookDir </>)
    .   filter (".xml" `List.isSuffixOf`)
    <$> listDirectory docbookDir
  (errors, docs) <- fmap partitionEithers . for allDocs $ \d ->
    runErr (loadDocumentation d)
  unless (null errors) $ do
    sayErr "Errors while loading documentation:"
    traverse_ (traverse_ sayErr) errors
  let allDocumentations = concat docs
      docMap = Map.fromList ((dDocumentee &&& id) <$> allDocumentations)
  pure (`Map.lookup` docMap)

loadDocumentation
  :: (Member (Embed IO) r, HasErr r) => FilePath -> Sem r [Documentation]
loadDocumentation f = do
  let isValid = \case
        TopLevel (CName n) -> "vma" `T.isPrefixOf` T.toLower n
        Nested p _         -> isValid (TopLevel p)
  txt <- liftIO $ T.readFile f
  fromEither (docBookToDocumentation isValid txt)

docBookToDocumentation
  :: (Documentee -> Bool)
  -- ^ Is a valid documentee name
  -> Text
  -- ^ The docbook string
  -> Either Text [Documentation]
docBookToDocumentation isValid db = mdo
  let readerOptions = def
  pandoc             <- first show $ runPure (readDocBook readerOptions db)
  (removed, subDocs) <- splitDocumentation name pandoc
  name               <- guessDocumentee isValid removed
  pure $ Documentation (TopLevel name) removed : subDocs

-- | If the description is a bullet list of "enames" then remove those from the
-- original documentation and return them separately.
--
-- Return the original documentation with the new document sections removed
splitDocumentation :: CName -> Pandoc -> Either Text (Pandoc, [Documentation])
splitDocumentation parent p@(Pandoc meta bs) =
  let
    replaceHeader = \case
      Header _ _ [Str s, Space, Str "Struct", Space, Str "Reference"] : Plain [Str s'] : t
        | s == s'
        -> Para [Str s] : t
      bs -> bs

    removeIncludeDirective = bottomUp $ \case
      Para [Code _ "#include <vk_mem_alloc.h>"] : t -> t
      t -> t

    bs' = removeIncludeDirective . replaceHeader $ bs
  in
    Right (Pandoc meta bs', [])
