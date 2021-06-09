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
import           Polysemy
import           Text.Pandoc
import           Safe                           ( lastMay )

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
  (errors, docs) <- partitionEithers <$> sequence
    ( runErr (loadHeaderDocumentation (docbookDir </> "vk__mem__alloc_8h.xml"))
    : (allDocs <&> \d -> runErr (loadDocumentation d))
    )
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
        Chapter _          -> True
  txt <- liftIO $ T.readFile f
  fromEither (docBookStructToDocumentation isValid txt)

loadHeaderDocumentation
  :: (Member (Embed IO) r, HasErr r) => FilePath -> Sem r [Documentation]
loadHeaderDocumentation f = do
  let isValid = \case
        TopLevel (CName n) -> "vma" `T.isPrefixOf` T.toLower n
        Nested p _         -> isValid (TopLevel p)
        Chapter _          -> True
  txt <- liftIO $ T.readFile f
  fromEither (docBookHeaderToDocumentation isValid txt)

docBookHeaderToDocumentation
  :: (Documentee -> Bool)
  -- ^ Is a valid documentee name
  -> Text
  -- ^ The docbook string
  -> Either Text [Documentation]
docBookHeaderToDocumentation isValid db = do
  let readerOptions = def
  pandoc <- first show $ runPure (readDocBook readerOptions db)
  splitHeaderDocumentation isValid pandoc

docBookStructToDocumentation
  :: (Documentee -> Bool)
  -- ^ Is a valid documentee name
  -> Text
  -- ^ The docbook string
  -> Either Text [Documentation]
docBookStructToDocumentation isValid db = mdo
  let readerOptions = def
  pandoc             <- first show $ runPure (readDocBook readerOptions db)
  (removed, subDocs) <- splitStructDocumentation name pandoc
  name               <- guessDocumentee isValid removed
  pure $ Documentation (TopLevel name) removed : subDocs

-- | If the description is a bullet list of "enames" then remove those from the
-- original documentation and return them separately.
--
-- Return the original documentation with the new document sections removed
splitStructDocumentation
  :: CName -> Pandoc -> Either Text (Pandoc, [Documentation])
splitStructDocumentation parent (Pandoc meta bs) =
  let
    replaceHeader = \case
      Header _ _ [Str s, Space, Str "Struct", Space, Str "Reference"] : Plain [Str s'] : t
        | s == s'
        -> Para [Str s] : t
      bs -> bs

    -- Doxygen+Pandoc results in some uninteresting type+member name garbage
    removeUninteresting = bottomUp $ \case
      -- documentation provenance
      Para [Str "The", Space, Str "documentation", Space, Str "for", Space, Str "this", Space, Str "struct", Space, Str "was", Space, Str "generated", Space, Str "from", Space, Str "the", Space, Str "following", Space, Str "file:"] : xs
        -> case xs of
          Plain [Str "include/"] : Plain [Str "vk_mem_alloc.h"] : ys -> ys
          Plain [Str "vk_mem_alloc.h"] : ys -> ys
          _ -> xs
      -- C stugg
      Para [Code _ "#include <vk_mem_alloc.h>"]             : t  -> t
      -- Boring headers
      Header 2 _ [Str "Detailed", Space, Str "Description"] : xs -> xs
      Header 2 _ [Str "Public"  , Space, Str "Attributes" ] : xs -> xs
      Header 2 _ [Str "Member", Space, Str "Data", Space, Str "Documentation"] : xs
        -> xs
      -- member list
      BulletList bullets : xs | all isMemberBullet bullets -> xs
      xs -> xs

    isMemberBullet = \case
      [Para []] -> False
      [Para ws] | Link ("", [], []) [Str _memberName] (_, _) <- List.last ws ->
        True
      _ -> False

    -- Returns (member name, header, non garbage remainder)
    isTypeMemberGarbage = \case
      h@(Header 3 ("", [], []) [Str m1]) : Plain [Str m2] : Plain [Str t1] : Plain [Str t2] : Plain [Str m3] : Para [Code ("", [], []) c] : xs
        | m1 == m2
        , m1 == m3
        , t1 == t2
        , (t1 <> "::" <> m1) `T.isSuffixOf` c
        -> Just (m1, h, xs)
      _ -> Nothing

    extractMembers = iterateSuffixes $ \case
      xs
        | Just (member, h, xs) <- isTypeMemberGarbage xs
        , Section _ sectionBlocks rem <- (h : xs)
        -> ( Just
             (Documentation (Nested parent (CName member))
                            (Pandoc meta sectionBlocks)
             )
           , rem
           )
      xs -> (Nothing, xs)

    (ms, bs') = extractMembers . removeUninteresting . replaceHeader $ bs
  in
    Right (Pandoc meta bs', ms)

-- | Doxygen puts documentation for commands and enumerations in a single file
-- for the documentation for a header. Extract them here.
splitHeaderDocumentation
  :: (Documentee -> Bool) -> Pandoc -> Either Text [Documentation]
splitHeaderDocumentation isValid (Pandoc meta bs) =
  let
    takeDoc = \case
      -- An enum
      NamedSection name sectionBlocks rem
        | documentee <- TopLevel (CName name)
        , isValid documentee
        , Plain [Str name1] : Plain [Str "vk_mem_alloc.h"] : Plain [Str "vk_mem_alloc.h"] : Plain [Str name2] : Para [Code ("", [], []) enumName] : sectionRem <-
          sectionBlocks
        , name == name1
        , name == name2
        , Just enumName <- T.dropPrefix "enum " enumName
        , enumName == name1
        -> let
             removeEnumTable = \case
               x : xs | Just ds <- enumValuesFromTable (CName name) meta x ->
                 (Just ds, xs)
               xs -> (Nothing, xs)
             (valueDocs, withoutValueDocs) =
               iterateSuffixes removeEnumTable sectionRem
           in
             ( Just
               ( Documentation documentee (Pandoc meta withoutValueDocs)
               : concat valueDocs
               )
             , rem
             )
      -- A command
      NamedSection nameWithParens sectionBlocks rem
        | Just name <- T.dropSuffix "()" nameWithParens
        , documentee <- TopLevel (CName name)
        , isValid documentee
        , Plain [Str name1] : Plain [Str "vk_mem_alloc.h"] : Plain [Str "vk_mem_alloc.h"] : Plain [Str name2] : Para [Code ("", [], []) _funDecl] : sectionRem <-
          sectionBlocks
        , name == name1
        , name == name2
        -> (Just [Documentation documentee (Pandoc meta sectionRem)], rem)
      -- A command
      xs -> (Nothing, xs)
  in  pure . concat . fst . iterateSuffixes takeDoc $ bs

enumValuesFromTable :: CName -> Meta -> Block -> Maybe [Documentation]
enumValuesFromTable parent meta = \case
  Table _ (Caption _ [Plain [Str "Enumerator"]]) _ _ bodies _ ->
    let rs =
          [ [ c | Cell _ _ _ _ c <- r ]
          | TableBody _ _ rs1 rs2 <- bodies
          , Row _ r               <- rs1 <> rs2
          ]
    in  traverse enumeratorRowToDocumentation rs
  _ -> Nothing
 where
  enumeratorRowToDocumentation = \case
    [[Plain n], ds] -> do
      Str valueName <- lastMay n
      pure $ Documentation (Nested parent (CName valueName)) (Pandoc meta ds)
    _ -> Nothing

pattern NamedSection :: Text -> [Block] -> [Block] -> [Block]
pattern NamedSection name blocks remainder
  <- Header headerLevel _ [Str name]
   : (break (isHeaderLE headerLevel) -> (blocks, remainder))
