module Documentation
  ( Documentation(..)
  , Documentee(..)
  , docBookToDocumentation
  , splitDocumentation
  , guessDocumentee
  , iterateSuffixesM
  , iterateSuffixes
  , pattern Section
  , isHeaderLE
  , main
  ) where

import           Control.Monad
import           Data.Char                      ( isLower )
import           Data.Default
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text                     as T
import           Documentation.RunAsciiDoctor
                                         hiding ( main )
import           Relude                  hiding ( elem
                                                , rem
                                                )
import           Say
import           Spec.Flavor
import           Spec.Name
import           System.Environment
import           System.FilePath                ( takeBaseName )
import           Text.Pandoc
import           Text.Show.Pretty

data Documentation = Documentation
  { dDocumentee    :: Documentee
    -- ^ The name of the thing being documented
  , dDocumentation :: Pandoc
    -- ^ The documentation itself
  }
  deriving Show

data Documentee
  = TopLevel CName
  | Nested CName CName
  | Chapter Text
  deriving (Show, Eq, Ord)

docBookToDocumentation
  :: SpecFlavor
  -> Text
  -- ^ The docbook string
  -> Text
  -- ^ The documentee name
  -> Either Text [Documentation]
docBookToDocumentation specFlavor db name = do
  let readerOptions = def
  pandoc <- first show $ runPure (readDocBook readerOptions db)
  let prefix = case specFlavor of
        SpecVk -> "VK_"
        SpecXr -> "XR_"

  if prefix `T.isPrefixOf` name && T.any isLower name
    then pure [Documentation (Chapter name) pandoc]
    else do
      (removed, unmergedSubDocs) <- splitDocumentation (CName name) pandoc

      let mergedSubDocs = mergeSubDocs unmergedSubDocs
      pure $ Documentation (TopLevel (CName name)) removed : mergedSubDocs

mergeSubDocs :: [Documentation] -> [Documentation]
mergeSubDocs unmergedSubDocs =
  let sorted = sortOn dDocumentee unmergedSubDocs
  in  foldr
        (curry
          (\case
            (x, y : ys) | dDocumentee x == dDocumentee y ->
              Documentation (dDocumentee x)
                            (dDocumentation x <> dDocumentation y)
                : ys
            (x, ys) -> x : ys
          )
        )
        []
        sorted

guessDocumentee :: (Documentee -> Bool) -> Pandoc -> Either Text CName
guessDocumentee isValid (Pandoc _ bs) = do
  firstWord <- case bs of
    Para (Str n : _) : _ -> pure n
    _                    -> Left "Unable to find first word in documentation"
  if isValid (TopLevel (CName firstWord))
    then pure (CName firstWord)
    -- TODO: Fix error message here.
    else
      Left
      $  "First word of documentation doesn't isn't a valid documentee name: "
      <> show firstWord

-- | If the description is a bullet list of "enames" then remove those from the
-- original documentation and return them separately.
--
-- Return the original documentation with the new document sections removed
splitDocumentation :: CName -> Pandoc -> Either Text (Pandoc, [Documentation])
splitDocumentation parent (Pandoc meta bs) = do
  (es, bs') <- iterateSuffixesM (splitPrefix meta) bs
  pure (Pandoc meta bs', join es)
 where
  splitPrefix m = \case
    -- Remove the "Document Notes" section
    Section "_document_notes"  _    rem -> pure (Nothing, rem)

    -- Remove the "C Specification" section
    Section "_c_specification" _    rem -> pure (Nothing, rem)

    -- Remove the "Name" header
    Section "_name"            bs'' rem -> pure (Nothing, bs'' ++ rem)

    -- If the description section is a list of documentation for enumeration
    -- values or members, split them into separate documentation elements
    xs@(Section sectionTag bs'' rem)
      | h : _ <- xs
      , sectionTag
        `elem` [ "_parameters"
               , "_description"
               , "_members"
               , "fundamentals-successcodes"
               , "fundamentals-errorcodes"
               ]
      -> case memberDocs parent m bs'' of
        Left  _                    -> pure (Nothing, xs)
        Right (ds, []            ) -> pure (Just ds, rem)
        Right (ds, leftoverBlocks) -> do
          let includeSubdocumentation = any
                (\case
                  Para ws ->
                    [Str "The", Space, Str "following", Space, Str "bits"]
                      `isPrefixOf` ws

                  _ -> False
                )
                leftoverBlocks
              --- ^ Include the member docs as well, some documentation makes
              -- reference to them and makes no sense without the bullet
              -- points, for instance 'VkFormatFeatureFlagBits'
              ps = if includeSubdocumentation then bs'' else leftoverBlocks
          pure (Just ds, h : ps ++ rem)

    -- Leave everything else alone
    xs -> pure (Nothing, xs)

pattern Section :: Text -> [Block] -> [Block] -> [Block]
pattern Section ref blocks remainder <-
  Header headerLevel (ref, _, _) _ : (break (isHeaderLE headerLevel) -> (blocks, remainder))

isHeaderLE :: Int -> Block -> Bool
isHeaderLE n = \case
  Header n' _ _ -> n' <= n
  _             -> False

-- Handle struct members, enum docs and function parameter documentation
memberDocs
  :: CName
  -- ^ Parent name
  -> Meta
  -> [Block]
  -> Either Text ([Documentation], [Block])
  -- ^ The documentation and the leftover blocks
memberDocs parent m blocks =
  let
    extractBulletList = \case
      BulletList bullets ->
        let
          enumDoc :: [Block] -> Either Text Documentation
          enumDoc = \case
            p@(Para (dropBeginningSpan -> Code ("", [], []) memberName : _)) : ps
              | -- To avoid drawing in a list of suffixes (OpenXR's XrResult)
                not ("_" `T.isPrefixOf` memberName)
              -> pure Documentation
                { dDocumentee    = Nested parent (CName memberName)
                , dDocumentation = Pandoc m (p : ps)
                }
            _ -> Left "Unhandled member documentation declaration"
        in
          (, []) <$> traverse enumDoc bullets
      Table _attrs _caption _align (TableHead _ [Row _ [Cell _ _ _ _ [Plain [Str "Enum"]], Cell _ _ _ _ [Plain [Str "Description"]]]]) [TableBody _ _ [] rows] _foot
        -> let
             enumDoc :: Row -> Either Text Documentation
             enumDoc = \case
               (Row _ [Cell _ _ _ _ [Para [Code ("", [], []) memberName]], Cell _ _ _ _ bs])
                 -> pure Documentation
                   { dDocumentee    = Nested parent (CName memberName)
                   , dDocumentation = Pandoc m bs
                   }
               _ -> Left "Unhandled enum doc row"
           in
             (, []) <$> traverse enumDoc rows
      d -> Right ([], [d])
  in  mconcat <$> traverse extractBulletList blocks

-- | Since 1.2.160, sections start with this span listing the valid usage rule,
-- remove this before identifying member docs.
dropBeginningSpan :: [Inline] -> [Inline]
dropBeginningSpan = \case
  Span _ _ : SoftBreak : xs -> xs
  Span _ _ : Space     : xs -> xs
  xs                        -> xs

main :: IO ()
main = do
  [flavor, vulkanDocsDir, manPage] <- getArgs
  let f = case flavor of
        "vk" -> SpecVk
        "xr" -> SpecXr
        _    -> error "invalid flavor"
  manTxtToDocbook f [] vulkanDocsDir manPage >>= \case
    Left e -> sayErr e
    Right d' ->
      case docBookToDocumentation f d' (T.pack (takeBaseName manPage)) of
        Left  e  -> sayErr e
        Right ds -> for_ ds pPrint

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- |
-- >>> :{
--  iterateSuffixesM
--    (\case
--      1 : 2 : xs -> pure @Identity (Just "one,two", xs)
--      xs         -> pure (Nothing, xs)
--    )
--    [0 .. 4]
-- :}
-- Identity (["one,two"], [0,3,4])
iterateSuffixesM
  :: forall m a b
   . Monad m
  => ([a] -> m (Maybe b, [a]))
  -- ^ A function which takes a list and returns some @b@ and the list without
  -- whatever prefix the @b@ is used in place of
  -> [a]
  -- ^ A list to extract @b@s from
  -> m ([b], [a])
  -- ^ (The list of @b@s, the list without those prefixes)
iterateSuffixesM split' = foldrM go ([], [])
 where
  go :: a -> ([b], [a]) -> m ([b], [a])
  go x (ss, xs) = first (maybe ss (: ss)) <$> split' (x : xs)

iterateSuffixes
  :: forall a b
   . ([a] -> (Maybe b, [a]))
  -- ^ A function which takes a list and returns some @b@ and the list without
  -- whatever prefix the @b@ is used in place of
  -> [a]
  -- ^ A list to extract @b@s from
  -> ([b], [a])
  -- ^ (The list of @b@s, the list without those prefixes)
iterateSuffixes = coerce (iterateSuffixesM @Identity @a @b)
