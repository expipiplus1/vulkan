{-# language TemplateHaskell #-}
{-# language QuasiQuotes #-}
module Render.Enum where

import           Data.Text.Prettyprint.Doc
import qualified Data.Vector                   as V
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( lift )
import           Text.Printf
import           Text.Show

import           Data.Bits
import           Foreign.Storable
import           Numeric

import           CType                          ( CType(TypeName) )
import qualified Data.Text                     as T
import           Error
import           GHC.Read                       ( Read(readPrec) )
import           Haskell                       as H
import           Language.Haskell.TH            ( mkName )
import qualified Prelude                       as P
import           Render.Element
import           Render.SpecInfo
import           Render.Type
import           Spec.Parse
import           Text.InterpolatedString.Perl6.Unindented

renderEnum
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Enum'
  -> Sem r RenderElement
renderEnum e@Enum {..} = do
  RenderParams {..} <- input
  genRe ("enum " <> unCName eName) $ do
    tellCanFormat

    innerTy <- case eType of
      AnEnum               -> pure $ ConT ''Int32
      ABitmask _ Bitmask32 -> cToHsType DoNotPreserve (TypeName flagsTypeName)
      ABitmask _ Bitmask64 ->
        cToHsType DoNotPreserve (TypeName flags64TypeName)
    let n       = mkTyName eName
        conName = mkConName eName eName

    -- Export the type cinnamon first so that it appears above the Flags in the
    -- Haddocks, this means when viewing the page there, the user will also
    -- have the flags visible
    case eType of
      ABitmask flags _ | flags /= eName -> do
        let flagsName = mkTyName flags
        let syn :: HasRenderElem r => Sem r ()
            syn = do
              tellExport (EType flagsName)
              tellDoc $ "type" <+> pretty flagsName <+> "=" <+> pretty n
        syn
        tellBoot syn
      _ -> pure ()

    (patterns, patternExports) <-
      V.unzip <$> traverseV (renderEnumValue eName conName eType) eValues
    tellExport (Export n True patternExports Reexportable)
    tellBoot $ do
      tellExport (EType n)
      tellDoc $ "data" <+> pretty n
    tDoc <- renderType innerTy
    let complete = case eType of
          AnEnum       -> completePragma n (mkPatternName . evName <$> eValues)
          ABitmask _ _ -> Nothing
    tellImport (TyConName "Zero")
    derivedClasses <- do
      tellImport ''Storable
      let always = ["Eq", "Ord", "Storable", "Zero"]
      special <- case eType of
        AnEnum       -> pure []
        ABitmask _ _ -> do
          tellImport ''Bits
          tellImport ''FiniteBits
          pure ["Bits", "FiniteBits"]
      pure (always <> special)
    let

      zeroComment = case eType of
        AnEnum | all ((/= 0) . evValue) eValues ->
          "-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error"
            <> line
        _ -> mempty
    tellDocWithHaddock $ \getDoc ->
      vsep
        $  [ getDoc (TopLevel eName)
           , "newtype"
           <+> pretty n
           <+> "="
           <+> pretty conName
           <+> tDoc
           <>  line
           <>  indent 2 ("deriving newtype" <+> tupled derivedClasses)
           , zeroComment
           , vsep (toList (($ getDoc) <$> patterns))
           ]
        ++ maybeToList complete
    renderReadShowInstances e

completePragma :: HName -> V.Vector HName -> Maybe (Doc ())
completePragma ty pats = if V.null pats
  then Nothing
  else
    Just
    $   "{-# complete"
    <+> align (vsep (punctuate "," (pretty <$> V.toList pats)))
    <+> "::"
    <+> pretty ty
    <+> "#-}"

renderEnumValue
  :: (HasErr r, HasRenderParams r)
  => CName
  -- ^ Enum name for fetching documentation
  -> HName
  -- ^ Constructor name
  -> EnumType
  -> EnumValue
  -> Sem r ((Documentee -> Doc ()) -> Doc (), Export)
renderEnumValue eName conName enumType EnumValue {..} = do
  RenderParams {..} <- input
  let n = mkPatternName evName
      v = case enumType of
        AnEnum               -> showsPrec 9 evValue ""
        ABitmask _ Bitmask32 -> printf "0x%08x" evValue
        ABitmask _ Bitmask64 -> printf "0x%016x" evValue
  pure
    ( \getDoc -> vsep
      [ getDoc (Nested eName evName)
      , "pattern" <+> pretty n <+> "=" <+> pretty conName <+> pretty v
      ]
    , EPat n
    )

----------------------------------------------------------------
-- Read and Show instances
----------------------------------------------------------------

renderReadShowInstances
  :: (HasErr r, HasRenderParams r, HasRenderElem r) => Enum' -> Sem r ()
renderReadShowInstances e@Enum {..} = do
  RenderParams {..} <- input
  let n        = mkTyName eName
      conName  = mkConName eName eName
      patNames = mkPatternName . evName <$> toList eValues
  let enumPrefix       = commonPrefix (T.unpack . unName <$> patNames)
      dropCommonPrefix = T.drop (length enumPrefix)
      prefixStringName = "enumPrefix" <> pretty n
      conNameName      = "conName" <> pretty conName
      showTableName    = "showTable" <> pretty n
  tableElems <- forV patNames $ \pat -> do
    tellImport pat
    pure $ tupled [pretty pat, viaShow (dropCommonPrefix (unName pat))]
  tellDoc $ vsep
    [ conNameName <+> ":: String"
    , conNameName <+> "=" <+> dquotes (pretty conName)
    , emptyDoc
    , prefixStringName <+> ":: String"
    , prefixStringName <+> "=" <+> viaShow enumPrefix
    , emptyDoc
    , showTableName <+> ":: [(" <> pretty n <> ", String)]"
    , showTableName <+> "=" <+> list tableElems
    ]
  renderShowInstance prefixStringName showTableName conNameName e
  renderReadInstance prefixStringName showTableName conNameName e

-- >>> commonPrefix ["hello", "help"]
-- "hel"
--
-- >>> commonPrefix []
-- []
--
-- >>> commonPrefix ["foo", "foobar"]
-- "foo"
--
-- >>> commonPrefix ["foo", "foobar", "beach"]
-- ""
commonPrefix :: Eq a => [[a]] -> [a]
commonPrefix = \case
  [] -> []
  xs ->
    let shortest = P.minimum (length <$> xs)
        ts       = take shortest (transpose xs)
        sames    = takeWhile (\(c : cs) -> all (== c) cs) ts
    in  P.head <$> sames

renderShowInstance
  :: (HasErr r, HasRenderParams r, HasRenderElem r)
  => Doc ()
  -> Doc ()
  -> Doc ()
  -> Enum'
  -> Sem r ()
renderShowInstance prefixString showTableName conNameName Enum {..} = do
  RenderParams {..} <- input
  let n       = mkTyName eName
      conName = mkConName eName eName
  tellImportWith n      conName
  tellImportWith ''Show 'P.showsPrec
  tellImport (mkName (T.unpack modulePrefix <> ".Internal.Utils.enumShowsPrec"))
  shows <- case eType of
    AnEnum -> do
      tellImport 'showsPrec
      pure ("(showsPrec 11)" :: Text)
    ABitmask _ _ -> do
      tellImport 'showString
      tellImport 'showHex
      pure "(\\x -> showString \"0x\" . showHex x)"
  tellDoc [qqi|
    instance Show {n} where
      showsPrec = enumShowsPrec {prefixString} {showTableName} {conNameName} (\\({conName} x) -> x) {shows}
  |]

renderReadInstance
  :: (HasErr r, HasRenderParams r, HasRenderElem r)
  => Doc ()
  -> Doc ()
  -> Doc ()
  -> Enum'
  -> Sem r ()
renderReadInstance prefixString showTableName conNameName Enum {..} = do
  RenderParams {..} <- input
  let n       = mkTyName eName
      conName = mkConName eName eName
  tellImportWith n      conName
  tellImportWith ''Read 'readPrec
  tellImport (mkName (T.unpack modulePrefix <> ".Internal.Utils.enumReadPrec"))
  tellDoc [qqi|
    instance Read {n} where
      readPrec = enumReadPrec {prefixString} {showTableName} {conNameName} {conName}
  |]
