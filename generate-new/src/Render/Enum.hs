{-# language TemplateHaskell #-}
module Render.Enum
  where

import           Data.Text.Prettyprint.Doc
import qualified Data.Vector                   as V
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( lift )
import           Text.Printf
import qualified Text.Read                     as R
import           Text.Show

import           Data.Bits
import           Foreign.Storable
import           GHC.Read                hiding ( parens )
import           Numeric
import           Text.Read               hiding ( parens )

import           CType                          ( CType(TypeName) )
import           Error
import           Haskell                       as H
import           Render.Element
import           Render.SpecInfo
import           Render.Type
import           Render.Utils
import           Spec.Parse

renderEnum
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Enum'
  -> Sem r RenderElement
renderEnum e@Enum {..} = do
  RenderParams {..} <- input
  genRe ("enum " <> unCName eName) $ do
    innerTy <- case eType of
      AnEnum     -> pure $ ConT ''Int32
      -- TODO: remove vulkan specific stuff
      ABitmask _ -> cToHsType DoNotPreserve (TypeName "VkFlags")
    let n       = mkTyName eName
        conName = mkConName eName eName

    -- Export the type cinnamon first so that it appears above the Flags in the
    -- Haddocks, this means when viewing the page there, the user will also
    -- have the flags visible
    case eType of
      ABitmask flags | flags /= eName -> do
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
          AnEnum     -> completePragma n (mkPatternName . evName <$> eValues)
          ABitmask _ -> Nothing
    tellImport (TyConName "Zero")
    derivedClasses <- do
      tellImport ''Storable
      let always = ["Eq", "Ord", "Storable", "Zero"]
      special <- case eType of
        AnEnum     -> pure []
        ABitmask _ -> do
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
    renderShowInstance e
    renderReadInstance e

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
        AnEnum     -> showsPrec 9 evValue ""
        ABitmask _ -> printf "0x%08x" evValue
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

renderShowInstance
  :: (HasErr r, HasRenderParams r, HasRenderElem r) => Enum' -> Sem r ()
renderShowInstance Enum {..} = do
  RenderParams {..} <- input
  let n       = mkTyName eName
      conName = mkConName eName eName
  valueCases <- forV eValues $ \EnumValue {..} -> do
    let pat = mkPatternName evName
    tellImport pat
    tellImport 'showString
    pure $ pretty pat <+> "-> showString" <+> viaShow (unName pat)
  defaultCase <- do
    tellImportWith n conName
    tellImport 'showParen
    tellImport 'showString
    (prefix, shows) <- case eType of
      AnEnum -> do
        tellImport 'showsPrec
        pure ("", "showsPrec 11")
      ABitmask _ -> do
        tellImport 'showHex
        pure ("0x", "showHex")
    pure $ pretty conName <+> "x -> showParen (p >= 11)" <+> parens
      (   "showString"
      <+> viaShow (unName conName <> " " <> prefix)
      <+> "."
      <+> shows
      <+> "x"
      )
  let cases = toList valueCases <> [defaultCase]
  tellDoc $ "instance Show" <+> pretty n <+> "where" <> line <> indent
    2
    ("showsPrec p = \\case" <> line <> indent 2 (vsep cases))

renderReadInstance
  :: (HasErr r, HasRenderParams r, HasRenderElem r) => Enum' -> Sem r ()
renderReadInstance Enum {..} = do
  RenderParams {..} <- input
  let n       = mkTyName eName
      conName = mkConName eName eName
  matchTuples <- forV eValues $ \EnumValue {..} -> do
    let pat = mkPatternName evName
    tellImport pat
    pure $ tupled [viaShow (unName pat), "pure" <+> pretty pat]
  tellImportWith ''Read 'readPrec
  tellImport 'R.parens
  tellImport 'choose
  tellImport '(+++)
  tellImport 'expectP
  tellImportWith ''Lexeme 'Ident
  tellImport 'step
  tellImport 'prec
  tellDoc $ "instance Read" <+> pretty n <+> "where" <> line <> indent
    2
    ("readPrec = parens" <+> parens
      (align
        (vsep
          [ "choose" <+> align
            (brackets (hsep (punctuate (line <> ",") (toList matchTuples))))
          , "+++"
          , "prec 10" <+> parens
            (doBlock
              [ "expectP" <+> parens ("Ident" <+> viaShow (unName conName))
              , "v <- step readPrec"
              , "pure" <+> parens (pretty conName <+> "v")
              ]
            )
          ]
        )
      )
    )
