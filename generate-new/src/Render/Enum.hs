{-# language TemplateHaskell #-}
module Render.Enum
  where

import           Text.Printf
import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Text.Show
import qualified Text.Read                     as R
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import qualified Data.Vector                   as V

import           Foreign.Storable
import           Data.Bits
import           Numeric
import           GHC.Read                hiding ( parens )
import           Text.Read               hiding ( parens )

import           Spec.Parse
import           Haskell                       as H
import           Error
import           Render.Element
import           Render.Utils
import           CType                          ( CType(TypeName) )
import           Render.Type
import           Render.SpecInfo
import           Documentation

renderEnum
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Enum'
  -> Sem r RenderElement
renderEnum e@Enum {..} = do
  RenderParams {..} <- ask
  genRe ("enum " <> unCName eName) $ do
    let n       = mkTyName eName
        conName = mkConName eName eName
    innerTy <- case eType of
      AnEnum   -> pure $ ConT ''Int32
      -- TODO: remove vulkan specific stuff
      ABitmask -> cToHsType DoNotPreserve (TypeName "VkFlags")
    (patterns, patternExports) <-
      V.unzip <$> traverseV (renderEnumValue eName conName eType) eValues
    tellExport (Export n True patternExports)
    tellBoot $ do
      tellExport (EType n)
      tellDoc $ "data" <+> pretty n
    tDoc <- renderType innerTy
    let complete = case eType of
          AnEnum   -> completePragma n (mkPatternName . evName <$> eValues)
          ABitmask -> Nothing
    tellImport (TyConName "Zero")
    derivedClasses <- do
      tellImport ''Storable
      let always = ["Eq", "Ord", "Storable", "Zero"]
      special <- case eType of
        AnEnum   -> pure []
        ABitmask -> do
          tellImport ''Bits
          pure ["Bits"]
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
  :: (HasErr r, Member (Reader RenderParams) r)
  => CName
  -> HName
  -- ^ Constructor name
  -> EnumType
  -> EnumValue
  -> Sem r ((Documentee -> Doc ()) -> Doc (), Export)
renderEnumValue eName conName enumType EnumValue {..} = do
  RenderParams {..} <- ask
  let n = mkPatternName evName
      v = case enumType of
        AnEnum   -> showsPrec 9 evValue ""
        ABitmask -> printf "0x%08x" evValue
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
  RenderParams {..} <- ask
  let n   = mkTyName eName
      con = mkConName eName eName
  valueCases <- forV eValues $ \EnumValue {..} -> do
    let pat = mkPatternName evName
    tellImport pat
    tellImport 'showString
    pure $ pretty pat <+> "-> showString" <+> viaShow (unName pat)
  defaultCase <- do
    tellImportWith n con
    tellImport 'showParen
    tellImport 'showString
    (prefix, shows) <- case eType of
      AnEnum -> do
        tellImport 'showsPrec
        pure ("", "showsPrec 11")
      ABitmask -> do
        tellImport 'showHex
        pure ("0x", "showHex")
    pure $ pretty con <+> "x -> showParen (p >= 11)" <+> parens
      (   "showString"
      <+> viaShow (unName con <> " " <> prefix)
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
  RenderParams {..} <- ask
  let n   = mkTyName eName
      con = mkConName eName eName
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
              [ "expectP" <+> parens ("Ident" <+> viaShow (unName con))
              , "v <- step readPrec"
              , "pure" <+> parens (pretty con <+> "v")
              ]
            )
          ]
        )
      )
    )
