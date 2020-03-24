{-# language TemplateHaskell #-}
module Render.Enum
  where

import           Text.Printf
import           Prelude                        ( Show(..) )
import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import qualified Data.Vector                   as V

import           Foreign.Storable
import           Data.Bits

import           Spec.Parse
import           Haskell                       as H
import           Error
import           Render.Element
import           CType                          ( CType(TypeName) )
import           Render.Type
import           Render.SpecInfo

renderEnum
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Enum'
  -> Sem r RenderElement
renderEnum Enum {..} = do
  RenderParams {..} <- ask
  genRe ("enum " <> unCName eName) $ do
    let n       = mkTyName eName
        conName = mkConName eName eName
    innerTy <- case eType of
      AnEnum   -> pure $ ConT ''Int32
      -- TODO: remove vulkan specific stuff
      ABitmask -> cToHsType DoNotPreserve (TypeName "VkFlags")
    (patterns, patternExports) <-
      V.unzip <$> traverseV (renderEnumValue conName eType) eValues
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
      let always = ["Eq", "Ord", "Storable", "Read", "Show", "Zero"]
      special <- case eType of
        AnEnum   -> pure []
        ABitmask -> do
          tellImport ''Bits
          pure ["Bits"]
      pure (always <> special)
    let

      zeroComment = case eType of
        AnEnum | all ((/= 0) . evValue) eValues -> do
          "-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error"
            <> line
        _ -> mempty
    tellDoc
      .  vsep
      $  [ "newtype"
         <+> pretty n
         <+> "="
         <+> pretty conName
         <+> tDoc
         <>  line
         <>  indent 2 ("deriving newtype" <+> tupled derivedClasses)
         , zeroComment
         , vsep (toList patterns)
         ]
      ++ maybeToList complete

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
  => HName
  -- ^ Constructor name
  -> EnumType
  -> EnumValue
  -> Sem r (Doc (), Export)
renderEnumValue conName enumType EnumValue {..} = do
  RenderParams {..} <- ask
  let n = mkPatternName evName
      v = case enumType of
            AnEnum -> showsPrec 9 evValue ""
            ABitmask -> printf "0x%08x" evValue
  pure
    ( "pattern"
    <+> pretty n
    <+> "="
    <+> pretty conName
    <+> pretty v
    , EPat n
    )
