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

import           Spec.Parse
import           Haskell                       as H
import           Error
import           Render.Element

renderEnum
  :: (HasErr r, Member (Reader RenderParams) r) => Enum' -> Sem r RenderElement
renderEnum Enum {..} = do
  RenderParams {..} <- ask
  genRe ("enum " <> eName) $ do
    let n       = mkTyName eName
        conName = mkConName eName eName
        innerTy = case eType of
          AnEnum   -> ConT ''Int32
          ABitmask -> ConT (typeName "VkFlags")
    (patterns, patternExports) <-
      V.unzip <$> traverseV (renderEnumValue conName eType) eValues
    tellExport (Export (TyConName n) True patternExports)
    tDoc <- renderType innerTy
    let complete = case eType of
          AnEnum   -> completePragma n (mkPatternName . evName <$> eValues)
          ABitmask -> Nothing
    derivedClasses <- (["Eq", "Ord", "Storable"] <>) <$> case eType of
      AnEnum   -> pure []
      ABitmask -> do
        tellImport (TyConName "Zero")
        pure ["Zero"]
    tellImport ''Storable
    tellDoc
      .  vsep
      $  [ "newtype"
         <+> pretty n
         <+> "="
         <+> pretty conName
         <+> tDoc
         <>  line
         <>  indent 2 ("deriving newtype" <+> tupled derivedClasses)
         , vsep (toList patterns)
         ]
      ++ maybeToList complete

completePragma :: Text -> V.Vector Text -> Maybe (Doc ())
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
  => Text
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
