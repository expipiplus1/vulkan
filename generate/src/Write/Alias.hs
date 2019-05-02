{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Alias
  ( writeAliases
  , writeFullEnumAliases
  ) where

import           Control.Applicative
import           Data.Text                         hiding ( concat )
import           Control.Arrow                            ( (&&&) )
import           Data.Functor
import           Data.Foldable
import           Data.List.Extra
import           Data.List.NonEmpty                       ( NonEmpty(..) )
import           Data.Semigroup
import           Data.Either.Validation
import           Data.Text.Prettyprint.Doc
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Alias
import           Spec.Savvy.APIConstant
import           Spec.Savvy.Command
import           Spec.Savvy.Enum
import           Spec.Savvy.Error
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import           Spec.Savvy.Type.Haskell
import           Spec.Savvy.Feature
import           Write.Element                     hiding ( TypeName )
import qualified Write.Element                 as WE
import           Write.Struct
import           Write.Util
import           Write.Command
import           Write.ConstantExtension

writeAliases :: Aliases -> Validation [SpecError] [WriteElement]
writeAliases Aliases{..} =
  sequenceA . concat $
    [ writeCommandAlias <$> commandAliases
    , writeEnumAlias <$> enumAliases
    , pure . writeTypeAlias <$> handleAliases
    , liftA2 (liftA2 (<>)) (pure . writeTypeAlias) writeStructPatternAlias <$> structAliases
    , writeConstantAlias <$> constantAliases
    , writePatternAlias (TypeName . eName . fst) <$> enumExtensionAliases
    , writeConstantExtensionAliases <$> constantExtensionAliases
    ]

writeCommandAlias
  :: Alias Command
  -> Validation [SpecError] WriteElement
writeCommandAlias alias@Alias{..} = eitherToValidation $ do
  command@Command{..} <- aliasTarget alias
  let t = commandDynamicType command
  (tyDoc, (is, es)) <- toHsType t
  let weImports    = is
      weDoc getDoc = [qci|
        {document getDoc (TopLevel aName)}
        {aName} :: {tyDoc}
        {aName} = {aAliasName}
      |]
      weExtensions = es
      weName       = "Value Alias: " <> aName
      weProvides   = [Unguarded $ Term aName]
      weDepends    = Unguarded <$> TermName aAliasName : typeDepends t
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement          = Nothing
  pure WriteElement {..}

writePatternAlias
  :: (a -> Type)
  -> Alias a
  -> Validation [SpecError] WriteElement
writePatternAlias getType alias@Alias{..} = eitherToValidation $ do
  target <- aliasTarget alias
  (t, (is, es)) <- toHsType (getType target)
  let ds = typeDepends (getType target)
  writePatternAliasWithType (t, (is, ds, es)) (TopLevel aName) alias

writePatternAliasWithType
  :: (Doc (), ([Guarded Import], [HaskellName], [Text]))
  -> Documentee
  -- ^ Documentation name
  -> Alias a
  -> Either [SpecError] WriteElement
writePatternAliasWithType (t, (is, ds, es)) docName Alias{..} = do
  let weImports    = is
      weDoc getDoc = [qci|
        {document getDoc docName}
        pattern {aName} :: {t}
        pattern {aName} = {aAliasName}
      |]
      weExtensions = "PatternSynonyms" : es
      weName       = "Pattern Alias: " <> aName
      weProvides   = [Unguarded $ Pattern aName]
      weDepends    = Unguarded <$> PatternName aAliasName : ds
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement          = Nothing
  pure WriteElement {..}

writeConstantExtensionAliases
  :: Alias ConstantExtension -> Validation [SpecError] WriteElement
writeConstantExtensionAliases a@Alias {..} = eitherToValidation $ do
  target <- aliasTarget a
  let (tyDoc, is) = constantExtensionType (const Nothing) target
  writePatternAliasWithType (tyDoc, (is, [], [])) (TopLevel (ceName target)) a


writeEnumAlias
  :: Alias Enum
  -> Validation [SpecError] WriteElement
writeEnumAlias eAlias = pure (writeTypeAlias eAlias)

writeFullEnumAliases
  :: (Alias Enum, [Alias EnumElement], [Alias EnumExtension])
  -> Validation [SpecError] WriteElement
writeFullEnumAliases (eAlias, eeAliases, exAliases) =
  let
    writeEnumerant :: Alias a -> Validation [SpecError] WriteElement
    writeEnumerant enumerantAlias = eitherToValidation $ do
      (enumType, (is, es)) <- toHsType (TypeName (aName eAlias))
      let ds  = typeDepends (TypeName (aName eAlias))
          t   = [qci|(a ~ {enumType}) => a|]
          es' = "TypeFamilies" : es
      writePatternAliasWithType (t, (is, ds, es'))
                                (Nested (aName eAlias) (aName enumerantAlias))
                                enumerantAlias
  in
    fmap sconcat
    .  sequenceA
    $  pure (writeTypeAlias eAlias)
    :| (pure
         (writeCompletePragma (aName eAlias)
                              ((aName <$> eeAliases) ++ (aName <$> exAliases))
         )
       )
    :  (  (eeAliases <&> writeEnumerant)
       ++ (nubOrdOn aName exAliases <&> writeEnumerant)
       )

writeCompletePragma :: Text -> [Text] -> WriteElement
writeCompletePragma typeName patterns =
  let
    weImports = []
    weDoc _ = if Prelude.null patterns
      then
        [qci|-- No complete pragma for {pretty typeName} as it has no patterns|]
      else
        [qci|\{-# complete {hsep $ punctuate "," (pretty <$> patterns)} :: {pretty typeName} #-}|]
    weExtensions           = []
    weName                 = "complete pragma for " <> typeName
    weProvides             = []
    weDepends = Unguarded <$> WE.TypeName typeName : (PatternName <$> patterns)
    weUndependableProvides = []
    weSourceDepends        = []
    weBootElement          = Nothing
    w                      = WriteElement { .. }
  in
    w

writeTypeAlias
  :: Alias a
  -> WriteElement
writeTypeAlias Alias{..} =
  let weImports    = []
      weDoc getDoc = [qci|
        {document getDoc (TopLevel aName)}
        type {aName} = {aAliasName}
      |]
      weExtensions = []
      weName       = "Type Alias: " <> aName
      weProvides   = [Unguarded $ TypeAlias aName]
      weDepends    = [Unguarded $ WE.TypeName aAliasName]
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement = Just w { weDepends = [], weSourceDepends = weDepends }
      w = WriteElement{..}
  in w

writeStructPatternAlias :: Alias Struct -> Validation [SpecError] WriteElement
writeStructPatternAlias alias@Alias{..} = eitherToValidation $ do
  Struct{..} <- aliasTarget alias
  (t, (is, es)) <-
    protoToHsTypeNonIO (TypeName aName) (((Just . smName) &&& smType) <$> sMembers)
  let memberNames = smName . fixMemberName <$> sMembers
      weImports    = is
      weDoc getDoc = [qci|
        {document getDoc (TopLevel aName)}
        pattern {aName} :: {t}
        pattern {aName} {hsep (pretty <$> memberNames)} = {aAliasName} {hsep (pretty <$> memberNames)}
|]
      weExtensions = "PatternSynonyms" : es
      weName       = "Struct Pattern Alias: " <> aName
      weProvides   = [Unguarded $ Pattern aName]
      weDepends    = -- This is not correct if we have a struct alias of a struct alias
                     Unguarded <$>
                       WE.TypeName "(:::)" :
                       WE.TypeName aAliasName :
                       (typeDepends . smType =<< sMembers)
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement          = Nothing
  pure WriteElement {..}

writeConstantAlias :: Alias APIConstant -> Validation [SpecError] WriteElement
writeConstantAlias alias = eitherToValidation $ do
  APIConstant {..} <- aliasTarget alias
  validationToEither $ case acValue of
    IntegralValue _ -> (<> writeTypeAlias alias) <$> writePatternAlias (const (TypeName "Integral a => a")) alias
    FloatValue    _ -> writePatternAlias (const Float) alias
    Word32Value   _ -> writePatternAlias (const (TypeName "uint32_t")) alias
    Word64Value   _ -> writePatternAlias (const (TypeName "uint64_t")) alias
