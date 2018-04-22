{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Alias
  ( writeAliases
  ) where

import           Control.Applicative
import           Control.Arrow                            ((&&&))
import           Data.Either.Validation
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Alias
import           Spec.Savvy.APIConstant
import           Spec.Savvy.Command
import           Spec.Savvy.Enum
import           Spec.Savvy.Error
import           Spec.Savvy.Struct
import           Spec.Savvy.Type
import           Spec.Savvy.Type.Haskell

import           Write.Element                            hiding (TypeName)
import qualified Write.Element                            as WE
import           Write.Struct
import           Write.Util

writeAliases :: Aliases -> Validation [SpecError] [WriteElement]
writeAliases Aliases{..} =
  sequenceA . concat $
    [ writeValueAlias commandType <$> commandAliases
    , pure . writeTypeAlias <$> enumAliases
    , pure . writeTypeAlias <$> handleAliases
    , liftA2 (liftA2 (<>)) (pure . writeTypeAlias) writeStructPatternAlias <$> structAliases
    , writeConstantAlias <$> constantAliases
    , writePatternAlias (TypeName . eName . fst) <$> enumExtensionAliases
    ]

writeValueAlias
  :: (a -> Type)
  -> Alias a
  -> Validation [SpecError] WriteElement
writeValueAlias getType alias@Alias{..} = eitherToValidation $ do
  target <- aliasTarget alias
  (t, (is, es)) <- toHsType (getType target)
  let weImports    = is
      weDoc getDoc = [qci|
        {document getDoc (TopLevel aName)}
        {aName} :: {t}
        {aName} = {aAliasName}
|]
      weExtensions = es
      weName       = "Value Alias: " <> aName
      weProvides   = [Unguarded $ Term aName]
      weDepends    = Unguarded <$> [TermName aAliasName] ++ typeDepends (getType target)
  pure WriteElement {..}

writePatternAlias
  :: (a -> Type)
  -> Alias a
  -> Validation [SpecError] WriteElement
writePatternAlias getType alias@Alias{..} = eitherToValidation $ do
  target <- aliasTarget alias
  (t, (is, es)) <- toHsType (getType target)
  let weImports    = is
      weDoc getDoc = [qci|
        {document getDoc (TopLevel aName)}
        pattern {aName} :: {t}
        pattern {aName} = {aAliasName}
|]
      weExtensions = "PatternSynonyms" : es
      weName       = "Pattern Alias: " <> aName
      weProvides   = [Unguarded $ Pattern aName]
      weDepends    = Unguarded <$> PatternName aAliasName : typeDepends (getType target)
  pure WriteElement {..}

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
  in WriteElement {..}

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
                     Unguarded <$> [WE.TypeName aAliasName] ++
                       (typeDepends . smType =<< sMembers)
  pure WriteElement {..}

writeConstantAlias :: Alias APIConstant -> Validation [SpecError] WriteElement
writeConstantAlias alias = eitherToValidation $ do
  APIConstant {..} <- aliasTarget alias
  validationToEither $ case acValue of
    IntegralValue _ -> (<> writeTypeAlias alias) <$> writePatternAlias (const (TypeName "Integral a => a")) alias
    FloatValue    _ -> writePatternAlias (const Float) alias
    Word32Value   _ -> writePatternAlias (const (TypeName "uint32_t")) alias
    Word64Value   _ -> writePatternAlias (const (TypeName "uint64_t")) alias
