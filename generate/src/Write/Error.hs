{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Write.Error
  ( WriteError(..)
  , prettyWriteError
  ) where

import           Data.Semigroup
import           Data.Text

data WriteError
  = EnumTypeNameMismatch Text Text
  | EnumTypeMissing Text
  | AliasTargetMissing Text
  | Other Text
    -- ^ Used for testing in development

prettyWriteError :: WriteError -> Text
prettyWriteError = \case
  EnumTypeNameMismatch et e ->
    "The names of the EnumType and Enum given to writeEnum do not match"
      <+> et
      <+> "and"
      <+> e
      <+> "respectively"
  EnumTypeMissing e ->
    "An enumeration value declaration references an undeclared enum type called"
      <+> e
  AliasTargetMissing e ->
    "An alias target references an undeclared type called"
      <+> e
  Other e -> e

(<+>) :: Text -> Text -> Text
a <+> b = a <> " " <> b
