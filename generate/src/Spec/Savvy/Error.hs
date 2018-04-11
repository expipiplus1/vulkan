{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Savvy.Error
  ( SpecError(..)
  , prettySpecError
  ) where

import           Data.Semigroup
import           Data.Text

data SpecError
  = EnumTypeMissing Text
  | AliasTargetMissing Text
  | EnumExtensionNumberMissing Text
  | WeirdBitmaskType Text
  | Other Text
    -- ^ Used for testing in development

prettySpecError :: SpecError -> Text
prettySpecError = \case
  EnumTypeMissing e ->
    "An enumeration value declaration references an undeclared enum type called"
      <+> e
  AliasTargetMissing e ->
    "An alias target references an undeclared type called" <+> e
  EnumExtensionNumberMissing e ->
    "An enum extension doesn't have an enum number and its value is unknown:"
      <+> e
  WeirdBitmaskType t ->
    "A bitmask isn't backed by VkFlags, backed instead by: "
      <+> t
  Other e -> e

(<+>) :: Text -> Text -> Text
a <+> b = a <> " " <> b
