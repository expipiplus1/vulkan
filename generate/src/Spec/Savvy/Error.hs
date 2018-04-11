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
  | TypeParseError Text Text
  | TypeNameParseError Text Text
  | UnhandledCType Text
  | UnhandledCTypeSpecifier Text
  | UnhandledCArraySize Text
  | NegativeCArraySize Integer
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
  TypeParseError s e -> "Failed to parse `" <> s <> "`: \n" <> e
  TypeNameParseError s e -> "Failed to create C identifier from `" <> s <> "`: \n" <> e
  UnhandledCType c -> "Unhandled C Type: " <> c
  UnhandledCTypeSpecifier c -> "Unhandled C Type specifier: " <> c
  UnhandledCArraySize c -> "Unhandled C array size: " <> c
  NegativeCArraySize c -> "Negative C array size: " <> showText c
  Other e -> e

(<+>) :: Text -> Text -> Text
a <+> b = a <> " " <> b

showText :: Show a => a -> Text
showText = pack . show

