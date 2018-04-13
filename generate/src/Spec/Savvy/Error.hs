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
  | MismatchTypeName Text Text
  | SizingBadType Text
  | AligningBadType Text
  | UnknownType Text
  | UnknownTypeSize Text
  | UnknownTypeAlignment Text
  | UnknownConstantValue Text
  | UnableToReadValue Text
  | UnhandledSubtraction Text
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
    "A bitmask isn't backed by VkFlags, backed instead by: " <+> t
  TypeParseError s e -> "Failed to parse `" <> s <> "`: \n" <> e
  TypeNameParseError s e ->
    "Failed to create C identifier from `" <> s <> "`: \n" <> e
  UnhandledCType          c -> "Unhandled C Type:" <+> c
  UnhandledCTypeSpecifier c -> "Unhandled C Type specifier:" <+> c
  UnhandledCArraySize     c -> "Unhandled C array size:" <+> c
  NegativeCArraySize      c -> "Negative C array size:" <+> showText c
  MismatchTypeName cId sName ->
    "C Identifier and Spec name mismatch:" <+> cId <+> "vs" <+> sName
  SizingBadType t -> "Trying to get the size of an unhandled type:" <+> t
  AligningBadType t ->
    "Trying to get the alignment of an unhandled type:" <+> t
  UnknownType     t -> "Trying to lookup an unknown type named:" <+> t
  UnknownTypeSize t -> "Trying to get the size of an unknown type named:" <+> t
  UnknownTypeAlignment t ->
    "Trying to get the alignment of an unknown type named:" <+> t
  UnknownConstantValue c ->
    "Trying to get the value of an unknown constant named:" <+> c
  UnableToReadValue    c -> "Unable to read value: " <+> c
  UnhandledSubtraction d -> "Unable to subtract (extend 'subtracted'): " <+> d
  Other                e -> e

(<+>) :: Text -> Text -> Text
a <+> b = a <> " " <> b

showText :: Show a => a -> Text
showText = pack . show

