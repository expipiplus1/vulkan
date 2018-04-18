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
  | DefineError Text
  | PreprocessorError Text
  | MultipleExportError1 Text [Text]
  | MultipleExportError2 Text [(Text,Text)]
  | UnexportedNameWriteElement Text Text
  | UnexportedName Text
  | UnknownFeature Text
  | UnableToParseHeaderVersion Text
  | ModuleWithoutExports Text
  | UnknownAliasTarget Text Text
  | AliasLoop [Text]
  | UnknownExtendedEnum Text
  | HandleToNonPointerType Text
  | RequiredExportMissing Text [Text]
  | UnknownPlatform Text
  | Other Text
    -- ^ Used for testing in development
  deriving (Show)

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
  DefineError          e -> "Error parsing preprocessor defines:" <+> e
  PreprocessorError    e -> "Error running preprocessor:" <+> e
  MultipleExportError1 e ms ->
    "Symbol provided by more than one WriteElement: "
      <+> e
      <+> "from"
      <+> showText ms
  MultipleExportError2 e ms ->
    "Symbol exported from more than one module: "
      <+> e
      <+> "from"
      <+> showText ms
  UnexportedNameWriteElement e s ->
    "Symbol exported from no WriteElement: "
      <+> e
      <+> "(Required by ModuleSeed: "
      <+> s
      <>  ")"
  UnexportedName             e -> "Symbol exported from no module: " <+> e
  UnknownFeature             e -> "Couldn't find feature named:" <+> e
  UnableToParseHeaderVersion t -> "Unable to parse VK_HEADER_VERSION:" <+> t
  ModuleWithoutExports       m -> "Module has no exports:" <+> m
  UnknownAliasTarget a t ->
    "Unable to find target for alias: name: " <+> a <+> " alias target:" <+> t
  AliasLoop              as -> "Loop in alias definitions:" <+> showText as
  UnknownExtendedEnum    e  -> "Can't find enum to extend:" <+> e
  HandleToNonPointerType h  -> "Handle to non pointer type" <+> h
  RequiredExportMissing e ns ->
    "Required Symbol exported by no modules:"
      <+> e
      <+> "Required by WriteElements: "
      <+> showText ns
  UnknownPlatform p -> "Unknown platform" <+> p
  Other e -> e

(<+>) :: Text -> Text -> Text
a <+> b = a <> " " <> b

showText :: Show a => a -> Text
showText = pack . show

