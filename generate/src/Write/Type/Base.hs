{-# LANGUAGE QuasiQuotes #-}

module Write.Type.Base
  ( writeBaseType
  ) where

import           Spec.Type
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.TypeConverter
import           Write.WriteMonad

isTransparentBaseType :: BaseType -> Bool
isTransparentBaseType bt = btName bt `elem` ["VkFlags"]

writeBaseType :: BaseType -> Write Doc
writeBaseType bt = if isTransparentBaseType bt
                     then writeTransparentBaseType bt
                     else writeNewBaseType bt

writeTransparentBaseType :: BaseType -> Write Doc
writeTransparentBaseType bt = do
  hsType <- cTypeToHsTypeString (btCType bt)
  pure [qc|type {btName bt} = {hsType}
|]

writeNewBaseType :: BaseType -> Write Doc
writeNewBaseType bt = do
  doesDeriveStorable
  hsType <- cTypeToHsTypeString (btCType bt)
  pure [qc|newtype {btName bt} = {btName bt} {hsType}
  deriving (Eq, Ord, Storable)
|]

