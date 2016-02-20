{-# LANGUAGE QuasiQuotes #-}

module Write.Type.Base
  ( writeBaseTypes
  ) where

import Spec.Type
import Text.InterpolatedString.Perl6
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.TypeConverter

isTransparentBaseType :: BaseType -> Bool
isTransparentBaseType bt = btName bt `elem` ["VkFlags"]

writeBaseTypes :: TypeConverter -> [BaseType] -> String
writeBaseTypes tc bts = [qc|-- * Base Types

{vcat $ writeBaseType tc <$> bts}|] 

writeBaseType :: TypeConverter -> BaseType -> Doc
writeBaseType tc bt = if isTransparentBaseType bt
                        then writeTransparentBaseType tc bt
                        else writeNewBaseType tc bt

writeTransparentBaseType :: TypeConverter -> BaseType -> Doc
writeTransparentBaseType tc bt = [qc|type {btName bt} = {tc (btCType bt)}
|]

writeNewBaseType :: TypeConverter -> BaseType -> Doc
writeNewBaseType tc bt = [qc|newtype {btName bt} = {btName bt} {tc (btCType bt)}
  deriving (Eq, Storable)
|]

