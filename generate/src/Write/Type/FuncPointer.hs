{-# LANGUAGE QuasiQuotes #-}

module Write.Type.FuncPointer
  ( writeFuncPointerTypes
  ) where

import Spec.Type
import Text.InterpolatedString.Perl6
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.TypeConverter

writeFuncPointerTypes :: TypeConverter -> [FuncPointerType] -> String
writeFuncPointerTypes tc fpts = [qc|-- * FuncPointer Types

{vcat $ writeFuncPointerType tc <$> fpts}|] 

writeFuncPointerType :: TypeConverter -> FuncPointerType -> Doc
writeFuncPointerType tc fpt = [qc|type {fptName fpt} = {tc (fptCType fpt)}
|]

