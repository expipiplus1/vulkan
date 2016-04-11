{-# LANGUAGE QuasiQuotes #-}

module Write.Type.FuncPointer
  ( writeFuncPointerType
  ) where

import           Spec.Type
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.TypeConverter
import           Write.WriteMonad

writeFuncPointerType :: FuncPointerType -> Write Doc
writeFuncPointerType fpt = do
  hsType <- cTypeToHsTypeString (fptCType fpt)
  pure [qc|type {fptHsName fpt} = {hsType}
|]

