{-# LANGUAGE QuasiQuotes #-}

module Write.Type.Handle
  ( writeHandleTypes
  ) where

import Language.C.Types as C
import Spec.Type
import Text.InterpolatedString.Perl6
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.TypeConverter

writeHandleTypes :: TypeConverter -> [HandleType] -> String
writeHandleTypes tc hts = [qc|-- * Handle Types

{vcat $ writeHandleType tc <$> hts}|] 

writeHandleType :: TypeConverter -> HandleType -> Doc
writeHandleType tc ht = 
  let cType = htCType ht
  in case cType of
       Ptr [] t@(TypeDef (Struct _)) -> writeDispatchableHandleType tc ht t
       t@(TypeDef (TypeName _)) -> writeNonDispatchableHandleType tc ht t
       t -> error ("Unhandled handle type " ++ show t ++
                   ", have more been added to the spec?")

writeDispatchableHandleType :: TypeConverter -> HandleType -> CType -> Doc
writeDispatchableHandleType tc ht t = [qc|data {tc t}
type {htName ht} = Ptr {tc t}
|]

writeNonDispatchableHandleType :: TypeConverter -> HandleType -> CType -> Doc
writeNonDispatchableHandleType tc ht t = [qc|newtype {htName ht} = {htName ht} {tc t}
  deriving (Eq, Storable)
|]
