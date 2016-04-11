{-# LANGUAGE QuasiQuotes #-}

module Write.Type.Handle
  ( writeHandleType
  ) where

import           Data.String
import           Language.C.Types              as C
import           Spec.Type
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.TypeConverter
import           Write.Utils
import           Write.WriteMonad

writeHandleType :: HandleType -> Write Doc
writeHandleType ht =
  let cType = htCType ht
  in case cType of
       Ptr [] t@(TypeDef (Struct _)) -> writeDispatchableHandleType ht t
       t@(TypeDef (TypeName _)) -> writeNonDispatchableHandleType ht t
       t -> error ("Unhandled handle type " ++ show t ++
                   ", have more been added to the spec?")

writeDispatchableHandleType :: HandleType -> CType -> Write Doc
writeDispatchableHandleType ht t = do
  tellRequiredName (ExternalName (ModuleName "Foreign.Ptr") "Ptr")
  hsType <- cTypeToHsTypeString t
  pure [qc|data {hsType}
type {htHsName ht} = Ptr {hsType}
|]

writeNonDispatchableHandleType :: HandleType -> CType -> Write Doc
writeNonDispatchableHandleType ht t = do
  doesDeriveStorable
  hsType <- cTypeToHsTypeString t
  boot <- isBoot
  let derivingString :: Doc
      derivingString = if boot
                         then [qc|
instance Eq {htHsName ht}
instance Storable {htHsName ht}|]
                         else fromString "deriving (Eq, Storable)"
  pure [qc|newtype {htHsName ht} = {htHsName ht} {hsType}
  {derivingString}
|]
