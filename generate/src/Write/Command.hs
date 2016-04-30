{-# LANGUAGE QuasiQuotes #-}

module Write.Command
  ( writeCommand
  ) where

import           Language.C.Types
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.Syntax
import           Spec.Command
import           Spec.Type                     (CType)
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.TypeConverter
import           Write.WriteMonad

writeCommand :: Command -> Write Doc
writeCommand c = do
  commandType <- writeCommandType c
  pure [qc|-- ** {cHsName c}
foreign import ccall "{unCIdentifier $ cSymbol c}" {cHsName c} ::
  {commandType}
|]

writeCommandType :: Command -> Write String
writeCommandType c = do
  hsReturnType <- (simpleCon "IO" `TyApp`) <$> cTypeToHsType (cReturnType c)
  hsParameterTypes <- traverse (cTypeToHsType . lowerArrayToPointer)
                              (pType <$> cParameters c)
  let hsType = foldr TyFun hsReturnType hsParameterTypes
  pure $ prettyPrint hsType

lowerArrayToPointer :: CType -> CType
lowerArrayToPointer cType =
  case cType of
    Array _ t -> Ptr [] t
    t -> t
