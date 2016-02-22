{-# LANGUAGE QuasiQuotes #-}

module Write.Command
  ( writeCommands
  ) where

import Data.Maybe(fromMaybe)
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax
import Spec.Command
import Text.InterpolatedString.Perl6
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.Utils
import Write.TypeConverter
import Spec.Type(CType)
import Language.C.Types

writeCommands :: [Command] -> String
writeCommands cs = [qc|-- * Commands

{vcat $ writeCommand <$> cs}|] 

writeCommand :: Command -> Doc
writeCommand c = [qc|-- ** {cName c}
foreign import ccall "{cName c}" {cName c} :: 
  {writeCommandType c}
|]

writeCommandType :: Command -> String
writeCommandType c = prettyPrint hsType
  where hsType = foldr TyFun hsReturnType hsParameterTypes
        hsReturnType = simpleCon "IO" `TyApp` cTypeToHsType' (cReturnType c)
        hsParameterTypes = cTypeToHsType' . lowerArrayToPointer . pType <$> cParameters c
  
lowerArrayToPointer :: CType -> CType
lowerArrayToPointer cType = 
  case cType of
    Array _ t -> Ptr [] t
    t -> t
