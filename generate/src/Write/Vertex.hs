module Write.Vertex where

import           Spec.Graph
import           Text.PrettyPrint.Leijen.Text hiding ((<$>))
import           Write.Command
import           Write.Constant
import           Write.Enum
import           Write.Type.Base
import           Write.Type.Bitmask
import           Write.Type.Define
import           Write.Type.FuncPointer
import           Write.Type.Handle
import           Write.Type.Struct
import           Write.WriteMonad

writeVertices :: [SourceEntity] -> Write Doc
writeVertices = fmap vcat . traverse writeVertex

writeVertex :: SourceEntity -> Write Doc
writeVertex se =
  case se of
    AnInclude _ -> pure empty
    ADefine define -> writeDefine define
    ABaseType baseType -> writeBaseType baseType
    APlatformType _ -> pure empty
    ABitmaskType bitmaskType bitmaskMay -> writeBitmaskType bitmaskType bitmaskMay
    AHandleType handleType -> writeHandleType handleType
    AnEnumType _ -> pure empty -- handled by enum
    AFuncPointerType funcPointerType -> writeFuncPointerType funcPointerType
    AStructType structType -> writeStructType structType
    AUnionType unionType -> writeUnionType unionType
    ACommand command -> writeCommand command
    AnEnum enum -> writeEnum enum
    ABitmask _ -> pure empty -- Handled by bitmasktype
    AConstant constant -> writeConstant constant
