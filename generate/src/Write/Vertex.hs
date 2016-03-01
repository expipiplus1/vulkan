module Write.Vertex where

import Data.List(find)
import Data.String
import Spec.Graph
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.Command
import Write.Enum
import Write.Constant
import Write.Type.Base
import Write.Type.Bitmask
import Write.Type.Define
import Write.Type.Handle
import Write.Type.Struct
import Write.Type.FuncPointer
import Write.Utils
import Write.WriteMonad

writeVertices :: [Vertex] -> Write Doc
writeVertices = fmap vcat . traverse writeVertex

writeVertex :: Vertex -> Write Doc
writeVertex v = 
  case vSourceEntity v of
    AnInclude include -> pure empty
    ADefine define -> writeDefine define
    ABaseType baseType -> writeBaseType baseType
    APlatformType platformType -> pure empty
    ABitmaskType bitmaskType -> 
      let bitmaskMay = do bitmaskName <- swapSuffix "Flags" "FlagBits" 
                                                    (vName v)
                          bitmaskVertex <- find ((== bitmaskName) . vName) 
                                                (vDependencies v)
                          vertexToBitmask bitmaskVertex
      in writeBitmaskType bitmaskType bitmaskMay
    AHandleType handleType -> writeHandleType handleType
    AnEnumType enumType -> pure empty -- handled by enum
    AFuncPointerType funcPointerType -> writeFuncPointerType funcPointerType
    AStructType structType -> writeStructType structType
    AUnionType unionType -> writeUnionType unionType
    ACommand command -> writeCommand command
    AnEnum enum -> writeEnum enum
    ABitmask bitmask -> pure empty -- Handled by bitmasktype
    AConstant constant -> writeConstant constant
    _ -> error "unhandled vertex type"
