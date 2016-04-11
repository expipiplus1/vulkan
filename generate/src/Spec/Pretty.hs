module Spec.Pretty
  ( prettifySpec
  ) where

import           Data.HashMap.Lazy   as M
import           Data.Maybe          (catMaybes)
import           Language.C.Types
import           Spec.Command
import           Spec.Spec
import           Spec.Type
import           Write.TypeConverter
import           Write.Utils
import           Write.WriteMonad

lowerArrayToPointer :: CType -> CType
lowerArrayToPointer cType =
  case cType of
    Array _ t -> Ptr [] t
    t -> t

prettifySpec :: Spec -> Spec
prettifySpec spec = spec { sTypes = types
                         , sCommands = commands
                         }
  where
    types = nameType <$> sTypes spec
    nameType (ADefine d) = ADefine d { dHsName = camelCase_ $ dHsName d }
    nameType (AStructType st) = AStructType st { stMembers = nameStructMember <$> stMembers st }
    nameType (AUnionType ut) = AUnionType ut { utMembers = nameStructMember <$> utMembers ut }
    nameType (AHandleType h) = AHandleType h { htHsName = dropVK $ htHsName h }
    nameType t = t
    nameStructMember sm = sm { smHsName = recordName $ smHsName sm
                             , smHsType = mapType $ smCType sm
                             }

    recordName "type" = "_type"
    recordName "module" = "_module"
    recordName "alignment" = "_alignment"
    recordName n = n

    typeNameMap = M.fromList $ catMaybes $ typeDeclTypeNameMap <$> types
    mapType t = fst $ runWrite undefined undefined (cTypeToHsTypeMap typeNameMap t)

    commands = nameCommand <$> sCommands spec
    nameCommand c = c { cHsReturnType = mapType $ cReturnType c
                      , cParameters = nameParameter <$> cParameters c
                      }
    nameParameter p = p { pHsType = mapType $ lowerArrayToPointer $ pType p
                        }
