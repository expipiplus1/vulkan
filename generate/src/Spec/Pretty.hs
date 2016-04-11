module Spec.Pretty
  ( prettifySpec
  ) where

import           Spec.Command
import           Spec.Spec
import           Spec.Type
import           Write.Utils

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
                             }

    recordName "type" = "_type"
    recordName "module" = "_module"
    recordName "alignment" = "_alignment"
    recordName n = n

    commands = nameCommand <$> sCommands spec
    nameCommand c = c { cParameters = nameParameter <$> cParameters c
                      }
    nameParameter p = p
