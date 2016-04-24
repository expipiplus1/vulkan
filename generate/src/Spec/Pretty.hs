module Spec.Pretty
  ( prettifySpec
  ) where

import           Spec.Bitmask
import           Spec.Command
import           Spec.Constant
import           Spec.Enum
import           Spec.Spec
import           Spec.Type
import           Write.Utils

prettifySpec :: Spec -> Spec
prettifySpec spec = spec { sTypes = nameType <$> sTypes spec
                         , sEnums = nameEnum <$> sEnums spec
                         , sConstants = nameConstant <$> sConstants spec
                         , sBitmasks = nameBitmask <$> sBitmasks spec
                         , sCommands = nameCommand <$> sCommands spec
                         }
  where
    nameType (ADefine d) = ADefine d { dHsName = camelCase_ . dropVK $ dHsName d }
    nameType (AStructType st) = AStructType st { stHsName = dropVK $ stHsName st
                                               , stMembers = nameStructMember <$> stMembers st
                                               }
    nameType (AUnionType ut) = AUnionType ut { utHsName = dropVK $ utHsName ut
                                             , utMembers = nameStructMember <$> utMembers ut
                                             }
    nameType (AHandleType h) = AHandleType h { htHsName = dropVK $ htHsName h }
    nameType (AnEnumType h) = AnEnumType h { etHsName = dropVK $ etHsName h }
    nameType (ABitmaskType h) = ABitmaskType h { bmtHsName = dropVK $ bmtHsName h }
    nameType (ABaseType h) = ABaseType h { btHsName = dropVK $ btHsName h }
    nameType t = t
    nameStructMember sm = sm { smHsName = nameRecord $ smHsName sm
                             }

    nameRecord "type" = "_type"
    nameRecord "module" = "_module"
    nameRecord "alignment" = "_alignment"
    nameRecord n = n

    nameEnum e = e { eHsName = dropVK $ eHsName e
                   , eElements = nameEnumElement <$> eElements e
                   }

    nameConstant c = c { Spec.Constant.cHsName = pascalCase_ . dropVK $ Spec.Constant.cHsName c }

    nameEnumElement ee = ee { eeHsName = pascalCase_ . dropVK $ eeHsName ee }

    nameBitmask bm = bm { bmValues = nameBitmaskValue <$> bmValues bm
                        , bmBitPositions = nameBitmaskBitPosition <$> bmBitPositions bm
                        }

    nameBitmaskValue bmv = bmv { bmvHsName = pascalCase_ . dropVK $ bmvHsName bmv }
    nameBitmaskBitPosition bmbp = bmbp { bmbpHsName = pascalCase_ . dropVK $ bmbpHsName bmbp }

    nameCommand c = c { Spec.Command.cHsName = lowerFirst . dropVK $ Spec.Command.cHsName c
                      , cParameters = nameParameter <$> cParameters c
                      }
    nameParameter p = p
