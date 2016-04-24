module Spec.Pretty
  ( prettifySpec
  ) where

import           Data.Maybe        (catMaybes)
import           Spec.Bitmask
import           Spec.Command
import           Spec.Constant
import           Spec.Enum
import           Spec.ExtensionTag
import           Spec.Spec
import           Spec.Tag
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
    tags = (tName <$> sTags spec) ++ (catMaybes $ stringToExtensionTag <$> ["KHR", "EXT"])
    name = fst . breakNameTag tags . dropVK

    nameType (ADefine d) = ADefine d { dHsName = camelCase_ . name $ dHsName d }
    nameType (AStructType st) = AStructType st { stHsName = name $ stHsName st
                                               , stMembers = nameStructMember <$> stMembers st
                                               }
    nameType (AUnionType ut) = AUnionType ut { utHsName = name $ utHsName ut
                                             , utMembers = nameStructMember <$> utMembers ut
                                             }
    nameType (AHandleType h) = AHandleType h { htHsName = name $ htHsName h }
    nameType (AnEnumType h) = AnEnumType h { etHsName = name $ etHsName h }
    nameType (ABitmaskType h) = ABitmaskType h { bmtHsName = name $ bmtHsName h }
    nameType (ABaseType h) = ABaseType h { btHsName = name $ btHsName h }
    nameType t = t
    nameStructMember sm = sm { smHsName = nameRecord $ smHsName sm
                             }

    nameRecord "type" = "_type"
    nameRecord "module" = "_module"
    nameRecord "alignment" = "_alignment"
    nameRecord n = n

    nameEnum e = e { eHsName = name $ eHsName e
                   , eElements = nameEnumElement <$> eElements e
                   }

    nameConstant c = c { Spec.Constant.cHsName = pascalCase_ . name $ Spec.Constant.cHsName c }

    nameEnumElement ee = ee { eeHsName = pascalCase_ . name $ eeHsName ee }

    nameBitmask bm = bm { bmValues = nameBitmaskValue <$> bmValues bm
                        , bmBitPositions = nameBitmaskBitPosition <$> bmBitPositions bm
                        }

    nameBitmaskValue bmv = bmv { bmvHsName = pascalCase_ . name $ bmvHsName bmv }
    nameBitmaskBitPosition bmbp = bmbp { bmbpHsName = pascalCase_ . name $ bmbpHsName bmbp }

    nameCommand c = c { Spec.Command.cHsName = lowerFirst . name $ Spec.Command.cHsName c
                      , cParameters = nameParameter <$> cParameters c
                      }
    nameParameter p = p
