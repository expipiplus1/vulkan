module Spec where

import Spec.Bitmask
import Spec.Command
import Spec.Constant
import Spec.Enum
import Spec.Section
import Spec.Type
import Prelude hiding (Enum)

data Spec = Spec { sTypes :: [TypeDecl]
                 , sConstants :: [Constant]
                 , sEnums :: [Enum]
                 , sBitmasks :: [Bitmask]
                 , sCommands :: [Command]
                 , sCopyright :: String
                 , sSections :: Section
                 }
  deriving (Show)
