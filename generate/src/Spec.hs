module Spec where

import Spec.Bitmask
import Spec.Command
import Spec.Enum
import Spec.Type
import Prelude hiding (Enum)

data Spec = Spec { sTypes :: [TypeDecl]
                 , sEnums :: [Enum]
                 , sBitmasks :: [Bitmask]
                 , sCommands :: [Command]
                 , sCopyright :: String
                 }
  deriving (Show)
