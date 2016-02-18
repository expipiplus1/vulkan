module Spec where

import Spec.Bitmask
import Spec.Command
import Spec.Enum
import Prelude hiding (Enum)

data Spec = Spec { sEnums :: [Enum]
                 , sBitmasks :: [Bitmask]
                 , sCommands :: [Command]
                 }
  deriving (Show)
