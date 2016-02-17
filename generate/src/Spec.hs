module Spec where

import Spec.Enum
import Spec.Bitmask
import Prelude hiding (Enum)

data Spec = Spec { sEnums :: [Enum]
                 , sBitmasks :: [Bitmask]
                 }
  deriving (Show)
