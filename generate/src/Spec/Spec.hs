module Spec.Spec where

import Spec.Bitmask
import Spec.Command
import Spec.Constant
import Spec.Enum
import Spec.Extension
import Spec.Section
import Spec.Type
import Prelude hiding (Enum)

-- | The Specification in a format which closely resembles the xml specification
data Spec = Spec { sTypes :: [TypeDecl]
                 , sConstants :: [Constant]
                 , sEnums :: [Enum]
                 , sBitmasks :: [Bitmask]
                 , sCommands :: [Command]
                 , sCopyright :: String
                 , sSections :: [Section]
                 , sExtensions :: [Extension]
                 }
  deriving (Show)
