module Spec.Spec where

import           Prelude        hiding (Enum)
import           Spec.Bitmask
import           Spec.Command
import           Spec.Constant
import           Spec.Enum
import           Spec.Extension
import           Spec.Section
import           Spec.Tag
import           Spec.Type
import           Spec.VendorID

-- | The Specification in a format which closely resembles the xml specification
data Spec = Spec { sTypes      :: [TypeDecl]
                 , sConstants  :: [Constant]
                 , sEnums      :: [Enum]
                 , sBitmasks   :: [Bitmask]
                 , sCommands   :: [Command]
                 , sCopyright  :: String
                 , sSections   :: [Section]
                 , sExtensions :: [Extension]
                 , sTags       :: [Tag]
                 , sVendorIDs  :: [VendorID]
                 }
  deriving (Show)

getSpecExtensionTags :: Spec -> [String]
getSpecExtensionTags spec =
  (tName <$> sTags spec) ++ (viName <$> sVendorIDs spec)
