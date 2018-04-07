module Spec.Spec where

import           Prelude           hiding (Enum)
import           Spec.Bitmask
import           Spec.Command
import           Spec.Constant
import           Spec.Enum
import           Spec.Extension
import           Spec.ExtensionTag
import           Spec.Platform
import           Spec.Section
import           Spec.Tag
import           Spec.Type
import           Spec.VendorID

-- | The Specification in a format which closely resembles the xml specification
data Spec = Spec { sCopyright  :: String
                 , sVendorIDs  :: [VendorID]
                 , sPlatforms  :: [Platform]
                 , sTags       :: [Tag]
                 , sTypes      :: [TypeDecl]
                 , sConstants  :: [Constant]
                 , sEnums      :: [Enum]
                 , sBitmasks   :: [Bitmask]
                 , sCommands   :: [Command]
                 , sSections   :: [Section]
                 , sExtensions :: [Extension]
                 }
  deriving (Show)

getSpecExtensionTags :: Spec -> [ExtensionTag]
getSpecExtensionTags spec =
  (tName <$> sTags spec) ++ (viName <$> sVendorIDs spec)
