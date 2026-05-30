module Vulkan.JSON.Extension (Extension (..)) where

import           Data.Aeson.TH                  (deriveJSON)
import           Data.Map.Strict                (Map)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)

import           Vulkan.JSON.EnumField          (EnumField)
import           Vulkan.JSON.FeatureRequirement (FeatureRequirement)
import           Vulkan.JSON.Flag               (Flag)
import           Vulkan.JSON.Internal           (vkAesonOptions)
import           Vulkan.JSON.Ref                (Ref)

data Extension = Extension
  { name               :: Text
  , nameString         :: Text
  , specVersion        :: Text
  , specVersionValue   :: Int
  , instance'          :: Bool
  , device             :: Bool
  , depends            :: Maybe Text
  , vendorTag          :: Text
  , platform           :: Maybe Text
  , protect            :: Maybe Text
  , provisional        :: Bool
  , promotedTo         :: Maybe Text
  , deprecatedBy       :: Maybe Text
  , obsoletedBy        :: Maybe Text
  , specialUse         :: [Text]
  , featureRequirement :: [FeatureRequirement]
  , ratified           :: Bool
  , handles            :: [Ref]
  , commands           :: [Ref]
  , structs            :: [Ref]
  , enums              :: [Ref]
  , bitmasks           :: [Ref]
  , flags              :: [Ref]
  , enumFields         :: Map Text [EnumField]
  , flagBits           :: Map Text [Flag]
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Extension)
