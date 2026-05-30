module Vulkan.JSON.Version (Version (..)) where

import           Data.Aeson.TH                   (deriveJSON)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

import           Vulkan.JSON.FeatureRequirement  (FeatureRequirement)
import           Vulkan.JSON.Internal            (vkAesonOptions)

data Version = Version
  { name               :: Text
  , nameString         :: Text
  , nameApi            :: Text
  , featureRequirement :: [FeatureRequirement]
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Version)
