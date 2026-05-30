module Vulkan.JSON.FeatureRequirement (FeatureRequirement (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data FeatureRequirement = FeatureRequirement
  { struct  :: Text
  , field   :: Text
  , depends :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''FeatureRequirement)
