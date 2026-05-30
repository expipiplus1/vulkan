module Vulkan.JSON.VideoRequiredCapabilities (VideoRequiredCapabilities (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data VideoRequiredCapabilities = VideoRequiredCapabilities
  { struct :: Text
  , member :: Text
  , value  :: Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''VideoRequiredCapabilities)
