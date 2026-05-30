module Vulkan.JSON.VideoProfiles (VideoProfiles (..)) where

import           Data.Aeson.TH                   (deriveJSON)
import           Data.Map.Strict                 (Map)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

import           Vulkan.JSON.Internal            (vkAesonOptions)
import           Vulkan.JSON.VideoProfileMember  (VideoProfileMember)

data VideoProfiles = VideoProfiles
  { name    :: Text
  , members :: Map Text VideoProfileMember
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''VideoProfiles)
