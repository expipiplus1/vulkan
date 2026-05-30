module Vulkan.JSON.VideoProfileMember (VideoProfileMember (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Map.Strict      (Map)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data VideoProfileMember = VideoProfileMember
  { name   :: Text
  , values :: Map Text Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''VideoProfileMember)
