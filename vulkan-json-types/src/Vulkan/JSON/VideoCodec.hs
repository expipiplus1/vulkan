module Vulkan.JSON.VideoCodec (VideoCodec (..)) where

import           Data.Aeson.TH               (deriveJSON)
import           Data.Map.Strict             (Map)
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)

import           Vulkan.JSON.Internal        (vkAesonOptions)
import           Vulkan.JSON.VideoFormat     (VideoFormat)
import           Vulkan.JSON.VideoProfiles   (VideoProfiles)

data VideoCodec = VideoCodec
  { name         :: Text
  , value        :: Maybe Text
  , profiles     :: Map Text VideoProfiles
  , capabilities :: Map Text Text
  , formats      :: Map Text VideoFormat
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''VideoCodec)
