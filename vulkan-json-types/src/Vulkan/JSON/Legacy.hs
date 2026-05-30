module Vulkan.JSON.Legacy (Legacy (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)
import           Vulkan.JSON.Ref      (Ref)

data Legacy = Legacy
  { link         :: Text
  , version      :: Maybe Ref
  , extensions   :: [Text]
  , supersededBy :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''Legacy)
