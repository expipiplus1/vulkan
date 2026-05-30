module Vulkan.JSON.SyncPipelineStage (SyncPipelineStage (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Internal (vkAesonOptions)

data SyncPipelineStage = SyncPipelineStage
  { order  :: Maybe Text
  , before :: Maybe Text
  , after  :: Maybe Text
  , value  :: Text
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''SyncPipelineStage)
