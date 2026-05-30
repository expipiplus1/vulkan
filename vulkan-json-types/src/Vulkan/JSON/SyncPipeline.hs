module Vulkan.JSON.SyncPipeline (SyncPipeline (..)) where

import           Data.Aeson.TH                  (deriveJSON)
import           Data.Text                      (Text)
import           GHC.Generics                   (Generic)

import           Vulkan.JSON.Internal           (vkAesonOptions)
import           Vulkan.JSON.SyncPipelineStage  (SyncPipelineStage)

data SyncPipeline = SyncPipeline
  { name    :: Text
  , depends :: [Text]
  , stages  :: [SyncPipelineStage]
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''SyncPipeline)
