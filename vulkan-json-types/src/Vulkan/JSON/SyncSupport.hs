module Vulkan.JSON.SyncSupport (SyncSupport (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Flag     (Flag)
import           Vulkan.JSON.Internal (vkAesonOptions)

data SyncSupport = SyncSupport
  { queues :: Maybe [Text]
  , stages :: Maybe [Flag]
  , max    :: Bool
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''SyncSupport)
