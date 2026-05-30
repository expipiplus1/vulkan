module Vulkan.JSON.SyncEquivalent (SyncEquivalent (..)) where

import           Data.Aeson.TH        (deriveJSON)
import           GHC.Generics         (Generic)

import           Vulkan.JSON.Flag     (Flag)
import           Vulkan.JSON.Internal (vkAesonOptions)

data SyncEquivalent = SyncEquivalent
  { stages   :: Maybe [Flag]
  , accesses :: Maybe [Flag]
  , max      :: Bool
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''SyncEquivalent)
