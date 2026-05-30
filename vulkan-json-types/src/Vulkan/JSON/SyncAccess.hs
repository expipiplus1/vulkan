module Vulkan.JSON.SyncAccess (SyncAccess (..)) where

import           Data.Aeson.TH               (deriveJSON)
import           GHC.Generics                (Generic)

import           Vulkan.JSON.Flag            (Flag)
import           Vulkan.JSON.Internal        (vkAesonOptions)
import           Vulkan.JSON.SyncEquivalent  (SyncEquivalent)
import           Vulkan.JSON.SyncSupport     (SyncSupport)

data SyncAccess = SyncAccess
  { flag       :: Flag
  , support    :: SyncSupport
  , equivalent :: SyncEquivalent
  }
  deriving stock (Eq, Show, Generic)

$(deriveJSON vkAesonOptions ''SyncAccess)
