{-| Init helpers for headless applications — no window, no surface, no
window-system instance extensions.
-}
module Vulkan.Utils.Init.Headless
  ( allocateInstance
  ) where

import Control.Monad.Trans.Resource (MonadResource)
import Vulkan.Core10 (ApplicationInfo, Instance)
import Vulkan.Requirement (InstanceRequirement)
import Vulkan.Utils.Initialization (allocateVulkanInstance)

{- | Build a Vulkan 'Instance' for a headless application. Equivalent to
@'allocateVulkanInstance' 'mempty'@.
-}
allocateInstance
  :: (MonadResource m)
  => Maybe ApplicationInfo
  -> [InstanceRequirement]
  -> [InstanceRequirement]
  -> m Instance
allocateInstance = allocateVulkanInstance mempty
