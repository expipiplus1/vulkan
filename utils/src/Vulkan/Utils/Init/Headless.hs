-- | Init helpers for headless applications — no window, no surface, no
-- window-system instance extensions.
module Vulkan.Utils.Init.Headless
  ( withInstance
  ) where

import           Control.Monad.Trans.Resource ( MonadResource )
import           Vulkan.Core10                ( ApplicationInfo, Instance )
import           Vulkan.Requirement           ( InstanceRequirement )
import           Vulkan.Utils.Init            ( withVulkanInstance )

-- | Build a Vulkan 'Instance' for a headless application. Equivalent to
-- @'withVulkanInstance' 'mempty'@.
withInstance
  :: MonadResource m
  => Maybe ApplicationInfo
  -> [InstanceRequirement]
  -> [InstanceRequirement]
  -> m Instance
withInstance = withVulkanInstance mempty
