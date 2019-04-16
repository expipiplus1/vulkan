{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1
  ( CommandPoolTrimFlags
  , CommandPoolTrimFlagsKHR
  , trimCommandPool
  , pattern VK_ERROR_OUT_OF_POOL_MEMORY
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  ) where

import qualified Graphics.Vulkan.C.Dynamic
  ( trimCommandPool
  )


import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags(..)
  )
import Graphics.Vulkan.Core10.CommandPool
  ( CommandPool
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( pattern VK_ERROR_OUT_OF_POOL_MEMORY
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  )


-- No documentation found for TopLevel "CommandPoolTrimFlags"
type CommandPoolTrimFlags = VkCommandPoolTrimFlags
-- No documentation found for TopLevel "CommandPoolTrimFlagsKHR"
type CommandPoolTrimFlagsKHR = CommandPoolTrimFlags

-- | Wrapper for 'vkTrimCommandPool'
trimCommandPool :: Device ->  CommandPool ->  CommandPoolTrimFlags ->  IO ()
trimCommandPool = \(Device device commandTable) -> \commandPool -> \flags -> Graphics.Vulkan.C.Dynamic.trimCommandPool commandTable device commandPool flags *> (pure ())
