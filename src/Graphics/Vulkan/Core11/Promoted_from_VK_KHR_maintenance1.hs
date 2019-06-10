{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1
  ( CommandPoolTrimFlags
  , CommandPoolTrimFlagsKHR
  , trimCommandPool
  , pattern ERROR_OUT_OF_POOL_MEMORY
  , pattern FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  ) where




import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags(..)
  , vkTrimCommandPool
  )
import Graphics.Vulkan.Core10.CommandPool
  ( CommandPool
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_OUT_OF_POOL_MEMORY
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  )


-- No documentation found for TopLevel "CommandPoolTrimFlags"
type CommandPoolTrimFlags = VkCommandPoolTrimFlags


-- No complete pragma for CommandPoolTrimFlags as it has no patterns

-- No documentation found for TopLevel "CommandPoolTrimFlagsKHR"
type CommandPoolTrimFlagsKHR = CommandPoolTrimFlags


-- No documentation found for TopLevel "vkTrimCommandPool"
trimCommandPool :: Device ->  CommandPool ->  CommandPoolTrimFlags ->  IO ()
trimCommandPool = undefined {- {wrapped (pretty cName) :: Doc ()} -}
