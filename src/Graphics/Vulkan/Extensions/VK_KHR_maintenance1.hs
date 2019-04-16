{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_maintenance1
  ( trimCommandPoolKHR
  , pattern VK_KHR_MAINTENANCE1_SPEC_VERSION
  , pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME
  , pattern VK_ERROR_OUT_OF_POOL_MEMORY_KHR
  , pattern VK_ERROR_OUT_OF_POOL_MEMORY
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  , CommandPoolTrimFlagsKHR
  ) where




import Graphics.Vulkan.Core10.CommandPool
  ( CommandPool
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1
  ( CommandPoolTrimFlags
  , trimCommandPool
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( pattern VK_ERROR_OUT_OF_POOL_MEMORY
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_maintenance1
  ( pattern VK_ERROR_OUT_OF_POOL_MEMORY_KHR
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR
  , pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME
  , pattern VK_KHR_MAINTENANCE1_SPEC_VERSION
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1
  ( CommandPoolTrimFlagsKHR
  )


trimCommandPoolKHR :: Device ->  CommandPool ->  CommandPoolTrimFlags ->  IO ()
trimCommandPoolKHR = trimCommandPool
