{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_maintenance1
  ( trimCommandPoolKHR
  , pattern ERROR_OUT_OF_POOL_MEMORY_KHR
  , pattern FORMAT_FEATURE_TRANSFER_DST_BIT_KHR
  , pattern FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR
  , pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR
  , pattern KHR_MAINTENANCE1_EXTENSION_NAME
  , pattern KHR_MAINTENANCE1_SPEC_VERSION
  , CommandPoolTrimFlagsKHR
  , pattern ERROR_OUT_OF_POOL_MEMORY
  , pattern FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  , VkImageCreateFlagBits(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_maintenance1
  ( pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME
  , pattern VK_KHR_MAINTENANCE1_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.CommandPool
  ( CommandPool
  )
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_OUT_OF_POOL_MEMORY
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , pattern FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1
  ( CommandPoolTrimFlags
  , trimCommandPool
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1
  ( CommandPoolTrimFlagsKHR
  )


trimCommandPoolKHR :: Device ->  CommandPool ->  CommandPoolTrimFlags ->  IO ()
trimCommandPoolKHR = trimCommandPool

-- No documentation found for TopLevel "ERROR_OUT_OF_POOL_MEMORY_KHR"
pattern ERROR_OUT_OF_POOL_MEMORY_KHR :: VkResult
pattern ERROR_OUT_OF_POOL_MEMORY_KHR = ERROR_OUT_OF_POOL_MEMORY

-- No documentation found for TopLevel "FORMAT_FEATURE_TRANSFER_DST_BIT_KHR"
pattern FORMAT_FEATURE_TRANSFER_DST_BIT_KHR :: VkFormatFeatureFlagBits
pattern FORMAT_FEATURE_TRANSFER_DST_BIT_KHR = FORMAT_FEATURE_TRANSFER_DST_BIT

-- No documentation found for TopLevel "FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR"
pattern FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR :: VkFormatFeatureFlagBits
pattern FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR = FORMAT_FEATURE_TRANSFER_SRC_BIT

-- No documentation found for TopLevel "IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR"
pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR :: VkImageCreateFlagBits
pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR = IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE1_EXTENSION_NAME"
pattern KHR_MAINTENANCE1_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_MAINTENANCE1_EXTENSION_NAME = VK_KHR_MAINTENANCE1_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE1_SPEC_VERSION"
pattern KHR_MAINTENANCE1_SPEC_VERSION :: Integral a => a
pattern KHR_MAINTENANCE1_SPEC_VERSION = VK_KHR_MAINTENANCE1_SPEC_VERSION
