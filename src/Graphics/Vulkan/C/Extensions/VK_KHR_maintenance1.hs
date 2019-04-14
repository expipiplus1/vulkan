{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_maintenance1
  ( VkCommandPoolTrimFlagsKHR
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkTrimCommandPoolKHR
#endif
  , pattern VK_ERROR_OUT_OF_POOL_MEMORY_KHR
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR
  , pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME
  , pattern VK_KHR_MAINTENANCE1_SPEC_VERSION
  , pattern VK_ERROR_OUT_OF_POOL_MEMORY
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  ) where

import Data.String
  ( IsString
  )



#if defined(EXPOSE_CORE11_COMMANDS)
import Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPool
  )
#endif
import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  , VkImageCreateFlagBits(..)
  )

#if defined(EXPOSE_CORE11_COMMANDS)
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags(..)
  , pattern VK_ERROR_OUT_OF_POOL_MEMORY
  , pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  )

#if defined(EXPOSE_CORE11_COMMANDS)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( vkTrimCommandPool
  )
#endif

#if defined(EXPOSE_CORE11_COMMANDS)
import Graphics.Vulkan.NamedType
  ( (:::)
  )
#endif


-- No documentation found for TopLevel "VkCommandPoolTrimFlagsKHR"
type VkCommandPoolTrimFlagsKHR = VkCommandPoolTrimFlags

#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkTrimCommandPoolKHR"
vkTrimCommandPoolKHR :: ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ()
vkTrimCommandPoolKHR = vkTrimCommandPool
#endif
-- No documentation found for TopLevel "VK_ERROR_OUT_OF_POOL_MEMORY_KHR"
pattern VK_ERROR_OUT_OF_POOL_MEMORY_KHR :: VkResult
pattern VK_ERROR_OUT_OF_POOL_MEMORY_KHR = VK_ERROR_OUT_OF_POOL_MEMORY
-- No documentation found for TopLevel "VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR"
pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT_KHR = VK_FORMAT_FEATURE_TRANSFER_DST_BIT
-- No documentation found for TopLevel "VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR"
pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT_KHR = VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
-- No documentation found for TopLevel "VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR"
pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT_KHR = VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
-- No documentation found for TopLevel "VK_KHR_MAINTENANCE1_EXTENSION_NAME"
pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_MAINTENANCE1_EXTENSION_NAME = "VK_KHR_maintenance1"
-- No documentation found for TopLevel "VK_KHR_MAINTENANCE1_SPEC_VERSION"
pattern VK_KHR_MAINTENANCE1_SPEC_VERSION :: Integral a => a
pattern VK_KHR_MAINTENANCE1_SPEC_VERSION = 2
