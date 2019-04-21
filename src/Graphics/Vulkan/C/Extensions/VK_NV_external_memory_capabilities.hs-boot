{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalImageFormatPropertiesNV
  , VkExternalMemoryFeatureFlagBitsNV
  , VkExternalMemoryFeatureFlagsNV
  , VkExternalMemoryHandleTypeFlagBitsNV
  , VkExternalMemoryHandleTypeFlagsNV
  , FN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV
  , PFN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV
  ) where

import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkFormat
  , VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageTiling
  , VkImageType
  , VkImageCreateFlags
  , VkImageUsageFlags
  , VkPhysicalDevice
  )


data VkExternalImageFormatPropertiesNV

data VkExternalMemoryFeatureFlagBitsNV

-- | VkExternalMemoryFeatureFlagsNV - Bitmask of
-- VkExternalMemoryFeatureFlagBitsNV
--
-- = Description
--
-- 'VkExternalMemoryFeatureFlagsNV' is a bitmask type for setting a mask of
-- zero or more 'VkExternalMemoryFeatureFlagBitsNV'.
--
-- = See Also
--
-- No cross-references are available
type VkExternalMemoryFeatureFlagsNV = VkExternalMemoryFeatureFlagBitsNV

data VkExternalMemoryHandleTypeFlagBitsNV

-- | VkExternalMemoryHandleTypeFlagsNV - Bitmask of
-- VkExternalMemoryHandleTypeFlagBitsNV
--
-- = Description
--
-- 'VkExternalMemoryHandleTypeFlagsNV' is a bitmask type for setting a mask
-- of zero or more 'VkExternalMemoryHandleTypeFlagBitsNV'.
--
-- = See Also
--
-- No cross-references are available
type VkExternalMemoryHandleTypeFlagsNV = VkExternalMemoryHandleTypeFlagBitsNV

type FN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("externalHandleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pExternalImageFormatProperties" ::: Ptr VkExternalImageFormatPropertiesNV) -> IO VkResult
type PFN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV = FunPtr FN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV
