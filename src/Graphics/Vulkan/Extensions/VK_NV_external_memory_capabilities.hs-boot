{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( ExternalMemoryFeatureFlagBitsNV
  , ExternalMemoryFeatureFlagsNV
  , ExternalMemoryHandleTypeFlagBitsNV
  , ExternalMemoryHandleTypeFlagsNV
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalMemoryFeatureFlagBitsNV
  , VkExternalMemoryHandleTypeFlagBitsNV
  )


-- | VkExternalMemoryFeatureFlagBitsNV - Bitmask specifying external memory
-- features
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV'
type ExternalMemoryFeatureFlagBitsNV = VkExternalMemoryFeatureFlagBitsNV

-- | VkExternalMemoryFeatureFlagsNV - Bitmask of
-- VkExternalMemoryFeatureFlagBitsNV
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryFeatureFlagsNV'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryFeatureFlagBitsNV'.
--
-- = See Also
--
-- No cross-references are available
type ExternalMemoryFeatureFlagsNV = ExternalMemoryFeatureFlagBitsNV

-- | VkExternalMemoryHandleTypeFlagBitsNV - Bitmask specifying external
-- memory handle types
--
-- = See Also
--
-- No cross-references are available
type ExternalMemoryHandleTypeFlagBitsNV = VkExternalMemoryHandleTypeFlagBitsNV

-- | VkExternalMemoryHandleTypeFlagsNV - Bitmask of
-- VkExternalMemoryHandleTypeFlagBitsNV
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagsNV'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV'.
--
-- = See Also
--
-- No cross-references are available
type ExternalMemoryHandleTypeFlagsNV = ExternalMemoryHandleTypeFlagBitsNV
