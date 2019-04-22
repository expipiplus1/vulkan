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
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryFeatureFlagsNV',
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
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryFeatureFlagBitsNV'
type ExternalMemoryFeatureFlagsNV = ExternalMemoryFeatureFlagBitsNV

-- | VkExternalMemoryHandleTypeFlagBitsNV - Bitmask specifying external
-- memory handle types
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagsNV'
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
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory.VkExportMemoryAllocateInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalMemoryHandleTypeFlagBitsNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.VkImportMemoryWin32HandleInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.vkGetMemoryWin32HandleNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV'
type ExternalMemoryHandleTypeFlagsNV = ExternalMemoryHandleTypeFlagBitsNV
