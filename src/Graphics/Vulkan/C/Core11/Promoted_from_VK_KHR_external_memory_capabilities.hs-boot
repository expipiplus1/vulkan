{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalBufferProperties
  , VkExternalImageFormatProperties
  , VkExternalMemoryFeatureFlagBits
  , VkExternalMemoryFeatureFlags
  , VkExternalMemoryHandleTypeFlagBits
  , VkExternalMemoryHandleTypeFlags
  , VkExternalMemoryProperties
  , VkPhysicalDeviceExternalBufferInfo
  , VkPhysicalDeviceExternalImageFormatInfo
  , VkPhysicalDeviceIDProperties
  , FN_vkGetPhysicalDeviceExternalBufferProperties
  , PFN_vkGetPhysicalDeviceExternalBufferProperties
  ) where

import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )


data VkExternalBufferProperties

data VkExternalImageFormatProperties

data VkExternalMemoryFeatureFlagBits

-- | VkExternalMemoryFeatureFlags - Bitmask of
-- VkExternalMemoryFeatureFlagBits
--
-- = Description
--
-- 'VkExternalMemoryFeatureFlags' is a bitmask type for setting a mask of
-- zero or more 'VkExternalMemoryFeatureFlagBits'.
--
-- = See Also
--
-- 'VkExternalMemoryFeatureFlagBits', 'VkExternalMemoryProperties'
type VkExternalMemoryFeatureFlags = VkExternalMemoryFeatureFlagBits

data VkExternalMemoryHandleTypeFlagBits

-- | VkExternalMemoryHandleTypeFlags - Bitmask of
-- VkExternalMemoryHandleTypeFlagBits
--
-- = Description
--
-- 'VkExternalMemoryHandleTypeFlags' is a bitmask type for setting a mask
-- of zero or more 'VkExternalMemoryHandleTypeFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo',
-- 'VkExternalMemoryHandleTypeFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo',
-- 'VkExternalMemoryProperties'
type VkExternalMemoryHandleTypeFlags = VkExternalMemoryHandleTypeFlagBits

data VkExternalMemoryProperties

data VkPhysicalDeviceExternalBufferInfo

data VkPhysicalDeviceExternalImageFormatInfo

data VkPhysicalDeviceIDProperties

type FN_vkGetPhysicalDeviceExternalBufferProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalBufferInfo" ::: Ptr VkPhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr VkExternalBufferProperties) -> IO ()
type PFN_vkGetPhysicalDeviceExternalBufferProperties = FunPtr FN_vkGetPhysicalDeviceExternalBufferProperties
