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

-- No documentation found for TopLevel "VkExternalMemoryFeatureFlags"
type VkExternalMemoryFeatureFlags = VkExternalMemoryFeatureFlagBits

data VkExternalMemoryHandleTypeFlagBits

-- No documentation found for TopLevel "VkExternalMemoryHandleTypeFlags"
type VkExternalMemoryHandleTypeFlags = VkExternalMemoryHandleTypeFlagBits

data VkExternalMemoryProperties

data VkPhysicalDeviceExternalBufferInfo

data VkPhysicalDeviceExternalImageFormatInfo

data VkPhysicalDeviceIDProperties

type FN_vkGetPhysicalDeviceExternalBufferProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalBufferInfo" ::: Ptr VkPhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr VkExternalBufferProperties) -> IO ()
type PFN_vkGetPhysicalDeviceExternalBufferProperties = FunPtr FN_vkGetPhysicalDeviceExternalBufferProperties
