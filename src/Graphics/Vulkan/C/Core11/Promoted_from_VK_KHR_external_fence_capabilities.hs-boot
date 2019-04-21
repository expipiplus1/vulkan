{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceFeatureFlagBits
  , VkExternalFenceFeatureFlags
  , VkExternalFenceHandleTypeFlagBits
  , VkExternalFenceHandleTypeFlags
  , VkExternalFenceProperties
  , VkPhysicalDeviceExternalFenceInfo
  , FN_vkGetPhysicalDeviceExternalFenceProperties
  , PFN_vkGetPhysicalDeviceExternalFenceProperties
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


data VkExternalFenceFeatureFlagBits

-- | VkExternalFenceFeatureFlags - Bitmask of VkExternalFenceFeatureFlagBits
--
-- = Description
--
-- 'VkExternalFenceFeatureFlags' is a bitmask type for setting a mask of
-- zero or more 'VkExternalFenceFeatureFlagBits'.
--
-- = See Also
--
-- 'VkExternalFenceFeatureFlagBits', 'VkExternalFenceProperties'
type VkExternalFenceFeatureFlags = VkExternalFenceFeatureFlagBits

data VkExternalFenceHandleTypeFlagBits

-- | VkExternalFenceHandleTypeFlags - Bitmask of
-- VkExternalFenceHandleTypeFlagBits
--
-- = Description
--
-- 'VkExternalFenceHandleTypeFlags' is a bitmask type for setting a mask of
-- zero or more 'VkExternalFenceHandleTypeFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo',
-- 'VkExternalFenceHandleTypeFlagBits', 'VkExternalFenceProperties'
type VkExternalFenceHandleTypeFlags = VkExternalFenceHandleTypeFlagBits

data VkExternalFenceProperties

data VkPhysicalDeviceExternalFenceInfo

type FN_vkGetPhysicalDeviceExternalFenceProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalFenceInfo" ::: Ptr VkPhysicalDeviceExternalFenceInfo) -> ("pExternalFenceProperties" ::: Ptr VkExternalFenceProperties) -> IO ()
type PFN_vkGetPhysicalDeviceExternalFenceProperties = FunPtr FN_vkGetPhysicalDeviceExternalFenceProperties
