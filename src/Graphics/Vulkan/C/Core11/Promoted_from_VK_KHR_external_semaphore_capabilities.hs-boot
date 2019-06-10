{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreFeatureFlagBits
  , VkExternalSemaphoreFeatureFlags
  , VkExternalSemaphoreHandleTypeFlagBits
  , VkExternalSemaphoreHandleTypeFlags
  , VkExternalSemaphoreProperties
  , VkPhysicalDeviceExternalSemaphoreInfo
  , FN_vkGetPhysicalDeviceExternalSemaphoreProperties
  , PFN_vkGetPhysicalDeviceExternalSemaphoreProperties
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


data VkExternalSemaphoreFeatureFlagBits

-- No documentation found for TopLevel "VkExternalSemaphoreFeatureFlags"
type VkExternalSemaphoreFeatureFlags = VkExternalSemaphoreFeatureFlagBits

data VkExternalSemaphoreHandleTypeFlagBits

-- No documentation found for TopLevel "VkExternalSemaphoreHandleTypeFlags"
type VkExternalSemaphoreHandleTypeFlags = VkExternalSemaphoreHandleTypeFlagBits

data VkExternalSemaphoreProperties

data VkPhysicalDeviceExternalSemaphoreInfo

type FN_vkGetPhysicalDeviceExternalSemaphoreProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalSemaphoreInfo" ::: Ptr VkPhysicalDeviceExternalSemaphoreInfo) -> ("pExternalSemaphoreProperties" ::: Ptr VkExternalSemaphoreProperties) -> IO ()
type PFN_vkGetPhysicalDeviceExternalSemaphoreProperties = FunPtr FN_vkGetPhysicalDeviceExternalSemaphoreProperties
