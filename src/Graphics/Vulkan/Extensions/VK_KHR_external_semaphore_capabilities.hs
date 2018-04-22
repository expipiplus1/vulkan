{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_capabilities
  ( pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
  , vkGetPhysicalDeviceExternalSemaphorePropertiesKHR
  , VkExternalSemaphoreHandleTypeFlagBitsKHR
  , VkExternalSemaphoreFeatureFlagBitsKHR
  , VkExternalSemaphoreHandleTypeFlagsKHR
  , VkExternalSemaphoreFeatureFlagsKHR
  , VkPhysicalDeviceExternalSemaphoreInfoKHR
  , pattern VkPhysicalDeviceExternalSemaphoreInfoKHR
  , VkExternalSemaphorePropertiesKHR
  , pattern VkExternalSemaphorePropertiesKHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreFeatureFlagBits(..)
  , VkExternalSemaphoreHandleTypeFlagBits(..)
  , VkExternalSemaphoreProperties(..)
  , VkPhysicalDeviceExternalSemaphoreInfo(..)
  , VkExternalSemaphoreFeatureFlags
  , VkExternalSemaphoreHandleTypeFlags
  , vkGetPhysicalDeviceExternalSemaphoreProperties
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  )


-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME = "VK_KHR_external_semaphore_capabilities"
-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalSemaphorePropertiesKHR"
vkGetPhysicalDeviceExternalSemaphorePropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalSemaphoreInfo" ::: Ptr VkPhysicalDeviceExternalSemaphoreInfo) -> ("pExternalSemaphoreProperties" ::: Ptr VkExternalSemaphoreProperties) -> IO ()
vkGetPhysicalDeviceExternalSemaphorePropertiesKHR = vkGetPhysicalDeviceExternalSemaphoreProperties
-- No documentation found for TopLevel "VkExternalSemaphoreHandleTypeFlagBitsKHR"
type VkExternalSemaphoreHandleTypeFlagBitsKHR = VkExternalSemaphoreHandleTypeFlagBits
-- No documentation found for TopLevel "VkExternalSemaphoreFeatureFlagBitsKHR"
type VkExternalSemaphoreFeatureFlagBitsKHR = VkExternalSemaphoreFeatureFlagBits
-- No documentation found for TopLevel "VkExternalSemaphoreHandleTypeFlagsKHR"
type VkExternalSemaphoreHandleTypeFlagsKHR = VkExternalSemaphoreHandleTypeFlags
-- No documentation found for TopLevel "VkExternalSemaphoreFeatureFlagsKHR"
type VkExternalSemaphoreFeatureFlagsKHR = VkExternalSemaphoreFeatureFlags
-- No documentation found for TopLevel "VkPhysicalDeviceExternalSemaphoreInfoKHR"
type VkPhysicalDeviceExternalSemaphoreInfoKHR = VkPhysicalDeviceExternalSemaphoreInfo


-- No documentation found for TopLevel "VkPhysicalDeviceExternalSemaphoreInfoKHR"
pattern VkPhysicalDeviceExternalSemaphoreInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleType" ::: VkExternalSemaphoreHandleTypeFlagBits) -> VkPhysicalDeviceExternalSemaphoreInfoKHR
pattern VkPhysicalDeviceExternalSemaphoreInfoKHR vkSType vkPNext vkHandleType = VkPhysicalDeviceExternalSemaphoreInfo vkSType vkPNext vkHandleType
-- No documentation found for TopLevel "VkExternalSemaphorePropertiesKHR"
type VkExternalSemaphorePropertiesKHR = VkExternalSemaphoreProperties


-- No documentation found for TopLevel "VkExternalSemaphorePropertiesKHR"
pattern VkExternalSemaphorePropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("exportFromImportedHandleTypes" ::: VkExternalSemaphoreHandleTypeFlags) -> ("compatibleHandleTypes" ::: VkExternalSemaphoreHandleTypeFlags) -> ("externalSemaphoreFeatures" ::: VkExternalSemaphoreFeatureFlags) -> VkExternalSemaphorePropertiesKHR
pattern VkExternalSemaphorePropertiesKHR vkSType vkPNext vkExportFromImportedHandleTypes vkCompatibleHandleTypes vkExternalSemaphoreFeatures = VkExternalSemaphoreProperties vkSType vkPNext vkExportFromImportedHandleTypes vkCompatibleHandleTypes vkExternalSemaphoreFeatures
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR = VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR :: VkExternalSemaphoreHandleTypeFlagBits
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR :: VkExternalSemaphoreHandleTypeFlagBits
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR :: VkExternalSemaphoreHandleTypeFlagBits
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR"
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR :: VkExternalSemaphoreHandleTypeFlagBits
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR"
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR :: VkExternalSemaphoreHandleTypeFlagBits
pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR = VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR"
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR :: VkExternalSemaphoreFeatureFlagBits
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR = VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR"
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR :: VkExternalSemaphoreFeatureFlagBits
pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR = VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
