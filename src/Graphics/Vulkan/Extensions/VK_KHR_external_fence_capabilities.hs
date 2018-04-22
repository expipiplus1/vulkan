{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_capabilities
  ( pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
  , vkGetPhysicalDeviceExternalFencePropertiesKHR
  , VkExternalFenceHandleTypeFlagBitsKHR
  , VkExternalFenceFeatureFlagBitsKHR
  , VkExternalFenceHandleTypeFlagsKHR
  , VkExternalFenceFeatureFlagsKHR
  , VkPhysicalDeviceExternalFenceInfoKHR
  , pattern VkPhysicalDeviceExternalFenceInfoKHR
  , VkExternalFencePropertiesKHR
  , pattern VkExternalFencePropertiesKHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR
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
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceFeatureFlagBits(..)
  , VkExternalFenceHandleTypeFlagBits(..)
  , VkExternalFenceProperties(..)
  , VkPhysicalDeviceExternalFenceInfo(..)
  , VkExternalFenceFeatureFlags
  , VkExternalFenceHandleTypeFlags
  , vkGetPhysicalDeviceExternalFenceProperties
  , pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  )


-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME = "VK_KHR_external_fence_capabilities"
-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalFencePropertiesKHR"
vkGetPhysicalDeviceExternalFencePropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalFenceInfo" ::: Ptr VkPhysicalDeviceExternalFenceInfo) -> ("pExternalFenceProperties" ::: Ptr VkExternalFenceProperties) -> IO ()
vkGetPhysicalDeviceExternalFencePropertiesKHR = vkGetPhysicalDeviceExternalFenceProperties
-- No documentation found for TopLevel "VkExternalFenceHandleTypeFlagBitsKHR"
type VkExternalFenceHandleTypeFlagBitsKHR = VkExternalFenceHandleTypeFlagBits
-- No documentation found for TopLevel "VkExternalFenceFeatureFlagBitsKHR"
type VkExternalFenceFeatureFlagBitsKHR = VkExternalFenceFeatureFlagBits
-- No documentation found for TopLevel "VkExternalFenceHandleTypeFlagsKHR"
type VkExternalFenceHandleTypeFlagsKHR = VkExternalFenceHandleTypeFlags
-- No documentation found for TopLevel "VkExternalFenceFeatureFlagsKHR"
type VkExternalFenceFeatureFlagsKHR = VkExternalFenceFeatureFlags
-- No documentation found for TopLevel "VkPhysicalDeviceExternalFenceInfoKHR"
type VkPhysicalDeviceExternalFenceInfoKHR = VkPhysicalDeviceExternalFenceInfo


-- No documentation found for TopLevel "VkPhysicalDeviceExternalFenceInfoKHR"
pattern VkPhysicalDeviceExternalFenceInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleType" ::: VkExternalFenceHandleTypeFlagBits) -> VkPhysicalDeviceExternalFenceInfoKHR
pattern VkPhysicalDeviceExternalFenceInfoKHR vkSType vkPNext vkHandleType = VkPhysicalDeviceExternalFenceInfo vkSType vkPNext vkHandleType
-- No documentation found for TopLevel "VkExternalFencePropertiesKHR"
type VkExternalFencePropertiesKHR = VkExternalFenceProperties


-- No documentation found for TopLevel "VkExternalFencePropertiesKHR"
pattern VkExternalFencePropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("exportFromImportedHandleTypes" ::: VkExternalFenceHandleTypeFlags) -> ("compatibleHandleTypes" ::: VkExternalFenceHandleTypeFlags) -> ("externalFenceFeatures" ::: VkExternalFenceFeatureFlags) -> VkExternalFencePropertiesKHR
pattern VkExternalFencePropertiesKHR vkSType vkPNext vkExportFromImportedHandleTypes vkCompatibleHandleTypes vkExternalFenceFeatures = VkExternalFenceProperties vkSType vkPNext vkExportFromImportedHandleTypes vkCompatibleHandleTypes vkExternalFenceFeatures
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR = VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR :: VkExternalFenceHandleTypeFlagBits
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR :: VkExternalFenceHandleTypeFlagBits
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR :: VkExternalFenceHandleTypeFlagBits
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR"
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR :: VkExternalFenceHandleTypeFlagBits
pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR = VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR"
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR :: VkExternalFenceFeatureFlagBits
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR = VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR"
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR :: VkExternalFenceFeatureFlagBits
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR = VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
