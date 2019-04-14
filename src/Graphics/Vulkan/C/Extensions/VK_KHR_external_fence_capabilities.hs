{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_capabilities
  ( VkExternalFenceFeatureFlagBitsKHR
  , VkExternalFenceFeatureFlagsKHR
  , VkExternalFenceHandleTypeFlagBitsKHR
  , VkExternalFenceHandleTypeFlagsKHR
  , VkExternalFencePropertiesKHR
  , pattern VkExternalFencePropertiesKHR
  , VkPhysicalDeviceExternalFenceInfoKHR
  , pattern VkPhysicalDeviceExternalFenceInfoKHR
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkGetPhysicalDeviceExternalFencePropertiesKHR
#endif
  , pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR
  , pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern VK_LUID_SIZE_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  , pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  , VkPhysicalDeviceIDPropertiesKHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )

#if defined(EXPOSE_CORE11_COMMANDS)
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceFeatureFlagBits(..)
  , VkExternalFenceHandleTypeFlagBits(..)
  , VkExternalFenceProperties(..)
  , VkPhysicalDeviceExternalFenceInfo(..)
  , VkExternalFenceFeatureFlags
  , VkExternalFenceHandleTypeFlags
  , pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  )

#if defined(EXPOSE_CORE11_COMMANDS)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( vkGetPhysicalDeviceExternalFenceProperties
  )
#endif
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities
  ( VkPhysicalDeviceIDPropertiesKHR
  , pattern VK_LUID_SIZE_KHR
  )


-- No documentation found for TopLevel "VkExternalFenceFeatureFlagBitsKHR"
type VkExternalFenceFeatureFlagBitsKHR = VkExternalFenceFeatureFlagBits
-- No documentation found for TopLevel "VkExternalFenceFeatureFlagsKHR"
type VkExternalFenceFeatureFlagsKHR = VkExternalFenceFeatureFlags
-- No documentation found for TopLevel "VkExternalFenceHandleTypeFlagBitsKHR"
type VkExternalFenceHandleTypeFlagBitsKHR = VkExternalFenceHandleTypeFlagBits
-- No documentation found for TopLevel "VkExternalFenceHandleTypeFlagsKHR"
type VkExternalFenceHandleTypeFlagsKHR = VkExternalFenceHandleTypeFlags
-- No documentation found for TopLevel "VkExternalFencePropertiesKHR"
type VkExternalFencePropertiesKHR = VkExternalFenceProperties


-- No documentation found for TopLevel "VkExternalFencePropertiesKHR"
pattern VkExternalFencePropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("exportFromImportedHandleTypes" ::: VkExternalFenceHandleTypeFlags) -> ("compatibleHandleTypes" ::: VkExternalFenceHandleTypeFlags) -> ("externalFenceFeatures" ::: VkExternalFenceFeatureFlags) -> VkExternalFencePropertiesKHR
pattern VkExternalFencePropertiesKHR vkSType vkPNext vkExportFromImportedHandleTypes vkCompatibleHandleTypes vkExternalFenceFeatures = VkExternalFenceProperties vkSType vkPNext vkExportFromImportedHandleTypes vkCompatibleHandleTypes vkExternalFenceFeatures
-- No documentation found for TopLevel "VkPhysicalDeviceExternalFenceInfoKHR"
type VkPhysicalDeviceExternalFenceInfoKHR = VkPhysicalDeviceExternalFenceInfo


-- No documentation found for TopLevel "VkPhysicalDeviceExternalFenceInfoKHR"
pattern VkPhysicalDeviceExternalFenceInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("handleType" ::: VkExternalFenceHandleTypeFlagBits) -> VkPhysicalDeviceExternalFenceInfoKHR
pattern VkPhysicalDeviceExternalFenceInfoKHR vkSType vkPNext vkHandleType = VkPhysicalDeviceExternalFenceInfo vkSType vkPNext vkHandleType

#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalFencePropertiesKHR"
vkGetPhysicalDeviceExternalFencePropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalFenceInfo" ::: Ptr VkPhysicalDeviceExternalFenceInfo) -> ("pExternalFenceProperties" ::: Ptr VkExternalFenceProperties) -> IO ()
vkGetPhysicalDeviceExternalFencePropertiesKHR = vkGetPhysicalDeviceExternalFenceProperties
#endif
-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR"
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR :: VkExternalFenceFeatureFlagBits
pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR = VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR"
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR :: VkExternalFenceFeatureFlagBits
pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR = VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
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
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME"
pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME = "VK_KHR_external_fence_capabilities"
-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION"
pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR = VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
