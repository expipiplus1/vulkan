{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_capabilities
  ( ExternalFencePropertiesKHR
  , PhysicalDeviceExternalFenceInfoKHR
  , getPhysicalDeviceExternalFencePropertiesKHR
  , pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR
  , pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR
  , pattern KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
  , pattern KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION
  , pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR
  , ExternalFenceHandleTypeFlagsKHR
  , ExternalFenceHandleTypeFlagBitsKHR
  , ExternalFenceFeatureFlagsKHR
  , ExternalFenceFeatureFlagBitsKHR
  , PhysicalDeviceIDPropertiesKHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  , pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceFeatureFlagBits(..)
  , VkExternalFenceHandleTypeFlagBits(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_capabilities
  ( pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceProperties(..)
  , PhysicalDeviceExternalFenceInfo(..)
  , getPhysicalDeviceExternalFenceProperties
  , pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceFeatureFlagBitsKHR
  , ExternalFenceFeatureFlagsKHR
  , ExternalFenceHandleTypeFlagBitsKHR
  , ExternalFenceHandleTypeFlagsKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_capabilities
  ( PhysicalDeviceIDPropertiesKHR
  )


type ExternalFencePropertiesKHR = ExternalFenceProperties
-- TODO: Pattern constructor alias)

type PhysicalDeviceExternalFenceInfoKHR = PhysicalDeviceExternalFenceInfo
-- TODO: Pattern constructor alias)

getPhysicalDeviceExternalFencePropertiesKHR :: PhysicalDevice ->  PhysicalDeviceExternalFenceInfo ->  IO (ExternalFenceProperties)
getPhysicalDeviceExternalFencePropertiesKHR = getPhysicalDeviceExternalFenceProperties

-- No documentation found for TopLevel "EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR"
pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR :: VkExternalFenceFeatureFlagBits
pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR = EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT

-- No documentation found for TopLevel "EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR"
pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR :: VkExternalFenceFeatureFlagBits
pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR = EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT

-- No documentation found for TopLevel "EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR :: VkExternalFenceHandleTypeFlagBits
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT

-- No documentation found for TopLevel "EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR :: VkExternalFenceHandleTypeFlagBits
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT

-- No documentation found for TopLevel "EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR :: VkExternalFenceHandleTypeFlagBits
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT

-- No documentation found for TopLevel "EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR"
pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR :: VkExternalFenceHandleTypeFlagBits
pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR = EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME"
pattern KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME = VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION"
pattern KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION :: Integral a => a
pattern KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION = VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION

-- No documentation found for TopLevel "STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR :: VkStructureType
pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR = STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES

-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
