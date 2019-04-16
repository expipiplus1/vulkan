{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_fence_capabilities
  ( ExternalFencePropertiesKHR
  , PhysicalDeviceExternalFenceInfoKHR
  , getPhysicalDeviceExternalFencePropertiesKHR
  , pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern VK_LUID_SIZE_KHR
  , ExternalFenceHandleTypeFlagsKHR
  , ExternalFenceHandleTypeFlagBitsKHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  , ExternalFenceFeatureFlagsKHR
  , ExternalFenceFeatureFlagBitsKHR
  , pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  , PhysicalDeviceIDPropertiesKHR
  ) where




import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceProperties(..)
  , PhysicalDeviceExternalFenceInfo(..)
  , getPhysicalDeviceExternalFenceProperties
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_capabilities
  ( pattern VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
  , pattern VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR
  , pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities
  ( pattern VK_LUID_SIZE_KHR
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
