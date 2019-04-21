{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphorePropertiesKHR
  , PhysicalDeviceExternalSemaphoreInfoKHR
  , getPhysicalDeviceExternalSemaphorePropertiesKHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  , pattern VK_LUID_SIZE_KHR
  , ExternalSemaphoreHandleTypeFlagsKHR
  , ExternalSemaphoreHandleTypeFlagBitsKHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
  , ExternalSemaphoreFeatureFlagsKHR
  , ExternalSemaphoreFeatureFlagBitsKHR
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
  , PhysicalDeviceIDPropertiesKHR
  ) where




import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreProperties(..)
  , PhysicalDeviceExternalSemaphoreInfo(..)
  , getPhysicalDeviceExternalSemaphoreProperties
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities
  ( pattern VK_LUID_SIZE_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_capabilities
  ( pattern VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
  , pattern VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
  , pattern VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreFeatureFlagBitsKHR
  , ExternalSemaphoreFeatureFlagsKHR
  , ExternalSemaphoreHandleTypeFlagBitsKHR
  , ExternalSemaphoreHandleTypeFlagsKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_capabilities
  ( PhysicalDeviceIDPropertiesKHR
  )


type ExternalSemaphorePropertiesKHR = ExternalSemaphoreProperties
-- TODO: Pattern constructor alias)

type PhysicalDeviceExternalSemaphoreInfoKHR = PhysicalDeviceExternalSemaphoreInfo
-- TODO: Pattern constructor alias)

getPhysicalDeviceExternalSemaphorePropertiesKHR :: PhysicalDevice ->  PhysicalDeviceExternalSemaphoreInfo ->  IO (ExternalSemaphoreProperties)
getPhysicalDeviceExternalSemaphorePropertiesKHR = getPhysicalDeviceExternalSemaphoreProperties
