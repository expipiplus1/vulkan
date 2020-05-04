{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_external_semaphore_capabilities  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR
                                                                 , pattern STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR
                                                                 , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
                                                                 , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
                                                                 , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
                                                                 , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR
                                                                 , pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR
                                                                 , pattern EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR
                                                                 , pattern EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR
                                                                 , getPhysicalDeviceExternalSemaphorePropertiesKHR
                                                                 , ExternalSemaphoreHandleTypeFlagsKHR
                                                                 , ExternalSemaphoreFeatureFlagsKHR
                                                                 , ExternalSemaphoreHandleTypeFlagBitsKHR
                                                                 , ExternalSemaphoreFeatureFlagBitsKHR
                                                                 , PhysicalDeviceExternalSemaphoreInfoKHR
                                                                 , ExternalSemaphorePropertiesKHR
                                                                 , KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION
                                                                 , pattern KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION
                                                                 , KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
                                                                 , pattern KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME
                                                                 , PhysicalDeviceIDPropertiesKHR
                                                                 , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR
                                                                 , pattern LUID_SIZE_KHR
                                                                 ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities (getPhysicalDeviceExternalSemaphoreProperties)
import Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits (ExternalSemaphoreFeatureFlagBits)
import Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits (ExternalSemaphoreFeatureFlags)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities (ExternalSemaphoreProperties)
import Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities (PhysicalDeviceExternalSemaphoreInfo)
import Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits (ExternalSemaphoreFeatureFlags)
import Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits (ExternalSemaphoreFeatureFlagBits(EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT))
import Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits (ExternalSemaphoreFeatureFlags)
import Vulkan.Core11.Enums.ExternalSemaphoreFeatureFlagBits (ExternalSemaphoreFeatureFlagBits(EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT))
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits(EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT))
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits(EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT))
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits(EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT))
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits(EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT))
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits(EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO))
import Vulkan.Extensions.VK_KHR_external_memory_capabilities (PhysicalDeviceIDPropertiesKHR)
import Vulkan.Extensions.VK_KHR_external_memory_capabilities (pattern LUID_SIZE_KHR)
import Vulkan.Extensions.VK_KHR_external_memory_capabilities (pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR)
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES_KHR = STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES


-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_FD_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = EXTERNAL_SEMAPHORE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT_KHR = EXTERNAL_SEMAPHORE_HANDLE_TYPE_D3D12_FENCE_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR"
pattern EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT_KHR = EXTERNAL_SEMAPHORE_HANDLE_TYPE_SYNC_FD_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR"
pattern EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT_KHR = EXTERNAL_SEMAPHORE_FEATURE_EXPORTABLE_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR"
pattern EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT_KHR = EXTERNAL_SEMAPHORE_FEATURE_IMPORTABLE_BIT


-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalSemaphorePropertiesKHR"
getPhysicalDeviceExternalSemaphorePropertiesKHR = getPhysicalDeviceExternalSemaphoreProperties


-- No documentation found for TopLevel "VkExternalSemaphoreHandleTypeFlagsKHR"
type ExternalSemaphoreHandleTypeFlagsKHR = ExternalSemaphoreHandleTypeFlags


-- No documentation found for TopLevel "VkExternalSemaphoreFeatureFlagsKHR"
type ExternalSemaphoreFeatureFlagsKHR = ExternalSemaphoreFeatureFlags


-- No documentation found for TopLevel "VkExternalSemaphoreHandleTypeFlagBitsKHR"
type ExternalSemaphoreHandleTypeFlagBitsKHR = ExternalSemaphoreHandleTypeFlagBits


-- No documentation found for TopLevel "VkExternalSemaphoreFeatureFlagBitsKHR"
type ExternalSemaphoreFeatureFlagBitsKHR = ExternalSemaphoreFeatureFlagBits


-- No documentation found for TopLevel "VkPhysicalDeviceExternalSemaphoreInfoKHR"
type PhysicalDeviceExternalSemaphoreInfoKHR = PhysicalDeviceExternalSemaphoreInfo


-- No documentation found for TopLevel "VkExternalSemaphorePropertiesKHR"
type ExternalSemaphorePropertiesKHR = ExternalSemaphoreProperties


type KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION"
pattern KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_SPEC_VERSION = 1


type KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME = "VK_KHR_external_semaphore_capabilities"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME"
pattern KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_SEMAPHORE_CAPABILITIES_EXTENSION_NAME = "VK_KHR_external_semaphore_capabilities"

