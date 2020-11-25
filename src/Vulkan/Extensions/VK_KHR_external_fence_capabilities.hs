{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_external_fence_capabilities"
module Vulkan.Extensions.VK_KHR_external_fence_capabilities  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR
                                                             , pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR
                                                             , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
                                                             , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
                                                             , pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
                                                             , pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR
                                                             , pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR
                                                             , pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR
                                                             , getPhysicalDeviceExternalFencePropertiesKHR
                                                             , ExternalFenceHandleTypeFlagsKHR
                                                             , ExternalFenceFeatureFlagsKHR
                                                             , ExternalFenceHandleTypeFlagBitsKHR
                                                             , ExternalFenceFeatureFlagBitsKHR
                                                             , PhysicalDeviceExternalFenceInfoKHR
                                                             , ExternalFencePropertiesKHR
                                                             , KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION
                                                             , pattern KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION
                                                             , KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
                                                             , pattern KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME
                                                             , PhysicalDeviceIDPropertiesKHR
                                                             , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR
                                                             , pattern LUID_SIZE_KHR
                                                             ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities (getPhysicalDeviceExternalFenceProperties)
import Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits (ExternalFenceFeatureFlagBits)
import Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits (ExternalFenceFeatureFlags)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlags)
import Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities (ExternalFenceProperties)
import Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities (PhysicalDeviceExternalFenceInfo)
import Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits (ExternalFenceFeatureFlags)
import Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits (ExternalFenceFeatureFlagBits(EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT))
import Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits (ExternalFenceFeatureFlags)
import Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits (ExternalFenceFeatureFlagBits(EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT))
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlagBits(EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT))
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlagBits(EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT))
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlagBits(EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT))
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlagBits(EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO))
import Vulkan.Extensions.VK_KHR_external_memory_capabilities (PhysicalDeviceIDPropertiesKHR)
import Vulkan.Extensions.VK_KHR_external_memory_capabilities (pattern LUID_SIZE_KHR)
import Vulkan.Extensions.VK_KHR_external_memory_capabilities (pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR)
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR = STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES


-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
pattern EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR"
pattern EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR = EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR"
pattern EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR = EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR"
pattern EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR = EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT


-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalFencePropertiesKHR"
getPhysicalDeviceExternalFencePropertiesKHR = getPhysicalDeviceExternalFenceProperties


-- No documentation found for TopLevel "VkExternalFenceHandleTypeFlagsKHR"
type ExternalFenceHandleTypeFlagsKHR = ExternalFenceHandleTypeFlags


-- No documentation found for TopLevel "VkExternalFenceFeatureFlagsKHR"
type ExternalFenceFeatureFlagsKHR = ExternalFenceFeatureFlags


-- No documentation found for TopLevel "VkExternalFenceHandleTypeFlagBitsKHR"
type ExternalFenceHandleTypeFlagBitsKHR = ExternalFenceHandleTypeFlagBits


-- No documentation found for TopLevel "VkExternalFenceFeatureFlagBitsKHR"
type ExternalFenceFeatureFlagBitsKHR = ExternalFenceFeatureFlagBits


-- No documentation found for TopLevel "VkPhysicalDeviceExternalFenceInfoKHR"
type PhysicalDeviceExternalFenceInfoKHR = PhysicalDeviceExternalFenceInfo


-- No documentation found for TopLevel "VkExternalFencePropertiesKHR"
type ExternalFencePropertiesKHR = ExternalFenceProperties


type KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION"
pattern KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION = 1


type KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME = "VK_KHR_external_fence_capabilities"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME"
pattern KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME = "VK_KHR_external_fence_capabilities"

