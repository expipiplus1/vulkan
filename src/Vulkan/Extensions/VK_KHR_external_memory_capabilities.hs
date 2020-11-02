{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_external_memory_capabilities  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR
                                                              , pattern STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR
                                                              , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR
                                                              , pattern STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR
                                                              , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR
                                                              , pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR
                                                              , pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR
                                                              , pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR
                                                              , pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR
                                                              , pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR
                                                              , pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR
                                                              , pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR
                                                              , pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR
                                                              , pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR
                                                              , pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR
                                                              , pattern LUID_SIZE_KHR
                                                              , getPhysicalDeviceExternalBufferPropertiesKHR
                                                              , ExternalMemoryHandleTypeFlagsKHR
                                                              , ExternalMemoryFeatureFlagsKHR
                                                              , ExternalMemoryHandleTypeFlagBitsKHR
                                                              , ExternalMemoryFeatureFlagBitsKHR
                                                              , ExternalMemoryPropertiesKHR
                                                              , PhysicalDeviceExternalImageFormatInfoKHR
                                                              , ExternalImageFormatPropertiesKHR
                                                              , PhysicalDeviceExternalBufferInfoKHR
                                                              , ExternalBufferPropertiesKHR
                                                              , PhysicalDeviceIDPropertiesKHR
                                                              , KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
                                                              , pattern KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION
                                                              , KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
                                                              , pattern KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME
                                                              ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (getPhysicalDeviceExternalBufferProperties)
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (ExternalBufferProperties)
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (ExternalImageFormatProperties)
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlags)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (ExternalMemoryProperties)
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (PhysicalDeviceExternalBufferInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (PhysicalDeviceExternalImageFormatInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (PhysicalDeviceIDProperties)
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlags)
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlagBits(EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT))
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlags)
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlagBits(EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT))
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlags)
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlagBits(EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT))
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT))
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT))
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT))
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT))
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT))
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT))
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT))
import Vulkan.Core10.APIConstants (pattern LUID_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR = STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR = STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES


-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR = EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR = EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR = EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR = EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR = EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR = EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR"
pattern EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR = EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR"
pattern EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR = EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR"
pattern EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR = EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT


-- No documentation found for TopLevel "VK_EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR"
pattern EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR = EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT


-- No documentation found for TopLevel "VK_LUID_SIZE_KHR"
pattern LUID_SIZE_KHR = LUID_SIZE


-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalBufferPropertiesKHR"
getPhysicalDeviceExternalBufferPropertiesKHR = getPhysicalDeviceExternalBufferProperties


-- No documentation found for TopLevel "VkExternalMemoryHandleTypeFlagsKHR"
type ExternalMemoryHandleTypeFlagsKHR = ExternalMemoryHandleTypeFlags


-- No documentation found for TopLevel "VkExternalMemoryFeatureFlagsKHR"
type ExternalMemoryFeatureFlagsKHR = ExternalMemoryFeatureFlags


-- No documentation found for TopLevel "VkExternalMemoryHandleTypeFlagBitsKHR"
type ExternalMemoryHandleTypeFlagBitsKHR = ExternalMemoryHandleTypeFlagBits


-- No documentation found for TopLevel "VkExternalMemoryFeatureFlagBitsKHR"
type ExternalMemoryFeatureFlagBitsKHR = ExternalMemoryFeatureFlagBits


-- No documentation found for TopLevel "VkExternalMemoryPropertiesKHR"
type ExternalMemoryPropertiesKHR = ExternalMemoryProperties


-- No documentation found for TopLevel "VkPhysicalDeviceExternalImageFormatInfoKHR"
type PhysicalDeviceExternalImageFormatInfoKHR = PhysicalDeviceExternalImageFormatInfo


-- No documentation found for TopLevel "VkExternalImageFormatPropertiesKHR"
type ExternalImageFormatPropertiesKHR = ExternalImageFormatProperties


-- No documentation found for TopLevel "VkPhysicalDeviceExternalBufferInfoKHR"
type PhysicalDeviceExternalBufferInfoKHR = PhysicalDeviceExternalBufferInfo


-- No documentation found for TopLevel "VkExternalBufferPropertiesKHR"
type ExternalBufferPropertiesKHR = ExternalBufferProperties


-- No documentation found for TopLevel "VkPhysicalDeviceIDPropertiesKHR"
type PhysicalDeviceIDPropertiesKHR = PhysicalDeviceIDProperties


type KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION"
pattern KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION = 1


type KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = "VK_KHR_external_memory_capabilities"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME"
pattern KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME = "VK_KHR_external_memory_capabilities"

