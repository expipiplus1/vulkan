{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_memory_capabilities - instance extension
--
-- = Registered Extension Number
--
-- 72
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- = Deprecation state
--
-- -   /Promoted/ to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-10-17
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_KHR_dedicated_allocation@.
--
--     -   Interacts with @VK_NV_dedicated_allocation@.
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Ian Elliot, Google
--
--     -   Jesse Hall, Google
--
--     -   James Jones, NVIDIA
--
-- == Description
--
-- An application may wish to reference device memory in multiple Vulkan
-- logical devices or instances, in multiple processes, and\/or in multiple
-- APIs. This extension provides a set of capability queries and handle
-- definitions that allow an application to determine what types of
-- “external” memory handles an implementation supports for a given set of
-- use cases.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Commands
--
-- -   'getPhysicalDeviceExternalBufferPropertiesKHR'
--
-- == New Structures
--
-- -   'ExternalBufferPropertiesKHR'
--
-- -   'ExternalMemoryPropertiesKHR'
--
-- -   'PhysicalDeviceExternalBufferInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2':
--
--     -   'ExternalImageFormatPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2':
--
--     -   'PhysicalDeviceExternalImageFormatInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceIDPropertiesKHR'
--
-- == New Enums
--
-- -   'ExternalMemoryFeatureFlagBitsKHR'
--
-- -   'ExternalMemoryHandleTypeFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'ExternalMemoryFeatureFlagsKHR'
--
-- -   'ExternalMemoryHandleTypeFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_MEMORY_CAPABILITIES_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_MEMORY_CAPABILITIES_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.LUID_SIZE_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits.ExternalMemoryFeatureFlagBits':
--
--     -   'EXTERNAL_MEMORY_FEATURE_DEDICATED_ONLY_BIT_KHR'
--
--     -   'EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT_KHR'
--
--     -   'EXTERNAL_MEMORY_FEATURE_IMPORTABLE_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits':
--
--     -   'EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT_KHR'
--
--     -   'EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT_KHR'
--
--     -   'EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT_KHR'
--
--     -   'EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT_KHR'
--
--     -   'EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT_KHR'
--
--     -   'EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR'
--
--     -   'EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES_KHR'
--
--     -   'STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR'
--
-- == Issues
--
-- 1) Why do so many external memory capabilities need to be queried on a
-- per-memory-handle-type basis?
--
-- __PROPOSED RESOLUTION__: This is because some handle types are based on
-- OS-native objects that have far more limited capabilities than the very
-- generic Vulkan memory objects. Not all memory handle types can name
-- memory objects that support 3D images, for example. Some handle types
-- cannot even support the deferred image and memory binding behavior of
-- Vulkan and require specifying the image when allocating or importing the
-- memory object.
--
-- 2) Do the 'ExternalImageFormatPropertiesKHR' and
-- 'ExternalBufferPropertiesKHR' structs need to include a list of memory
-- type bits that support the given handle type?
--
-- __PROPOSED RESOLUTION__: No. The memory types that don’t support the
-- handle types will simply be filtered out of the results returned by
-- 'Vulkan.Core10.MemoryManagement.getImageMemoryRequirements' and
-- 'Vulkan.Core10.MemoryManagement.getBufferMemoryRequirements' when a set
-- of handle types was specified at image or buffer creation time.
--
-- 3) Should the non-opaque handle types be moved to their own extension?
--
-- __PROPOSED RESOLUTION__: Perhaps. However, defining the handle type bits
-- does very little and does not require any platform-specific types on its
-- own, and it’s easier to maintain the bitfield values in a single
-- extension for now. Presumably more handle types could be added by
-- separate extensions though, and it would be midly weird to have some
-- platform-specific ones defined in the core spec and some in extensions
--
-- 4) Do we need a @D3D11_TILEPOOL@ type?
--
-- __PROPOSED RESOLUTION__: No. This is technically possible, but the
-- synchronization is awkward. D3D11 surfaces must be synchronized using
-- shared mutexes, and these synchronization primitives are shared by the
-- entire memory object, so D3D11 shared allocations divided among multiple
-- buffer and image bindings may be difficult to synchronize.
--
-- 5) Should the Windows 7-compatible handle types be named “KMT” handles
-- or “GLOBAL_SHARE” handles?
--
-- __PROPOSED RESOLUTION__: KMT, simply because it is more concise.
--
-- 6) How do applications identify compatible devices and drivers across
-- instance, process, and API boundaries when sharing memory?
--
-- __PROPOSED RESOLUTION__: New device properties are exposed that allow
-- applications to correctly correlate devices and drivers. A device and
-- driver UUID that must both match to ensure sharing compatibility between
-- two Vulkan instances, or a Vulkan instance and an extensible external
-- API are added. To allow correlating with Direct3D devices, a device LUID
-- is added that corresponds to a DXGI adapter LUID. A driver ID is not
-- needed for Direct3D because mismatched driver component versions are not
-- a currently supported configuration on the Windows OS. Should support
-- for such configurations be introduced at the OS level, further Vulkan
-- extensions would be needed to correlate userspace component builds.
--
-- == Version History
--
-- -   Revision 1, 2016-10-17 (James Jones)
--
--     -   Initial version
--
-- = See Also
--
-- 'Vulkan.Core10.APIConstants.LUID_SIZE_KHR',
-- 'ExternalBufferPropertiesKHR', 'ExternalImageFormatPropertiesKHR',
-- 'ExternalMemoryFeatureFlagBitsKHR', 'ExternalMemoryFeatureFlagsKHR',
-- 'ExternalMemoryHandleTypeFlagBitsKHR',
-- 'ExternalMemoryHandleTypeFlagsKHR', 'ExternalMemoryPropertiesKHR',
-- 'PhysicalDeviceExternalBufferInfoKHR',
-- 'PhysicalDeviceExternalImageFormatInfoKHR',
-- 'PhysicalDeviceIDPropertiesKHR',
-- 'getPhysicalDeviceExternalBufferPropertiesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory_capabilities Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

