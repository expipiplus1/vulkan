{-# language CPP #-}
-- | = Name
--
-- VK_KHR_external_fence_capabilities - instance extension
--
-- == VK_KHR_external_fence_capabilities
--
-- [__Name String__]
--     @VK_KHR_external_fence_capabilities@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     113
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_external_fence_capabilities] @critsec%0A<<Here describe the issue or question you have about the VK_KHR_external_fence_capabilities extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-05-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Jesse Hall, Google
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Cass Everitt, Oculus
--
--     -   Contributors to @VK_KHR_external_semaphore_capabilities@
--
-- == Description
--
-- An application may wish to reference device fences in multiple Vulkan
-- logical devices or instances, in multiple processes, and\/or in multiple
-- APIs. This extension provides a set of capability queries and handle
-- definitions that allow an application to determine what types of
-- “external” fence handles an implementation supports for a given set of
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
-- -   'getPhysicalDeviceExternalFencePropertiesKHR'
--
-- == New Structures
--
-- -   'ExternalFencePropertiesKHR'
--
-- -   'PhysicalDeviceExternalFenceInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'Vulkan.Extensions.VK_KHR_external_memory_capabilities.PhysicalDeviceIDPropertiesKHR'
--
-- == New Enums
--
-- -   'ExternalFenceFeatureFlagBitsKHR'
--
-- -   'ExternalFenceHandleTypeFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'ExternalFenceFeatureFlagsKHR'
--
-- -   'ExternalFenceHandleTypeFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_EXTERNAL_FENCE_CAPABILITIES_EXTENSION_NAME'
--
-- -   'KHR_EXTERNAL_FENCE_CAPABILITIES_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.LUID_SIZE_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalFenceFeatureFlagBits.ExternalFenceFeatureFlagBits':
--
--     -   'EXTERNAL_FENCE_FEATURE_EXPORTABLE_BIT_KHR'
--
--     -   'EXTERNAL_FENCE_FEATURE_IMPORTABLE_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits.ExternalFenceHandleTypeFlagBits':
--
--     -   'EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_FD_BIT_KHR'
--
--     -   'EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_BIT_KHR'
--
--     -   'EXTERNAL_FENCE_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT_KHR'
--
--     -   'EXTERNAL_FENCE_HANDLE_TYPE_SYNC_FD_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO_KHR'
--
--     -   'Vulkan.Extensions.VK_KHR_external_memory_capabilities.STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2017-05-08 (Jesse Hall)
--
--     -   Initial version
--
-- == See Also
--
-- 'Vulkan.Core10.APIConstants.LUID_SIZE_KHR',
-- 'ExternalFenceFeatureFlagBitsKHR', 'ExternalFenceFeatureFlagsKHR',
-- 'ExternalFenceHandleTypeFlagBitsKHR', 'ExternalFenceHandleTypeFlagsKHR',
-- 'ExternalFencePropertiesKHR', 'PhysicalDeviceExternalFenceInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_memory_capabilities.PhysicalDeviceIDPropertiesKHR',
-- 'getPhysicalDeviceExternalFencePropertiesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_fence_capabilities Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

