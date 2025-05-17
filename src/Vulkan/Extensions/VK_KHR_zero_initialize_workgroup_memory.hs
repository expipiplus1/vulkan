{-# language CPP #-}
-- | = Name
--
-- VK_KHR_zero_initialize_workgroup_memory - device extension
--
-- == VK_KHR_zero_initialize_workgroup_memory
--
-- [__Name String__]
--     @VK_KHR_zero_initialize_workgroup_memory@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     326
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Alan Baker
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_zero_initialize_workgroup_memory] @alan-baker%0A*Here describe the issue or question you have about the VK_KHR_zero_initialize_workgroup_memory extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-11-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Alan Baker, Google
--
--     -   Jeff Bolz, Nvidia
--
--     -   Faith Ekstrand, Intel
--
-- == Description
--
-- This extension allows the use of a null constant initializer on shader
-- Workgroup memory variables, allowing implementations to expose any
-- special hardware or instructions they may have. Zero initialization is
-- commonly used by applications running untrusted content (e.g. web
-- browsers) as way of defeating memory-scraping attacks.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME'
--
-- -   'KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES_KHR'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- KHR suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2020-11-18 (Alan Baker)
--
--     -   Internal draft version
--
-- == See Also
--
-- 'PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_zero_initialize_workgroup_memory Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_zero_initialize_workgroup_memory  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES_KHR
                                                                  , PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR
                                                                  , KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION
                                                                  , pattern KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION
                                                                  , KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME
                                                                  , pattern KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME
                                                                  ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_KHR_zero_initialize_workgroup_memory (PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR"
type PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR = PhysicalDeviceZeroInitializeWorkgroupMemoryFeatures


type KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION"
pattern KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_SPEC_VERSION = 1


type KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME = "VK_KHR_zero_initialize_workgroup_memory"

-- No documentation found for TopLevel "VK_KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME"
pattern KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_ZERO_INITIALIZE_WORKGROUP_MEMORY_EXTENSION_NAME = "VK_KHR_zero_initialize_workgroup_memory"

