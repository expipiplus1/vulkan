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
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Alan Baker
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_zero_initialize_workgroup_memory:%20&body=@alan-baker%20 >
--
-- [__Last Modified Date__]
--     2020-11-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--     None
--
-- [__Contributors__]
--
--     -   Alan Baker, Google
--
--     -   Jeff Bolz, Nvidia
--
--     -   Jason Ekstrand, Intel
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
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ZERO_INITIALIZE_WORKGROUP_MEMORY_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-11-18 (Alan Baker)
--
--     -   Internal draft version
--
-- = See Also
--
-- 'PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_zero_initialize_workgroup_memory Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_zero_initialize_workgroup_memory  (PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR

instance ToCStruct PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR
instance Show PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR

instance FromCStruct PhysicalDeviceZeroInitializeWorkgroupMemoryFeaturesKHR

