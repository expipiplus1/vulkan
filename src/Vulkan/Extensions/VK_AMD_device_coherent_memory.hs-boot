{-# language CPP #-}
-- | = Name
--
-- VK_AMD_device_coherent_memory - device extension
--
-- == VK_AMD_device_coherent_memory
--
-- [__Name String__]
--     @VK_AMD_device_coherent_memory@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     230
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_device_coherent_memory] @tobski%0A<<Here describe the issue or question you have about the VK_AMD_device_coherent_memory extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-02-04
--
-- [__Contributors__]
--
--     -   Ping Fu, AMD
--
--     -   Timothy Lottes, AMD
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension adds the device coherent and device uncached memory
-- types. Any device accesses to device coherent memory are automatically
-- made visible to any other device access. Device uncached memory
-- indicates to applications that caches are disabled for a particular
-- memory type, which guarantees device coherence.
--
-- Device coherent and uncached memory are expected to have lower
-- performance for general access than non-device coherent memory, but can
-- be useful in certain scenarios; particularly so for debugging.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCoherentMemoryFeaturesAMD'
--
-- == New Enum Constants
--
-- -   'AMD_DEVICE_COHERENT_MEMORY_EXTENSION_NAME'
--
-- -   'AMD_DEVICE_COHERENT_MEMORY_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MemoryPropertyFlagBits':
--
--     -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD'
--
--     -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD'
--
-- == Version History
--
-- -   Revision 1, 2019-02-04 (Tobias Hector)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceCoherentMemoryFeaturesAMD'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_device_coherent_memory Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_device_coherent_memory  (PhysicalDeviceCoherentMemoryFeaturesAMD) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceCoherentMemoryFeaturesAMD

instance ToCStruct PhysicalDeviceCoherentMemoryFeaturesAMD
instance Show PhysicalDeviceCoherentMemoryFeaturesAMD

instance FromCStruct PhysicalDeviceCoherentMemoryFeaturesAMD

