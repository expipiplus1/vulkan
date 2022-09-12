{-# language CPP #-}
-- | = Name
--
-- VK_EXT_memory_priority - device extension
--
-- == VK_EXT_memory_priority
--
-- [__Name String__]
--     @VK_EXT_memory_priority@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     239
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_memory_priority] @jeffbolznv%0A*Here describe the issue or question you have about the VK_EXT_memory_priority extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-10-08
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
-- == Description
--
-- This extension adds a @priority@ value specified at memory allocation
-- time. On some systems with both device-local and non-device-local memory
-- heaps, the implementation may transparently move memory from one heap to
-- another when a heap becomes full (for example, when the total memory
-- used across all processes exceeds the size of the heap). In such a case,
-- this priority value may be used to determine which allocations are more
-- likely to remain in device-local memory.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'MemoryPriorityAllocateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMemoryPriorityFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MEMORY_PRIORITY_EXTENSION_NAME'
--
-- -   'EXT_MEMORY_PRIORITY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2018-10-08 (Jeff Bolz)
--
--     -   Initial revision
--
-- == See Also
--
-- 'MemoryPriorityAllocateInfoEXT',
-- 'PhysicalDeviceMemoryPriorityFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_memory_priority Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_memory_priority  ( MemoryPriorityAllocateInfoEXT
                                                 , PhysicalDeviceMemoryPriorityFeaturesEXT
                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data MemoryPriorityAllocateInfoEXT

instance ToCStruct MemoryPriorityAllocateInfoEXT
instance Show MemoryPriorityAllocateInfoEXT

instance FromCStruct MemoryPriorityAllocateInfoEXT


data PhysicalDeviceMemoryPriorityFeaturesEXT

instance ToCStruct PhysicalDeviceMemoryPriorityFeaturesEXT
instance Show PhysicalDeviceMemoryPriorityFeaturesEXT

instance FromCStruct PhysicalDeviceMemoryPriorityFeaturesEXT

