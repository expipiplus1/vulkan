{-# language CPP #-}
-- | = Name
--
-- VK_EXT_map_memory_placed - device extension
--
-- = VK_EXT_map_memory_placed
--
-- [__Name String__]
--     @VK_EXT_map_memory_placed@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     273
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_map_memory2 VK_KHR_map_memory2>
--
-- [__Contact__]
--
--     -   Faith Ekstrand
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_map_memory_placed] @gfxstrand%0A*Here describe the issue or question you have about the VK_EXT_map_memory_placed extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_map_memory_placed.adoc VK_EXT_map_memory_placed>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Depends on apitext:VK_KHR_map_memory2
--
--     -   Interacts with apitext:VK_EXT_external_memory_host
--
-- [__Contributors__]
--
--     -   Faith Ekstrand, Collabora
--
--     -   Tobias Hector, AMD
--
--     -   James Jones, NVIDIA
--
--     -   Georg Lehmann, Valve
--
--     -   Derek Lesho, Codeweavers
--
-- == Description
--
-- This extension allows an application to request that
-- 'Vulkan.Extensions.VK_KHR_map_memory2.mapMemory2KHR' attempt to place
-- the memory map at a particular virtual address.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_map_memory2.MemoryMapInfoKHR':
--
--     -   'MemoryMapPlacedInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMapMemoryPlacedFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMapMemoryPlacedPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MAP_MEMORY_PLACED_EXTENSION_NAME'
--
-- -   'EXT_MAP_MEMORY_PLACED_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.MemoryMapFlagBits.MemoryMapFlagBits':
--
--     -   'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_map_memory2.MemoryUnmapFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_map_memory2.MEMORY_UNMAP_RESERVE_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_MAP_PLACED_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAP_MEMORY_PLACED_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 0, 2024-01-14 (Faith Ekstrand)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_map_memory_placed Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_map_memory_placed  ( MemoryMapPlacedInfoEXT
                                                   , PhysicalDeviceMapMemoryPlacedFeaturesEXT
                                                   , PhysicalDeviceMapMemoryPlacedPropertiesEXT
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data MemoryMapPlacedInfoEXT

instance ToCStruct MemoryMapPlacedInfoEXT
instance Show MemoryMapPlacedInfoEXT

instance FromCStruct MemoryMapPlacedInfoEXT


data PhysicalDeviceMapMemoryPlacedFeaturesEXT

instance ToCStruct PhysicalDeviceMapMemoryPlacedFeaturesEXT
instance Show PhysicalDeviceMapMemoryPlacedFeaturesEXT

instance FromCStruct PhysicalDeviceMapMemoryPlacedFeaturesEXT


data PhysicalDeviceMapMemoryPlacedPropertiesEXT

instance ToCStruct PhysicalDeviceMapMemoryPlacedPropertiesEXT
instance Show PhysicalDeviceMapMemoryPlacedPropertiesEXT

instance FromCStruct PhysicalDeviceMapMemoryPlacedPropertiesEXT

