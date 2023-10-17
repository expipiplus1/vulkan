{-# language CPP #-}
-- | = Name
--
-- VK_KHR_map_memory2 - device extension
--
-- == VK_KHR_map_memory2
--
-- [__Name String__]
--     @VK_KHR_map_memory2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     272
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   Faith Ekstrand
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_map_memory2] @gfxstrand%0A*Here describe the issue or question you have about the VK_KHR_map_memory2 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_map_memory2.adoc VK_KHR_map_memory2>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-14
--
-- [__Interactions and External Dependencies__]
--
--     -   None
--
-- [__Contributors__]
--
--     -   Faith Ekstrand, Collabora
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension provides extensible versions of the Vulkan memory map and
-- unmap entry points. The new entry points are functionally identical to
-- the core entry points, except that their parameters are specified using
-- extensible structures that can be used to pass extension-specific
-- information.
--
-- == New Commands
--
-- -   'mapMemory2KHR'
--
-- -   'unmapMemory2KHR'
--
-- == New Structures
--
-- -   'MemoryMapInfoKHR'
--
-- -   'MemoryUnmapInfoKHR'
--
-- == New Bitmasks
--
-- -   'MemoryUnmapFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAP_MEMORY_2_EXTENSION_NAME'
--
-- -   'KHR_MAP_MEMORY_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_MAP_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_UNMAP_INFO_KHR'
--
-- == Version History
--
-- -   Revision 0, 2022-08-03 (Faith Ekstrand)
--
--     -   Internal revisions
--
-- -   Revision 1, 2023-03-14
--
--     -   Public release
--
-- == See Also
--
-- 'MemoryMapInfoKHR', 'MemoryUnmapFlagsKHR', 'MemoryUnmapInfoKHR',
-- 'mapMemory2KHR', 'unmapMemory2KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_map_memory2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_map_memory2  ( MemoryMapInfoKHR
                                             , MemoryUnmapInfoKHR
                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data MemoryMapInfoKHR

instance ToCStruct MemoryMapInfoKHR
instance Show MemoryMapInfoKHR

instance FromCStruct MemoryMapInfoKHR


data MemoryUnmapInfoKHR

instance ToCStruct MemoryUnmapInfoKHR
instance Show MemoryUnmapInfoKHR

instance FromCStruct MemoryUnmapInfoKHR

