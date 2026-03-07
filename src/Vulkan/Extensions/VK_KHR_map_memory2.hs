{-# language CPP #-}
-- | = Name
--
-- VK_KHR_map_memory2 - device extension
--
-- = VK_KHR_map_memory2
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
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4-promotions Vulkan 1.4>
--
-- [__Contact__]
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
-- unmap commands. The new commands are functionally identical to the core
-- commands, except that their parameters are specified using extensible
-- structures that can be used to pass extension-specific information.
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
-- == New Enums
--
-- -   'MemoryUnmapFlagBitsKHR'
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
--     -   'STRUCTURE_TYPE_MEMORY_MAP_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_MEMORY_UNMAP_INFO_KHR'
--
-- == Promotion to Vulkan 1.4
--
-- Functionality in this extension is included in core Vulkan 1.4 with the
-- KHR suffix omitted. The original type, enum, and command names are still
-- available as aliases of the core functionality.
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
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_map_memory2 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_map_memory2  ( pattern STRUCTURE_TYPE_MEMORY_MAP_INFO_KHR
                                             , pattern STRUCTURE_TYPE_MEMORY_UNMAP_INFO_KHR
                                             , mapMemory2KHR
                                             , unmapMemory2KHR
                                             , MemoryUnmapFlagsKHR
                                             , MemoryUnmapFlagBitsKHR
                                             , MemoryMapInfoKHR
                                             , MemoryUnmapInfoKHR
                                             , KHR_MAP_MEMORY_2_SPEC_VERSION
                                             , pattern KHR_MAP_MEMORY_2_SPEC_VERSION
                                             , KHR_MAP_MEMORY_2_EXTENSION_NAME
                                             , pattern KHR_MAP_MEMORY_2_EXTENSION_NAME
                                             ) where

import Data.String (IsString)
import Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap (mapMemory2)
import Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap (unmapMemory2)
import Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap (MemoryMapInfo)
import Vulkan.Core14.Enums.MemoryUnmapFlagBits (MemoryUnmapFlagBits)
import Vulkan.Core14.Enums.MemoryUnmapFlagBits (MemoryUnmapFlags)
import Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap (MemoryUnmapInfo)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_MAP_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_UNMAP_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_MAP_INFO_KHR"
pattern STRUCTURE_TYPE_MEMORY_MAP_INFO_KHR = STRUCTURE_TYPE_MEMORY_MAP_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_UNMAP_INFO_KHR"
pattern STRUCTURE_TYPE_MEMORY_UNMAP_INFO_KHR = STRUCTURE_TYPE_MEMORY_UNMAP_INFO


-- No documentation found for TopLevel "vkMapMemory2KHR"
mapMemory2KHR = mapMemory2


-- No documentation found for TopLevel "vkUnmapMemory2KHR"
unmapMemory2KHR = unmapMemory2


-- No documentation found for TopLevel "VkMemoryUnmapFlagsKHR"
type MemoryUnmapFlagsKHR = MemoryUnmapFlags


-- No documentation found for TopLevel "VkMemoryUnmapFlagBitsKHR"
type MemoryUnmapFlagBitsKHR = MemoryUnmapFlagBits


-- No documentation found for TopLevel "VkMemoryMapInfoKHR"
type MemoryMapInfoKHR = MemoryMapInfo


-- No documentation found for TopLevel "VkMemoryUnmapInfoKHR"
type MemoryUnmapInfoKHR = MemoryUnmapInfo


type KHR_MAP_MEMORY_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAP_MEMORY_2_SPEC_VERSION"
pattern KHR_MAP_MEMORY_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAP_MEMORY_2_SPEC_VERSION = 1


type KHR_MAP_MEMORY_2_EXTENSION_NAME = "VK_KHR_map_memory2"

-- No documentation found for TopLevel "VK_KHR_MAP_MEMORY_2_EXTENSION_NAME"
pattern KHR_MAP_MEMORY_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAP_MEMORY_2_EXTENSION_NAME = "VK_KHR_map_memory2"

