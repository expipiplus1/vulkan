{-# language CPP #-}
-- | = Name
--
-- VK_EXT_external_memory_metal - device extension
--
-- = VK_EXT_external_memory_metal
--
-- [__Name String__]
--     @VK_EXT_external_memory_metal@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     603
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory VK_KHR_external_memory>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Aitor Camacho Larrondo
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_external_memory_metal] @aitor-lunarg%0A*Here describe the issue or question you have about the VK_EXT_external_memory_metal extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_external_memory_metal.adoc VK_EXT_external_memory_metal>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-07-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Aitor Camacho Larrondo, LunarG Inc.
--
-- == Description
--
-- An application may wish to reference device memory in multiple Vulkan
-- device instances, in multiple processes, and\/or in Metal API. This
-- extension enables an application to export and import Metal handles from
-- Vulkan memory objects such that the underlying resources can be
-- referenced outside the scope of the Vulkan device instance that created
-- them.
--
-- == New Commands
--
-- -   'getMemoryMetalHandleEXT'
--
-- -   'getMemoryMetalHandlePropertiesEXT'
--
-- == New Structures
--
-- -   'MemoryGetMetalHandleInfoEXT'
--
-- -   'MemoryMetalHandlePropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'ImportMemoryMetalHandleInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_EXTERNAL_MEMORY_METAL_EXTENSION_NAME'
--
-- -   'EXT_EXTERNAL_MEMORY_METAL_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLBUFFER_BIT_EXT'
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLHEAP_BIT_EXT'
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_MTLTEXTURE_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMPORT_MEMORY_METAL_HANDLE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_METAL_HANDLE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_METAL_HANDLE_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2024-07-18 (Aitor Camacho Larrondo)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_external_memory_metal Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_external_memory_metal  ( ImportMemoryMetalHandleInfoEXT
                                                       , MemoryGetMetalHandleInfoEXT
                                                       , MemoryMetalHandlePropertiesEXT
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImportMemoryMetalHandleInfoEXT

instance ToCStruct ImportMemoryMetalHandleInfoEXT
instance Show ImportMemoryMetalHandleInfoEXT

instance FromCStruct ImportMemoryMetalHandleInfoEXT


data MemoryGetMetalHandleInfoEXT

instance ToCStruct MemoryGetMetalHandleInfoEXT
instance Show MemoryGetMetalHandleInfoEXT

instance FromCStruct MemoryGetMetalHandleInfoEXT


data MemoryMetalHandlePropertiesEXT

instance ToCStruct MemoryMetalHandlePropertiesEXT
instance Show MemoryMetalHandlePropertiesEXT

instance FromCStruct MemoryMetalHandlePropertiesEXT

