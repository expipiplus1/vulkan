{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_tile_memory_heap - device extension
--
-- = VK_QCOM_tile_memory_heap
--
-- [__Name String__]
--     @VK_QCOM_tile_memory_heap@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     548
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_memory_requirements2 VK_KHR_get_memory_requirements2>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_QCOM_tile_properties
--
-- [__Contact__]
--
--     -   Patrick Boyle
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_tile_memory_heap] @pboyleQCOM%0A*Here describe the issue or question you have about the VK_QCOM_tile_memory_heap extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_tile_memory_heap.adoc VK_QCOM_tile_memory_heap>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-05
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_QCOM_tile_properties@
--
--     -   Interacts with @VK_QCOM_tile_shading@
--
-- [__Contributors__]
--
--     -   Patrick Boyle, Qualcomm Technologies, Inc.
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Srihari Babu Alla, Qualcomm Technologies, Inc.
--
--     -   Kevin Matlage, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension adds a new memory heap which allows applications to
-- allocate and manage tile memory. A tile memory heap is denoted by the
-- new
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
-- property. Memory contents within this heap behave differently than other
-- heaps and only persist its memory contents within a command buffer
-- submission batch boundary. This boundary may be extended to a queue
-- submit boundary by querying @queueSubmitBoundary@ in the new
-- 'PhysicalDeviceTileMemoryHeapPropertiesQCOM' structure.
--
-- Tile memory from this heap can be bound to VkImages or VkBuffers. The
-- following new usage flags
-- 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TILE_MEMORY_BIT_QCOM',
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TILE_MEMORY_BIT_QCOM',
-- 'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_TILE_MEMORY_BIT_QCOM'
-- were added for this. A new extended structure is added to get memory
-- requirements for tile memory 'TileMemoryRequirementsQCOM'.
--
-- A new command is added to bind tile memory to a command buffer in order
-- to access and persist tile memory contents while executing commands
-- 'cmdBindTileMemoryQCOM'.
--
-- This extension can be used in combination with
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>
-- with the new structure 'TileMemorySizeInfoQCOM'.
--
-- == Issues
--
-- None.
--
-- == New Commands
--
-- -   'cmdBindTileMemoryQCOM'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'TileMemoryBindInfoQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2':
--
--     -   'TileMemoryRequirementsQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTileMemoryHeapFeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceTileMemoryHeapPropertiesQCOM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Pass.RenderPassCreateInfo',
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo':
--
--     -   'TileMemorySizeInfoQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_TILE_MEMORY_HEAP_EXTENSION_NAME'
--
-- -   'QCOM_TILE_MEMORY_HEAP_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TILE_MEMORY_BIT_QCOM'
--
-- -   Extending
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2':
--
--     -   'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_TILE_MEMORY_BIT_QCOM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TILE_MEMORY_BIT_QCOM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.MemoryHeapFlagBits.MemoryHeapFlagBits':
--
--     -   'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_MEMORY_HEAP_PROPERTIES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TILE_MEMORY_BIND_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TILE_MEMORY_REQUIREMENTS_QCOM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TILE_MEMORY_SIZE_INFO_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2025-03-26 (Patrick Boyle)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_tile_memory_heap Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_tile_memory_heap  ( PhysicalDeviceTileMemoryHeapFeaturesQCOM
                                                   , PhysicalDeviceTileMemoryHeapPropertiesQCOM
                                                   , TileMemoryBindInfoQCOM
                                                   , TileMemoryRequirementsQCOM
                                                   , TileMemorySizeInfoQCOM
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceTileMemoryHeapFeaturesQCOM

instance ToCStruct PhysicalDeviceTileMemoryHeapFeaturesQCOM
instance Show PhysicalDeviceTileMemoryHeapFeaturesQCOM

instance FromCStruct PhysicalDeviceTileMemoryHeapFeaturesQCOM


data PhysicalDeviceTileMemoryHeapPropertiesQCOM

instance ToCStruct PhysicalDeviceTileMemoryHeapPropertiesQCOM
instance Show PhysicalDeviceTileMemoryHeapPropertiesQCOM

instance FromCStruct PhysicalDeviceTileMemoryHeapPropertiesQCOM


data TileMemoryBindInfoQCOM

instance ToCStruct TileMemoryBindInfoQCOM
instance Show TileMemoryBindInfoQCOM

instance FromCStruct TileMemoryBindInfoQCOM


data TileMemoryRequirementsQCOM

instance ToCStruct TileMemoryRequirementsQCOM
instance Show TileMemoryRequirementsQCOM

instance FromCStruct TileMemoryRequirementsQCOM


data TileMemorySizeInfoQCOM

instance ToCStruct TileMemorySizeInfoQCOM
instance Show TileMemorySizeInfoQCOM

instance FromCStruct TileMemorySizeInfoQCOM

