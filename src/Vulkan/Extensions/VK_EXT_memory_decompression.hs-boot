{-# language CPP #-}
-- | = Name
--
-- VK_EXT_memory_decompression - device extension
--
-- = VK_EXT_memory_decompression
--
-- [__Name String__]
--     @VK_EXT_memory_decompression@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     551
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_memory_decompression] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_EXT_memory_decompression extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_memory_decompression.adoc VK_EXT_memory_decompression>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-23
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Spencer Fricke, LunarG
--
-- == Description
--
-- This extension adds support for performing memory to memory
-- decompression.
--
-- == New Commands
--
-- -   'cmdDecompressMemoryEXT'
--
-- -   'cmdDecompressMemoryIndirectCountEXT'
--
-- == New Structures
--
-- -   'DecompressMemoryInfoEXT'
--
-- -   'DecompressMemoryRegionEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMemoryDecompressionFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMemoryDecompressionPropertiesEXT'
--
-- == New Enums
--
-- -   'MemoryDecompressionMethodFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'MemoryDecompressionMethodFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MEMORY_DECOMPRESSION_EXTENSION_NAME'
--
-- -   'EXT_MEMORY_DECOMPRESSION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MEMORY_DECOMPRESSION_READ_BIT_EXT'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MEMORY_DECOMPRESSION_WRITE_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2':
--
--     -   'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_MEMORY_DECOMPRESSION_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MEMORY_DECOMPRESSION_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DECOMPRESS_MEMORY_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_EXT'
--
-- == Issues
--
-- 1) How does an app know the minimum size that @decompressedSize@ should
-- be set to?
--
-- __RESOLVED__: When decompressing, data is typically processed in chunks.
-- For example, with GDeflate 1.0, data is streamed in 64 KB blocks, but
-- the final block may be smaller. The exact size of this last block
-- depends on the compression method and original data size and so it must
-- be stored in the compressed bitstream so that the decompressor can set
-- @decompressedSize@ correctly. It is still ok for the last block to take
-- up all 64 KB, but setting it too low will cause issues and is undefined
-- behavior. It is a known limitation that the validation layers will not
-- be able to detect the minimum size of @decompressedSize@ unless it
-- decides to implement each decompression method specification.
--
-- == Version History
--
-- -   Revision 1, 2025-01-23 (Daniel Koch)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_memory_decompression Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_memory_decompression  ( DecompressMemoryInfoEXT
                                                      , DecompressMemoryRegionEXT
                                                      , PhysicalDeviceMemoryDecompressionFeaturesEXT
                                                      , PhysicalDeviceMemoryDecompressionPropertiesEXT
                                                      , MemoryDecompressionMethodFlagsEXT
                                                      , MemoryDecompressionMethodFlagBitsEXT
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DecompressMemoryInfoEXT

instance ToCStruct DecompressMemoryInfoEXT
instance Show DecompressMemoryInfoEXT

instance FromCStruct DecompressMemoryInfoEXT


data DecompressMemoryRegionEXT

instance ToCStruct DecompressMemoryRegionEXT
instance Show DecompressMemoryRegionEXT

instance FromCStruct DecompressMemoryRegionEXT


data PhysicalDeviceMemoryDecompressionFeaturesEXT

instance ToCStruct PhysicalDeviceMemoryDecompressionFeaturesEXT
instance Show PhysicalDeviceMemoryDecompressionFeaturesEXT

instance FromCStruct PhysicalDeviceMemoryDecompressionFeaturesEXT


data PhysicalDeviceMemoryDecompressionPropertiesEXT

instance ToCStruct PhysicalDeviceMemoryDecompressionPropertiesEXT
instance Show PhysicalDeviceMemoryDecompressionPropertiesEXT

instance FromCStruct PhysicalDeviceMemoryDecompressionPropertiesEXT


type MemoryDecompressionMethodFlagsEXT = MemoryDecompressionMethodFlagBitsEXT

data MemoryDecompressionMethodFlagBitsEXT

