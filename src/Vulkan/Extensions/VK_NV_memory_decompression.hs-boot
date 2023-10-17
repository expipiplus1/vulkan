{-# language CPP #-}
-- | = Name
--
-- VK_NV_memory_decompression - device extension
--
-- == VK_NV_memory_decompression
--
-- [__Name String__]
--     @VK_NV_memory_decompression@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     428
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_memory_decompression] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_NV_memory_decompression extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-01-31
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension adds support for performing memory to memory
-- decompression.
--
-- == New Commands
--
-- -   'cmdDecompressMemoryIndirectCountNV'
--
-- -   'cmdDecompressMemoryNV'
--
-- == New Structures
--
-- -   'DecompressMemoryRegionNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMemoryDecompressionFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMemoryDecompressionPropertiesNV'
--
-- == New Enums
--
-- -   'MemoryDecompressionMethodFlagBitsNV'
--
-- == New Bitmasks
--
-- -   'MemoryDecompressionMethodFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_MEMORY_DECOMPRESSION_EXTENSION_NAME'
--
-- -   'NV_MEMORY_DECOMPRESSION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_DECOMPRESSION_PROPERTIES_NV'
--
-- == Version History
--
-- -   Revision 1, 2022-01-31 (Vikram Kushwaha)
--
--     -   Initial draft
--
-- == See Also
--
-- 'DecompressMemoryRegionNV', 'MemoryDecompressionMethodFlagBitsNV',
-- 'MemoryDecompressionMethodFlagsNV',
-- 'PhysicalDeviceMemoryDecompressionFeaturesNV',
-- 'PhysicalDeviceMemoryDecompressionPropertiesNV',
-- 'cmdDecompressMemoryIndirectCountNV', 'cmdDecompressMemoryNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_memory_decompression Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_memory_decompression  ( DecompressMemoryRegionNV
                                                     , PhysicalDeviceMemoryDecompressionFeaturesNV
                                                     , PhysicalDeviceMemoryDecompressionPropertiesNV
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DecompressMemoryRegionNV

instance ToCStruct DecompressMemoryRegionNV
instance Show DecompressMemoryRegionNV

instance FromCStruct DecompressMemoryRegionNV


data PhysicalDeviceMemoryDecompressionFeaturesNV

instance ToCStruct PhysicalDeviceMemoryDecompressionFeaturesNV
instance Show PhysicalDeviceMemoryDecompressionFeaturesNV

instance FromCStruct PhysicalDeviceMemoryDecompressionFeaturesNV


data PhysicalDeviceMemoryDecompressionPropertiesNV

instance ToCStruct PhysicalDeviceMemoryDecompressionPropertiesNV
instance Show PhysicalDeviceMemoryDecompressionPropertiesNV

instance FromCStruct PhysicalDeviceMemoryDecompressionPropertiesNV

