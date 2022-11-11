{-# language CPP #-}
-- | = Name
--
-- VK_NV_copy_memory_indirect - device extension
--
-- == VK_NV_copy_memory_indirect
--
-- [__Name String__]
--     @VK_NV_copy_memory_indirect@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     427
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
--     -   Requires @VK_KHR_buffer_device_address@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_copy_memory_indirect] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_NV_copy_memory_indirect extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-10-14
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension adds support for performing copies between memory and
-- image regions using indirect parameters that are read by the device from
-- a buffer during execution. This functionality /may/ be useful for
-- performing copies where the copy parameters are not known during the
-- command buffer creation time.
--
-- == New Commands
--
-- -   'cmdCopyMemoryIndirectNV'
--
-- -   'cmdCopyMemoryToImageIndirectNV'
--
-- == New Structures
--
-- -   'CopyMemoryIndirectCommandNV'
--
-- -   'CopyMemoryToImageIndirectCommandNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCopyMemoryIndirectFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCopyMemoryIndirectPropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_COPY_MEMORY_INDIRECT_EXTENSION_NAME'
--
-- -   'NV_COPY_MEMORY_INDIRECT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_NV'
--
-- == Version History
--
-- -   Revision 1, 2022-10-14 (Vikram Kushwaha)
--
--     -   Initial draft
--
-- == See Also
--
-- 'CopyMemoryIndirectCommandNV', 'CopyMemoryToImageIndirectCommandNV',
-- 'PhysicalDeviceCopyMemoryIndirectFeaturesNV',
-- 'PhysicalDeviceCopyMemoryIndirectPropertiesNV',
-- 'cmdCopyMemoryIndirectNV', 'cmdCopyMemoryToImageIndirectNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_copy_memory_indirect Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_copy_memory_indirect  ( CopyMemoryIndirectCommandNV
                                                     , CopyMemoryToImageIndirectCommandNV
                                                     , PhysicalDeviceCopyMemoryIndirectFeaturesNV
                                                     , PhysicalDeviceCopyMemoryIndirectPropertiesNV
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CopyMemoryIndirectCommandNV

instance ToCStruct CopyMemoryIndirectCommandNV
instance Show CopyMemoryIndirectCommandNV

instance FromCStruct CopyMemoryIndirectCommandNV


data CopyMemoryToImageIndirectCommandNV

instance ToCStruct CopyMemoryToImageIndirectCommandNV
instance Show CopyMemoryToImageIndirectCommandNV

instance FromCStruct CopyMemoryToImageIndirectCommandNV


data PhysicalDeviceCopyMemoryIndirectFeaturesNV

instance ToCStruct PhysicalDeviceCopyMemoryIndirectFeaturesNV
instance Show PhysicalDeviceCopyMemoryIndirectFeaturesNV

instance FromCStruct PhysicalDeviceCopyMemoryIndirectFeaturesNV


data PhysicalDeviceCopyMemoryIndirectPropertiesNV

instance ToCStruct PhysicalDeviceCopyMemoryIndirectPropertiesNV
instance Show PhysicalDeviceCopyMemoryIndirectPropertiesNV

instance FromCStruct PhysicalDeviceCopyMemoryIndirectPropertiesNV

