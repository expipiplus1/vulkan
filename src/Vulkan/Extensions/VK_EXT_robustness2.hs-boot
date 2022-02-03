{-# language CPP #-}
-- | = Name
--
-- VK_EXT_robustness2 - device extension
--
-- == VK_EXT_robustness2
--
-- [__Name String__]
--     @VK_EXT_robustness2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     287
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
--     -   Liam Middlebrook
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_robustness2] @liam-middlebrook%0A<<Here describe the issue or question you have about the VK_EXT_robustness2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-01-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Liam Middlebrook, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds stricter requirements for how out of bounds reads
-- and writes are handled. Most accesses /must/ be tightly bounds-checked,
-- out of bounds writes /must/ be discarded, out of bound reads /must/
-- return zero. Rather than allowing multiple possible (0,0,0,x) vectors,
-- the out of bounds values are treated as zero, and then missing
-- components are inserted based on the format as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#textures-conversion-to-rgba Conversion to RGBA>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#fxvertex-input-extraction vertex input attribute extraction>.
--
-- These additional requirements /may/ be expensive on some
-- implementations, and should only be enabled when truly necessary.
--
-- This extension also adds support for “null descriptors”, where
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE' /can/ be used instead of a
-- valid handle. Accesses to null descriptors have well-defined behavior,
-- and do not rely on robustness.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRobustness2FeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceRobustness2PropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_ROBUSTNESS_2_EXTENSION_NAME'
--
-- -   'EXT_ROBUSTNESS_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT'
--
-- == Issues
--
-- 1.  Why do
--     'PhysicalDeviceRobustness2PropertiesEXT'::@robustUniformBufferAccessSizeAlignment@
--     and
--     'PhysicalDeviceRobustness2PropertiesEXT'::@robustStorageBufferAccessSizeAlignment@
--     exist?
--
-- __RESOLVED__: Some implementations cannot efficiently tightly
-- bounds-check all buffer accesses. Rather, the size of the bound range is
-- padded to some power of two multiple, up to 256 bytes for uniform
-- buffers and up to 4 bytes for storage buffers, and that padded size is
-- bounds-checked. This is sufficient to implement D3D-like behavior,
-- because D3D only allows binding whole uniform buffers or ranges that are
-- a multiple of 256 bytes, and D3D raw and structured buffers only support
-- 32-bit accesses.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2019-11-01 (Jeff Bolz, Liam Middlebrook)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceRobustness2FeaturesEXT',
-- 'PhysicalDeviceRobustness2PropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_robustness2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_robustness2  ( PhysicalDeviceRobustness2FeaturesEXT
                                             , PhysicalDeviceRobustness2PropertiesEXT
                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceRobustness2FeaturesEXT

instance ToCStruct PhysicalDeviceRobustness2FeaturesEXT
instance Show PhysicalDeviceRobustness2FeaturesEXT

instance FromCStruct PhysicalDeviceRobustness2FeaturesEXT


data PhysicalDeviceRobustness2PropertiesEXT

instance ToCStruct PhysicalDeviceRobustness2PropertiesEXT
instance Show PhysicalDeviceRobustness2PropertiesEXT

instance FromCStruct PhysicalDeviceRobustness2PropertiesEXT

