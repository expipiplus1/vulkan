{-# language CPP #-}
-- | = Name
--
-- VK_EXT_texel_buffer_alignment - device extension
--
-- = Registered Extension Number
--
-- 282
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-06-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds more expressive alignment requirements for uniform
-- and storage texel buffers. Some implementations have single texel
-- alignment requirements that can’t be expressed via
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minTexelBufferOffsetAlignment@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTexelBufferAlignmentFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceTexelBufferAlignmentPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_TEXEL_BUFFER_ALIGNMENT_EXTENSION_NAME'
--
-- -   'EXT_TEXEL_BUFFER_ALIGNMENT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2019-06-06 (Jeff Bolz)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceTexelBufferAlignmentFeaturesEXT',
-- 'PhysicalDeviceTexelBufferAlignmentPropertiesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_texel_buffer_alignment Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_texel_buffer_alignment  ( PhysicalDeviceTexelBufferAlignmentFeaturesEXT
                                                        , PhysicalDeviceTexelBufferAlignmentPropertiesEXT
                                                        ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceTexelBufferAlignmentFeaturesEXT

instance ToCStruct PhysicalDeviceTexelBufferAlignmentFeaturesEXT
instance Show PhysicalDeviceTexelBufferAlignmentFeaturesEXT

instance FromCStruct PhysicalDeviceTexelBufferAlignmentFeaturesEXT


data PhysicalDeviceTexelBufferAlignmentPropertiesEXT

instance ToCStruct PhysicalDeviceTexelBufferAlignmentPropertiesEXT
instance Show PhysicalDeviceTexelBufferAlignmentPropertiesEXT

instance FromCStruct PhysicalDeviceTexelBufferAlignmentPropertiesEXT

