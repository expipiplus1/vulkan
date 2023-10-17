{-# language CPP #-}
-- | = Name
--
-- VK_EXT_texel_buffer_alignment - device extension
--
-- == VK_EXT_texel_buffer_alignment
--
-- [__Name String__]
--     @VK_EXT_texel_buffer_alignment@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     282
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_texel_buffer_alignment] @jeffbolznv%0A*Here describe the issue or question you have about the VK_EXT_texel_buffer_alignment extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-06-06
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds more expressive alignment requirements for uniform
-- and storage texel buffers. Some implementations have single texel
-- alignment requirements that cannot be expressed via
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
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- EXT suffix omitted. However, only the properties structure is promoted.
-- The feature structure is not promoted and @texelBufferAlignment@ is
-- enabled if using a Vulkan 1.3 instance. The original type name is still
-- available as an alias of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2019-06-06 (Jeff Bolz)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceTexelBufferAlignmentFeaturesEXT',
-- 'PhysicalDeviceTexelBufferAlignmentPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_texel_buffer_alignment Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_texel_buffer_alignment  (PhysicalDeviceTexelBufferAlignmentFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceTexelBufferAlignmentFeaturesEXT

instance ToCStruct PhysicalDeviceTexelBufferAlignmentFeaturesEXT
instance Show PhysicalDeviceTexelBufferAlignmentFeaturesEXT

instance FromCStruct PhysicalDeviceTexelBufferAlignmentFeaturesEXT

