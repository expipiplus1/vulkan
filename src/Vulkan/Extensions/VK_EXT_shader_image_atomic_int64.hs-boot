{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_image_atomic_int64 - device extension
--
-- == VK_EXT_shader_image_atomic_int64
--
-- [__Name String__]
--     @VK_EXT_shader_image_atomic_int64@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     235
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_image_atomic_int64] @tobski%0A<<Here describe the issue or question you have about the VK_EXT_shader_image_atomic_int64 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-14
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_image_int64.html SPV_EXT_shader_image_int64>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_shader_image_int64.txt GLSL_EXT_shader_image_int64>
--
-- [__Contributors__]
--
--     -   Matthaeus Chajdas, AMD
--
--     -   Graham Wihlidal, Epic Games
--
--     -   Tobias Hector, AMD
--
--     -   Jeff Bolz, Nvidia
--
--     -   Jason Ekstrand, Intel
--
-- == Description
--
-- This extension extends existing 64-bit integer atomic support to enable
-- these operations on images as well.
--
-- When working with large 2- or 3-dimensional data sets (e.g.
-- rasterization or screen-space effects), image accesses are generally
-- more efficient than equivalent buffer accesses. This extension allows
-- applications relying on 64-bit integer atomics in this manner to quickly
-- improve performance with only relatively minor code changes.
--
-- 64-bit integer atomic support is guaranteed for optimally tiled images
-- with the 'Vulkan.Core10.Enums.Format.FORMAT_R64_UINT' and
-- 'Vulkan.Core10.Enums.Format.FORMAT_R64_SINT' formats.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderImageAtomicInt64FeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_IMAGE_ATOMIC_INT64_EXTENSION_NAME'
--
-- -   'EXT_SHADER_IMAGE_ATOMIC_INT64_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2020-07-14 (Tobias Hector)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceShaderImageAtomicInt64FeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_shader_image_atomic_int64 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_image_atomic_int64  (PhysicalDeviceShaderImageAtomicInt64FeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderImageAtomicInt64FeaturesEXT

instance ToCStruct PhysicalDeviceShaderImageAtomicInt64FeaturesEXT
instance Show PhysicalDeviceShaderImageAtomicInt64FeaturesEXT

instance FromCStruct PhysicalDeviceShaderImageAtomicInt64FeaturesEXT

