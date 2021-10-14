{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_atomic_float2 - device extension
--
-- == VK_EXT_shader_atomic_float2
--
-- [__Name String__]
--     @VK_EXT_shader_atomic_float2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     274
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_EXT_shader_atomic_float@
--
-- [__Contact__]
--
--     -   Jason Ekstrand
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_atomic_float2] @jekstrand%0A<<Here describe the issue or question you have about the VK_EXT_shader_atomic_float2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-08-14
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires the VK_EXT_shader_atomic_float
--         extension.
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_atomic_float_min_max.html SPV_EXT_shader_atomic_float_min_max>
--         and
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_atomic_float16_add.html SPV_EXT_shader_atomic_float16_add>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_shader_atomic_float2.txt GLSL_EXT_shader_atomic_float2>
--
-- [__Contributors__]
--
--     -   Jason Ekstrand, Intel
--
-- == Description
--
-- This extension allows a shader to perform 16-bit floating-point atomic
-- operations on buffer and workgroup memory as well as floating-point
-- atomic minimum and maximum operations on buffer, workgroup, and image
-- memory. It advertises the SPIR-V @AtomicFloat16AddEXT@ capability which
-- allows atomic add operations on 16-bit floating-point numbers and the
-- SPIR-V @AtomicFloat16MinMaxEXT@, @AtomicFloat32MinMaxEXT@ and
-- @AtomicFloat64MinMaxEXT@ capabilities which allow atomic minimum and
-- maximum operations on floating-point numbers. The supported operations
-- include @OpAtomicFAddEXT@, @OpAtomicFMinEXT@ and @OpAtomicFMaxEXT@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderAtomicFloat2FeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_ATOMIC_FLOAT_2_EXTENSION_NAME'
--
-- -   'EXT_SHADER_ATOMIC_FLOAT_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_2_FEATURES_EXT'
--
-- == Issues
--
-- 1) Should this extension add support for 16-bit image atomics?
--
-- __RESOLVED__: No. While Vulkan supports creating storage images with
-- 'Vulkan.Core10.Enums.Format.FORMAT_R16_SFLOAT' and doing load and store
-- on them, the data in the shader has a 32-bit representation. Vulkan
-- currently has no facility for even basic reading or writing such images
-- using 16-bit float values in the shader. Adding such functionality would
-- be required before 16-bit image atomics would make sense and is outside
-- the scope of this extension.
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat16AddEXT AtomicFloat32MinMaxEXT>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat16MinMaxEXT AtomicFloat32MinMaxEXT>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat32MinMaxEXT AtomicFloat32MinMaxEXT>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat64MinMaxEXT AtomicFloat64MinMaxEXT>
--
-- == Version History
--
-- -   Revision 1, 2020-08-14 (Jason Ekstrand)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceShaderAtomicFloat2FeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_atomic_float2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_atomic_float2  (PhysicalDeviceShaderAtomicFloat2FeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderAtomicFloat2FeaturesEXT

instance ToCStruct PhysicalDeviceShaderAtomicFloat2FeaturesEXT
instance Show PhysicalDeviceShaderAtomicFloat2FeaturesEXT

instance FromCStruct PhysicalDeviceShaderAtomicFloat2FeaturesEXT

