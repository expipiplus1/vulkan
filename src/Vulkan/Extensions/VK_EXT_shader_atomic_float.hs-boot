{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_atomic_float - device extension
--
-- == VK_EXT_shader_atomic_float
--
-- [__Name String__]
--     @VK_EXT_shader_atomic_float@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     261
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
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_atomic_float] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_EXT_shader_atomic_float extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-15
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_atomic_float_add.html SPV_EXT_shader_atomic_float_add>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_shader_atomic_float.txt GL_EXT_shader_atomic_float>
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension allows a shader to contain floating-point atomic
-- operations on buffer, workgroup, and image memory. It also advertises
-- the SPIR-V @AtomicFloat32AddEXT@ and @AtomicFloat64AddEXT@ capabilities
-- that allows atomic addition on floating-points numbers. The supported
-- operations include @OpAtomicFAddEXT@, @OpAtomicExchange@, @OpAtomicLoad@
-- and @OpAtomicStore@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderAtomicFloatFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_ATOMIC_FLOAT_EXTENSION_NAME'
--
-- -   'EXT_SHADER_ATOMIC_FLOAT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_FEATURES_EXT'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat32AddEXT AtomicFloat32AddEXT>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat64AddEXT AtomicFloat64AddEXT>
--
-- == Version History
--
-- -   Revision 1, 2020-07-15 (Vikram Kushwaha)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceShaderAtomicFloatFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_shader_atomic_float Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_atomic_float  (PhysicalDeviceShaderAtomicFloatFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderAtomicFloatFeaturesEXT

instance ToCStruct PhysicalDeviceShaderAtomicFloatFeaturesEXT
instance Show PhysicalDeviceShaderAtomicFloatFeaturesEXT

instance FromCStruct PhysicalDeviceShaderAtomicFloatFeaturesEXT

