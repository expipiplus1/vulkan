{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_clock - device extension
--
-- == VK_KHR_shader_clock
--
-- [__Name String__]
--     @VK_KHR_shader_clock@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     182
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
--     -   Aaron Hagan
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_clock] @ahagan%0A<<Here describe the issue or question you have about the VK_KHR_shader_clock extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-4-25
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_shader_clock.html SPV_KHR_shader_clock>.
--
--     -   This extension provides API support for
--         <https://registry.khronos.org/OpenGL/extensions/ARB/ARB_shader_clock.txt ARB_shader_clock>
--         and
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GL_EXT_shader_realtime_clock.txt EXT_shader_realtime_clock>
--
-- [__Contributors__]
--
--     -   Aaron Hagan, AMD
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension advertises the SPIR-V @ShaderClockKHR@ capability for
-- Vulkan, which allows a shader to query a real-time or monotonically
-- incrementing counter at the subgroup level or across the device level.
-- The two valid SPIR-V scopes for @OpReadClockKHR@ are @Subgroup@ and
-- 'Vulkan.Core10.Handles.Device'.
--
-- When using GLSL source-based shading languages, the
-- @clockRealtime*EXT@() timing functions map to the @OpReadClockKHR@
-- instruction with a scope of 'Vulkan.Core10.Handles.Device', and the
-- @clock*ARB@() timing functions map to the @OpReadClockKHR@ instruction
-- with a scope of @Subgroup@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderClockFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_CLOCK_EXTENSION_NAME'
--
-- -   'KHR_SHADER_CLOCK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-ShaderClockKHR ShaderClockKHR>
--
-- == Version History
--
-- -   Revision 1, 2019-4-25 (Aaron Hagan)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceShaderClockFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_clock Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_clock  (PhysicalDeviceShaderClockFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderClockFeaturesKHR

instance ToCStruct PhysicalDeviceShaderClockFeaturesKHR
instance Show PhysicalDeviceShaderClockFeaturesKHR

instance FromCStruct PhysicalDeviceShaderClockFeaturesKHR

