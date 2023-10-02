{-# language CPP #-}
-- | = Name
--
-- VK_KHR_fragment_shader_barycentric - device extension
--
-- == VK_KHR_fragment_shader_barycentric
--
-- [__Name String__]
--     @VK_KHR_fragment_shader_barycentric@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     323
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Stu Smith
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_fragment_shader_barycentric.adoc VK_KHR_fragment_shader_barycentric>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-03-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_fragment_shader_barycentric.html SPV_KHR_fragment_shader_barycentric>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_fragment_shader_barycentric.txt GL_EXT_fragment_shader_barycentric>
--
-- [__Contributors__]
--
--     -   Stu Smith, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Slawek Grajewski, Intel
--
--     -   Pat Brown, NVIDIA
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Contributors to the VK_NV_fragment_shader_barycentric
--         specification
--
-- == Description
--
-- This extension is based on the @VK_NV_fragment_shader_barycentric@
-- extension, and adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_fragment_shader_barycentric.html SPV_KHR_fragment_shader_barycentric>
--
-- The extension provides access to three additional fragment shader
-- variable decorations in SPIR-V:
--
-- -   @PerVertexKHR@, which indicates that a fragment shader input will
--     not have interpolated values, but instead must be accessed with an
--     extra array index that identifies one of the vertices of the
--     primitive producing the fragment
--
-- -   @BaryCoordKHR@, which indicates that the variable is a
--     three-component floating-point vector holding barycentric weights
--     for the fragment produced using perspective interpolation
--
-- -   @BaryCoordNoPerspKHR@, which indicates that the variable is a
--     three-component floating-point vector holding barycentric weights
--     for the fragment produced using linear interpolation
--
-- When using GLSL source-based shader languages, the following variables
-- from @GL_EXT_fragment_shader_barycentric@ map to these SPIR-V built-in
-- decorations:
--
-- -   @in vec3 gl_BaryCoordEXT;@ → @BaryCoordKHR@
--
-- -   @in vec3 gl_BaryCoordNoPerspEXT;@ → @BaryCoordNoPerspKHR@
--
-- GLSL variables declared using the @pervertexEXT@ GLSL qualifier are
-- expected to be decorated with @PerVertexKHR@ in SPIR-V.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentShaderBarycentricFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentShaderBarycentricPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME'
--
-- -   'KHR_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_PROPERTIES_KHR'
--
-- == New Built-In Variables
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-barycoordkhr BaryCoordKHR>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-barycoordnoperspkhr BaryCoordNoPerspKHR>
--
-- == New SPIR-V Decorations
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-interpolation-decorations-pervertexkhr PerVertexKHR>
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-FragmentBarycentricKHR FragmentBarycentricKHR>
--
-- == Issues
--
-- 1) What are the interactions with MSAA and how are @BaryCoordKHR@ and
-- @BaryCoordNoPerspKHR@ interpolated?
--
-- __RESOLVED__: The inputs decorated with @BaryCoordKHR@ or
-- @BaryCoordNoPerspKHR@ /may/ also be decorated with the @Centroid@ or
-- @Sample@ qualifiers to specify interpolation, like any other fragment
-- shader input. If
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderSampleRateInterpolationFunctions shaderSampleRateInterpolationFunctions>
-- is enabled, the extended instructions InterpolateAtCentroid,
-- InterpolateAtOffset, and InterpolateAtSample from the GLSL.std.450 /may/
-- also be used with inputs decorated with @BaryCoordKHR@ or
-- @BaryCoordNoPerspKHR@.
--
-- == Version History
--
-- -   Revision 1, 2022-03-10 (Stu Smith)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceFragmentShaderBarycentricFeaturesKHR',
-- 'PhysicalDeviceFragmentShaderBarycentricPropertiesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_fragment_shader_barycentric Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_fragment_shader_barycentric  ( PhysicalDeviceFragmentShaderBarycentricFeaturesKHR
                                                             , PhysicalDeviceFragmentShaderBarycentricPropertiesKHR
                                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceFragmentShaderBarycentricFeaturesKHR

instance ToCStruct PhysicalDeviceFragmentShaderBarycentricFeaturesKHR
instance Show PhysicalDeviceFragmentShaderBarycentricFeaturesKHR

instance FromCStruct PhysicalDeviceFragmentShaderBarycentricFeaturesKHR


data PhysicalDeviceFragmentShaderBarycentricPropertiesKHR

instance ToCStruct PhysicalDeviceFragmentShaderBarycentricPropertiesKHR
instance Show PhysicalDeviceFragmentShaderBarycentricPropertiesKHR

instance FromCStruct PhysicalDeviceFragmentShaderBarycentricPropertiesKHR

