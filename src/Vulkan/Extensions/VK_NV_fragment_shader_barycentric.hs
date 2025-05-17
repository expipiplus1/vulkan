{-# language CPP #-}
-- | = Name
--
-- VK_NV_fragment_shader_barycentric - device extension
--
-- == VK_NV_fragment_shader_barycentric
--
-- [__Name String__]
--     @VK_NV_fragment_shader_barycentric@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     204
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_fragment_shader_barycentric.html SPV_NV_fragment_shader_barycentric>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shader_barycentric VK_KHR_fragment_shader_barycentric>
--         extension
--
-- [__Contact__]
--
--     -   Pat Brown
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_fragment_shader_barycentric] @nvpbrown%0A*Here describe the issue or question you have about the VK_NV_fragment_shader_barycentric extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-08-03
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_fragment_shader_barycentric.txt GL_NV_fragment_shader_barycentric>
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_fragment_shader_barycentric.html SPV_NV_fragment_shader_barycentric>
--
-- The extension provides access to three additional fragment shader
-- variable decorations in SPIR-V:
--
-- -   @PerVertexNV@, which indicates that a fragment shader input will not
--     have interpolated values, but instead must be accessed with an extra
--     array index that identifies one of the vertices of the primitive
--     producing the fragment
--
-- -   @BaryCoordNV@, which indicates that the variable is a
--     three-component floating-point vector holding barycentric weights
--     for the fragment produced using perspective interpolation
--
-- -   @BaryCoordNoPerspNV@, which indicates that the variable is a
--     three-component floating-point vector holding barycentric weights
--     for the fragment produced using linear interpolation
--
-- When using GLSL source-based shader languages, the following variables
-- from @GL_NV_fragment_shader_barycentric@ maps to these SPIR-V built-in
-- decorations:
--
-- -   @in vec3 gl_BaryCoordNV;@ → @BaryCoordNV@
--
-- -   @in vec3 gl_BaryCoordNoPerspNV;@ → @BaryCoordNoPerspNV@
--
-- GLSL variables declared using the @__pervertexNV@ GLSL qualifier are
-- expected to be decorated with @PerVertexNV@ in SPIR-V.
--
-- == Promotion to @VK_KHR_fragment_shader_barycentric@
--
-- All functionality in this extension is included in
-- @VK_KHR_fragment_shader_barycentric@, with the suffix changed to KHR.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentShaderBarycentricFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME'
--
-- -   'NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV'
--
-- == New Built-In Variables
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-barycoordkhr BaryCoordNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-barycoordnoperspkhr BaryCoordNoPerspNV>
--
-- == New SPIR-V Decorations
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-interpolation-decorations-pervertexkhr PerVertexNV>
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-FragmentBarycentricKHR FragmentBarycentricNV>
--
-- == Issues
--
-- (1) The AMD_shader_explicit_vertex_parameter extension provides similar
-- functionality. Why write a new extension, and how is this extension
-- different?
--
-- __RESOLVED__: For the purposes of Vulkan\/SPIR-V, we chose to implement
-- a separate extension due to several functional differences.
--
-- First, the hardware supporting this extension can provide a
-- three-component barycentric weight vector for variables decorated with
-- @BaryCoordNV@, while variables decorated with @BaryCoordSmoothAMD@
-- provide only two components. In some cases, it may be more efficient to
-- explicitly interpolate an attribute via:
--
-- > float value = (baryCoordNV.x * v[0].attrib +
-- >                baryCoordNV.y * v[1].attrib +
-- >                baryCoordNV.z * v[2].attrib);
--
-- instead of
--
-- > float value = (baryCoordSmoothAMD.x * (v[0].attrib - v[2].attrib) +
-- >                baryCoordSmoothAMD.y * (v[1].attrib - v[2].attrib) +
-- >                v[2].attrib);
--
-- Additionally, the semantics of the decoration @BaryCoordPullModelAMD@ do
-- not appear to map to anything supported by the initial hardware
-- implementation of this extension.
--
-- This extension provides a smaller number of decorations than the AMD
-- extension, as we expect that shaders could derive variables decorated
-- with things like @BaryCoordNoPerspCentroidAMD@ with explicit attribute
-- interpolation instructions. One other relevant difference is that
-- explicit per-vertex attribute access using this extension does not
-- require a constant vertex number.
--
-- (2) Why do the built-in SPIR-V decorations for this extension include
-- two separate built-ins @BaryCoordNV@ and @BaryCoordNoPerspNV@ when a “no
-- perspective” variable could be decorated with @BaryCoordNV@ and
-- @NoPerspective@?
--
-- __RESOLVED__: The SPIR-V extension for this feature chose to mirror the
-- behavior of the GLSL extension, which provides two built-in variables.
-- Additionally, it is not clear that its a good idea (or even legal) to
-- have two variables using the “same attribute”, but with different
-- interpolation modifiers.
--
-- == Version History
--
-- -   Revision 1, 2018-08-03 (Pat Brown)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceFragmentShaderBarycentricFeaturesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_fragment_shader_barycentric Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_fragment_shader_barycentric  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV
                                                            , PhysicalDeviceFragmentShaderBarycentricFeaturesNV
                                                            , NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
                                                            , pattern NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION
                                                            , NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
                                                            , pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME
                                                            , PhysicalDeviceFragmentShaderBarycentricFeaturesKHR(..)
                                                            ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_fragment_shader_barycentric (PhysicalDeviceFragmentShaderBarycentricFeaturesKHR)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_KHR))
import Vulkan.Extensions.VK_KHR_fragment_shader_barycentric (PhysicalDeviceFragmentShaderBarycentricFeaturesKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV = STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_KHR


-- No documentation found for TopLevel "VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV"
type PhysicalDeviceFragmentShaderBarycentricFeaturesNV = PhysicalDeviceFragmentShaderBarycentricFeaturesKHR


type NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION"
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION :: forall a . Integral a => a
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_SPEC_VERSION = 1


type NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME = "VK_NV_fragment_shader_barycentric"

-- No documentation found for TopLevel "VK_NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME"
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_FRAGMENT_SHADER_BARYCENTRIC_EXTENSION_NAME = "VK_NV_fragment_shader_barycentric"

