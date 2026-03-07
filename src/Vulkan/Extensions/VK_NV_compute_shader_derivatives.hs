{-# language CPP #-}
-- | = Name
--
-- VK_NV_compute_shader_derivatives - device extension
--
-- = VK_NV_compute_shader_derivatives
--
-- [__Name String__]
--     @VK_NV_compute_shader_derivatives@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     202
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_compute_shader_derivatives.html SPV_NV_compute_shader_derivatives>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_compute_shader_derivatives VK_KHR_compute_shader_derivatives>
--         extension
--
-- [__Contact__]
--
--     -   Pat Brown
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_compute_shader_derivatives] @nvpbrown%0A*Here describe the issue or question you have about the VK_NV_compute_shader_derivatives extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-07-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_compute_shader_derivatives.txt GL_NV_compute_shader_derivatives>
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
-- == Description
--
-- This extension adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_compute_shader_derivatives.html SPV_NV_compute_shader_derivatives>
-- SPIR-V extension.
--
-- The SPIR-V extension provides two new execution modes, both of which
-- allow compute shaders to use built-ins that evaluate compute derivatives
-- explicitly or implicitly. Derivatives will be computed via differencing
-- over a 2x2 group of shader invocations. The @DerivativeGroupQuadsNV@
-- execution mode assembles shader invocations into 2x2 groups, where each
-- group has x and y coordinates of the local invocation ID of the form
-- (2m+{0,1}, 2n+{0,1}). The @DerivativeGroupLinearNV@ execution mode
-- assembles shader invocations into 2x2 groups, where each group has local
-- invocation index values of the form 4m+{0,1,2,3}.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceComputeShaderDerivativesFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME'
--
-- -   'NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV'
--
-- == New SPIR-V Capability
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-ComputeDerivativeGroupQuadsKHR ComputeDerivativeGroupQuadsNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-ComputeDerivativeGroupLinearKHR ComputeDerivativeGroupLinearNV>
--
-- == Issues
--
-- (1) Should we specify that the groups of four shader invocations used
-- for derivatives in a compute shader are the same groups of four
-- invocations that form a “quad” in shader subgroups?
--
-- __RESOLVED__: Yes.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2018-07-19 (Pat Brown)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_compute_shader_derivatives Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_compute_shader_derivatives  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV
                                                           , PhysicalDeviceComputeShaderDerivativesFeaturesNV
                                                           , NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
                                                           , pattern NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION
                                                           , NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
                                                           , pattern NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME
                                                           , PhysicalDeviceComputeShaderDerivativesFeaturesKHR(..)
                                                           ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_compute_shader_derivatives (PhysicalDeviceComputeShaderDerivativesFeaturesKHR)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_KHR))
import Vulkan.Extensions.VK_KHR_compute_shader_derivatives (PhysicalDeviceComputeShaderDerivativesFeaturesKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV = STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_KHR


-- No documentation found for TopLevel "VkPhysicalDeviceComputeShaderDerivativesFeaturesNV"
type PhysicalDeviceComputeShaderDerivativesFeaturesNV = PhysicalDeviceComputeShaderDerivativesFeaturesKHR


type NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION"
pattern NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION :: forall a . Integral a => a
pattern NV_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION = 1


type NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME = "VK_NV_compute_shader_derivatives"

-- No documentation found for TopLevel "VK_NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME"
pattern NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME = "VK_NV_compute_shader_derivatives"

