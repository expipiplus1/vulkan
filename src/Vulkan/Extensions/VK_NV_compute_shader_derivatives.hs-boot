{-# language CPP #-}
-- | = Name
--
-- VK_NV_compute_shader_derivatives - device extension
--
-- == VK_NV_compute_shader_derivatives
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
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Pat Brown
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_compute_shader_derivatives:%20&body=@nvpbrown%20 >
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
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_compute_shader_derivatives.html SPV_NV_compute_shader_derivatives>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_compute_shader_derivatives.txt GL_NV_compute_shader_derivatives>
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
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV'
--
-- == New SPIR-V Capability
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-ComputeDerivativeGroupQuadsNV ComputeDerivativeGroupQuadsNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-ComputeDerivativeGroupLinearNV ComputeDerivativeGroupLinearNV>
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
-- = See Also
--
-- 'PhysicalDeviceComputeShaderDerivativesFeaturesNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_compute_shader_derivatives Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_compute_shader_derivatives  (PhysicalDeviceComputeShaderDerivativesFeaturesNV) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceComputeShaderDerivativesFeaturesNV

instance ToCStruct PhysicalDeviceComputeShaderDerivativesFeaturesNV
instance Show PhysicalDeviceComputeShaderDerivativesFeaturesNV

instance FromCStruct PhysicalDeviceComputeShaderDerivativesFeaturesNV

