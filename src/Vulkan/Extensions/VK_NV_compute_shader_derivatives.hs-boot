{-# language CPP #-}
-- | = Name
--
-- VK_NV_compute_shader_derivatives - device extension
--
-- = Registered Extension Number
--
-- 202
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
--     2018-07-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         {spirv}\/NV\/SPV_NV_compute_shader_derivatives.html[@SPV_NV_compute_shader_derivatives@]
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
-- {spirv}\/NV\/SPV_NV_compute_shader_derivatives.html[@SPV_NV_compute_shader_derivatives@]
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
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-computederivatives-quads ComputeDerivativeGroupQuadsNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-computederivatives-linear ComputeDerivativeGroupLinearNV>
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

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceComputeShaderDerivativesFeaturesNV

instance ToCStruct PhysicalDeviceComputeShaderDerivativesFeaturesNV
instance Show PhysicalDeviceComputeShaderDerivativesFeaturesNV

instance FromCStruct PhysicalDeviceComputeShaderDerivativesFeaturesNV

