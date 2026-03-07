{-# language CPP #-}
-- | = Name
--
-- VK_KHR_compute_shader_derivatives - device extension
--
-- = VK_KHR_compute_shader_derivatives
--
-- [__Name String__]
--     @VK_KHR_compute_shader_derivatives@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     512
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_compute_shader_derivatives.html SPV_KHR_compute_shader_derivatives>
--
-- [__Contact__]
--
--     -   Jean-Noe Morissette
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_compute_shader_derivatives] @MagicPoncho%0A*Here describe the issue or question you have about the VK_KHR_compute_shader_derivatives extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_compute_shader_derivatives.adoc VK_KHR_compute_shader_derivatives>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-06-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_compute_shader_derivatives.html SPV_KHR_compute_shader_derivatives>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/khr/GLSL_KHR_compute_shader_derivatives.txt GL_KHR_compute_shader_derivatives>
--
-- [__Contributors__]
--
--     -   Jean-Noe Morissette, Epic Games
--
--     -   Daniel Koch, NVIDIA
--
--     -   Pat Brown, NVIDIA
--
--     -   Stu Smith, AMD
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Tobias Hector, AMD
--
--     -   Ralph Potter, Samsung
--
--     -   Pan Gao, Huawei
--
--     -   Samuel (Sheng-Wen) Huang, MediaTek
--
--     -   Graeme Leese, Broadcom
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Matthew Netsh, Qualcomm
--
-- == Description
--
-- This extension adds Vulkan support for the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_compute_shader_derivatives.html SPV_KHR_compute_shader_derivatives>
-- SPIR-V extension.
--
-- The SPIR-V extension provides two new execution modes, both of which
-- allow execution models with defined workgroups to use built-ins that
-- evaluate derivatives explicitly or implicitly. Derivatives will be
-- computed via differencing over a 2x2 group of shader invocations. The
-- @DerivativeGroupQuadsKHR@ execution mode assembles shader invocations
-- into 2x2 groups, where each group has x and y coordinates of the local
-- invocation ID of the form (2m+{0,1}, 2n+{0,1}). The
-- @DerivativeGroupLinearKHR@ execution mode assembles shader invocations
-- into 2x2 groups, where each group has local invocation index values of
-- the form 4m+{0,1,2,3}.
--
-- The new execution modes are supported in compute shaders and optionally
-- (see
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-meshAndTaskShaderDerivatives meshAndTaskShaderDerivatives>)
-- in mesh and task shaders.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceComputeShaderDerivativesFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceComputeShaderDerivativesPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_COMPUTE_SHADER_DERIVATIVES_EXTENSION_NAME'
--
-- -   'KHR_COMPUTE_SHADER_DERIVATIVES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_PROPERTIES_KHR'
--
-- == New SPIR-V Capability
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-ComputeDerivativeGroupQuadsKHR ComputeDerivativeGroupQuadsKHR>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-ComputeDerivativeGroupLinearKHR ComputeDerivativeGroupLinearKHR>
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-02-27 (Jean-Noe Morissette)
--
--     -   Initial draft
--
--     -   Add properties and clarify mesh and task support (Daniel Koch)
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_compute_shader_derivatives Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_compute_shader_derivatives  ( PhysicalDeviceComputeShaderDerivativesFeaturesKHR
                                                            , PhysicalDeviceComputeShaderDerivativesPropertiesKHR
                                                            ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceComputeShaderDerivativesFeaturesKHR

instance ToCStruct PhysicalDeviceComputeShaderDerivativesFeaturesKHR
instance Show PhysicalDeviceComputeShaderDerivativesFeaturesKHR

instance FromCStruct PhysicalDeviceComputeShaderDerivativesFeaturesKHR


data PhysicalDeviceComputeShaderDerivativesPropertiesKHR

instance ToCStruct PhysicalDeviceComputeShaderDerivativesPropertiesKHR
instance Show PhysicalDeviceComputeShaderDerivativesPropertiesKHR

instance FromCStruct PhysicalDeviceComputeShaderDerivativesPropertiesKHR

