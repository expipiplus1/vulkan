{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_maximal_reconvergence - device extension
--
-- == VK_KHR_shader_maximal_reconvergence
--
-- [__Name String__]
--     @VK_KHR_shader_maximal_reconvergence@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     435
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_maximal_reconvergence.html SPV_KHR_maximal_reconvergence>
--
-- [__Contact__]
--
--     -   Alan Baker
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_maximal_reconvergence] @alan-baker%0A*Here describe the issue or question you have about the VK_KHR_shader_maximal_reconvergence extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-11-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Requires SPIR-V 1.3.
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_maximal_reconvergence.html SPV_KHR_maximal_reconvergence>
--
-- [__Contributors__]
--
--     -   Alan Baker, Google
--
-- == Description
--
-- This extension allows the use of the @SPV_KHR_maximal_reconvergence@
-- SPIR-V extension in shader modules. @SPV_KHR_maximal_reconvergence@
-- provides stronger guarantees that diverged subgroups will reconverge.
-- These guarantees should match shader author intuition about divergence
-- and reconvergence of invocations based on the structure of the code in
-- the HLL.
--
-- Developers should utilize this extension if they require stronger
-- guarantees about reconvergence than either the core spec or
-- SPV_KHR_subgroup_uniform_control_flow. This extension will define the
-- rules that govern how invocations diverge and reconverge in a way that
-- should match developer intuition. It allows robust programs to be
-- written relying on subgroup operations and other tangled instructions.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_MAXIMAL_RECONVERGENCE_EXTENSION_NAME'
--
-- -   'KHR_SHADER_MAXIMAL_RECONVERGENCE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MAXIMAL_RECONVERGENCE_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-MaximallyReconvergesKHR MaximallyReconvergesKHR>
--
-- == Version History
--
-- -   Revision 1, 2021-11-12 (Alan Baker)
--
--     -   Internal draft version
--
-- == See Also
--
-- 'PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_maximal_reconvergence Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_maximal_reconvergence  (PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR

instance ToCStruct PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR
instance Show PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR

instance FromCStruct PhysicalDeviceShaderMaximalReconvergenceFeaturesKHR

