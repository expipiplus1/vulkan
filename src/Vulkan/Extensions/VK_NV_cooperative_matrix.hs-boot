{-# language CPP #-}
-- | = Name
--
-- VK_NV_cooperative_matrix - device extension
--
-- == VK_NV_cooperative_matrix
--
-- [__Name String__]
--     @VK_NV_cooperative_matrix@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     250
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
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_cooperative_matrix] @jeffbolznv%0A*Here describe the issue or question you have about the VK_NV_cooperative_matrix extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-02-05
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_matrix.html SPV_NV_cooperative_matrix>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_cooperative_matrix.txt GL_NV_cooperative_matrix>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Markus Tavenrath, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension adds support for using cooperative matrix types in
-- SPIR-V. Cooperative matrix types are medium-sized matrices that are
-- primarily supported in compute shaders, where the storage for the matrix
-- is spread across all invocations in some scope (usually a subgroup) and
-- those invocations cooperate to efficiently perform matrix multiplies.
--
-- Cooperative matrix types are defined by the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_matrix.html SPV_NV_cooperative_matrix>
-- SPIR-V extension and can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_cooperative_matrix.txt GL_NV_cooperative_matrix>
-- GLSL extension.
--
-- This extension includes support for enumerating the matrix types and
-- dimensions that are supported by the implementation.
--
-- == New Commands
--
-- -   'getPhysicalDeviceCooperativeMatrixPropertiesNV'
--
-- == New Structures
--
-- -   'CooperativeMatrixPropertiesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCooperativeMatrixFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCooperativeMatrixPropertiesNV'
--
-- == New Enums
--
-- -   'ComponentTypeNV'
--
-- -   'ScopeNV'
--
-- == New Enum Constants
--
-- -   'NV_COOPERATIVE_MATRIX_EXTENSION_NAME'
--
-- -   'NV_COOPERATIVE_MATRIX_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_cooperative_matrix.ComponentTypeKHR':
--
--     -   'COMPONENT_TYPE_FLOAT16_NV'
--
--     -   'COMPONENT_TYPE_FLOAT32_NV'
--
--     -   'COMPONENT_TYPE_FLOAT64_NV'
--
--     -   'COMPONENT_TYPE_SINT16_NV'
--
--     -   'COMPONENT_TYPE_SINT32_NV'
--
--     -   'COMPONENT_TYPE_SINT64_NV'
--
--     -   'COMPONENT_TYPE_SINT8_NV'
--
--     -   'COMPONENT_TYPE_UINT16_NV'
--
--     -   'COMPONENT_TYPE_UINT32_NV'
--
--     -   'COMPONENT_TYPE_UINT64_NV'
--
--     -   'COMPONENT_TYPE_UINT8_NV'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_cooperative_matrix.ScopeKHR':
--
--     -   'SCOPE_DEVICE_NV'
--
--     -   'SCOPE_QUEUE_FAMILY_NV'
--
--     -   'SCOPE_SUBGROUP_NV'
--
--     -   'SCOPE_WORKGROUP_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixNV CooperativeMatrixNV>
--
-- == Issues
--
-- (1) What matrix properties will be supported in practice?
--
-- __RESOLVED__: In NVIDIA’s initial implementation, we will support:
--
-- -   AType = BType = fp16 CType = DType = fp16 MxNxK = 16x8x16 scope =
--     Subgroup
--
-- -   AType = BType = fp16 CType = DType = fp16 MxNxK = 16x8x8 scope =
--     Subgroup
--
-- -   AType = BType = fp16 CType = DType = fp32 MxNxK = 16x8x16 scope =
--     Subgroup
--
-- -   AType = BType = fp16 CType = DType = fp32 MxNxK = 16x8x8 scope =
--     Subgroup
--
-- == Version History
--
-- -   Revision 1, 2019-02-05 (Jeff Bolz)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'ComponentTypeNV', 'CooperativeMatrixPropertiesNV',
-- 'PhysicalDeviceCooperativeMatrixFeaturesNV',
-- 'PhysicalDeviceCooperativeMatrixPropertiesNV', 'ScopeNV',
-- 'getPhysicalDeviceCooperativeMatrixPropertiesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_cooperative_matrix Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_cooperative_matrix  ( CooperativeMatrixPropertiesNV
                                                   , PhysicalDeviceCooperativeMatrixFeaturesNV
                                                   , PhysicalDeviceCooperativeMatrixPropertiesNV
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CooperativeMatrixPropertiesNV

instance ToCStruct CooperativeMatrixPropertiesNV
instance Show CooperativeMatrixPropertiesNV

instance FromCStruct CooperativeMatrixPropertiesNV


data PhysicalDeviceCooperativeMatrixFeaturesNV

instance ToCStruct PhysicalDeviceCooperativeMatrixFeaturesNV
instance Show PhysicalDeviceCooperativeMatrixFeaturesNV

instance FromCStruct PhysicalDeviceCooperativeMatrixFeaturesNV


data PhysicalDeviceCooperativeMatrixPropertiesNV

instance ToCStruct PhysicalDeviceCooperativeMatrixPropertiesNV
instance Show PhysicalDeviceCooperativeMatrixPropertiesNV

instance FromCStruct PhysicalDeviceCooperativeMatrixPropertiesNV

