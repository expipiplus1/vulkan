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
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_cooperative_matrix:%20&body=@jeffbolznv%20 >
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
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixNV CooperativeMatrixNV>
--
-- == Issues
--
-- (1) What matrix properties will be supported in practice?
--
-- RESOLVED: In NVIDIA’s initial implementation, we will support:
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
-- = See Also
--
-- 'ComponentTypeNV', 'CooperativeMatrixPropertiesNV',
-- 'PhysicalDeviceCooperativeMatrixFeaturesNV',
-- 'PhysicalDeviceCooperativeMatrixPropertiesNV', 'ScopeNV',
-- 'getPhysicalDeviceCooperativeMatrixPropertiesNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_cooperative_matrix Vulkan Specification>
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

