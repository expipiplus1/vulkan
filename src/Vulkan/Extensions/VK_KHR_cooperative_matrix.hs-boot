{-# language CPP #-}
-- | = Name
--
-- VK_KHR_cooperative_matrix - device extension
--
-- == VK_KHR_cooperative_matrix
--
-- [__Name String__]
--     @VK_KHR_cooperative_matrix@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     507
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_cooperative_matrix] @kpet%0A*Here describe the issue or question you have about the VK_KHR_cooperative_matrix extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-05-03
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_cooperative_matrix.html SPV_KHR_cooperative_matrix>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/khr/GLSL_KHR_cooperative_matrix.txt GLSL_KHR_cooperative_matrix>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Markus Tavenrath, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Kevin Petit, Arm Ltd.
--
--     -   Boris Zanin, AMD
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
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_cooperative_matrix.html SPV_KHR_cooperative_matrix>
-- SPIR-V extension and can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/master/extensions/khr/GLSL_KHR_cooperative_matrix.txt GLSL_KHR_cooperative_matrix>
-- GLSL extension.
--
-- This extension includes support for enumerating the matrix types and
-- dimensions that are supported by the implementation.
--
-- == New Commands
--
-- -   'getPhysicalDeviceCooperativeMatrixPropertiesKHR'
--
-- == New Structures
--
-- -   'CooperativeMatrixPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCooperativeMatrixFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCooperativeMatrixPropertiesKHR'
--
-- == New Enums
--
-- -   'ComponentTypeKHR'
--
-- -   'ScopeKHR'
--
-- == New Enum Constants
--
-- -   'KHR_COOPERATIVE_MATRIX_EXTENSION_NAME'
--
-- -   'KHR_COOPERATIVE_MATRIX_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixKHR CooperativeMatrixKHR>
--
-- == Version History
--
-- -   Revision 2, 2023-05-03 (Kevin Petit)
--
--     -   First KHR revision
--
-- -   Revision 1, 2019-02-05 (Jeff Bolz)
--
--     -   NVIDIA vendor extension
--
-- == See Also
--
-- 'ComponentTypeKHR', 'CooperativeMatrixPropertiesKHR',
-- 'PhysicalDeviceCooperativeMatrixFeaturesKHR',
-- 'PhysicalDeviceCooperativeMatrixPropertiesKHR', 'ScopeKHR',
-- 'getPhysicalDeviceCooperativeMatrixPropertiesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_cooperative_matrix Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_cooperative_matrix  ( CooperativeMatrixPropertiesKHR
                                                    , PhysicalDeviceCooperativeMatrixFeaturesKHR
                                                    , PhysicalDeviceCooperativeMatrixPropertiesKHR
                                                    , ScopeKHR
                                                    , ComponentTypeKHR
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CooperativeMatrixPropertiesKHR

instance ToCStruct CooperativeMatrixPropertiesKHR
instance Show CooperativeMatrixPropertiesKHR

instance FromCStruct CooperativeMatrixPropertiesKHR


data PhysicalDeviceCooperativeMatrixFeaturesKHR

instance ToCStruct PhysicalDeviceCooperativeMatrixFeaturesKHR
instance Show PhysicalDeviceCooperativeMatrixFeaturesKHR

instance FromCStruct PhysicalDeviceCooperativeMatrixFeaturesKHR


data PhysicalDeviceCooperativeMatrixPropertiesKHR

instance ToCStruct PhysicalDeviceCooperativeMatrixPropertiesKHR
instance Show PhysicalDeviceCooperativeMatrixPropertiesKHR

instance FromCStruct PhysicalDeviceCooperativeMatrixPropertiesKHR


data ScopeKHR


data ComponentTypeKHR

