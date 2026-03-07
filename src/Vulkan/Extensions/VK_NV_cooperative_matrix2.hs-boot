{-# language CPP #-}
-- | = Name
--
-- VK_NV_cooperative_matrix2 - device extension
--
-- = VK_NV_cooperative_matrix2
--
-- [__Name String__]
--     @VK_NV_cooperative_matrix2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     594
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_cooperative_matrix VK_KHR_cooperative_matrix>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_matrix2.html SPV_NV_cooperative_matrix2>
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_tensor_addressing.html SPV_NV_tensor_addressing>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_cooperative_matrix2] @jeffbolznv%0A*Here describe the issue or question you have about the VK_NV_cooperative_matrix2 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_cooperative_matrix2.adoc VK_NV_cooperative_matrix2>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-08-01
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_cooperative_matrix2.txt GLSL_NV_cooperative_matrix2>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Karthik Vaidyanathan, NVIDIA
--
-- == Description
--
-- This extension adds several new features building on the cooperative
-- matrix types added in VK_KHR_cooperative_matrix. The goal is to add and
-- accelerate features beyond just simple GEMM kernels, including adding
-- support for type\/use conversions, reductions, per-element operations,
-- and tensor addressing, and also to improve usability and out-of-the-box
-- performance by adding support for more flexible matrix sizes, and
-- workgroup scope matrices with compiler-managed staging through shared
-- memory.
--
-- The new functionality is defined by the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_tensor_addressing.html SPV_NV_tensor_addressing>
-- and
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_cooperative_matrix2.html SPV_NV_cooperative_matrix2>
-- SPIR-V extensions and can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_cooperative_matrix2.txt GLSL_NV_cooperative_matrix2>
-- GLSL extension.
--
-- This extension includes support for enumerating the matrix types and
-- dimensions that are supported by the implementation, and which specific
-- features are supported.
--
-- == New Commands
--
-- -   'getPhysicalDeviceCooperativeMatrixFlexibleDimensionsPropertiesNV'
--
-- == New Structures
--
-- -   'CooperativeMatrixFlexibleDimensionsPropertiesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCooperativeMatrix2FeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCooperativeMatrix2PropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_COOPERATIVE_MATRIX_2_EXTENSION_NAME'
--
-- -   'NV_COOPERATIVE_MATRIX_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COOPERATIVE_MATRIX_FLEXIBLE_DIMENSIONS_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_2_PROPERTIES_NV'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-TensorAddressingNV TensorAddressingNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixReductionsNV CooperativeMatrixReductionsNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixConversionsNV CooperativeMatrixConversionsNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixPerElementOperationsNV CooperativeMatrixPerElementOperationsNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixTensorAddressingNV CooperativeMatrixTensorAddressingNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixBlockLoadsNV CooperativeMatrixBlockLoadsNV>
--
-- == Version History
--
-- -   Revision 1, 2024-08-01 (Jeff Bolz)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_cooperative_matrix2 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_cooperative_matrix2  ( CooperativeMatrixFlexibleDimensionsPropertiesNV
                                                    , PhysicalDeviceCooperativeMatrix2FeaturesNV
                                                    , PhysicalDeviceCooperativeMatrix2PropertiesNV
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CooperativeMatrixFlexibleDimensionsPropertiesNV

instance ToCStruct CooperativeMatrixFlexibleDimensionsPropertiesNV
instance Show CooperativeMatrixFlexibleDimensionsPropertiesNV

instance FromCStruct CooperativeMatrixFlexibleDimensionsPropertiesNV


data PhysicalDeviceCooperativeMatrix2FeaturesNV

instance ToCStruct PhysicalDeviceCooperativeMatrix2FeaturesNV
instance Show PhysicalDeviceCooperativeMatrix2FeaturesNV

instance FromCStruct PhysicalDeviceCooperativeMatrix2FeaturesNV


data PhysicalDeviceCooperativeMatrix2PropertiesNV

instance ToCStruct PhysicalDeviceCooperativeMatrix2PropertiesNV
instance Show PhysicalDeviceCooperativeMatrix2PropertiesNV

instance FromCStruct PhysicalDeviceCooperativeMatrix2PropertiesNV

