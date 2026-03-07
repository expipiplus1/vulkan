{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_cooperative_matrix_conversion - device extension
--
-- = VK_QCOM_cooperative_matrix_conversion
--
-- [__Name String__]
--     @VK_QCOM_cooperative_matrix_conversion@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     173
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
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/QCOM/SPV_QCOM_cooperative_matrix_conversion.html SPV_QCOM_cooperative_matrix_conversion>
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_cooperative_matrix_conversion] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_cooperative_matrix_conversion extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_cooperative_matrix_conversion.adoc VK_QCOM_cooperative_matrix_conversion>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-01-28
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/qcom/GLSL_QCOM_cooperative_matrix_conversion.txt GLSL_QCOM_cooperative_matrix_conversion>
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
--     -   Elina Kamenetskaya, Qualcomm Technologies, Inc
--
--     -   Alex Bourd, Qualcomm Technologies, Inc
--
--     -   Ruihao Zhang, Qualcomm Technologies, Inc
--
--     -   Wooyoung Kim, Qualcomm Technologies, Inc
--
-- == Description
--
-- This extension adds support for new SPIR-V shader instructions that
-- allow loading and storing a cooperative matrix without needing to stage
-- through shared memory and to allow bit casting arrays.
--
-- These instructions are defined by the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/QCOM/SPV_QCOM_cooperative_matrix_conversion.html SPV_QCOM_cooperative_matrix_conversion>
-- SPIR-V extension and can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/main/extensions/qcom/GLSL_QCOM_cooperative_matrix_conversion.txt GLSL_QCOM_cooperative_matrix_conversion>
-- GLSL extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_COOPERATIVE_MATRIX_CONVERSION_EXTENSION_NAME'
--
-- -   'QCOM_COOPERATIVE_MATRIX_CONVERSION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_CONVERSION_FEATURES_QCOM'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-CooperativeMatrixConversionQCOM CooperativeMatrixConversionQCOM>
--
-- == Version History
--
-- -   Revision 1, 2026-01-28 (Matthew Netsch)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_cooperative_matrix_conversion Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_cooperative_matrix_conversion  (PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM

instance ToCStruct PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM
instance Show PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM

instance FromCStruct PhysicalDeviceCooperativeMatrixConversionFeaturesQCOM

