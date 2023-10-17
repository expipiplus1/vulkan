{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_filter_cubic_weights - device extension
--
-- == VK_QCOM_filter_cubic_weights
--
-- [__Name String__]
--     @VK_QCOM_filter_cubic_weights@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     520
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_filter_cubic_weights] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_filter_cubic_weights extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-06-23
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
--     -   Jonathan Wicks, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension extends cubic filtering by adding the ability to select a
-- set of weights. Without this extension, the weights used in cubic
-- filtering are limited to those corresponding to a Catmull-Rom spline.
-- This extension adds support for 3 additional spline weights.
--
-- This extension adds a new structure that /can/ be added to the @pNext@
-- chain of 'Vulkan.Core10.Sampler.SamplerCreateInfo' that /can/ be used to
-- specify which set of cubic weights are used in cubic filtering. A
-- similar structure can be added to the @pNext@ chain of
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BlitImageInfo2' to
-- specify cubic weights used in a blit operation.
--
-- With this extension weights corresponding to the following additional
-- splines can be selected for cubic filtered sampling and blits:
--
-- -   Zero Tangent Cardinal
--
-- -   B-Spline
--
-- -   Mitchell-Netravali
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BlitImageInfo2':
--
--     -   'BlitImageCubicWeightsInfoQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCubicWeightsFeaturesQCOM'
--
-- -   Extending 'Vulkan.Core10.Sampler.SamplerCreateInfo':
--
--     -   'SamplerCubicWeightsCreateInfoQCOM'
--
-- == New Enums
--
-- -   'CubicFilterWeightsQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_FILTER_CUBIC_WEIGHTS_EXTENSION_NAME'
--
-- -   'QCOM_FILTER_CUBIC_WEIGHTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BLIT_IMAGE_CUBIC_WEIGHTS_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_WEIGHTS_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_CUBIC_WEIGHTS_CREATE_INFO_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2023-06-23 (jleger)
--
--     -   Initial version
--
-- == See Also
--
-- 'BlitImageCubicWeightsInfoQCOM', 'CubicFilterWeightsQCOM',
-- 'PhysicalDeviceCubicWeightsFeaturesQCOM',
-- 'SamplerCubicWeightsCreateInfoQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_filter_cubic_weights Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_filter_cubic_weights  ( BlitImageCubicWeightsInfoQCOM
                                                       , PhysicalDeviceCubicWeightsFeaturesQCOM
                                                       , SamplerCubicWeightsCreateInfoQCOM
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data BlitImageCubicWeightsInfoQCOM

instance ToCStruct BlitImageCubicWeightsInfoQCOM
instance Show BlitImageCubicWeightsInfoQCOM

instance FromCStruct BlitImageCubicWeightsInfoQCOM


data PhysicalDeviceCubicWeightsFeaturesQCOM

instance ToCStruct PhysicalDeviceCubicWeightsFeaturesQCOM
instance Show PhysicalDeviceCubicWeightsFeaturesQCOM

instance FromCStruct PhysicalDeviceCubicWeightsFeaturesQCOM


data SamplerCubicWeightsCreateInfoQCOM

instance ToCStruct SamplerCubicWeightsCreateInfoQCOM
instance Show SamplerCubicWeightsCreateInfoQCOM

instance FromCStruct SamplerCubicWeightsCreateInfoQCOM

