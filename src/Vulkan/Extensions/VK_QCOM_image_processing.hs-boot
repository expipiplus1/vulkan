{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_image_processing - device extension
--
-- == VK_QCOM_image_processing
--
-- [__Name String__]
--     @VK_QCOM_image_processing@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     441
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_format_feature_flags2@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_image_processing] @jackohound%0A<<Here describe the issue or question you have about the VK_QCOM_image_processing extension>> >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_image_processing.adoc VK_QCOM_image_processing>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-07-08
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/QCOM/SPV_QCOM_image_processing.html SPV_QCOM_image_processing>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/qcom/GLSL_QCOM_image_processing.txt GL_QCOM_image_processing>
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
--     -   Ruihao Zhang, Qualcomm Technologies, Inc.
--
-- == Description
--
-- GPUs are commonly used to process images for various applications from
-- 3D graphics to UI and from composition to compute applications. Simple
-- scaling and filtering can be done with bilinear filtering, which comes
-- for free during texture sampling. However, as screen sizes get larger
-- and more use-cases rely on GPU such as camera and video post-processing
-- needs, there is increasing demand for GPU to support higher order
-- filtering and other advanced image processing.
--
-- This extension introduces a new set of SPIR-V built-in functions for
-- image processing. It exposes the following new imaging operations
--
-- -   The @OpImageSampleWeightedQCOM@ instruction takes 3 operands:
--     /sampled image/, /weight image/, and texture coordinates. The
--     instruction computes a weighted average of an MxN region of texels
--     in the /sampled image/, using a set of MxN weights in the /weight
--     image/.
--
-- -   The @OpImageBoxFilterQCOM@ instruction takes 3 operands: /sampled
--     image/, /box size/, and texture coordinates. Note that /box size/
--     specifies a floating point width and height in texels. The
--     instruction computes a weighted average of all texels in the
--     /sampled image/ that are covered (either partially or fully) by a
--     box with the specified size and centered at the specified texture
--     coordinates.
--
-- -   The @OpImageBlockMatchSADQCOM@ and @OpImageBlockMatchSSDQCOM@
--     instructions each takes 5 operands: /target image/, /target
--     coordinates/, /reference image/, /reference coordinates/, and /block
--     size/. Each instruction computes an error metric, that describes
--     whether a block of texels in the /target image/ matches a
--     corresponding block of texels in the /reference image/. The error
--     metric is computed per-component. @OpImageBlockMatchSADQCOM@
--     computes \"Sum Of Absolute Difference\" and
--     @OpImageBlockMatchSSDQCOM@ computes \"Sum of Squared Difference\".
--
-- Each of the image processing instructions operate only on 2D images. The
-- instructions do not-support sampling of mipmap, multi-plane,
-- multi-layer, multi-sampled, or depth\/stencil images. The instructions
-- can be used in any shader stage.
--
-- Implementations of this this extension should support these operations
-- natively at the HW instruction level, offering potential performance
-- gains as well as ease of development.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.ImageView.ImageViewCreateInfo':
--
--     -   'ImageViewSampleWeightCreateInfoQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageProcessingFeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceImageProcessingPropertiesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_IMAGE_PROCESSING_EXTENSION_NAME'
--
-- -   'QCOM_IMAGE_PROCESSING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM'
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLE_BLOCK_MATCH_BIT_QCOM'
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLE_WEIGHT_BIT_QCOM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_SAMPLE_WEIGHT_CREATE_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_PROPERTIES_QCOM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2022-07-08 (Jeff Leger)
--
-- == See Also
--
-- 'ImageViewSampleWeightCreateInfoQCOM',
-- 'PhysicalDeviceImageProcessingFeaturesQCOM',
-- 'PhysicalDeviceImageProcessingPropertiesQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_image_processing Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_image_processing  ( ImageViewSampleWeightCreateInfoQCOM
                                                   , PhysicalDeviceImageProcessingFeaturesQCOM
                                                   , PhysicalDeviceImageProcessingPropertiesQCOM
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImageViewSampleWeightCreateInfoQCOM

instance ToCStruct ImageViewSampleWeightCreateInfoQCOM
instance Show ImageViewSampleWeightCreateInfoQCOM

instance FromCStruct ImageViewSampleWeightCreateInfoQCOM


data PhysicalDeviceImageProcessingFeaturesQCOM

instance ToCStruct PhysicalDeviceImageProcessingFeaturesQCOM
instance Show PhysicalDeviceImageProcessingFeaturesQCOM

instance FromCStruct PhysicalDeviceImageProcessingFeaturesQCOM


data PhysicalDeviceImageProcessingPropertiesQCOM

instance ToCStruct PhysicalDeviceImageProcessingPropertiesQCOM
instance Show PhysicalDeviceImageProcessingPropertiesQCOM

instance FromCStruct PhysicalDeviceImageProcessingPropertiesQCOM

