{-# language CPP #-}
-- | = Name
--
-- VK_NV_optical_flow - device extension
--
-- == VK_NV_optical_flow
--
-- [__Name String__]
--     @VK_NV_optical_flow@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     465
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
--
-- [__Contact__]
--
--     -   Carsten Rohde
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_optical_flow] @crohde%0A*Here describe the issue or question you have about the VK_NV_optical_flow extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-09-26
--
-- [__Contributors__]
--
--     -   Carsten Rohde, NVIDIA
--
--     -   Vipul Parashar, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
-- == Description
--
-- Optical flow are fundamental algorithms in computer vision (CV) area.
-- This extension allows applications to estimate 2D displacement of pixels
-- between two frames.
--
-- Note
--
-- This extension is designed to be used with upcoming NVIDIA Optical Flow
-- SDK Version 5 which will be available on NVIDIA Developer webpage.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.OpticalFlowSessionNV'
--
-- == New Commands
--
-- -   'bindOpticalFlowSessionImageNV'
--
-- -   'cmdOpticalFlowExecuteNV'
--
-- -   'createOpticalFlowSessionNV'
--
-- -   'destroyOpticalFlowSessionNV'
--
-- -   'getPhysicalDeviceOpticalFlowImageFormatsNV'
--
-- == New Structures
--
-- -   'OpticalFlowExecuteInfoNV'
--
-- -   'OpticalFlowImageFormatPropertiesNV'
--
-- -   'OpticalFlowSessionCreateInfoNV'
--
-- -   Extending 'OpticalFlowSessionCreateInfoNV':
--
--     -   'OpticalFlowSessionCreatePrivateDataInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceOpticalFlowFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2',
--     'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'OpticalFlowImageFormatInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceOpticalFlowPropertiesNV'
--
-- == New Enums
--
-- -   'OpticalFlowExecuteFlagBitsNV'
--
-- -   'OpticalFlowGridSizeFlagBitsNV'
--
-- -   'OpticalFlowPerformanceLevelNV'
--
-- -   'OpticalFlowSessionBindingPointNV'
--
-- -   'OpticalFlowSessionCreateFlagBitsNV'
--
-- -   'OpticalFlowUsageFlagBitsNV'
--
-- == New Bitmasks
--
-- -   'OpticalFlowExecuteFlagsNV'
--
-- -   'OpticalFlowGridSizeFlagsNV'
--
-- -   'OpticalFlowSessionCreateFlagsNV'
--
-- -   'OpticalFlowUsageFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_OPTICAL_FLOW_EXTENSION_NAME'
--
-- -   'NV_OPTICAL_FLOW_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_READ_BIT_NV'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_WRITE_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_R16G16_S10_5_NV'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_OPTICAL_FLOW_COST_BIT_NV'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_OPTICAL_FLOW_IMAGE_BIT_NV'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_OPTICAL_FLOW_VECTOR_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_OPTICAL_FLOW_SESSION_NV'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlagBits':
--
--     -   'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_OPTICAL_FLOW_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPTICAL_FLOW_EXECUTE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_PRIVATE_DATA_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_OPTICAL_FLOW_PROPERTIES_NV'
--
-- == Examples
--
-- > // Example querying available input formats
-- > VkOpticalFlowImageFormatInfoNV ofFormatInfo = { VK_STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_INFO_NV };
-- > ofFormatInfo.usage = VK_OPTICAL_FLOW_USAGE_INPUT_BIT_NV;
-- >
-- > uint32_t count = 0;
-- > vkGetPhysicalDeviceOpticalFlowImageFormatsNV(physicalDevice, &ofFormatInfo, &count, NULL);
-- > VkOpticalFlowImageFormatPropertiesNV* fmt = new VkOpticalFlowImageFormatPropertiesNV[count];
-- > memset(fmt, 0, count  * sizeof(VkOpticalFlowImageFormatPropertiesNV));
-- > for (uint32_t i = 0; i < count; i++) {
-- >     fmt[i].sType = VK_STRUCTURE_TYPE_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_NV;
-- > }
-- > vkGetPhysicalDeviceOpticalFlowImageFormatsNV(physicalDevice, &ofFormatInfo, &count, fmt);
-- >
-- > // Pick one of the available formats
-- > VkFormat inputFormat = fmt[0].format;
-- >
-- > // Check feature support for optimal tiling
-- > VkFormatProperties3 formatProperties3 = { VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_3 };
-- > VkFormatProperties2 formatProperties2 = { VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2, &formatProperties3 };
-- > vkGetPhysicalDeviceFormatProperties2(physicalDevice, inputFormat, &formatProperties2);
-- > if (!(formatProperties3.optimalTilingFeatures & VK_FORMAT_FEATURE_2_OPTICAL_FLOW_IMAGE_BIT_NV)) {
-- >     return false;
-- > }
-- >
-- > // Check support for image creation parameters
-- > VkPhysicalDeviceImageFormatInfo2 imageFormatInfo2 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2, &ofFormatInfo };
-- > imageFormatInfo2.format = inputFormat;
-- > imageFormatInfo2.type = VK_IMAGE_TYPE_2D;
-- > imageFormatInfo2.tiling = VK_IMAGE_TILING_OPTIMAL;
-- > imageFormatInfo2.usage = VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT;
-- >
-- > VkImageFormatProperties2 imageFormatProperties2 = { VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 };
-- > if (vkGetPhysicalDeviceImageFormatProperties2(physicalDevice, &imageFormatInfo2, &imageFormatProperties2) != VK_SUCCESS) {
-- >     return false;
-- > }
-- >
-- > VkImageCreateInfo imageCreateInfo = { VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO, &ofFormatInfo };
-- > imageCreateInfo.imageType = VK_IMAGE_TYPE_2D;
-- > imageCreateInfo.format = inputFormat;
-- > imageCreateInfo.extent = { width, height, (uint32_t)1};
-- > imageCreateInfo.mipLevels = 1;
-- > imageCreateInfo.arrayLayers = 1;
-- > imageCreateInfo.samples = VK_SAMPLE_COUNT_1_BIT;
-- > imageCreateInfo.usage = VK_IMAGE_USAGE_SAMPLED_BIT | VK_IMAGE_USAGE_TRANSFER_DST_BIT;;
-- > imageCreateInfo.tiling = VK_IMAGE_TILING_OPTIMAL;
-- >
-- > vkCreateImage(device, &imageCreateInfo, NULL, &input);
-- > "allocate memory, bind image, create view"
-- >
-- > "do the same for reference and output"
-- >
-- > // Create optical flow session
-- > VkOpticalFlowSessionCreateInfoNV sessionCreateInfo = { VK_STRUCTURE_TYPE_OPTICAL_FLOW_SESSION_CREATE_INFO_NV };
-- > sessionCreateInfo.width = width;
-- > sessionCreateInfo.height = height;
-- > sessionCreateInfo.imageFormat = inputFormat;
-- > sessionCreateInfo.flowVectorFormat = outputFormat;
-- > sessionCreateInfo.outputGridSize = VK_OPTICAL_FLOW_GRID_SIZE_4X4_BIT_NV;
-- > sessionCreateInfo.performanceLevel = VK_OPTICAL_FLOW_PERFORMANCE_LEVEL_SLOW_NV;
-- > VkOpticalFlowSessionNV session;
-- > vkCreateOpticalFlowSessionNV(device, &sessionCreateInfo, NULL, &session);
-- >
-- > "allocate command buffer"
-- >
-- > "transfer images to VK_PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV"
-- > "transfer input images to VK_ACCESS_2_OPTICAL_FLOW_READ_BIT_NV and output image to VK_ACCESS_2_OPTICAL_FLOW_WRITE_BIT_NV"
-- >
-- > vkBindOpticalFlowSessionImageNV(device, session, VK_OPTICAL_FLOW_SESSION_BINDING_POINT_INPUT_NV, inputView, VK_IMAGE_LAYOUT_GENERAL);
-- > vkBindOpticalFlowSessionImageNV(device, session, VK_OPTICAL_FLOW_SESSION_BINDING_POINT_REFERENCE_NV, refView, VK_IMAGE_LAYOUT_GENERAL);
-- > vkBindOpticalFlowSessionImageNV(device, session, VK_OPTICAL_FLOW_SESSION_BINDING_POINT_FLOW_VECTOR_NV, outputView, VK_IMAGE_LAYOUT_GENERAL);
-- >
-- > VkOpticalFlowExecuteInfoNV opticalFlowExecuteInfo = { VK_STRUCTURE_TYPE_OPTICAL_FLOW_EXECUTE_INFO_NV };
-- > vkCmdOpticalFlowExecuteNV(cmd, session, &opticalFlowExecuteInfo);
-- >
-- > "submit command buffer"
--
-- == Version History
--
-- -   Revision 1, 2022-09-26 (Carsten Rohde)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'OpticalFlowExecuteFlagBitsNV', 'OpticalFlowExecuteFlagsNV',
-- 'OpticalFlowExecuteInfoNV', 'OpticalFlowGridSizeFlagBitsNV',
-- 'OpticalFlowGridSizeFlagsNV', 'OpticalFlowImageFormatInfoNV',
-- 'OpticalFlowImageFormatPropertiesNV', 'OpticalFlowPerformanceLevelNV',
-- 'OpticalFlowSessionBindingPointNV',
-- 'OpticalFlowSessionCreateFlagBitsNV', 'OpticalFlowSessionCreateFlagsNV',
-- 'OpticalFlowSessionCreateInfoNV',
-- 'OpticalFlowSessionCreatePrivateDataInfoNV',
-- 'Vulkan.Extensions.Handles.OpticalFlowSessionNV',
-- 'OpticalFlowUsageFlagBitsNV', 'OpticalFlowUsageFlagsNV',
-- 'PhysicalDeviceOpticalFlowFeaturesNV',
-- 'PhysicalDeviceOpticalFlowPropertiesNV',
-- 'bindOpticalFlowSessionImageNV', 'cmdOpticalFlowExecuteNV',
-- 'createOpticalFlowSessionNV', 'destroyOpticalFlowSessionNV',
-- 'getPhysicalDeviceOpticalFlowImageFormatsNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_optical_flow Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_optical_flow  ( OpticalFlowExecuteInfoNV
                                             , OpticalFlowImageFormatInfoNV
                                             , OpticalFlowImageFormatPropertiesNV
                                             , OpticalFlowSessionCreateInfoNV
                                             , OpticalFlowSessionCreatePrivateDataInfoNV
                                             , PhysicalDeviceOpticalFlowFeaturesNV
                                             , PhysicalDeviceOpticalFlowPropertiesNV
                                             , OpticalFlowSessionBindingPointNV
                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data OpticalFlowExecuteInfoNV

instance ToCStruct OpticalFlowExecuteInfoNV
instance Show OpticalFlowExecuteInfoNV

instance FromCStruct OpticalFlowExecuteInfoNV


data OpticalFlowImageFormatInfoNV

instance ToCStruct OpticalFlowImageFormatInfoNV
instance Show OpticalFlowImageFormatInfoNV

instance FromCStruct OpticalFlowImageFormatInfoNV


data OpticalFlowImageFormatPropertiesNV

instance ToCStruct OpticalFlowImageFormatPropertiesNV
instance Show OpticalFlowImageFormatPropertiesNV

instance FromCStruct OpticalFlowImageFormatPropertiesNV


type role OpticalFlowSessionCreateInfoNV nominal
data OpticalFlowSessionCreateInfoNV (es :: [Type])

instance ( Extendss OpticalFlowSessionCreateInfoNV es
         , PokeChain es ) => ToCStruct (OpticalFlowSessionCreateInfoNV es)
instance Show (Chain es) => Show (OpticalFlowSessionCreateInfoNV es)

instance ( Extendss OpticalFlowSessionCreateInfoNV es
         , PeekChain es ) => FromCStruct (OpticalFlowSessionCreateInfoNV es)


data OpticalFlowSessionCreatePrivateDataInfoNV

instance ToCStruct OpticalFlowSessionCreatePrivateDataInfoNV
instance Show OpticalFlowSessionCreatePrivateDataInfoNV

instance FromCStruct OpticalFlowSessionCreatePrivateDataInfoNV


data PhysicalDeviceOpticalFlowFeaturesNV

instance ToCStruct PhysicalDeviceOpticalFlowFeaturesNV
instance Show PhysicalDeviceOpticalFlowFeaturesNV

instance FromCStruct PhysicalDeviceOpticalFlowFeaturesNV


data PhysicalDeviceOpticalFlowPropertiesNV

instance ToCStruct PhysicalDeviceOpticalFlowPropertiesNV
instance Show PhysicalDeviceOpticalFlowPropertiesNV

instance FromCStruct PhysicalDeviceOpticalFlowPropertiesNV


data OpticalFlowSessionBindingPointNV

