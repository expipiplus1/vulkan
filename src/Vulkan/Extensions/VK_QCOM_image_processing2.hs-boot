{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_image_processing2 - device extension
--
-- == VK_QCOM_image_processing2
--
-- [__Name String__]
--     @VK_QCOM_image_processing2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     519
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_image_processing VK_QCOM_image_processing>
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_image_processing2] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_image_processing2 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-10
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/QCOM/SPV_QCOM_image_processing2.html SPV_QCOM_image_processing2>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/qcom/GLSL_QCOM_image_processing2.txt GL_QCOM_image_processing2>
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension enables support for the SPIR-V @TextureBlockMatch2QCOM@
-- capability. It builds on the functionality of QCOM_image_processing with
-- the addition of 4 new image processing operations.
--
-- -   The @opImageBlockMatchWindowSADQCOM@\` SPIR-V instruction builds
--     upon the functionality of @opImageBlockMatchSADQCOM@\` by repeatedly
--     performing block match operations across a 2D window. The “2D
--     windowExtent” and “compareMode” are are specified by
--     'SamplerBlockMatchWindowCreateInfoQCOM' in the sampler used to
--     create the /target image/. Like @OpImageBlockMatchSADQCOM@,
--     @opImageBlockMatchWindowSADQCOM@ computes an error metric, that
--     describes whether a block of texels in the /target image/ matches a
--     corresponding block of texels in the /reference image/. Unlike
--     @OpImageBlockMatchSADQCOM@, this instruction computes an error
--     metric at each (X,Y) location within the 2D window and returns
--     either the minimum or maximum error. The instruction only supports
--     single-component formats. Refer to the pseudocode below for details.
--
-- -   The @opImageBlockMatchWindowSSDQCOM@ follows the same pattern,
--     computing the SSD error metric at each location within the 2D
--     window.
--
-- -   The @opImageBlockMatchGatherSADQCOM@ builds upon
--     @OpImageBlockMatchSADQCOM@. This instruction computes an error
--     metric, that describes whether a block of texels in the /target
--     image/ matches a corresponding block of texels in the /reference
--     image/. The instruction computes the SAD error metric at 4 texel
--     offsets and returns the error metric for each offset in the
--     X,Y,Z,and W components. The instruction only supports
--     single-component texture formats. Refer to the pseudocode below for
--     details.
--
-- -   The @opImageBlockMatchGatherSSDQCOM@ follows the same pattern,
--     computing the SSD error metric for 4 offsets.
--
-- Each of the above 4 image processing instructions are limited to
-- single-component formats.
--
-- Below is the pseudocode for GLSL built-in function
-- @textureWindowBlockMatchSADQCOM@. The pseudocode for
-- @textureWindowBlockMatchSSD@ is identical other than replacing all
-- instances of @\"SAD\"@ with @\"SSD\"@.
--
-- > vec4 textureBlockMatchWindowSAD( sampler2D target,
-- >                                  uvec2 targetCoord,
-- >                                  samler2D reference,
-- >                                  uvec2 refCoord,
-- >                                  uvec2 blocksize) {
-- >     // compareMode (MIN or MAX) comes from the vkSampler associated with `target`
-- >     // uvec2 window  comes from the vkSampler associated with `target`
-- >     minSAD = INF;
-- >     maxSAD = -INF;
-- >     uvec2 minCoord;
-- >     uvec2 maxCoord;
-- >
-- >     for (uint x=0, x < window.width; x++) {
-- >         for (uint y=0; y < window.height; y++) {
-- >             float SAD = textureBlockMatchSAD(target,
-- >                                             targetCoord + uvec2(x, y),
-- >                                             reference,
-- >                                             refCoord,
-- >                                             blocksize).x;
-- >             // Note: the below comparison operator will produce undefined results
-- >             // if SAD is a denorm value.
-- >             if (SAD < minSAD) {
-- >                 minSAD = SAD;
-- >                 minCoord = uvec2(x,y);
-- >             }
-- >             if (SAD > maxSAD) {
-- >                 maxSAD = SAD;
-- >                 maxCoord = uvec2(x,y);
-- >             }
-- >         }
-- >     }
-- >     if (compareMode=MIN) {
-- >         return vec4(minSAD, minCoord.x, minCoord.y, 0.0);
-- >     } else {
-- >         return vec4(maxSAD, maxCoord.x, maxCoord.y, 0.0);
-- >     }
-- > }
--
-- Below is the pseudocode for @textureBlockMatchGatherSADQCOM@. The
-- pseudocode for @textureBlockMatchGatherSSD@ follows an identical
-- pattern.
--
-- > vec4 textureBlockMatchGatherSAD( sampler2D target,
-- >                                  uvec2 targetCoord,
-- >                                  samler2D reference,
-- >                                  uvec2 refCoord,
-- >                                  uvec2 blocksize) {
-- >     vec4 out;
-- >     for (uint x=0, x<4; x++) {
-- >             float SAD = textureBlockMatchSAD(target,
-- >                                             targetCoord + uvec2(x, 0),
-- >                                             reference,
-- >                                             refCoord,
-- >                                             blocksize).x;
-- >             out[x] = SAD;
-- >     }
-- >     return out;
-- > }
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageProcessing2FeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceImageProcessing2PropertiesQCOM'
--
-- -   Extending 'Vulkan.Core10.Sampler.SamplerCreateInfo':
--
--     -   'SamplerBlockMatchWindowCreateInfoQCOM'
--
-- == New Enums
--
-- -   'BlockMatchWindowCompareModeQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_IMAGE_PROCESSING_2_EXTENSION_NAME'
--
-- -   'QCOM_IMAGE_PROCESSING_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_PROCESSING_2_PROPERTIES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SAMPLER_BLOCK_MATCH_WINDOW_CREATE_INFO_QCOM'
--
-- == Issues
--
-- 1) What is the precision of the min\/max comparison checks?
--
-- __RESOLVED__: Intermediate computations for the new operations are
-- performed at 16-bit floating point precision. If the value of
-- @\"float SAD\"@ in the above code sample is a 16-bit denorm value, then
-- behavior of the MIN\/MAX comparison is undefined.
--
-- == Version History
--
-- -   Revision 1, 2023-03-10 (Jeff Leger)
--
-- == See Also
--
-- 'BlockMatchWindowCompareModeQCOM',
-- 'PhysicalDeviceImageProcessing2FeaturesQCOM',
-- 'PhysicalDeviceImageProcessing2PropertiesQCOM',
-- 'SamplerBlockMatchWindowCreateInfoQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_image_processing2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_image_processing2  ( PhysicalDeviceImageProcessing2FeaturesQCOM
                                                    , PhysicalDeviceImageProcessing2PropertiesQCOM
                                                    , SamplerBlockMatchWindowCreateInfoQCOM
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceImageProcessing2FeaturesQCOM

instance ToCStruct PhysicalDeviceImageProcessing2FeaturesQCOM
instance Show PhysicalDeviceImageProcessing2FeaturesQCOM

instance FromCStruct PhysicalDeviceImageProcessing2FeaturesQCOM


data PhysicalDeviceImageProcessing2PropertiesQCOM

instance ToCStruct PhysicalDeviceImageProcessing2PropertiesQCOM
instance Show PhysicalDeviceImageProcessing2PropertiesQCOM

instance FromCStruct PhysicalDeviceImageProcessing2PropertiesQCOM


data SamplerBlockMatchWindowCreateInfoQCOM

instance ToCStruct SamplerBlockMatchWindowCreateInfoQCOM
instance Show SamplerBlockMatchWindowCreateInfoQCOM

instance FromCStruct SamplerBlockMatchWindowCreateInfoQCOM

