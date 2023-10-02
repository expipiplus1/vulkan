{-# language CPP #-}
-- | = Name
--
-- VK_KHR_sampler_ycbcr_conversion - device extension
--
-- == VK_KHR_sampler_ycbcr_conversion
--
-- [__Name String__]
--     @VK_KHR_sampler_ycbcr_conversion@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     157
--
-- [__Revision__]
--     14
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance1 VK_KHR_maintenance1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_bind_memory2 VK_KHR_bind_memory2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_memory_requirements2 VK_KHR_get_memory_requirements2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Andrew Garrard
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_sampler_ycbcr_conversion] @fluppeteer%0A*Here describe the issue or question you have about the VK_KHR_sampler_ycbcr_conversion extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-08-11
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Andrew Garrard, Samsung Electronics
--
--     -   Tobias Hector, Imagination Technologies
--
--     -   James Jones, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Romain Guy, Google
--
--     -   Jesse Hall, Google
--
--     -   Tom Cooksey, ARM Ltd
--
--     -   Jeff Leger, Qualcomm Technologies, Inc
--
--     -   Jan-Harald Fredriksen, ARM Ltd
--
--     -   Jan Outters, Samsung Electronics
--
--     -   Alon Or-bach, Samsung Electronics
--
--     -   Michael Worcester, Imagination Technologies
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Tony Zlatinski, NVIDIA
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
-- == Description
--
-- The use of Y′CBCR sampler conversion is an area in 3D graphics not used
-- by most Vulkan developers. It is mainly used for processing inputs from
-- video decoders and cameras. The use of the extension assumes basic
-- knowledge of Y′CBCR concepts.
--
-- This extension provides the ability to perform specified color space
-- conversions during texture sampling operations for the Y′CBCR color
-- space natively. It also adds a selection of multi-planar formats, image
-- aspect plane, and the ability to bind memory to the planes of an image
-- collectively or separately.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. However, if Vulkan 1.1 is supported and this
-- extension is not, the @samplerYcbcrConversion@ capability is optional.
-- The original type, enum and command names are still available as aliases
-- of the core functionality.
--
-- == New Object Types
--
-- -   'SamplerYcbcrConversionKHR'
--
-- == New Commands
--
-- -   'createSamplerYcbcrConversionKHR'
--
-- -   'destroySamplerYcbcrConversionKHR'
--
-- == New Structures
--
-- -   'SamplerYcbcrConversionCreateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo':
--
--     -   'BindImagePlaneMemoryInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2':
--
--     -   'SamplerYcbcrConversionImageFormatPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.ImageMemoryRequirementsInfo2':
--
--     -   'ImagePlaneMemoryRequirementsInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSamplerYcbcrConversionFeaturesKHR'
--
-- -   Extending 'Vulkan.Core10.Sampler.SamplerCreateInfo',
--     'Vulkan.Core10.ImageView.ImageViewCreateInfo':
--
--     -   'SamplerYcbcrConversionInfoKHR'
--
-- == New Enums
--
-- -   'ChromaLocationKHR'
--
-- -   'SamplerYcbcrModelConversionKHR'
--
-- -   'SamplerYcbcrRangeKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME'
--
-- -   'KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core11.Enums.ChromaLocation.ChromaLocation':
--
--     -   'CHROMA_LOCATION_COSITED_EVEN_KHR'
--
--     -   'CHROMA_LOCATION_MIDPOINT_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT':
--
--     -   'DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR'
--
--     -   'FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR'
--
--     -   'FORMAT_B16G16R16G16_422_UNORM_KHR'
--
--     -   'FORMAT_B8G8R8G8_422_UNORM_KHR'
--
--     -   'FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR'
--
--     -   'FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR'
--
--     -   'FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR'
--
--     -   'FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR'
--
--     -   'FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR'
--
--     -   'FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR'
--
--     -   'FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR'
--
--     -   'FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR'
--
--     -   'FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR'
--
--     -   'FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR'
--
--     -   'FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR'
--
--     -   'FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR'
--
--     -   'FORMAT_G16B16G16R16_422_UNORM_KHR'
--
--     -   'FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR'
--
--     -   'FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR'
--
--     -   'FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR'
--
--     -   'FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR'
--
--     -   'FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR'
--
--     -   'FORMAT_G8B8G8R8_422_UNORM_KHR'
--
--     -   'FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR'
--
--     -   'FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR'
--
--     -   'FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR'
--
--     -   'FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR'
--
--     -   'FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR'
--
--     -   'FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR'
--
--     -   'FORMAT_R10X6G10X6_UNORM_2PACK16_KHR'
--
--     -   'FORMAT_R10X6_UNORM_PACK16_KHR'
--
--     -   'FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR'
--
--     -   'FORMAT_R12X4G12X4_UNORM_2PACK16_KHR'
--
--     -   'FORMAT_R12X4_UNORM_PACK16_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
--
--     -   'FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR'
--
--     -   'FORMAT_FEATURE_DISJOINT_BIT_KHR'
--
--     -   'FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR'
--
--     -   'FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR'
--
--     -   'FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR'
--
--     -   'FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR'
--
--     -   'FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits':
--
--     -   'IMAGE_ASPECT_PLANE_0_BIT_KHR'
--
--     -   'IMAGE_ASPECT_PLANE_1_BIT_KHR'
--
--     -   'IMAGE_ASPECT_PLANE_2_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'IMAGE_CREATE_DISJOINT_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.SamplerYcbcrModelConversion.SamplerYcbcrModelConversion':
--
--     -   'SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR'
--
--     -   'SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR'
--
--     -   'SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR'
--
--     -   'SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR'
--
--     -   'SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR'
--
-- -   Extending 'Vulkan.Core11.Enums.SamplerYcbcrRange.SamplerYcbcrRange':
--
--     -   'SAMPLER_YCBCR_RANGE_ITU_FULL_KHR'
--
--     -   'SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR'
--
--     -   'STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_debug_report VK_EXT_debug_report>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT'
--
-- == Version History
--
-- -   Revision 1, 2017-01-24 (Andrew Garrard)
--
--     -   Initial draft
--
-- -   Revision 2, 2017-01-25 (Andrew Garrard)
--
--     -   After initial feedback
--
-- -   Revision 3, 2017-01-27 (Andrew Garrard)
--
--     -   Higher bit depth formats, renaming, swizzle
--
-- -   Revision 4, 2017-02-22 (Andrew Garrard)
--
--     -   Added query function, formats as RGB, clarifications
--
-- -   Revision 5, 2017-04-?? (Andrew Garrard)
--
--     -   Simplified query and removed output conversions
--
-- -   Revision 6, 2017-04-24 (Andrew Garrard)
--
--     -   Tidying, incorporated new image query, restored transfer
--         functions
--
-- -   Revision 7, 2017-04-25 (Andrew Garrard)
--
--     -   Added cosited option\/midpoint requirement for formats,
--         “bypassConversion”
--
-- -   Revision 8, 2017-04-25 (Andrew Garrard)
--
--     -   Simplified further
--
-- -   Revision 9, 2017-04-27 (Andrew Garrard)
--
--     -   Disjoint no more
--
-- -   Revision 10, 2017-04-28 (Andrew Garrard)
--
--     -   Restored disjoint
--
-- -   Revision 11, 2017-04-29 (Andrew Garrard)
--
--     -   Now Ycbcr conversion, and KHR
--
-- -   Revision 12, 2017-06-06 (Andrew Garrard)
--
--     -   Added conversion to image view creation
--
-- -   Revision 13, 2017-07-13 (Andrew Garrard)
--
--     -   Allowed cosited-only chroma samples for formats
--
-- -   Revision 14, 2017-08-11 (Andrew Garrard)
--
--     -   Reflected quantization changes in BT.2100-1
--
-- == See Also
--
-- 'BindImagePlaneMemoryInfoKHR', 'ChromaLocationKHR',
-- 'ImagePlaneMemoryRequirementsInfoKHR',
-- 'PhysicalDeviceSamplerYcbcrConversionFeaturesKHR',
-- 'SamplerYcbcrConversionCreateInfoKHR',
-- 'SamplerYcbcrConversionImageFormatPropertiesKHR',
-- 'SamplerYcbcrConversionInfoKHR', 'SamplerYcbcrConversionKHR',
-- 'SamplerYcbcrModelConversionKHR', 'SamplerYcbcrRangeKHR',
-- 'createSamplerYcbcrConversionKHR', 'destroySamplerYcbcrConversionKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_sampler_ycbcr_conversion Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion  ( pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR
                                                          , pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR
                                                          , pattern STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR
                                                          , pattern STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR
                                                          , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR
                                                          , pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR
                                                          , pattern DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT
                                                          , pattern OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR
                                                          , pattern FORMAT_G8B8G8R8_422_UNORM_KHR
                                                          , pattern FORMAT_B8G8R8G8_422_UNORM_KHR
                                                          , pattern FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR
                                                          , pattern FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR
                                                          , pattern FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR
                                                          , pattern FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR
                                                          , pattern FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR
                                                          , pattern FORMAT_R10X6_UNORM_PACK16_KHR
                                                          , pattern FORMAT_R10X6G10X6_UNORM_2PACK16_KHR
                                                          , pattern FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR
                                                          , pattern FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR
                                                          , pattern FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR
                                                          , pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR
                                                          , pattern FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR
                                                          , pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR
                                                          , pattern FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR
                                                          , pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR
                                                          , pattern FORMAT_R12X4_UNORM_PACK16_KHR
                                                          , pattern FORMAT_R12X4G12X4_UNORM_2PACK16_KHR
                                                          , pattern FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR
                                                          , pattern FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR
                                                          , pattern FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR
                                                          , pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR
                                                          , pattern FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR
                                                          , pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR
                                                          , pattern FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR
                                                          , pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR
                                                          , pattern FORMAT_G16B16G16R16_422_UNORM_KHR
                                                          , pattern FORMAT_B16G16R16G16_422_UNORM_KHR
                                                          , pattern FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR
                                                          , pattern FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR
                                                          , pattern FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR
                                                          , pattern FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR
                                                          , pattern FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR
                                                          , pattern IMAGE_ASPECT_PLANE_0_BIT_KHR
                                                          , pattern IMAGE_ASPECT_PLANE_1_BIT_KHR
                                                          , pattern IMAGE_ASPECT_PLANE_2_BIT_KHR
                                                          , pattern IMAGE_CREATE_DISJOINT_BIT_KHR
                                                          , pattern FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR
                                                          , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR
                                                          , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR
                                                          , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR
                                                          , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR
                                                          , pattern FORMAT_FEATURE_DISJOINT_BIT_KHR
                                                          , pattern FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR
                                                          , pattern SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR
                                                          , pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR
                                                          , pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR
                                                          , pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR
                                                          , pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR
                                                          , pattern SAMPLER_YCBCR_RANGE_ITU_FULL_KHR
                                                          , pattern SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR
                                                          , pattern CHROMA_LOCATION_COSITED_EVEN_KHR
                                                          , pattern CHROMA_LOCATION_MIDPOINT_KHR
                                                          , createSamplerYcbcrConversionKHR
                                                          , destroySamplerYcbcrConversionKHR
                                                          , SamplerYcbcrConversionKHR
                                                          , SamplerYcbcrModelConversionKHR
                                                          , SamplerYcbcrRangeKHR
                                                          , ChromaLocationKHR
                                                          , SamplerYcbcrConversionInfoKHR
                                                          , SamplerYcbcrConversionCreateInfoKHR
                                                          , BindImagePlaneMemoryInfoKHR
                                                          , ImagePlaneMemoryRequirementsInfoKHR
                                                          , PhysicalDeviceSamplerYcbcrConversionFeaturesKHR
                                                          , SamplerYcbcrConversionImageFormatPropertiesKHR
                                                          , KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION
                                                          , pattern KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION
                                                          , KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME
                                                          , pattern KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME
                                                          , DebugReportObjectTypeEXT(..)
                                                          ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (createSamplerYcbcrConversion)
import Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (destroySamplerYcbcrConversion)
import Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (BindImagePlaneMemoryInfo)
import Vulkan.Core11.Enums.ChromaLocation (ChromaLocation)
import Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (ImagePlaneMemoryRequirementsInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (PhysicalDeviceSamplerYcbcrConversionFeatures)
import Vulkan.Core11.Handles (SamplerYcbcrConversion)
import Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionCreateInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionImageFormatProperties)
import Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionInfo)
import Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion)
import Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange)
import Vulkan.Core11.Enums.ChromaLocation (ChromaLocation(CHROMA_LOCATION_COSITED_EVEN))
import Vulkan.Core11.Enums.ChromaLocation (ChromaLocation(CHROMA_LOCATION_MIDPOINT))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT))
import Vulkan.Core10.Enums.Format (Format(FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_B16G16R16G16_422_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_B8G8R8G8_422_UNORM))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_DISJOINT_BIT))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G16B16G16R16_422_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G16_B16R16_2PLANE_420_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G16_B16R16_2PLANE_422_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G16_B16_R16_3PLANE_420_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G16_B16_R16_3PLANE_422_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G16_B16_R16_3PLANE_444_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G8B8G8R8_422_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G8_B8R8_2PLANE_420_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G8_B8R8_2PLANE_422_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G8_B8_R8_3PLANE_420_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G8_B8_R8_3PLANE_422_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_G8_B8_R8_3PLANE_444_UNORM))
import Vulkan.Core10.Enums.Format (Format(FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_R10X6G10X6_UNORM_2PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_R10X6_UNORM_PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_R12X4G12X4_UNORM_2PACK16))
import Vulkan.Core10.Enums.Format (Format(FORMAT_R12X4_UNORM_PACK16))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(IMAGE_ASPECT_PLANE_0_BIT))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(IMAGE_ASPECT_PLANE_1_BIT))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(IMAGE_ASPECT_PLANE_2_BIT))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(IMAGE_CREATE_DISJOINT_BIT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION))
import Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion(SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY))
import Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion(SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020))
import Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion(SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601))
import Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion(SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709))
import Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion(SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY))
import Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange(SAMPLER_YCBCR_RANGE_ITU_FULL))
import Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange(SAMPLER_YCBCR_RANGE_ITU_NARROW))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR = STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR"
pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO_KHR = STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO_KHR = STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR"
pattern STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO_KHR = STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES_KHR = STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES


-- No documentation found for TopLevel "VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR_EXT = DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT


-- No documentation found for TopLevel "VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR"
pattern OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_KHR = OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION


-- No documentation found for TopLevel "VK_FORMAT_G8B8G8R8_422_UNORM_KHR"
pattern FORMAT_G8B8G8R8_422_UNORM_KHR = FORMAT_G8B8G8R8_422_UNORM


-- No documentation found for TopLevel "VK_FORMAT_B8G8R8G8_422_UNORM_KHR"
pattern FORMAT_B8G8R8G8_422_UNORM_KHR = FORMAT_B8G8R8G8_422_UNORM


-- No documentation found for TopLevel "VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR"
pattern FORMAT_G8_B8_R8_3PLANE_420_UNORM_KHR = FORMAT_G8_B8_R8_3PLANE_420_UNORM


-- No documentation found for TopLevel "VK_FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR"
pattern FORMAT_G8_B8R8_2PLANE_420_UNORM_KHR = FORMAT_G8_B8R8_2PLANE_420_UNORM


-- No documentation found for TopLevel "VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR"
pattern FORMAT_G8_B8_R8_3PLANE_422_UNORM_KHR = FORMAT_G8_B8_R8_3PLANE_422_UNORM


-- No documentation found for TopLevel "VK_FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR"
pattern FORMAT_G8_B8R8_2PLANE_422_UNORM_KHR = FORMAT_G8_B8R8_2PLANE_422_UNORM


-- No documentation found for TopLevel "VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR"
pattern FORMAT_G8_B8_R8_3PLANE_444_UNORM_KHR = FORMAT_G8_B8_R8_3PLANE_444_UNORM


-- No documentation found for TopLevel "VK_FORMAT_R10X6_UNORM_PACK16_KHR"
pattern FORMAT_R10X6_UNORM_PACK16_KHR = FORMAT_R10X6_UNORM_PACK16


-- No documentation found for TopLevel "VK_FORMAT_R10X6G10X6_UNORM_2PACK16_KHR"
pattern FORMAT_R10X6G10X6_UNORM_2PACK16_KHR = FORMAT_R10X6G10X6_UNORM_2PACK16


-- No documentation found for TopLevel "VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR"
pattern FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16_KHR = FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16


-- No documentation found for TopLevel "VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR"
pattern FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16_KHR = FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16


-- No documentation found for TopLevel "VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR"
pattern FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16_KHR = FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16


-- No documentation found for TopLevel "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR"
pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16_KHR = FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16


-- No documentation found for TopLevel "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR"
pattern FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16_KHR = FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16


-- No documentation found for TopLevel "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR"
pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16_KHR = FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16


-- No documentation found for TopLevel "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR"
pattern FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16_KHR = FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16


-- No documentation found for TopLevel "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR"
pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16_KHR = FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16


-- No documentation found for TopLevel "VK_FORMAT_R12X4_UNORM_PACK16_KHR"
pattern FORMAT_R12X4_UNORM_PACK16_KHR = FORMAT_R12X4_UNORM_PACK16


-- No documentation found for TopLevel "VK_FORMAT_R12X4G12X4_UNORM_2PACK16_KHR"
pattern FORMAT_R12X4G12X4_UNORM_2PACK16_KHR = FORMAT_R12X4G12X4_UNORM_2PACK16


-- No documentation found for TopLevel "VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR"
pattern FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16_KHR = FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16


-- No documentation found for TopLevel "VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR"
pattern FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16_KHR = FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16


-- No documentation found for TopLevel "VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR"
pattern FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16_KHR = FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16


-- No documentation found for TopLevel "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR"
pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16_KHR = FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16


-- No documentation found for TopLevel "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR"
pattern FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16_KHR = FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16


-- No documentation found for TopLevel "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR"
pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16_KHR = FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16


-- No documentation found for TopLevel "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR"
pattern FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16_KHR = FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16


-- No documentation found for TopLevel "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR"
pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16_KHR = FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16


-- No documentation found for TopLevel "VK_FORMAT_G16B16G16R16_422_UNORM_KHR"
pattern FORMAT_G16B16G16R16_422_UNORM_KHR = FORMAT_G16B16G16R16_422_UNORM


-- No documentation found for TopLevel "VK_FORMAT_B16G16R16G16_422_UNORM_KHR"
pattern FORMAT_B16G16R16G16_422_UNORM_KHR = FORMAT_B16G16R16G16_422_UNORM


-- No documentation found for TopLevel "VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR"
pattern FORMAT_G16_B16_R16_3PLANE_420_UNORM_KHR = FORMAT_G16_B16_R16_3PLANE_420_UNORM


-- No documentation found for TopLevel "VK_FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR"
pattern FORMAT_G16_B16R16_2PLANE_420_UNORM_KHR = FORMAT_G16_B16R16_2PLANE_420_UNORM


-- No documentation found for TopLevel "VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR"
pattern FORMAT_G16_B16_R16_3PLANE_422_UNORM_KHR = FORMAT_G16_B16_R16_3PLANE_422_UNORM


-- No documentation found for TopLevel "VK_FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR"
pattern FORMAT_G16_B16R16_2PLANE_422_UNORM_KHR = FORMAT_G16_B16R16_2PLANE_422_UNORM


-- No documentation found for TopLevel "VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR"
pattern FORMAT_G16_B16_R16_3PLANE_444_UNORM_KHR = FORMAT_G16_B16_R16_3PLANE_444_UNORM


-- No documentation found for TopLevel "VK_IMAGE_ASPECT_PLANE_0_BIT_KHR"
pattern IMAGE_ASPECT_PLANE_0_BIT_KHR = IMAGE_ASPECT_PLANE_0_BIT


-- No documentation found for TopLevel "VK_IMAGE_ASPECT_PLANE_1_BIT_KHR"
pattern IMAGE_ASPECT_PLANE_1_BIT_KHR = IMAGE_ASPECT_PLANE_1_BIT


-- No documentation found for TopLevel "VK_IMAGE_ASPECT_PLANE_2_BIT_KHR"
pattern IMAGE_ASPECT_PLANE_2_BIT_KHR = IMAGE_ASPECT_PLANE_2_BIT


-- No documentation found for TopLevel "VK_IMAGE_CREATE_DISJOINT_BIT_KHR"
pattern IMAGE_CREATE_DISJOINT_BIT_KHR = IMAGE_CREATE_DISJOINT_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR"
pattern FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT_KHR = FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR = FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR = FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR = FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR = FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_DISJOINT_BIT_KHR"
pattern FORMAT_FEATURE_DISJOINT_BIT_KHR = FORMAT_FEATURE_DISJOINT_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR"
pattern FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT_KHR = FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT


-- No documentation found for TopLevel "VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY_KHR = SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY


-- No documentation found for TopLevel "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY_KHR = SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY


-- No documentation found for TopLevel "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709_KHR = SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709


-- No documentation found for TopLevel "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601_KHR = SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601


-- No documentation found for TopLevel "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020_KHR = SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020


-- No documentation found for TopLevel "VK_SAMPLER_YCBCR_RANGE_ITU_FULL_KHR"
pattern SAMPLER_YCBCR_RANGE_ITU_FULL_KHR = SAMPLER_YCBCR_RANGE_ITU_FULL


-- No documentation found for TopLevel "VK_SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR"
pattern SAMPLER_YCBCR_RANGE_ITU_NARROW_KHR = SAMPLER_YCBCR_RANGE_ITU_NARROW


-- No documentation found for TopLevel "VK_CHROMA_LOCATION_COSITED_EVEN_KHR"
pattern CHROMA_LOCATION_COSITED_EVEN_KHR = CHROMA_LOCATION_COSITED_EVEN


-- No documentation found for TopLevel "VK_CHROMA_LOCATION_MIDPOINT_KHR"
pattern CHROMA_LOCATION_MIDPOINT_KHR = CHROMA_LOCATION_MIDPOINT


-- No documentation found for TopLevel "vkCreateSamplerYcbcrConversionKHR"
createSamplerYcbcrConversionKHR = createSamplerYcbcrConversion


-- No documentation found for TopLevel "vkDestroySamplerYcbcrConversionKHR"
destroySamplerYcbcrConversionKHR = destroySamplerYcbcrConversion


-- No documentation found for TopLevel "VkSamplerYcbcrConversionKHR"
type SamplerYcbcrConversionKHR = SamplerYcbcrConversion


-- No documentation found for TopLevel "VkSamplerYcbcrModelConversionKHR"
type SamplerYcbcrModelConversionKHR = SamplerYcbcrModelConversion


-- No documentation found for TopLevel "VkSamplerYcbcrRangeKHR"
type SamplerYcbcrRangeKHR = SamplerYcbcrRange


-- No documentation found for TopLevel "VkChromaLocationKHR"
type ChromaLocationKHR = ChromaLocation


-- No documentation found for TopLevel "VkSamplerYcbcrConversionInfoKHR"
type SamplerYcbcrConversionInfoKHR = SamplerYcbcrConversionInfo


-- No documentation found for TopLevel "VkSamplerYcbcrConversionCreateInfoKHR"
type SamplerYcbcrConversionCreateInfoKHR = SamplerYcbcrConversionCreateInfo


-- No documentation found for TopLevel "VkBindImagePlaneMemoryInfoKHR"
type BindImagePlaneMemoryInfoKHR = BindImagePlaneMemoryInfo


-- No documentation found for TopLevel "VkImagePlaneMemoryRequirementsInfoKHR"
type ImagePlaneMemoryRequirementsInfoKHR = ImagePlaneMemoryRequirementsInfo


-- No documentation found for TopLevel "VkPhysicalDeviceSamplerYcbcrConversionFeaturesKHR"
type PhysicalDeviceSamplerYcbcrConversionFeaturesKHR = PhysicalDeviceSamplerYcbcrConversionFeatures


-- No documentation found for TopLevel "VkSamplerYcbcrConversionImageFormatPropertiesKHR"
type SamplerYcbcrConversionImageFormatPropertiesKHR = SamplerYcbcrConversionImageFormatProperties


type KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION = 14

-- No documentation found for TopLevel "VK_KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION"
pattern KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SAMPLER_YCBCR_CONVERSION_SPEC_VERSION = 14


type KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME = "VK_KHR_sampler_ycbcr_conversion"

-- No documentation found for TopLevel "VK_KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME"
pattern KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SAMPLER_YCBCR_CONVERSION_EXTENSION_NAME = "VK_KHR_sampler_ycbcr_conversion"

