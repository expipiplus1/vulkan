{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion  ( pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO_KHR
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
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (createSamplerYcbcrConversion)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (destroySamplerYcbcrConversion)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (BindImagePlaneMemoryInfo)
import Graphics.Vulkan.Core11.Enums.ChromaLocation (ChromaLocation)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (ImagePlaneMemoryRequirementsInfo)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (PhysicalDeviceSamplerYcbcrConversionFeatures)
import Graphics.Vulkan.Core11.Handles (SamplerYcbcrConversion)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionCreateInfo)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionImageFormatProperties)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionInfo)
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion)
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange)
import Graphics.Vulkan.Core11.Enums.ChromaLocation (ChromaLocation(CHROMA_LOCATION_COSITED_EVEN))
import Graphics.Vulkan.Core11.Enums.ChromaLocation (ChromaLocation(CHROMA_LOCATION_MIDPOINT))
import Graphics.Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_B16G16R16G16_422_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_B8G8R8G8_422_UNORM))
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT))
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_DISJOINT_BIT))
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT))
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT))
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT))
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT))
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G16B16G16R16_422_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G16_B16R16_2PLANE_420_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G16_B16R16_2PLANE_422_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G16_B16_R16_3PLANE_420_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G16_B16_R16_3PLANE_422_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G16_B16_R16_3PLANE_444_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G8B8G8R8_422_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G8_B8R8_2PLANE_420_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G8_B8R8_2PLANE_422_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G8_B8_R8_3PLANE_420_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G8_B8_R8_3PLANE_422_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_G8_B8_R8_3PLANE_444_UNORM))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_R10X6G10X6_UNORM_2PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_R10X6_UNORM_PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_R12X4G12X4_UNORM_2PACK16))
import Graphics.Vulkan.Core10.Enums.Format (Format(FORMAT_R12X4_UNORM_PACK16))
import Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(IMAGE_ASPECT_PLANE_0_BIT))
import Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(IMAGE_ASPECT_PLANE_1_BIT))
import Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(IMAGE_ASPECT_PLANE_2_BIT))
import Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(IMAGE_CREATE_DISJOINT_BIT))
import Graphics.Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION))
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion(SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY))
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion(SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020))
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion(SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601))
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion(SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709))
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion(SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY))
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange(SAMPLER_YCBCR_RANGE_ITU_FULL))
import Graphics.Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange(SAMPLER_YCBCR_RANGE_ITU_NARROW))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO))
import Graphics.Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
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

