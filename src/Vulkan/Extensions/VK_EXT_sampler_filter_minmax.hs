{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_sampler_filter_minmax  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
                                                       , pattern STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT
                                                       , pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT
                                                       , pattern SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT
                                                       , pattern SAMPLER_REDUCTION_MODE_MIN_EXT
                                                       , pattern SAMPLER_REDUCTION_MODE_MAX_EXT
                                                       , SamplerReductionModeEXT
                                                       , PhysicalDeviceSamplerFilterMinmaxPropertiesEXT
                                                       , SamplerReductionModeCreateInfoEXT
                                                       , EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION
                                                       , pattern EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION
                                                       , EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
                                                       , pattern EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME
                                                       ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax (PhysicalDeviceSamplerFilterMinmaxProperties)
import Vulkan.Core12.Enums.SamplerReductionMode (SamplerReductionMode)
import Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax (SamplerReductionModeCreateInfo)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT))
import Vulkan.Core12.Enums.SamplerReductionMode (SamplerReductionMode(SAMPLER_REDUCTION_MODE_MAX))
import Vulkan.Core12.Enums.SamplerReductionMode (SamplerReductionMode(SAMPLER_REDUCTION_MODE_MIN))
import Vulkan.Core12.Enums.SamplerReductionMode (SamplerReductionMode(SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT = STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT = FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT


-- No documentation found for TopLevel "VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT"
pattern SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT = SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE


-- No documentation found for TopLevel "VK_SAMPLER_REDUCTION_MODE_MIN_EXT"
pattern SAMPLER_REDUCTION_MODE_MIN_EXT = SAMPLER_REDUCTION_MODE_MIN


-- No documentation found for TopLevel "VK_SAMPLER_REDUCTION_MODE_MAX_EXT"
pattern SAMPLER_REDUCTION_MODE_MAX_EXT = SAMPLER_REDUCTION_MODE_MAX


-- No documentation found for TopLevel "VkSamplerReductionModeEXT"
type SamplerReductionModeEXT = SamplerReductionMode


-- No documentation found for TopLevel "VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT"
type PhysicalDeviceSamplerFilterMinmaxPropertiesEXT = PhysicalDeviceSamplerFilterMinmaxProperties


-- No documentation found for TopLevel "VkSamplerReductionModeCreateInfoEXT"
type SamplerReductionModeCreateInfoEXT = SamplerReductionModeCreateInfo


type EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION"
pattern EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION = 2


type EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME = "VK_EXT_sampler_filter_minmax"

-- No documentation found for TopLevel "VK_EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME"
pattern EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME = "VK_EXT_sampler_filter_minmax"

