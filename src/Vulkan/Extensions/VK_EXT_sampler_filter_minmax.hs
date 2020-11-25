{-# language CPP #-}
-- | = Name
--
-- VK_EXT_sampler_filter_minmax - device extension
--
-- == VK_EXT_sampler_filter_minmax
--
-- [__Name String__]
--     @VK_EXT_sampler_filter_minmax@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     131
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_sampler_filter_minmax:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-05-19
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- In unextended Vulkan, minification and magnification filters such as
-- LINEAR allow sampled image lookups to return a filtered texel value
-- produced by computing a weighted average of a collection of texels in
-- the neighborhood of the texture coordinate provided.
--
-- This extension provides a new sampler parameter which allows
-- applications to produce a filtered texel value by computing a
-- component-wise minimum (MIN) or maximum (MAX) of the texels that would
-- normally be averaged. The reduction mode is orthogonal to the
-- minification and magnification filter parameters. The filter parameters
-- are used to identify the set of texels used to produce a final filtered
-- value; the reduction mode identifies how these texels are combined.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the EXT suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceSamplerFilterMinmaxPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Sampler.SamplerCreateInfo':
--
--     -   'SamplerReductionModeCreateInfoEXT'
--
-- == New Enums
--
-- -   'SamplerReductionModeEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SAMPLER_FILTER_MINMAX_EXTENSION_NAME'
--
-- -   'EXT_SAMPLER_FILTER_MINMAX_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
--
--     -   'FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core12.Enums.SamplerReductionMode.SamplerReductionMode':
--
--     -   'SAMPLER_REDUCTION_MODE_MAX_EXT'
--
--     -   'SAMPLER_REDUCTION_MODE_MIN_EXT'
--
--     -   'SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT'
--
--     -   'STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 2, 2017-05-19 (Piers Daniell)
--
--     -   Renamed to EXT
--
-- -   Revision 1, 2017-03-25 (Jeff Bolz)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceSamplerFilterMinmaxPropertiesEXT',
-- 'SamplerReductionModeCreateInfoEXT', 'SamplerReductionModeEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_sampler_filter_minmax Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

