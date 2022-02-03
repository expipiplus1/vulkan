{-# language CPP #-}
-- | = Name
--
-- VK_EXT_fragment_density_map2 - device extension
--
-- == VK_EXT_fragment_density_map2
--
-- [__Name String__]
--     @VK_EXT_fragment_density_map2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     333
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_EXT_fragment_density_map@
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_fragment_density_map2] @mnetsch%0A<<Here describe the issue or question you have about the VK_EXT_fragment_density_map2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-06-16
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with Vulkan 1.1
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Jonathan Tinkham, Qualcomm Technologies, Inc.
--
--     -   Jonathan Wicks, Qualcomm Technologies, Inc.
--
--     -   Jan-Harald Fredriksen, ARM
--
-- == Description
--
-- This extension adds additional features and properties to
-- @VK_EXT_fragment_density_map@ in order to reduce fragment density map
-- host latency as well as improved queries for subsampled sampler
-- implementation-dependent behavior.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Pass.RenderPassCreateInfo',
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2':
--
--     -   'Vulkan.Extensions.VK_EXT_fragment_density_map.RenderPassFragmentDensityMapCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'Vulkan.Extensions.VK_EXT_fragment_density_map.EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME'
--
-- -   'Vulkan.Extensions.VK_EXT_fragment_density_map.EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
--
--     -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_FRAGMENT_DENSITY_MAP_BIT_EXT'
--
-- == Version History
--
-- -   Revision 1, 2020-06-16 (Matthew Netsch)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceFragmentDensityMap2FeaturesEXT',
-- 'PhysicalDeviceFragmentDensityMap2PropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_fragment_density_map2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_fragment_density_map2  ( PhysicalDeviceFragmentDensityMap2FeaturesEXT
                                                       , PhysicalDeviceFragmentDensityMap2PropertiesEXT
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceFragmentDensityMap2FeaturesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMap2FeaturesEXT
instance Show PhysicalDeviceFragmentDensityMap2FeaturesEXT

instance FromCStruct PhysicalDeviceFragmentDensityMap2FeaturesEXT


data PhysicalDeviceFragmentDensityMap2PropertiesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMap2PropertiesEXT
instance Show PhysicalDeviceFragmentDensityMap2PropertiesEXT

instance FromCStruct PhysicalDeviceFragmentDensityMap2PropertiesEXT

