{-# language CPP #-}
-- | = Name
--
-- VK_EXT_fragment_density_map - device extension
--
-- == VK_EXT_fragment_density_map
--
-- [__Name String__]
--     @VK_EXT_fragment_density_map@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     219
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_fragment_density_map:%20&body=@mnetsch%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-09-25
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_fragment_invocation_density.html SPV_EXT_fragment_invocation_density>
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Robert VanReenen, Qualcomm Technologies, Inc.
--
--     -   Jonathan Wicks, Qualcomm Technologies, Inc.
--
--     -   Tate Hornbeck, Qualcomm Technologies, Inc.
--
--     -   Sam Holmes, Qualcomm Technologies, Inc.
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Pat Brown, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension allows an application to specify areas of the render
-- target where the fragment shader may be invoked fewer times. These
-- fragments are broadcasted out to multiple pixels to cover the render
-- target.
--
-- The primary use of this extension is to reduce workloads in areas where
-- lower quality may not be perceived such as the distorted edges of a lens
-- or the periphery of a user’s gaze.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentDensityMapFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentDensityMapPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Pass.RenderPassCreateInfo',
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2':
--
--     -   'RenderPassFragmentDensityMapCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_FRAGMENT_DENSITY_MAP_EXTENSION_NAME'
--
-- -   'EXT_FRAGMENT_DENSITY_MAP_SPEC_VERSION'
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
-- == New or Modified Built-In Variables
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-fraginvocationcount FragInvocationCountEXT>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-fragsize FragSizeEXT>
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-FragmentDensityEXT FragmentDensityEXT>
--
-- == Version History
--
-- -   Revision 1, 2018-09-25 (Matthew Netsch)
--
--     -   Initial version
--
-- = See Also
--
-- 'PhysicalDeviceFragmentDensityMapFeaturesEXT',
-- 'PhysicalDeviceFragmentDensityMapPropertiesEXT',
-- 'RenderPassFragmentDensityMapCreateInfoEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_fragment_density_map  ( PhysicalDeviceFragmentDensityMapFeaturesEXT
                                                      , PhysicalDeviceFragmentDensityMapPropertiesEXT
                                                      , RenderPassFragmentDensityMapCreateInfoEXT
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceFragmentDensityMapFeaturesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMapFeaturesEXT
instance Show PhysicalDeviceFragmentDensityMapFeaturesEXT

instance FromCStruct PhysicalDeviceFragmentDensityMapFeaturesEXT


data PhysicalDeviceFragmentDensityMapPropertiesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMapPropertiesEXT
instance Show PhysicalDeviceFragmentDensityMapPropertiesEXT

instance FromCStruct PhysicalDeviceFragmentDensityMapPropertiesEXT


data RenderPassFragmentDensityMapCreateInfoEXT

instance ToCStruct RenderPassFragmentDensityMapCreateInfoEXT
instance Show RenderPassFragmentDensityMapCreateInfoEXT

instance FromCStruct RenderPassFragmentDensityMapCreateInfoEXT

