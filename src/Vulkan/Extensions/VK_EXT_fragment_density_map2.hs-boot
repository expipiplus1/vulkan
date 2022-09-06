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
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_EXT_fragment_density_map@ to be enabled for any
--         device-level functionality
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
--     -   'PhysicalDeviceFragmentDensityMap2FeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentDensityMap2PropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME'
--
-- -   'EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageViewCreateFlagBits.ImageViewCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageViewCreateFlagBits.IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT'
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_fragment_density_map2 Vulkan Specification>
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

