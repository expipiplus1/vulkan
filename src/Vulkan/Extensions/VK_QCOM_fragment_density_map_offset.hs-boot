{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_fragment_density_map_offset - device extension
--
-- == VK_QCOM_fragment_density_map_offset
--
-- [__Name String__]
--     @VK_QCOM_fragment_density_map_offset@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     426
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
--     -   Requires @VK_EXT_fragment_density_map@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_fragment_density_map_offset] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_fragment_density_map_offset extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-09-03
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Jonathan Wicks, Qualcomm Technologies, Inc.
--
--     -   Jonathan Tinkham, Qualcomm Technologies, Inc.
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension allows an application to specify offsets to a fragment
-- density map attachment, changing the framebuffer location where density
-- values are applied to without having to regenerate the fragment density
-- map.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassEndInfo':
--
--     -   'SubpassFragmentDensityMapOffsetEndInfoQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME'
--
-- -   'QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2021-09-03 (Matthew Netsch)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM',
-- 'PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM',
-- 'SubpassFragmentDensityMapOffsetEndInfoQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_fragment_density_map_offset Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_fragment_density_map_offset  ( PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM
                                                              , PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM
                                                              , SubpassFragmentDensityMapOffsetEndInfoQCOM
                                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM

instance ToCStruct PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM
instance Show PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM

instance FromCStruct PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM


data PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM

instance ToCStruct PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM
instance Show PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM

instance FromCStruct PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM


data SubpassFragmentDensityMapOffsetEndInfoQCOM

instance ToCStruct SubpassFragmentDensityMapOffsetEndInfoQCOM
instance Show SubpassFragmentDensityMapOffsetEndInfoQCOM

instance FromCStruct SubpassFragmentDensityMapOffsetEndInfoQCOM

