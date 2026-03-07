{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_fragment_density_map_offset - device extension
--
-- = VK_QCOM_fragment_density_map_offset
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
--     3
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map_offset VK_EXT_fragment_density_map_offset>
--         extension
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_fragment_density_map_offset] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_fragment_density_map_offset extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-06-17
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
--     -   Manan Katwala, Qualcomm Technologies, Inc.
--
--     -   Connor Abbott, Valve Corporation
--
-- == Description
--
-- This extension allows an application to specify offsets to a fragment
-- density map attachment, changing the location where the fragment density
-- map is applied to the framebuffer. This is helpful for eye-tracking use
-- cases where the fovea needs to be moved around the framebuffer to match
-- the eye pose.
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
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassEndInfo',
--     'Vulkan.Extensions.VK_KHR_maintenance10.RenderingEndInfoKHR':
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
--     -   'IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_QCOM'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_QCOM'
--
--     -   'STRUCTURE_TYPE_SUBPASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_QCOM'
--
-- == Version History
--
-- -   Revision 3, 2025-03-20 (Connor Abbott\/Matthew Netsch)
--
--     -   Fix clamp equation and clarify the offsets are FDM relative
--         (editorial only)
--
-- -   Revision 2, 2024-06-17 (Matthew Netsch)
--
--     -   Fix typo in spec regarding fragmentDensityMapOffset feature
--
-- -   Revision 1, 2021-09-03 (Matthew Netsch)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_fragment_density_map_offset Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_fragment_density_map_offset  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_QCOM
                                                              , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_QCOM
                                                              , pattern STRUCTURE_TYPE_SUBPASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_QCOM
                                                              , pattern IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_QCOM
                                                              , PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM
                                                              , PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM
                                                              , SubpassFragmentDensityMapOffsetEndInfoQCOM
                                                              , QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION
                                                              , pattern QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION
                                                              , QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME
                                                              , pattern QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME
                                                              , PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT(..)
                                                              , PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT(..)
                                                              , RenderPassFragmentDensityMapOffsetEndInfoEXT(..)
                                                              ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_EXT_fragment_density_map_offset (PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT)
import Vulkan.Extensions.VK_EXT_fragment_density_map_offset (PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT)
import Vulkan.Extensions.VK_EXT_fragment_density_map_offset (RenderPassFragmentDensityMapOffsetEndInfoEXT)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_EXT))
import Vulkan.Extensions.VK_EXT_fragment_density_map_offset (PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT(..))
import Vulkan.Extensions.VK_EXT_fragment_density_map_offset (PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT(..))
import Vulkan.Extensions.VK_EXT_fragment_density_map_offset (RenderPassFragmentDensityMapOffsetEndInfoEXT(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_QCOM"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_QCOM = STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_EXT


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_QCOM"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_QCOM = STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_EXT


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SUBPASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_QCOM"
pattern STRUCTURE_TYPE_SUBPASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_QCOM = STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_EXT


-- No documentation found for TopLevel "VK_IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_QCOM"
pattern IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_QCOM = IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT


-- No documentation found for TopLevel "VkPhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM"
type PhysicalDeviceFragmentDensityMapOffsetFeaturesQCOM = PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT


-- No documentation found for TopLevel "VkPhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM"
type PhysicalDeviceFragmentDensityMapOffsetPropertiesQCOM = PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT


-- No documentation found for TopLevel "VkSubpassFragmentDensityMapOffsetEndInfoQCOM"
type SubpassFragmentDensityMapOffsetEndInfoQCOM = RenderPassFragmentDensityMapOffsetEndInfoEXT


type QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION"
pattern QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION = 3


type QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME = "VK_QCOM_fragment_density_map_offset"

-- No documentation found for TopLevel "VK_QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME"
pattern QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME = "VK_QCOM_fragment_density_map_offset"

