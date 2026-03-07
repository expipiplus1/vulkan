{-# language CPP #-}
-- | = Name
--
-- VK_EXT_fragment_density_map_offset - device extension
--
-- = VK_EXT_fragment_density_map_offset
--
-- [__Name String__]
--     @VK_EXT_fragment_density_map_offset@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     620
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
--
-- [__Contact__]
--
--     -   Connor Abbott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_fragment_density_map_offset] @cwabbott0%0A*Here describe the issue or question you have about the VK_EXT_fragment_density_map_offset extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_fragment_density_map_offset.adoc VK_EXT_fragment_density_map_offset>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-02-14
--
-- [__Contributors__]
--
--     -   Connor Abbott, Valve Corporation
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
--     -   Mike Blumenkrantz, Valve Corporation
--
-- == Description
--
-- This extension allows an application to specify offsets to a fragment
-- density map attachment, changing the framebuffer location where density
-- values are applied to without having to regenerate the fragment density
-- map.
--
-- == New Commands
--
-- -   'cmdEndRendering2EXT'
--
-- == New Structures
--
-- -   'RenderingEndInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassEndInfo',
--     'Vulkan.Extensions.VK_KHR_maintenance10.RenderingEndInfoKHR':
--
--     -   'RenderPassFragmentDensityMapOffsetEndInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_FRAGMENT_DENSITY_MAP_OFFSET_EXTENSION_NAME'
--
-- -   'EXT_FRAGMENT_DENSITY_MAP_OFFSET_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_OFFSET_PROPERTIES_EXT'
--
--     -   'STRUCTURE_TYPE_RENDERING_END_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_OFFSET_END_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2025-02-14 (Connor Abbott)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_fragment_density_map_offset Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_fragment_density_map_offset  ( PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT
                                                             , PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT
                                                             , RenderPassFragmentDensityMapOffsetEndInfoEXT
                                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT
instance Show PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT

instance FromCStruct PhysicalDeviceFragmentDensityMapOffsetFeaturesEXT


data PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT
instance Show PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT

instance FromCStruct PhysicalDeviceFragmentDensityMapOffsetPropertiesEXT


data RenderPassFragmentDensityMapOffsetEndInfoEXT

instance ToCStruct RenderPassFragmentDensityMapOffsetEndInfoEXT
instance Show RenderPassFragmentDensityMapOffsetEndInfoEXT

instance FromCStruct RenderPassFragmentDensityMapOffsetEndInfoEXT

