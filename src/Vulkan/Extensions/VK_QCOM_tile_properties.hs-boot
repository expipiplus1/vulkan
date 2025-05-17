{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_tile_properties - device extension
--
-- == VK_QCOM_tile_properties
--
-- [__Name String__]
--     @VK_QCOM_tile_properties@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     485
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_KHR_dynamic_rendering
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_tile_properties] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_tile_properties extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_tile_properties.adoc VK_QCOM_tile_properties>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-07-11
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_EXT_subpass_merge_feedback@
--
-- [__Contributors__]
--
--     -   Jonathan Wicks, Qualcomm Technologies, Inc.
--
--     -   Jonathan Tinkham, Qualcomm Technologies, Inc.
--
--     -   Arpit Agarwal, Qualcomm Technologies, Inc.
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension allows an application to query the tile properties. This
-- extension supports both renderpasses and dynamic rendering.
--
-- == New Commands
--
-- -   'getDynamicRenderingTilePropertiesQCOM'
--
-- -   'getFramebufferTilePropertiesQCOM'
--
-- == New Structures
--
-- -   'TilePropertiesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTilePropertiesFeaturesQCOM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingInfoKHR'
--
-- == New Enum Constants
--
-- -   'QCOM_TILE_PROPERTIES_EXTENSION_NAME'
--
-- -   'QCOM_TILE_PROPERTIES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_PROPERTIES_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_TILE_PROPERTIES_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2022-07-11 (Arpit Agarwal)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceTilePropertiesFeaturesQCOM', 'TilePropertiesQCOM',
-- 'getDynamicRenderingTilePropertiesQCOM',
-- 'getFramebufferTilePropertiesQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_tile_properties Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_tile_properties  ( PhysicalDeviceTilePropertiesFeaturesQCOM
                                                  , TilePropertiesQCOM
                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceTilePropertiesFeaturesQCOM

instance ToCStruct PhysicalDeviceTilePropertiesFeaturesQCOM
instance Show PhysicalDeviceTilePropertiesFeaturesQCOM

instance FromCStruct PhysicalDeviceTilePropertiesFeaturesQCOM


data TilePropertiesQCOM

instance ToCStruct TilePropertiesQCOM
instance Show TilePropertiesQCOM

instance FromCStruct TilePropertiesQCOM

