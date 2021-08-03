{-# language CPP #-}
-- | = Name
--
-- VK_EXT_multi_draw - device extension
--
-- == VK_EXT_multi_draw
--
-- [__Name String__]
--     @VK_EXT_multi_draw@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     393
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_multi_draw:%20&body=@zmike%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-05-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, VALVE
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jason Ekstrand, INTEL
--
--     -   Spencer Fricke, SAMSUNG
--
--     -   Ricardo Garcia, IGALIA
--
--     -   Jon Leech, KHRONOS
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- Processing multiple draw commands in sequence incurs measurable overhead
-- within drivers due to repeated state checks and updates during dispatch.
-- This extension enables passing the entire sequence of draws directly to
-- the driver in order to avoid any such overhead, using an array of
-- 'MultiDrawInfoEXT' or 'MultiDrawIndexedInfoEXT' structs with
-- 'cmdDrawMultiEXT' or 'cmdDrawMultiIndexedEXT', respectively. These
-- functions could be used any time multiple draw commands are being
-- recorded without any state changes between them in order to maximize
-- performance.
--
-- == New Commands
--
-- -   'cmdDrawMultiEXT'
--
-- -   'cmdDrawMultiIndexedEXT'
--
-- == New Structures
--
-- -   'MultiDrawIndexedInfoEXT'
--
-- -   'MultiDrawInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMultiDrawFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMultiDrawPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MULTI_DRAW_EXTENSION_NAME'
--
-- -   'EXT_MULTI_DRAW_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTI_DRAW_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTI_DRAW_PROPERTIES_EXT'
--
-- == New or Modified Built-In Variables
--
-- -   (modified)@DrawIndex@
--
-- == Version History
--
-- -   Revision 1, 2021-01-20 (Mike Blumenkrantz)
--
--     -   Initial version
--
-- = See Also
--
-- 'MultiDrawIndexedInfoEXT', 'MultiDrawInfoEXT',
-- 'PhysicalDeviceMultiDrawFeaturesEXT',
-- 'PhysicalDeviceMultiDrawPropertiesEXT', 'cmdDrawMultiEXT',
-- 'cmdDrawMultiIndexedEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_multi_draw Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_multi_draw  ( MultiDrawIndexedInfoEXT
                                            , MultiDrawInfoEXT
                                            , PhysicalDeviceMultiDrawFeaturesEXT
                                            , PhysicalDeviceMultiDrawPropertiesEXT
                                            ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data MultiDrawIndexedInfoEXT

instance ToCStruct MultiDrawIndexedInfoEXT
instance Show MultiDrawIndexedInfoEXT

instance FromCStruct MultiDrawIndexedInfoEXT


data MultiDrawInfoEXT

instance ToCStruct MultiDrawInfoEXT
instance Show MultiDrawInfoEXT

instance FromCStruct MultiDrawInfoEXT


data PhysicalDeviceMultiDrawFeaturesEXT

instance ToCStruct PhysicalDeviceMultiDrawFeaturesEXT
instance Show PhysicalDeviceMultiDrawFeaturesEXT

instance FromCStruct PhysicalDeviceMultiDrawFeaturesEXT


data PhysicalDeviceMultiDrawPropertiesEXT

instance ToCStruct PhysicalDeviceMultiDrawPropertiesEXT
instance Show PhysicalDeviceMultiDrawPropertiesEXT

instance FromCStruct PhysicalDeviceMultiDrawPropertiesEXT

