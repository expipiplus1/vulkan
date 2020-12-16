{-# language CPP #-}
-- | = Name
--
-- VK_EXT_private_data - device extension
--
-- == VK_EXT_private_data
--
-- [__Name String__]
--     @VK_EXT_private_data@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     296
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
--     -   Matthew Rusch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_private_data:%20&body=@mattruschnv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-03-25
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthew Rusch, NVIDIA
--
--     -   Nuno Subtil, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- The \'VK_EXT_private_data\' extension is a device extension which
-- enables attaching arbitrary payloads to Vulkan objects. It introduces
-- the idea of private data slots as a means of storing a 64-bit unsigned
-- integer of application defined data. Private data slots can be created
-- or destroyed any time an associated device is available. Private data
-- slots can be reserved at device creation time, and limiting use to the
-- amount reserved will allow the extension to exhibit better performance
-- characteristics.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.PrivateDataSlotEXT'
--
-- == New Commands
--
-- -   'createPrivateDataSlotEXT'
--
-- -   'destroyPrivateDataSlotEXT'
--
-- -   'getPrivateDataEXT'
--
-- -   'setPrivateDataEXT'
--
-- == New Structures
--
-- -   'PrivateDataSlotCreateInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DevicePrivateDataCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePrivateDataFeaturesEXT'
--
-- == New Enums
--
-- -   'PrivateDataSlotCreateFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'PrivateDataSlotCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PRIVATE_DATA_EXTENSION_NAME'
--
-- -   'EXT_PRIVATE_DATA_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT'
--
-- == Examples
--
-- -   In progress
--
-- == Version History
--
-- -   Revision 1, 2020-01-15 (Matthew Rusch)
--
--     -   Initial draft
--
-- = See Also
--
-- 'DevicePrivateDataCreateInfoEXT',
-- 'PhysicalDevicePrivateDataFeaturesEXT',
-- 'PrivateDataSlotCreateFlagBitsEXT', 'PrivateDataSlotCreateFlagsEXT',
-- 'PrivateDataSlotCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.PrivateDataSlotEXT',
-- 'createPrivateDataSlotEXT', 'destroyPrivateDataSlotEXT',
-- 'getPrivateDataEXT', 'setPrivateDataEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_private_data Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_private_data  ( DevicePrivateDataCreateInfoEXT
                                              , PhysicalDevicePrivateDataFeaturesEXT
                                              , PrivateDataSlotCreateInfoEXT
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DevicePrivateDataCreateInfoEXT

instance ToCStruct DevicePrivateDataCreateInfoEXT
instance Show DevicePrivateDataCreateInfoEXT

instance FromCStruct DevicePrivateDataCreateInfoEXT


data PhysicalDevicePrivateDataFeaturesEXT

instance ToCStruct PhysicalDevicePrivateDataFeaturesEXT
instance Show PhysicalDevicePrivateDataFeaturesEXT

instance FromCStruct PhysicalDevicePrivateDataFeaturesEXT


data PrivateDataSlotCreateInfoEXT

instance ToCStruct PrivateDataSlotCreateInfoEXT
instance Show PrivateDataSlotCreateInfoEXT

instance FromCStruct PrivateDataSlotCreateInfoEXT

