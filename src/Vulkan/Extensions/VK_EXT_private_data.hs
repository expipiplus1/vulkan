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
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Matthew Rusch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_private_data] @mattruschnv%0A*Here describe the issue or question you have about the VK_EXT_private_data extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-03-25
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
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
-- This extension is a device extension which enables attaching arbitrary
-- payloads to Vulkan objects. It introduces the idea of private data slots
-- as a means of storing a 64-bit unsigned integer of application defined
-- data. Private data slots can be created or destroyed any time an
-- associated device is available. Private data slots can be reserved at
-- device creation time, and limiting use to the amount reserved will allow
-- the extension to exhibit better performance characteristics.
--
-- == New Object Types
--
-- -   'PrivateDataSlotEXT'
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
--     -   'OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT'
--
--     -   'STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- EXT suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == Examples
--
-- -   In progress
--
-- == Issues
--
-- (1) If I have to create a 'Vulkan.Core13.Handles.PrivateDataSlot' to
-- store and retrieve data on an object, how does this extension help me?
-- Will I not need to store the 'Vulkan.Core13.Handles.PrivateDataSlot'
-- mapping with each object, and if I am doing that, I might as well just
-- store the original data!
--
-- __RESOLVED:__ The 'Vulkan.Core13.Handles.PrivateDataSlot' can be thought
-- of as an opaque index into storage that is reserved in each object. That
-- is, you can use the same 'Vulkan.Core13.Handles.PrivateDataSlot' with
-- each object for a specific piece of information. For example, if a layer
-- wishes to track per-object information, the layer only needs to allocate
-- one 'Vulkan.Core13.Handles.PrivateDataSlot' per device and it can use
-- that private data slot for all of the device’s child objects. This
-- allows multiple layers to store private data without conflicting with
-- each other’s and\/or the application’s private data.
--
-- (2) What if I need to store more than 64-bits of information per object?
--
-- __RESOLVED:__ The data that you store per object could be a pointer to
-- another object or structure of your own allocation.
--
-- == Version History
--
-- -   Revision 1, 2020-01-15 (Matthew Rusch)
--
--     -   Initial draft
--
-- == See Also
--
-- 'DevicePrivateDataCreateInfoEXT',
-- 'PhysicalDevicePrivateDataFeaturesEXT', 'PrivateDataSlotCreateFlagsEXT',
-- 'PrivateDataSlotCreateInfoEXT', 'PrivateDataSlotEXT',
-- 'createPrivateDataSlotEXT', 'destroyPrivateDataSlotEXT',
-- 'getPrivateDataEXT', 'setPrivateDataEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_private_data Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_private_data  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT
                                              , pattern STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT
                                              , pattern STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT
                                              , pattern OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT
                                              , createPrivateDataSlotEXT
                                              , destroyPrivateDataSlotEXT
                                              , setPrivateDataEXT
                                              , getPrivateDataEXT
                                              , PrivateDataSlotCreateFlagsEXT
                                              , PrivateDataSlotEXT
                                              , DevicePrivateDataCreateInfoEXT
                                              , PrivateDataSlotCreateInfoEXT
                                              , PhysicalDevicePrivateDataFeaturesEXT
                                              , EXT_PRIVATE_DATA_SPEC_VERSION
                                              , pattern EXT_PRIVATE_DATA_SPEC_VERSION
                                              , EXT_PRIVATE_DATA_EXTENSION_NAME
                                              , pattern EXT_PRIVATE_DATA_EXTENSION_NAME
                                              ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_EXT_private_data (createPrivateDataSlot)
import Vulkan.Core13.Promoted_From_VK_EXT_private_data (destroyPrivateDataSlot)
import Vulkan.Core13.Promoted_From_VK_EXT_private_data (getPrivateData)
import Vulkan.Core13.Promoted_From_VK_EXT_private_data (setPrivateData)
import Vulkan.Core13.Promoted_From_VK_EXT_private_data (DevicePrivateDataCreateInfo)
import Vulkan.Core13.Promoted_From_VK_EXT_private_data (PhysicalDevicePrivateDataFeatures)
import Vulkan.Core13.Handles (PrivateDataSlot)
import Vulkan.Core13.Enums.PrivateDataSlotCreateFlags (PrivateDataSlotCreateFlags)
import Vulkan.Core13.Promoted_From_VK_EXT_private_data (PrivateDataSlotCreateInfo)
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PRIVATE_DATA_SLOT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT = STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT = STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO


-- No documentation found for TopLevel "VK_OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT"
pattern OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT = OBJECT_TYPE_PRIVATE_DATA_SLOT


-- No documentation found for TopLevel "vkCreatePrivateDataSlotEXT"
createPrivateDataSlotEXT = createPrivateDataSlot


-- No documentation found for TopLevel "vkDestroyPrivateDataSlotEXT"
destroyPrivateDataSlotEXT = destroyPrivateDataSlot


-- No documentation found for TopLevel "vkSetPrivateDataEXT"
setPrivateDataEXT = setPrivateData


-- No documentation found for TopLevel "vkGetPrivateDataEXT"
getPrivateDataEXT = getPrivateData


-- No documentation found for TopLevel "VkPrivateDataSlotCreateFlagsEXT"
type PrivateDataSlotCreateFlagsEXT = PrivateDataSlotCreateFlags


-- No documentation found for TopLevel "VkPrivateDataSlotEXT"
type PrivateDataSlotEXT = PrivateDataSlot


-- No documentation found for TopLevel "VkDevicePrivateDataCreateInfoEXT"
type DevicePrivateDataCreateInfoEXT = DevicePrivateDataCreateInfo


-- No documentation found for TopLevel "VkPrivateDataSlotCreateInfoEXT"
type PrivateDataSlotCreateInfoEXT = PrivateDataSlotCreateInfo


-- No documentation found for TopLevel "VkPhysicalDevicePrivateDataFeaturesEXT"
type PhysicalDevicePrivateDataFeaturesEXT = PhysicalDevicePrivateDataFeatures


type EXT_PRIVATE_DATA_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PRIVATE_DATA_SPEC_VERSION"
pattern EXT_PRIVATE_DATA_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PRIVATE_DATA_SPEC_VERSION = 1


type EXT_PRIVATE_DATA_EXTENSION_NAME = "VK_EXT_private_data"

-- No documentation found for TopLevel "VK_EXT_PRIVATE_DATA_EXTENSION_NAME"
pattern EXT_PRIVATE_DATA_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PRIVATE_DATA_EXTENSION_NAME = "VK_EXT_private_data"

