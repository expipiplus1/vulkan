{-# language CPP #-}
-- | = Name
--
-- VK_KHR_push_descriptor - device extension
--
-- == VK_KHR_push_descriptor
--
-- [__Name String__]
--     @VK_KHR_push_descriptor@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     81
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_push_descriptor] @jeffbolznv%0A*Here describe the issue or question you have about the VK_KHR_push_descriptor extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-09-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Michael Worcester, Imagination Technologies
--
-- == Description
--
-- This extension allows descriptors to be written into the command buffer,
-- while the implementation is responsible for managing their memory. Push
-- descriptors may enable easier porting from older APIs and in some cases
-- can be more efficient than writing descriptors into descriptor sets.
--
-- == New Commands
--
-- -   'cmdPushDescriptorSetKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_descriptor_update_template VK_KHR_descriptor_update_template>
-- is supported:
--
-- -   'cmdPushDescriptorSetWithTemplateKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- is supported:
--
-- -   'cmdPushDescriptorSetWithTemplateKHR'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePushDescriptorPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PUSH_DESCRIPTOR_EXTENSION_NAME'
--
-- -   'KHR_PUSH_DESCRIPTOR_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_descriptor_update_template VK_KHR_descriptor_update_template>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DescriptorUpdateTemplateType':
--
--     -   'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DescriptorUpdateTemplateType':
--
--     -   'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
--
-- == Version History
--
-- -   Revision 1, 2016-10-15 (Jeff Bolz)
--
--     -   Internal revisions
--
-- -   Revision 2, 2017-09-12 (Tobias Hector)
--
--     -   Added interactions with Vulkan 1.1
--
-- == See Also
--
-- 'PhysicalDevicePushDescriptorPropertiesKHR', 'cmdPushDescriptorSetKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_push_descriptor Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_push_descriptor  (PhysicalDevicePushDescriptorPropertiesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePushDescriptorPropertiesKHR

instance ToCStruct PhysicalDevicePushDescriptorPropertiesKHR
instance Show PhysicalDevicePushDescriptorPropertiesKHR

instance FromCStruct PhysicalDevicePushDescriptorPropertiesKHR

