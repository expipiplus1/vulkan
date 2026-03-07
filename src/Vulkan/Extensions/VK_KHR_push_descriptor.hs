{-# language CPP #-}
-- | = Name
--
-- VK_KHR_push_descriptor - device extension
--
-- = VK_KHR_push_descriptor
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
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_1
--
--     -   Interacts with VK_KHR_descriptor_update_template
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4-promotions Vulkan 1.4>
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_descriptor_update_template VK_KHR_descriptor_update_template>
-- is supported:
--
-- -   'Vulkan.Extensions.VK_KHR_descriptor_update_template.cmdPushDescriptorSetWithTemplateKHR'
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
--     -   'DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_descriptor_update_template VK_KHR_descriptor_update_template>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core11.Enums.DescriptorUpdateTemplateType.DescriptorUpdateTemplateType':
--
--     -   'Vulkan.Extensions.VK_KHR_descriptor_update_template.DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR'
--
-- == Promotion to Vulkan 1.4
--
-- Functionality in this extension is included in core Vulkan 1.4 with the
-- KHR suffix omitted. The original type, enum, and command names are still
-- available as aliases of the core functionality.
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
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_push_descriptor Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_push_descriptor  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
                                                 , pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
                                                 , cmdPushDescriptorSetKHR
                                                 , PhysicalDevicePushDescriptorPropertiesKHR
                                                 , KHR_PUSH_DESCRIPTOR_SPEC_VERSION
                                                 , pattern KHR_PUSH_DESCRIPTOR_SPEC_VERSION
                                                 , KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
                                                 , pattern KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
                                                 , pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
                                                 , cmdPushDescriptorSetWithTemplateKHR
                                                 ) where

import Data.String (IsString)
import Vulkan.Core14.Promoted_From_VK_KHR_push_descriptorRoadmap (cmdPushDescriptorSet)
import Vulkan.Core14.Promoted_From_VK_KHR_push_descriptorRoadmap (PhysicalDevicePushDescriptorProperties)
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlags)
import Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits (DescriptorSetLayoutCreateFlagBits(DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES))
import Vulkan.Extensions.VK_KHR_descriptor_update_template (cmdPushDescriptorSetWithTemplateKHR)
import Vulkan.Extensions.VK_KHR_descriptor_update_template (pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR)
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES


-- No documentation found for TopLevel "VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR"
pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR = DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT


-- No documentation found for TopLevel "vkCmdPushDescriptorSetKHR"
cmdPushDescriptorSetKHR = cmdPushDescriptorSet


-- No documentation found for TopLevel "VkPhysicalDevicePushDescriptorPropertiesKHR"
type PhysicalDevicePushDescriptorPropertiesKHR = PhysicalDevicePushDescriptorProperties


type KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION"
pattern KHR_PUSH_DESCRIPTOR_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2


type KHR_PUSH_DESCRIPTOR_EXTENSION_NAME = "VK_KHR_push_descriptor"

-- No documentation found for TopLevel "VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME"
pattern KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PUSH_DESCRIPTOR_EXTENSION_NAME = "VK_KHR_push_descriptor"

