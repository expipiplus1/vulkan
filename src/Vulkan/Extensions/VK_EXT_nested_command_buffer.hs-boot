{-# language CPP #-}
-- | = Name
--
-- VK_EXT_nested_command_buffer - device extension
--
-- == VK_EXT_nested_command_buffer
--
-- [__Name String__]
--     @VK_EXT_nested_command_buffer@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     452
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_nested_command_buffer] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_nested_command_buffer extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-09-18
--
-- [__Contributors__]
--
--     -   Daniel Story, Nintendo
--
--     -   Peter Kohaut, NVIDIA
--
--     -   Shahbaz Youssefi, Google
--
--     -   Slawomir Grajewski, Intel
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- With core Vulkan it is not legal to call
-- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' when recording
-- a secondary command buffer. This extension relaxes that restriction,
-- allowing secondary command buffers to execute other secondary command
-- buffers.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceNestedCommandBufferFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceNestedCommandBufferPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_NESTED_COMMAND_BUFFER_EXTENSION_NAME'
--
-- -   'EXT_NESTED_COMMAND_BUFFER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CONTENTS_INLINE_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_NESTED_COMMAND_BUFFER_PROPERTIES_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.SubpassContents.SubpassContents':
--
--     -   'Vulkan.Core10.Enums.SubpassContents.SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_EXT'
--
-- == Issues
--
-- 1) The Command Buffer Levels property for the Vulkan commands comes from
-- the @cmdbufferlevel@ attribute in @vk.xml@ for the command, and it is
-- currently not possible to modify this attribute based on whether an
-- extension is enabled. For this extension we want the @cmdbufferlevel@
-- attribute for vkCmdExecuteCommands to be @primary,secondary@ when this
-- extension is enabled and @primary@ otherwise.
--
-- __RESOLVED__: The @cmdbufferlevel@ attribute for
-- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands' has been
-- changed to @primary,secondary@ and a new VUID added to prohibit
-- recording this command in a secondary command buffer unless this
-- extension is enabled.
--
-- == Version History
--
-- -   Revision 1, 2023-09-18 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceNestedCommandBufferFeaturesEXT',
-- 'PhysicalDeviceNestedCommandBufferPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_nested_command_buffer Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_nested_command_buffer  ( PhysicalDeviceNestedCommandBufferFeaturesEXT
                                                       , PhysicalDeviceNestedCommandBufferPropertiesEXT
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceNestedCommandBufferFeaturesEXT

instance ToCStruct PhysicalDeviceNestedCommandBufferFeaturesEXT
instance Show PhysicalDeviceNestedCommandBufferFeaturesEXT

instance FromCStruct PhysicalDeviceNestedCommandBufferFeaturesEXT


data PhysicalDeviceNestedCommandBufferPropertiesEXT

instance ToCStruct PhysicalDeviceNestedCommandBufferPropertiesEXT
instance Show PhysicalDeviceNestedCommandBufferPropertiesEXT

instance FromCStruct PhysicalDeviceNestedCommandBufferPropertiesEXT

