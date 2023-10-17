{-# language CPP #-}
-- | = Name
--
-- VK_EXT_attachment_feedback_loop_dynamic_state - device extension
--
-- == VK_EXT_attachment_feedback_loop_dynamic_state
--
-- [__Name String__]
--     @VK_EXT_attachment_feedback_loop_dynamic_state@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     525
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_layout VK_EXT_attachment_feedback_loop_layout>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_attachment_feedback_loop_dynamic_state] @zmike%0A*Here describe the issue or question you have about the VK_EXT_attachment_feedback_loop_dynamic_state extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_attachment_feedback_loop_dynamic_state.adoc VK_EXT_attachment_feedback_loop_dynamic_state>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-04-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Daniel Story, Nintendo
--
--     -   Stu Smith, AMD
--
--     -   Samuel Pitoiset, Valve
--
--     -   Ricardo Garcia, Igalia
--
-- == Description
--
-- This extension adds support for setting attachment feedback loops
-- dynamically on command buffers.
--
-- == New Commands
--
-- -   'cmdSetAttachmentFeedbackLoopEnableEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_EXTENSION_NAME'
--
-- -   'EXT_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ATTACHMENT_FEEDBACK_LOOP_ENABLE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_DYNAMIC_STATE_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2023-04-28 (Mike Blumenkrantz)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT',
-- 'cmdSetAttachmentFeedbackLoopEnableEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_dynamic_state Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_attachment_feedback_loop_dynamic_state  (PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT

instance ToCStruct PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT
instance Show PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT

instance FromCStruct PhysicalDeviceAttachmentFeedbackLoopDynamicStateFeaturesEXT

