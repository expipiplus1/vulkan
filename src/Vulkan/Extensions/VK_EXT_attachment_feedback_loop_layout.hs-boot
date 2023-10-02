{-# language CPP #-}
-- | = Name
--
-- VK_EXT_attachment_feedback_loop_layout - device extension
--
-- == VK_EXT_attachment_feedback_loop_layout
--
-- [__Name String__]
--     @VK_EXT_attachment_feedback_loop_layout@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     340
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_attachment_feedback_loop_layout] @Joshua-Ashton%0A*Here describe the issue or question you have about the VK_EXT_attachment_feedback_loop_layout extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_attachment_feedback_loop_layout.adoc VK_EXT_attachment_feedback_loop_layout>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-04-04
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Joshua Ashton, Valve
--
--     -   Faith Ekstrand, Collabora
--
--     -   Bas Nieuwenhuizen, Google
--
--     -   Samuel Iglesias Gonsálvez, Igalia
--
--     -   Ralph Potter, Samsung
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Ricardo Garcia, Igalia
--
-- == Description
--
-- This extension adds a new image layout,
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT',
-- which allows applications to have an image layout in which they are able
-- to both render to and sample\/fetch from the same subresource of an
-- image in a given render pass.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_EXTENSION_NAME'
--
-- -   'EXT_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits':
--
--     -   'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_FEEDBACK_LOOP_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ATTACHMENT_FEEDBACK_LOOP_LAYOUT_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 2, 2022-04-04 (Joshua Ashton)
--
--     -   Renamed from VALVE to EXT.
--
-- -   Revision 1, 2021-03-09 (Joshua Ashton)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_layout Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_attachment_feedback_loop_layout  (PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT

instance ToCStruct PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT
instance Show PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT

instance FromCStruct PhysicalDeviceAttachmentFeedbackLoopLayoutFeaturesEXT

