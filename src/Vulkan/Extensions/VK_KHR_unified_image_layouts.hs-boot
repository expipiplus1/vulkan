{-# language CPP #-}
-- | = Name
--
-- VK_KHR_unified_image_layouts - device extension
--
-- = VK_KHR_unified_image_layouts
--
-- [__Name String__]
--     @VK_KHR_unified_image_layouts@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     528
--
-- [__Revision__]
--     1
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
--     -   Interacts with VK_VERSION_1_3
--
--     -   Interacts with VK_EXT_attachment_feedback_loop_layout
--
--     -   Interacts with VK_KHR_dynamic_rendering
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_unified_image_layouts] @syoussefi%0A*Here describe the issue or question you have about the VK_KHR_unified_image_layouts extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_unified_image_layouts.adoc VK_KHR_unified_image_layouts>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-10-15
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with
--         @VK_EXT_attachment_feedback_loop_layout@
--
--     -   This extension interacts with @VK_KHR_video_decode_queue@
--
--     -   This extension interacts with @VK_KHR_video_encode_queue@
--
--     -   This extension interacts with
--         @VK_KHR_video_encode_quantization_map@
--
-- [__Contributors__]
--
--     -   Ahmed Abdelkhalek, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Ting Wei, ARM
--
--     -   Faith Ekstrand, Collabora
--
--     -   Lina Versace, Google
--
--     -   Shahbaz Youssefi, Google
--
--     -   James Fitzpatrick, Imagination
--
--     -   Daniel Story, Nintendo
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Tony Zlatinski, NVIDIA
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Patrick Boyle, Qualcomm
--
--     -   Daniel Rakos, RasterGrid
--
--     -   Ralph Potter, Samsung
--
--     -   Hans-Kristian Arntzen, VALVE
--
--     -   Samuel Pitoiset, VALVE
--
-- == Description
--
-- This extension significantly simplifies synchronization in Vulkan by
-- removing the need for image layout transitions in most cases. In
-- particular, it guarantees that using the
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' layout everywhere
-- possible is just as efficient as using the other layouts.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceUnifiedImageLayoutsFeaturesKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_layout VK_EXT_attachment_feedback_loop_layout>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingAttachmentInfo':
--
--     -   'AttachmentFeedbackLoopInfoEXT'
--
-- == New Enum Constants
--
-- -   'KHR_UNIFIED_IMAGE_LAYOUTS_EXTENSION_NAME'
--
-- -   'KHR_UNIFIED_IMAGE_LAYOUTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFIED_IMAGE_LAYOUTS_FEATURES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_layout VK_EXT_attachment_feedback_loop_layout>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ATTACHMENT_FEEDBACK_LOOP_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2024-10-15 (Shahbaz Youssefi)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_unified_image_layouts Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_unified_image_layouts  ( AttachmentFeedbackLoopInfoEXT
                                                       , PhysicalDeviceUnifiedImageLayoutsFeaturesKHR
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AttachmentFeedbackLoopInfoEXT

instance ToCStruct AttachmentFeedbackLoopInfoEXT
instance Show AttachmentFeedbackLoopInfoEXT

instance FromCStruct AttachmentFeedbackLoopInfoEXT


data PhysicalDeviceUnifiedImageLayoutsFeaturesKHR

instance ToCStruct PhysicalDeviceUnifiedImageLayoutsFeaturesKHR
instance Show PhysicalDeviceUnifiedImageLayoutsFeaturesKHR

instance FromCStruct PhysicalDeviceUnifiedImageLayoutsFeaturesKHR

