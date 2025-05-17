{-# language CPP #-}
-- | = Name
--
-- VK_KHR_imageless_framebuffer - device extension
--
-- == VK_KHR_imageless_framebuffer
--
-- [__Name String__]
--     @VK_KHR_imageless_framebuffer@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     109
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance2 VK_KHR_maintenance2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_image_format_list VK_KHR_image_format_list>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_imageless_framebuffer] @tobias%0A*Here describe the issue or question you have about the VK_KHR_imageless_framebuffer extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-12-14
--
-- [__Contributors__]
--
--     -   Tobias Hector
--
--     -   Graham Wihlidal
--
-- == Description
--
-- This extension allows framebuffers to be created without the need for
-- creating images first, allowing more flexibility in how they are used,
-- and avoiding the need for many of the confusing compatibility rules.
--
-- Framebuffers are now created with a small amount of additional metadata
-- about the image views that will be used in
-- 'FramebufferAttachmentsCreateInfoKHR', and the actual image views are
-- provided at render pass begin time via
-- 'RenderPassAttachmentBeginInfoKHR'.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   'FramebufferAttachmentImageInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Pass.FramebufferCreateInfo':
--
--     -   'FramebufferAttachmentsCreateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImagelessFramebufferFeaturesKHR'
--
-- -   Extending 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo':
--
--     -   'RenderPassAttachmentBeginInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME'
--
-- -   'KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FramebufferCreateFlagBits':
--
--     -   'FRAMEBUFFER_CREATE_IMAGELESS_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2018-12-14 (Tobias Hector)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'FramebufferAttachmentImageInfoKHR',
-- 'FramebufferAttachmentsCreateInfoKHR',
-- 'PhysicalDeviceImagelessFramebufferFeaturesKHR',
-- 'RenderPassAttachmentBeginInfoKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_imageless_framebuffer Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_imageless_framebuffer  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES_KHR
                                                       , pattern STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO_KHR
                                                       , pattern STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO_KHR
                                                       , pattern STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO_KHR
                                                       , pattern FRAMEBUFFER_CREATE_IMAGELESS_BIT_KHR
                                                       , PhysicalDeviceImagelessFramebufferFeaturesKHR
                                                       , FramebufferAttachmentsCreateInfoKHR
                                                       , FramebufferAttachmentImageInfoKHR
                                                       , RenderPassAttachmentBeginInfoKHR
                                                       , KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION
                                                       , pattern KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION
                                                       , KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME
                                                       , pattern KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME
                                                       ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (FramebufferAttachmentImageInfo)
import Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (FramebufferAttachmentsCreateInfo)
import Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (PhysicalDeviceImagelessFramebufferFeatures)
import Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (RenderPassAttachmentBeginInfo)
import Vulkan.Core10.Enums.FramebufferCreateFlagBits (FramebufferCreateFlags)
import Vulkan.Core10.Enums.FramebufferCreateFlagBits (FramebufferCreateFlagBits(FRAMEBUFFER_CREATE_IMAGELESS_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO_KHR = STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO_KHR"
pattern STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO_KHR = STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO_KHR"
pattern STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO_KHR = STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO


-- No documentation found for TopLevel "VK_FRAMEBUFFER_CREATE_IMAGELESS_BIT_KHR"
pattern FRAMEBUFFER_CREATE_IMAGELESS_BIT_KHR = FRAMEBUFFER_CREATE_IMAGELESS_BIT


-- No documentation found for TopLevel "VkPhysicalDeviceImagelessFramebufferFeaturesKHR"
type PhysicalDeviceImagelessFramebufferFeaturesKHR = PhysicalDeviceImagelessFramebufferFeatures


-- No documentation found for TopLevel "VkFramebufferAttachmentsCreateInfoKHR"
type FramebufferAttachmentsCreateInfoKHR = FramebufferAttachmentsCreateInfo


-- No documentation found for TopLevel "VkFramebufferAttachmentImageInfoKHR"
type FramebufferAttachmentImageInfoKHR = FramebufferAttachmentImageInfo


-- No documentation found for TopLevel "VkRenderPassAttachmentBeginInfoKHR"
type RenderPassAttachmentBeginInfoKHR = RenderPassAttachmentBeginInfo


type KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION"
pattern KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_IMAGELESS_FRAMEBUFFER_SPEC_VERSION = 1


type KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME = "VK_KHR_imageless_framebuffer"

-- No documentation found for TopLevel "VK_KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME"
pattern KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_IMAGELESS_FRAMEBUFFER_EXTENSION_NAME = "VK_KHR_imageless_framebuffer"

