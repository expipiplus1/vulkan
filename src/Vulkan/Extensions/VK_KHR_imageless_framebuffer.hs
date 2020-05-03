{-# language CPP #-}
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

