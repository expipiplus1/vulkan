{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_imageless_framebuffer"
module Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer  ( FramebufferAttachmentImageInfo
                                                                 , FramebufferAttachmentsCreateInfo
                                                                 , PhysicalDeviceImagelessFramebufferFeatures
                                                                 , RenderPassAttachmentBeginInfo
                                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data FramebufferAttachmentImageInfo

instance ToCStruct FramebufferAttachmentImageInfo
instance Show FramebufferAttachmentImageInfo

instance FromCStruct FramebufferAttachmentImageInfo


data FramebufferAttachmentsCreateInfo

instance ToCStruct FramebufferAttachmentsCreateInfo
instance Show FramebufferAttachmentsCreateInfo

instance FromCStruct FramebufferAttachmentsCreateInfo


data PhysicalDeviceImagelessFramebufferFeatures

instance ToCStruct PhysicalDeviceImagelessFramebufferFeatures
instance Show PhysicalDeviceImagelessFramebufferFeatures

instance FromCStruct PhysicalDeviceImagelessFramebufferFeatures


data RenderPassAttachmentBeginInfo

instance ToCStruct RenderPassAttachmentBeginInfo
instance Show RenderPassAttachmentBeginInfo

instance FromCStruct RenderPassAttachmentBeginInfo

