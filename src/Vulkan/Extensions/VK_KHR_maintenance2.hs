{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_maintenance2  ( pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR
                                              , pattern IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR
                                              , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR
                                              , pattern STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR
                                              , pattern STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR
                                              , pattern STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR
                                              , pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR
                                              , pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR
                                              , pattern POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR
                                              , pattern POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR
                                              , pattern TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR
                                              , pattern TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR
                                              , PointClippingBehaviorKHR
                                              , TessellationDomainOriginKHR
                                              , InputAttachmentAspectReferenceKHR
                                              , RenderPassInputAttachmentAspectCreateInfoKHR
                                              , PhysicalDevicePointClippingPropertiesKHR
                                              , ImageViewUsageCreateInfoKHR
                                              , PipelineTessellationDomainOriginStateCreateInfoKHR
                                              , KHR_MAINTENANCE2_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE2_SPEC_VERSION
                                              , KHR_MAINTENANCE2_EXTENSION_NAME
                                              , pattern KHR_MAINTENANCE2_EXTENSION_NAME
                                              ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (ImageViewUsageCreateInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (InputAttachmentAspectReference)
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (PhysicalDevicePointClippingProperties)
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (PipelineTessellationDomainOriginStateCreateInfo)
import Vulkan.Core11.Enums.PointClippingBehavior (PointClippingBehavior)
import Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (RenderPassInputAttachmentAspectCreateInfo)
import Vulkan.Core11.Enums.TessellationDomainOrigin (TessellationDomainOrigin)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(IMAGE_CREATE_EXTENDED_USAGE_BIT))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL))
import Vulkan.Core11.Enums.PointClippingBehavior (PointClippingBehavior(POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES))
import Vulkan.Core11.Enums.PointClippingBehavior (PointClippingBehavior(POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO))
import Vulkan.Core11.Enums.TessellationDomainOrigin (TessellationDomainOrigin(TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT))
import Vulkan.Core11.Enums.TessellationDomainOrigin (TessellationDomainOrigin(TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT))
-- No documentation found for TopLevel "VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR"
pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR = IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT


-- No documentation found for TopLevel "VK_IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR"
pattern IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR = IMAGE_CREATE_EXTENDED_USAGE_BIT


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR = STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR = STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR = STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO


-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR"
pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR = IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL


-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR"
pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR = IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL


-- No documentation found for TopLevel "VK_POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR"
pattern POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR = POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES


-- No documentation found for TopLevel "VK_POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR"
pattern POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR = POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY


-- No documentation found for TopLevel "VK_TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR"
pattern TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR = TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT


-- No documentation found for TopLevel "VK_TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR"
pattern TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR = TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT


-- No documentation found for TopLevel "VkPointClippingBehaviorKHR"
type PointClippingBehaviorKHR = PointClippingBehavior


-- No documentation found for TopLevel "VkTessellationDomainOriginKHR"
type TessellationDomainOriginKHR = TessellationDomainOrigin


-- No documentation found for TopLevel "VkInputAttachmentAspectReferenceKHR"
type InputAttachmentAspectReferenceKHR = InputAttachmentAspectReference


-- No documentation found for TopLevel "VkRenderPassInputAttachmentAspectCreateInfoKHR"
type RenderPassInputAttachmentAspectCreateInfoKHR = RenderPassInputAttachmentAspectCreateInfo


-- No documentation found for TopLevel "VkPhysicalDevicePointClippingPropertiesKHR"
type PhysicalDevicePointClippingPropertiesKHR = PhysicalDevicePointClippingProperties


-- No documentation found for TopLevel "VkImageViewUsageCreateInfoKHR"
type ImageViewUsageCreateInfoKHR = ImageViewUsageCreateInfo


-- No documentation found for TopLevel "VkPipelineTessellationDomainOriginStateCreateInfoKHR"
type PipelineTessellationDomainOriginStateCreateInfoKHR = PipelineTessellationDomainOriginStateCreateInfo


type KHR_MAINTENANCE2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE2_SPEC_VERSION"
pattern KHR_MAINTENANCE2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE2_SPEC_VERSION = 1


type KHR_MAINTENANCE2_EXTENSION_NAME = "VK_KHR_maintenance2"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE2_EXTENSION_NAME"
pattern KHR_MAINTENANCE2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE2_EXTENSION_NAME = "VK_KHR_maintenance2"

