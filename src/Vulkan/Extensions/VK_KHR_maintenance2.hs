{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance2 - device extension
--
-- == VK_KHR_maintenance2
--
-- [__Name String__]
--     @VK_KHR_maintenance2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     118
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- [__Contact__]
--
--     -   Michael Worcester
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance2] @michaelworcester%0A*Here describe the issue or question you have about the VK_KHR_maintenance2 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-09-05
--
-- [__Contributors__]
--
--     -   Michael Worcester, Imagination Technologies
--
--     -   Stuart Smith, Imagination Technologies
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Daniel Rakos, AMD
--
--     -   Neil Henning, Codeplay
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- @VK_KHR_maintenance2@ adds a collection of minor features that were
-- intentionally left out or overlooked from the original Vulkan 1.0
-- release.
--
-- The new features are as follows:
--
-- -   Allow the application to specify which aspect of an input attachment
--     might be read for a given subpass.
--
-- -   Allow implementations to express the clipping behavior of points.
--
-- -   Allow creating images with usage flags that may not be supported for
--     the base image’s format, but are supported for image views of the
--     image that have a different but compatible format.
--
-- -   Allow creating uncompressed image views of compressed images.
--
-- -   Allow the application to select between an upper-left and lower-left
--     origin for the tessellation domain space.
--
-- -   Adds two new image layouts for depth stencil images to allow either
--     the depth or stencil aspect to be read-only while the other aspect
--     is writable.
--
-- == Input Attachment Specification
--
-- Input attachment specification allows an application to specify which
-- aspect of a multi-aspect image (e.g. a depth\/stencil format) will be
-- accessed via a @subpassLoad@ operation.
--
-- On some implementations there /may/ be a performance penalty if the
-- implementation does not know (at 'Vulkan.Core10.Pass.createRenderPass'
-- time) which aspect(s) of multi-aspect images /can/ be accessed as input
-- attachments.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   'InputAttachmentAspectReferenceKHR'
--
-- -   Extending 'Vulkan.Core10.ImageView.ImageViewCreateInfo':
--
--     -   'ImageViewUsageCreateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePointClippingPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo':
--
--     -   'PipelineTessellationDomainOriginStateCreateInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Pass.RenderPassCreateInfo':
--
--     -   'RenderPassInputAttachmentAspectCreateInfoKHR'
--
-- == New Enums
--
-- -   'PointClippingBehaviorKHR'
--
-- -   'TessellationDomainOriginKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE2_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE2_SPEC_VERSION'
--
-- -   'KHR_MAINTENANCE_2_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_2_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.ImageCreateFlagBits':
--
--     -   'IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR'
--
--     -   'IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR'
--
--     -   'IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.PointClippingBehavior.PointClippingBehavior':
--
--     -   'POINT_CLIPPING_BEHAVIOR_ALL_CLIP_PLANES_KHR'
--
--     -   'POINT_CLIPPING_BEHAVIOR_USER_CLIP_PLANES_ONLY_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES_KHR'
--
--     -   'STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.TessellationDomainOrigin.TessellationDomainOrigin':
--
--     -   'TESSELLATION_DOMAIN_ORIGIN_LOWER_LEFT_KHR'
--
--     -   'TESSELLATION_DOMAIN_ORIGIN_UPPER_LEFT_KHR'
--
-- == Input Attachment Specification Example
--
-- Consider the case where a render pass has two subpasses and two
-- attachments.
--
-- Attachment 0 has the format
-- 'Vulkan.Core10.Enums.Format.FORMAT_D24_UNORM_S8_UINT', attachment 1 has
-- some color format.
--
-- Subpass 0 writes to attachment 0, subpass 1 reads only the depth
-- information from attachment 0 (using inputAttachmentRead) and writes to
-- attachment 1.
--
-- >     VkInputAttachmentAspectReferenceKHR references[] = {
-- >         {
-- >             .subpass = 1,
-- >             .inputAttachmentIndex = 0,
-- >             .aspectMask = VK_IMAGE_ASPECT_DEPTH_BIT
-- >         }
-- >     };
-- >
-- >     VkRenderPassInputAttachmentAspectCreateInfoKHR specifyAspects = {
-- >         .sType = VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO_KHR,
-- >         .pNext = NULL,
-- >         .aspectReferenceCount = 1,
-- >         .pAspectReferences = references
-- >     };
-- >
-- >
-- >     VkRenderPassCreateInfo createInfo = {
-- >         ...
-- >         .pNext = &specifyAspects,
-- >         ...
-- >     };
-- >
-- >     vkCreateRenderPass(...);
--
-- == Issues
--
-- 1) What is the default tessellation domain origin?
--
-- __RESOLVED__: Vulkan 1.0 originally inadvertently documented a
-- lower-left origin, but the conformance tests and all implementations
-- implemented an upper-left origin. This extension adds a control to
-- select between lower-left (for compatibility with OpenGL) and
-- upper-left, and we retroactively fix unextended Vulkan to have a default
-- of an upper-left origin.
--
-- == Version History
--
-- -   Revision 1, 2017-04-28
--
-- == See Also
--
-- 'ImageViewUsageCreateInfoKHR', 'InputAttachmentAspectReferenceKHR',
-- 'PhysicalDevicePointClippingPropertiesKHR',
-- 'PipelineTessellationDomainOriginStateCreateInfoKHR',
-- 'PointClippingBehaviorKHR',
-- 'RenderPassInputAttachmentAspectCreateInfoKHR',
-- 'TessellationDomainOriginKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_maintenance2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance2  ( pattern KHR_MAINTENANCE2_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE2_EXTENSION_NAME
                                              , pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT_KHR
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
                                              , KHR_MAINTENANCE_2_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE_2_SPEC_VERSION
                                              , KHR_MAINTENANCE_2_EXTENSION_NAME
                                              , pattern KHR_MAINTENANCE_2_EXTENSION_NAME
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
-- No documentation found for TopLevel "VK_KHR_MAINTENANCE2_SPEC_VERSION"
pattern KHR_MAINTENANCE2_SPEC_VERSION = KHR_MAINTENANCE_2_SPEC_VERSION


-- No documentation found for TopLevel "VK_KHR_MAINTENANCE2_EXTENSION_NAME"
pattern KHR_MAINTENANCE2_EXTENSION_NAME = KHR_MAINTENANCE_2_EXTENSION_NAME


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


type KHR_MAINTENANCE_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_2_SPEC_VERSION"
pattern KHR_MAINTENANCE_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE_2_SPEC_VERSION = 1


type KHR_MAINTENANCE_2_EXTENSION_NAME = "VK_KHR_maintenance2"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_2_EXTENSION_NAME"
pattern KHR_MAINTENANCE_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE_2_EXTENSION_NAME = "VK_KHR_maintenance2"

