{-# language CPP #-}
-- | = Name
--
-- VK_ARM_rasterization_order_attachment_access - device extension
--
-- == VK_ARM_rasterization_order_attachment_access
--
-- [__Name String__]
--     @VK_ARM_rasterization_order_attachment_access@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     343
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to @VK_EXT_rasterization_order_attachment_access@
--         extension
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_rasterization_order_attachment_access] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_ARM_rasterization_order_attachment_access extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-11-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Jan-Harald Fredriksen, Arm
--
-- == Description
--
-- Render passes, and specifically subpass dependencies, enable much of the
-- same functionality as the framebuffer fetch and pixel local storage
-- extensions did for OpenGL ES. But certain techniques such as
-- programmable blending are awkward or impractical to implement with these
-- alone, in part because a self-dependency is required every time a
-- fragment will read a value at a given sample coordinate.
--
-- This extension extends the mechanism of input attachments to allow
-- access to framebuffer attachments when used as both input and color, or
-- depth\/stencil, attachments from one fragment to the next, in
-- rasterization order, without explicit synchronization.
--
-- See
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-feedbackloop renderpass feedback loops>
-- for more information.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesARM'
--
-- == New Enum Constants
--
-- -   'ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME'
--
-- -   'ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PipelineColorBlendStateCreateFlagBits':
--
--     -   'PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_ARM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PipelineDepthStencilStateCreateFlagBits':
--
--     -   'PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_ARM'
--
--     -   'PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_ARM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits':
--
--     -   'SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_ARM'
--
--     -   'SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_ARM'
--
--     -   'SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_ARM'
--
-- == Issues
--
-- 1) Is there any interaction with the @VK_KHR_dynamic_rendering@
-- extension?
--
-- No. This extension only affects reads from input attachments. Render
-- pass instances begun with
-- 'Vulkan.Extensions.VK_KHR_dynamic_rendering.cmdBeginRenderingKHR' do not
-- have input attachments and a different mechanism will be needed to
-- provide similar functionality in this case.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2021-11-12 (Jan-Harald Fredriksen)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesARM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_ARM_rasterization_order_attachment_access Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_rasterization_order_attachment_access  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_ARM
                                                                       , pattern PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_ARM
                                                                       , pattern PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_ARM
                                                                       , pattern PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_ARM
                                                                       , pattern SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_ARM
                                                                       , pattern SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_ARM
                                                                       , pattern SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_ARM
                                                                       , PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesARM
                                                                       , ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION
                                                                       , pattern ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION
                                                                       , ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME
                                                                       , pattern ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME
                                                                       , PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT(..)
                                                                       ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_EXT_rasterization_order_attachment_access (PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT)
import Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits (PipelineColorBlendStateCreateFlags)
import Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits (PipelineColorBlendStateCreateFlagBits(PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT))
import Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits (PipelineDepthStencilStateCreateFlags)
import Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits (PipelineDepthStencilStateCreateFlagBits(PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT))
import Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits (PipelineDepthStencilStateCreateFlags)
import Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits (PipelineDepthStencilStateCreateFlagBits(PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_EXT))
import Vulkan.Core10.Enums.SubpassDescriptionFlagBits (SubpassDescriptionFlags)
import Vulkan.Core10.Enums.SubpassDescriptionFlagBits (SubpassDescriptionFlagBits(SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_EXT))
import Vulkan.Core10.Enums.SubpassDescriptionFlagBits (SubpassDescriptionFlags)
import Vulkan.Core10.Enums.SubpassDescriptionFlagBits (SubpassDescriptionFlagBits(SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT))
import Vulkan.Core10.Enums.SubpassDescriptionFlagBits (SubpassDescriptionFlags)
import Vulkan.Core10.Enums.SubpassDescriptionFlagBits (SubpassDescriptionFlagBits(SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT))
import Vulkan.Extensions.VK_EXT_rasterization_order_attachment_access (PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_ARM"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_ARM = STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_EXT


-- No documentation found for TopLevel "VK_PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_ARM"
pattern PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_ARM = PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT


-- No documentation found for TopLevel "VK_PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_ARM"
pattern PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_ARM = PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT


-- No documentation found for TopLevel "VK_PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_ARM"
pattern PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_ARM = PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT


-- No documentation found for TopLevel "VK_SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_ARM"
pattern SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_ARM = SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_EXT


-- No documentation found for TopLevel "VK_SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_ARM"
pattern SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_ARM = SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT


-- No documentation found for TopLevel "VK_SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_ARM"
pattern SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_ARM = SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT


-- No documentation found for TopLevel "VkPhysicalDeviceRasterizationOrderAttachmentAccessFeaturesARM"
type PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesARM = PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT


type ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION"
pattern ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION = 1


type ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME = "VK_ARM_rasterization_order_attachment_access"

-- No documentation found for TopLevel "VK_ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME"
pattern ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME = "VK_ARM_rasterization_order_attachment_access"

