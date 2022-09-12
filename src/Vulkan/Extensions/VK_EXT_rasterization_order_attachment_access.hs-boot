{-# language CPP #-}
-- | = Name
--
-- VK_EXT_rasterization_order_attachment_access - device extension
--
-- == VK_EXT_rasterization_order_attachment_access
--
-- [__Name String__]
--     @VK_EXT_rasterization_order_attachment_access@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     464
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_rasterization_order_attachment_access] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_EXT_rasterization_order_attachment_access extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_rasterization_order_attachment_access.adoc VK_EXT_rasterization_order_attachment_access>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-07-04
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
-- This extension extends the mechanism of input attachments to allow
-- access to framebuffer attachments that are used both as input and as
-- color or depth\/stencil attachments from one fragment to the next, in
-- rasterization order, without explicit synchronization.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT'
--
-- == New Enums
--
-- -   'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PipelineColorBlendStateCreateFlagBits'
--
-- -   'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PipelineDepthStencilStateCreateFlagBits'
--
-- == New Enum Constants
--
-- -   'EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_EXTENSION_NAME'
--
-- -   'EXT_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PipelineColorBlendStateCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PipelineDepthStencilStateCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_FEATURES_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits':
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT'
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2022-07-04 (Jan-Harald Fredriksen)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT',
-- 'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PipelineColorBlendStateCreateFlagBits',
-- 'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PipelineDepthStencilStateCreateFlagBits'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_rasterization_order_attachment_access Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_rasterization_order_attachment_access  (PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT

instance ToCStruct PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT
instance Show PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT

instance FromCStruct PhysicalDeviceRasterizationOrderAttachmentAccessFeaturesEXT

