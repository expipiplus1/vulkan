{-# language CPP #-}
-- | = Name
--
-- VK_NV_linear_color_attachment - device extension
--
-- == VK_NV_linear_color_attachment
--
-- [__Name String__]
--     @VK_NV_linear_color_attachment@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     431
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Contact__]
--
--     -   sourav parmar
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_linear_color_attachment] @souravpNV%0A*Here describe the issue or question you have about the VK_NV_linear_color_attachment extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-12-02
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires @VK_KHR_format_feature_flags2@
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Sourav Parmar, NVIDIA
--
-- == Description
--
-- This extension expands support for using
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' images as color
-- attachments when all the color attachments in the render pass instance
-- have 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' tiling. This
-- extension adds a new flag bit
-- 'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV'
-- that extends the existing
-- 'Vulkan.Extensions.VK_KHR_format_feature_flags2.FormatFeatureFlagBits2KHR'
-- bits. This flag /can/ be set for renderable color formats in the
-- 'Vulkan.Extensions.VK_KHR_format_feature_flags2.FormatProperties3KHR'::@linearTilingFeatures@
-- format properties structure member. Formats with the
-- 'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV'
-- flag /may/ be used as color attachments as long as all the color
-- attachments in the render pass instance have
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' tiling, and the
-- formats their images views are created with have
-- 'Vulkan.Extensions.VK_KHR_format_feature_flags2.FormatProperties3KHR'::@linearTilingFeatures@
-- which include
-- 'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV'.
-- This extension supports both dynamic rendering and traditional render
-- passes.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceLinearColorAttachmentFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_LINEAR_COLOR_ATTACHMENT_EXTENSION_NAME'
--
-- -   'NV_LINEAR_COLOR_ATTACHMENT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LINEAR_COLOR_ATTACHMENT_FEATURES_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV'
--
-- == Version History
--
-- -   Revision 1, 2021-11-29 (sourav parmar)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceLinearColorAttachmentFeaturesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_linear_color_attachment Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_linear_color_attachment  (PhysicalDeviceLinearColorAttachmentFeaturesNV) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceLinearColorAttachmentFeaturesNV

instance ToCStruct PhysicalDeviceLinearColorAttachmentFeaturesNV
instance Show PhysicalDeviceLinearColorAttachmentFeaturesNV

instance FromCStruct PhysicalDeviceLinearColorAttachmentFeaturesNV

