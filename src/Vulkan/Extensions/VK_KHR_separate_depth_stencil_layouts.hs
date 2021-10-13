{-# language CPP #-}
-- | = Name
--
-- VK_KHR_separate_depth_stencil_layouts - device extension
--
-- == VK_KHR_separate_depth_stencil_layouts
--
-- [__Name String__]
--     @VK_KHR_separate_depth_stencil_layouts@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     242
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
--     -   Requires @VK_KHR_create_renderpass2@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_separate_depth_stencil_layouts] @pdaniell-nv%0A<<Here describe the issue or question you have about the VK_KHR_separate_depth_stencil_layouts extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-06-25
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
-- [__Contributors__]
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jesse Barker, Unity
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension allows image memory barriers for depth\/stencil images to
-- have just one of the
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
-- aspect bits set, rather than require both. This allows their layouts to
-- be set independently. To support depth\/stencil images with different
-- layouts for the depth and stencil aspects, the depth\/stencil attachment
-- interface has been updated to support a separate layout for stencil.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentDescription2':
--
--     -   'AttachmentDescriptionStencilLayoutKHR'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentReference2':
--
--     -   'AttachmentReferenceStencilLayoutKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME'
--
-- -   'KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR'
--
--     -   'IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR'
--
--     -   'IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL_KHR'
--
--     -   'IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT_KHR'
--
--     -   'STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2019-06-25 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'AttachmentDescriptionStencilLayoutKHR',
-- 'AttachmentReferenceStencilLayoutKHR',
-- 'PhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_separate_depth_stencil_layouts Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_separate_depth_stencil_layouts  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES_KHR
                                                                , pattern STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT_KHR
                                                                , pattern STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT_KHR
                                                                , pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR
                                                                , pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR
                                                                , pattern IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL_KHR
                                                                , pattern IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL_KHR
                                                                , PhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR
                                                                , AttachmentReferenceStencilLayoutKHR
                                                                , AttachmentDescriptionStencilLayoutKHR
                                                                , KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION
                                                                , pattern KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION
                                                                , KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME
                                                                , pattern KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME
                                                                ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (AttachmentDescriptionStencilLayout)
import Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (AttachmentReferenceStencilLayout)
import Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (PhysicalDeviceSeparateDepthStencilLayoutsFeatures)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT_KHR"
pattern STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT_KHR = STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT_KHR"
pattern STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT_KHR = STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT


-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR"
pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL_KHR = IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL


-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR"
pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL_KHR = IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL


-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL_KHR"
pattern IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL_KHR = IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL


-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL_KHR"
pattern IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL_KHR = IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL


-- No documentation found for TopLevel "VkPhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR"
type PhysicalDeviceSeparateDepthStencilLayoutsFeaturesKHR = PhysicalDeviceSeparateDepthStencilLayoutsFeatures


-- No documentation found for TopLevel "VkAttachmentReferenceStencilLayoutKHR"
type AttachmentReferenceStencilLayoutKHR = AttachmentReferenceStencilLayout


-- No documentation found for TopLevel "VkAttachmentDescriptionStencilLayoutKHR"
type AttachmentDescriptionStencilLayoutKHR = AttachmentDescriptionStencilLayout


type KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION"
pattern KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_SPEC_VERSION = 1


type KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME = "VK_KHR_separate_depth_stencil_layouts"

-- No documentation found for TopLevel "VK_KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME"
pattern KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SEPARATE_DEPTH_STENCIL_LAYOUTS_EXTENSION_NAME = "VK_KHR_separate_depth_stencil_layouts"

