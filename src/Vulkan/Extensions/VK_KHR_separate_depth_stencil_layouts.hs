{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_separate_depth_stencil_layouts"
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

