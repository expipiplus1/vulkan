{-# language CPP #-}
-- | = Name
--
-- VK_KHR_dynamic_rendering_local_read - device extension
--
-- = VK_KHR_dynamic_rendering_local_read
--
-- [__Name String__]
--     @VK_KHR_dynamic_rendering_local_read@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     233
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4-promotions Vulkan 1.4>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_dynamic_rendering_local_read] @tobski%0A*Here describe the issue or question you have about the VK_KHR_dynamic_rendering_local_read extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_dynamic_rendering_local_read.adoc VK_KHR_dynamic_rendering_local_read>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-11-03
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Connor Abbott, Valve
--
--     -   Pan Gao, Huawei
--
--     -   Lionel Landwerlin, Intel
--
--     -   Shahbaz Youssefi, Google
--
--     -   Alyssa Rosenzweig, Valve
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Graeme Leese, Broadcom
--
--     -   Piers Daniell, Nvidia
--
--     -   Stuart Smith, AMD
--
--     -   Daniel Story, Nintendo
--
--     -   James Fitzpatrick, Imagination
--
--     -   Piotr Byszewski, Mobica
--
--     -   Spencer Fricke, LunarG
--
--     -   Tom Olson, Arm
--
--     -   Michal Pietrasiuk, Intel
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Marty Johnson, Khronos
--
--     -   Wyvern Wang, Huawei
--
--     -   Jeff Bolz, Nvidia
--
--     -   Samuel (Sheng-Wen) Huang, MediaTek
--
-- == Description
--
-- This extension enables reads from attachments and resources written by
-- previous fragment shaders within a dynamic render pass.
--
-- == New Commands
--
-- -   'cmdSetRenderingAttachmentLocationsKHR'
--
-- -   'cmdSetRenderingInputAttachmentIndicesKHR'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'RenderingAttachmentLocationInfoKHR'
--
--     -   'RenderingInputAttachmentIndexInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME'
--
-- -   'KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'IMAGE_LAYOUT_RENDERING_LOCAL_READ_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO_KHR'
--
-- == Promotion to Vulkan 1.4
--
-- Functionality in this extension is included in core Vulkan 1.4, with the
-- KHR suffix omitted. However, Vulkan 1.4 implementations only have to
-- support local read for storage resources and single sampled color
-- attachments.
--
-- Support for reading depth\/stencil attachments and multi-sampled
-- attachments are respectively gated behind the new boolean
-- @dynamicRenderingLocalReadDepthStencilAttachments@ and
-- @dynamicRenderingLocalReadMultisampledAttachments@ properties, as
-- described in the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#versions-1.4 Version 1.4>
-- appendix.
--
-- The original type, enum, and command names are still available as
-- aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2023-11-03 (Tobias Hector)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_dynamic_rendering_local_read Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read  ( pattern IMAGE_LAYOUT_RENDERING_LOCAL_READ_KHR
                                                              , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES_KHR
                                                              , pattern STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO_KHR
                                                              , pattern STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO_KHR
                                                              , cmdSetRenderingAttachmentLocationsKHR
                                                              , cmdSetRenderingInputAttachmentIndicesKHR
                                                              , PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR
                                                              , RenderingAttachmentLocationInfoKHR
                                                              , RenderingInputAttachmentIndexInfoKHR
                                                              , KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION
                                                              , pattern KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION
                                                              , KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME
                                                              , pattern KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME
                                                              ) where

import Data.String (IsString)
import Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap (cmdSetRenderingAttachmentLocations)
import Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap (cmdSetRenderingInputAttachmentIndices)
import Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap (PhysicalDeviceDynamicRenderingLocalReadFeatures)
import Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap (RenderingAttachmentLocationInfo)
import Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap (RenderingInputAttachmentIndexInfo)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(IMAGE_LAYOUT_RENDERING_LOCAL_READ))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO))
-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_RENDERING_LOCAL_READ_KHR"
pattern IMAGE_LAYOUT_RENDERING_LOCAL_READ_KHR = IMAGE_LAYOUT_RENDERING_LOCAL_READ


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO_KHR"
pattern STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO_KHR = STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO_KHR"
pattern STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO_KHR = STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO


-- No documentation found for TopLevel "vkCmdSetRenderingAttachmentLocationsKHR"
cmdSetRenderingAttachmentLocationsKHR = cmdSetRenderingAttachmentLocations


-- No documentation found for TopLevel "vkCmdSetRenderingInputAttachmentIndicesKHR"
cmdSetRenderingInputAttachmentIndicesKHR = cmdSetRenderingInputAttachmentIndices


-- No documentation found for TopLevel "VkPhysicalDeviceDynamicRenderingLocalReadFeaturesKHR"
type PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR = PhysicalDeviceDynamicRenderingLocalReadFeatures


-- No documentation found for TopLevel "VkRenderingAttachmentLocationInfoKHR"
type RenderingAttachmentLocationInfoKHR = RenderingAttachmentLocationInfo


-- No documentation found for TopLevel "VkRenderingInputAttachmentIndexInfoKHR"
type RenderingInputAttachmentIndexInfoKHR = RenderingInputAttachmentIndexInfo


type KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION"
pattern KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DYNAMIC_RENDERING_LOCAL_READ_SPEC_VERSION = 1


type KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME = "VK_KHR_dynamic_rendering_local_read"

-- No documentation found for TopLevel "VK_KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME"
pattern KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DYNAMIC_RENDERING_LOCAL_READ_EXTENSION_NAME = "VK_KHR_dynamic_rendering_local_read"

