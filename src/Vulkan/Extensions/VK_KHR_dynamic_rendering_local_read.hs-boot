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
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
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
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_RENDERING_LOCAL_READ_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_LOCAL_READ_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_ATTACHMENT_LOCATION_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_INPUT_ATTACHMENT_INDEX_INFO_KHR'
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_dynamic_rendering_local_read  ( PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR
                                                              , RenderingAttachmentLocationInfoKHR
                                                              , RenderingInputAttachmentIndexInfoKHR
                                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR

instance ToCStruct PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR
instance Show PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR

instance FromCStruct PhysicalDeviceDynamicRenderingLocalReadFeaturesKHR


data RenderingAttachmentLocationInfoKHR

instance ToCStruct RenderingAttachmentLocationInfoKHR
instance Show RenderingAttachmentLocationInfoKHR

instance FromCStruct RenderingAttachmentLocationInfoKHR


data RenderingInputAttachmentIndexInfoKHR

instance ToCStruct RenderingInputAttachmentIndexInfoKHR
instance Show RenderingInputAttachmentIndexInfoKHR

instance FromCStruct RenderingInputAttachmentIndexInfoKHR

