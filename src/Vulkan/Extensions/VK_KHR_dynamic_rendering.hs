{-# language CPP #-}
-- | = Name
--
-- VK_KHR_dynamic_rendering - device extension
--
-- = VK_KHR_dynamic_rendering
--
-- [__Name String__]
--     @VK_KHR_dynamic_rendering@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     45
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--              or
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_depth_stencil_resolve VK_KHR_depth_stencil_resolve>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_dynamic_rendering] @tobski%0A*Here describe the issue or question you have about the VK_KHR_dynamic_rendering extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_dynamic_rendering.adoc VK_KHR_dynamic_rendering>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-10-06
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Arseny Kapoulkine, Roblox
--
--     -   François Duranleau, Gameloft
--
--     -   Stuart Smith, AMD
--
--     -   Hai Nguyen, Google
--
--     -   Jean-François Roy, Google
--
--     -   Jeff Leger, Qualcomm
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Piers Daniell, Nvidia
--
--     -   James Fitzpatrick, Imagination
--
--     -   Piotr Byszewski, Mobica
--
--     -   Jesse Hall, Google
--
--     -   Mike Blumenkrantz, Valve
--
-- == Description
--
-- This extension allows applications to create single-pass render pass
-- instances without needing to create render pass objects or framebuffers.
-- Dynamic render passes can also span across multiple primary command
-- buffers, rather than relying on secondary command buffers.
--
-- This extension also incorporates 'ATTACHMENT_STORE_OP_NONE_KHR' from
-- @VK_QCOM_render_pass_store_ops@, enabling applications to avoid
-- unnecessary synchronization when an attachment is not written during a
-- render pass.
--
-- == New Commands
--
-- -   'cmdBeginRenderingKHR'
--
-- -   'cmdEndRenderingKHR'
--
-- == New Structures
--
-- -   'RenderingAttachmentInfoKHR'
--
-- -   'RenderingInfoKHR'
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'CommandBufferInheritanceRenderingInfoKHR'
--
-- -   Extending
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineRenderingCreateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDynamicRenderingFeaturesKHR'
--
-- == New Enums
--
-- -   'RenderingFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'RenderingFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DYNAMIC_RENDERING_EXTENSION_NAME'
--
-- -   'KHR_DYNAMIC_RENDERING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AttachmentStoreOp.AttachmentStoreOp':
--
--     -   'ATTACHMENT_STORE_OP_NONE_KHR'
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR'
--
--     -   'RENDERING_RESUMING_BIT_KHR'
--
--     -   'RENDERING_SUSPENDING_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_RENDERING_INFO_KHR'
--
-- == Promotion to Vulkan 1.3
--
-- Vulkan APIs in this extension are included in core Vulkan 1.3, with the
-- KHR suffix omitted. External interactions defined by this extension,
-- such as SPIR-V token names, retain their original names. The original
-- Vulkan API names are still available as aliases of the core
-- functionality.
--
-- == Version History
--
-- -   Revision 1, 2021-10-06 (Tobias Hector)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_dynamic_rendering Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_dynamic_rendering  ( pattern STRUCTURE_TYPE_RENDERING_INFO_KHR
                                                   , pattern STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO_KHR
                                                   , pattern STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO_KHR
                                                   , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES_KHR
                                                   , pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO_KHR
                                                   , pattern ATTACHMENT_STORE_OP_NONE_KHR
                                                   , pattern RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR
                                                   , pattern RENDERING_SUSPENDING_BIT_KHR
                                                   , pattern RENDERING_RESUMING_BIT_KHR
                                                   , cmdBeginRenderingKHR
                                                   , cmdEndRenderingKHR
                                                   , RenderingFlagsKHR
                                                   , RenderingFlagBitsKHR
                                                   , PipelineRenderingCreateInfoKHR
                                                   , RenderingInfoKHR
                                                   , RenderingAttachmentInfoKHR
                                                   , PhysicalDeviceDynamicRenderingFeaturesKHR
                                                   , CommandBufferInheritanceRenderingInfoKHR
                                                   , KHR_DYNAMIC_RENDERING_SPEC_VERSION
                                                   , pattern KHR_DYNAMIC_RENDERING_SPEC_VERSION
                                                   , KHR_DYNAMIC_RENDERING_EXTENSION_NAME
                                                   , pattern KHR_DYNAMIC_RENDERING_EXTENSION_NAME
                                                   ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (cmdBeginRendering)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (cmdEndRendering)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (CommandBufferInheritanceRenderingInfo)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PhysicalDeviceDynamicRenderingFeatures)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PipelineRenderingCreateInfo)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (RenderingAttachmentInfo)
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlagBits)
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlags)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (RenderingInfo)
import Vulkan.Core10.Enums.AttachmentStoreOp (AttachmentStoreOp(ATTACHMENT_STORE_OP_NONE))
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlags)
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlagBits(RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT))
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlags)
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlagBits(RENDERING_RESUMING_BIT))
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlags)
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlagBits(RENDERING_SUSPENDING_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_RENDERING_INFO_KHR"
pattern STRUCTURE_TYPE_RENDERING_INFO_KHR = STRUCTURE_TYPE_RENDERING_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO_KHR"
pattern STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO_KHR = STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO_KHR = STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO_KHR"
pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO_KHR = STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO


-- No documentation found for TopLevel "VK_ATTACHMENT_STORE_OP_NONE_KHR"
pattern ATTACHMENT_STORE_OP_NONE_KHR = ATTACHMENT_STORE_OP_NONE


-- No documentation found for TopLevel "VK_RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR"
pattern RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT_KHR = RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT


-- No documentation found for TopLevel "VK_RENDERING_SUSPENDING_BIT_KHR"
pattern RENDERING_SUSPENDING_BIT_KHR = RENDERING_SUSPENDING_BIT


-- No documentation found for TopLevel "VK_RENDERING_RESUMING_BIT_KHR"
pattern RENDERING_RESUMING_BIT_KHR = RENDERING_RESUMING_BIT


-- No documentation found for TopLevel "vkCmdBeginRenderingKHR"
cmdBeginRenderingKHR = cmdBeginRendering


-- No documentation found for TopLevel "vkCmdEndRenderingKHR"
cmdEndRenderingKHR = cmdEndRendering


-- No documentation found for TopLevel "VkRenderingFlagsKHR"
type RenderingFlagsKHR = RenderingFlags


-- No documentation found for TopLevel "VkRenderingFlagBitsKHR"
type RenderingFlagBitsKHR = RenderingFlagBits


-- No documentation found for TopLevel "VkPipelineRenderingCreateInfoKHR"
type PipelineRenderingCreateInfoKHR = PipelineRenderingCreateInfo


-- No documentation found for TopLevel "VkRenderingInfoKHR"
type RenderingInfoKHR = RenderingInfo


-- No documentation found for TopLevel "VkRenderingAttachmentInfoKHR"
type RenderingAttachmentInfoKHR = RenderingAttachmentInfo


-- No documentation found for TopLevel "VkPhysicalDeviceDynamicRenderingFeaturesKHR"
type PhysicalDeviceDynamicRenderingFeaturesKHR = PhysicalDeviceDynamicRenderingFeatures


-- No documentation found for TopLevel "VkCommandBufferInheritanceRenderingInfoKHR"
type CommandBufferInheritanceRenderingInfoKHR = CommandBufferInheritanceRenderingInfo


type KHR_DYNAMIC_RENDERING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DYNAMIC_RENDERING_SPEC_VERSION"
pattern KHR_DYNAMIC_RENDERING_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DYNAMIC_RENDERING_SPEC_VERSION = 1


type KHR_DYNAMIC_RENDERING_EXTENSION_NAME = "VK_KHR_dynamic_rendering"

-- No documentation found for TopLevel "VK_KHR_DYNAMIC_RENDERING_EXTENSION_NAME"
pattern KHR_DYNAMIC_RENDERING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DYNAMIC_RENDERING_EXTENSION_NAME = "VK_KHR_dynamic_rendering"

