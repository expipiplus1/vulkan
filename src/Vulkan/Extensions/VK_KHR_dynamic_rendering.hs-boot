{-# language CPP #-}
-- | = Name
--
-- VK_KHR_dynamic_rendering - device extension
--
-- == VK_KHR_dynamic_rendering
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
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_dynamic_rendering] @tobski%0A<<Here describe the issue or question you have about the VK_KHR_dynamic_rendering extension>> >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_dynamic_rendering.asciidoc VK_KHR_dynamic_rendering>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-10-06
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
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
-- <VK_QCOM_render_pass_store_ops.html VK_QCOM_render_pass_store_ops>,
-- enabling applications to avoid unnecessary synchronization when an
-- attachment is not written during a render pass.
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
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineRenderingCreateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDynamicRenderingFeaturesKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_mixed_attachment_samples VK_AMD_mixed_attachment_samples>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'AttachmentSampleCountInfoAMD'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo':
--
--     -   'RenderingFragmentDensityMapAttachmentInfoEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shading_rate VK_KHR_fragment_shading_rate>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo':
--
--     -   'RenderingFragmentShadingRateAttachmentInfoKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'AttachmentSampleCountInfoNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_multiview_per_view_attributes VK_NVX_multiview_per_view_attributes>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo':
--
--     -   'MultiviewPerViewAttributesInfoNVX'
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
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_mixed_attachment_samples VK_AMD_mixed_attachment_samples>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
--     -   'PIPELINE_RASTERIZATION_STATE_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shading_rate VK_KHR_fragment_shading_rate>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
--     -   'PIPELINE_RASTERIZATION_STATE_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_multiview_per_view_attributes VK_NVX_multiview_per_view_attributes>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_ATTRIBUTES_INFO_NVX'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- KHR suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2021-10-06 (Tobias Hector)
--
--     -   Initial revision
--
-- == See Also
--
-- 'CommandBufferInheritanceRenderingInfoKHR',
-- 'PhysicalDeviceDynamicRenderingFeaturesKHR',
-- 'PipelineRenderingCreateInfoKHR', 'RenderingAttachmentInfoKHR',
-- 'RenderingFlagBitsKHR', 'RenderingFlagsKHR', 'RenderingInfoKHR',
-- 'cmdBeginRenderingKHR', 'cmdEndRenderingKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_dynamic_rendering Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_dynamic_rendering  ( AttachmentSampleCountInfoAMD
                                                   , MultiviewPerViewAttributesInfoNVX
                                                   , RenderingFragmentDensityMapAttachmentInfoEXT
                                                   , RenderingFragmentShadingRateAttachmentInfoKHR
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AttachmentSampleCountInfoAMD

instance ToCStruct AttachmentSampleCountInfoAMD
instance Show AttachmentSampleCountInfoAMD

instance FromCStruct AttachmentSampleCountInfoAMD


data MultiviewPerViewAttributesInfoNVX

instance ToCStruct MultiviewPerViewAttributesInfoNVX
instance Show MultiviewPerViewAttributesInfoNVX

instance FromCStruct MultiviewPerViewAttributesInfoNVX


data RenderingFragmentDensityMapAttachmentInfoEXT

instance ToCStruct RenderingFragmentDensityMapAttachmentInfoEXT
instance Show RenderingFragmentDensityMapAttachmentInfoEXT

instance FromCStruct RenderingFragmentDensityMapAttachmentInfoEXT


data RenderingFragmentShadingRateAttachmentInfoKHR

instance ToCStruct RenderingFragmentShadingRateAttachmentInfoKHR
instance Show RenderingFragmentShadingRateAttachmentInfoKHR

instance FromCStruct RenderingFragmentShadingRateAttachmentInfoKHR

