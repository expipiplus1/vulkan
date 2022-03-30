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
module Vulkan.Extensions.VK_KHR_dynamic_rendering  ( pattern STRUCTURE_TYPE_RENDERING_INFO_KHR
                                                   , pattern STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO_KHR
                                                   , pattern STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO_KHR
                                                   , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES_KHR
                                                   , pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO_KHR
                                                   , pattern ATTACHMENT_STORE_OP_NONE_KHR
                                                   , pattern PIPELINE_RASTERIZATION_STATE_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                   , pattern PIPELINE_RASTERIZATION_STATE_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT
                                                   , pattern STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_NV
                                                   , cmdBeginRenderingKHR
                                                   , cmdEndRenderingKHR
                                                   , RenderingFragmentShadingRateAttachmentInfoKHR(..)
                                                   , RenderingFragmentDensityMapAttachmentInfoEXT(..)
                                                   , AttachmentSampleCountInfoAMD(..)
                                                   , MultiviewPerViewAttributesInfoNVX(..)
                                                   , RenderingFlagsKHR
                                                   , RenderingFlagBitsKHR
                                                   , PipelineRenderingCreateInfoKHR
                                                   , RenderingInfoKHR
                                                   , RenderingAttachmentInfoKHR
                                                   , PhysicalDeviceDynamicRenderingFeaturesKHR
                                                   , CommandBufferInheritanceRenderingInfoKHR
                                                   , AttachmentSampleCountInfoNV
                                                   , KHR_DYNAMIC_RENDERING_SPEC_VERSION
                                                   , pattern KHR_DYNAMIC_RENDERING_SPEC_VERSION
                                                   , KHR_DYNAMIC_RENDERING_EXTENSION_NAME
                                                   , pattern KHR_DYNAMIC_RENDERING_EXTENSION_NAME
                                                   ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (cmdBeginRendering)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (cmdEndRendering)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (CommandBufferInheritanceRenderingInfo)
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Handles (ImageView)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PhysicalDeviceDynamicRenderingFeatures)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PipelineRenderingCreateInfo)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (RenderingAttachmentInfo)
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlagBits)
import Vulkan.Core13.Enums.RenderingFlagBits (RenderingFlags)
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (RenderingInfo)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.AttachmentStoreOp (AttachmentStoreOp(ATTACHMENT_STORE_OP_NONE))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDERING_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_ATTRIBUTES_INFO_NVX))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RENDERING_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_ATTACHMENT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR))
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


-- No documentation found for TopLevel "VK_PIPELINE_RASTERIZATION_STATE_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
pattern PIPELINE_RASTERIZATION_STATE_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR


-- No documentation found for TopLevel "VK_PIPELINE_RASTERIZATION_STATE_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT"
pattern PIPELINE_RASTERIZATION_STATE_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT = PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_NV"
pattern STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_NV = STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD


-- No documentation found for TopLevel "vkCmdBeginRenderingKHR"
cmdBeginRenderingKHR = cmdBeginRendering


-- No documentation found for TopLevel "vkCmdEndRenderingKHR"
cmdEndRenderingKHR = cmdEndRendering


-- | VkRenderingFragmentShadingRateAttachmentInfoKHR - Structure specifying
-- fragment shading rate attachment information
--
-- = Description
--
-- This structure can be included in the @pNext@ chain of
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo' to
-- define a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>.
-- If @imageView@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', or if this
-- structure is not specified, the implementation behaves as if a valid
-- shading rate attachment was specified with all texels specifying a
-- single pixel per fragment.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06147#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @layout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06148#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it
--     /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06149#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shadingRateAttachmentTexelSize.width@ /must/ be a power of two
--     value
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06150#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shadingRateAttachmentTexelSize.width@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxFragmentShadingRateAttachmentTexelSize maxFragmentShadingRateAttachmentTexelSize.width>
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06151#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shadingRateAttachmentTexelSize.width@ /must/ be greater than or
--     equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#limits-minFragmentShadingRateAttachmentTexelSize minFragmentShadingRateAttachmentTexelSize.width>
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06152#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shadingRateAttachmentTexelSize.height@ /must/ be a power of two
--     value
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06153#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shadingRateAttachmentTexelSize.height@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxFragmentShadingRateAttachmentTexelSize maxFragmentShadingRateAttachmentTexelSize.height>
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06154#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @shadingRateAttachmentTexelSize.height@ /must/ be greater than or
--     equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#limits-minFragmentShadingRateAttachmentTexelSize minFragmentShadingRateAttachmentTexelSize.height>
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06155#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     quotient of @shadingRateAttachmentTexelSize.width@ and
--     @shadingRateAttachmentTexelSize.height@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxFragmentShadingRateAttachmentTexelSizeAspectRatio maxFragmentShadingRateAttachmentTexelSizeAspectRatio>
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-06156#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     quotient of @shadingRateAttachmentTexelSize.height@ and
--     @shadingRateAttachmentTexelSize.width@ /must/ be less than or equal
--     to
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxFragmentShadingRateAttachmentTexelSizeAspectRatio maxFragmentShadingRateAttachmentTexelSizeAspectRatio>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR'
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageView-parameter#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @imageView@ /must/ be a valid 'Vulkan.Core10.Handles.ImageView'
--     handle
--
-- -   #VUID-VkRenderingFragmentShadingRateAttachmentInfoKHR-imageLayout-parameter#
--     @imageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shading_rate VK_KHR_fragment_shading_rate>,
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Handles.ImageView',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderingFragmentShadingRateAttachmentInfoKHR = RenderingFragmentShadingRateAttachmentInfoKHR
  { -- | @imageView@ is the image view that will be used as a fragment shading
    -- rate attachment.
    imageView :: ImageView
  , -- | @imageLayout@ is the layout that @imageView@ will be in during
    -- rendering.
    imageLayout :: ImageLayout
  , -- | @shadingRateAttachmentTexelSize@ specifies the number of pixels
    -- corresponding to each texel in @imageView@.
    shadingRateAttachmentTexelSize :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingFragmentShadingRateAttachmentInfoKHR)
#endif
deriving instance Show RenderingFragmentShadingRateAttachmentInfoKHR

instance ToCStruct RenderingFragmentShadingRateAttachmentInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingFragmentShadingRateAttachmentInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageView)) (imageView)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (imageLayout)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (shadingRateAttachmentTexelSize)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct RenderingFragmentShadingRateAttachmentInfoKHR where
  peekCStruct p = do
    imageView <- peek @ImageView ((p `plusPtr` 16 :: Ptr ImageView))
    imageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    shadingRateAttachmentTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 28 :: Ptr Extent2D))
    pure $ RenderingFragmentShadingRateAttachmentInfoKHR
             imageView imageLayout shadingRateAttachmentTexelSize

instance Storable RenderingFragmentShadingRateAttachmentInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderingFragmentShadingRateAttachmentInfoKHR where
  zero = RenderingFragmentShadingRateAttachmentInfoKHR
           zero
           zero
           zero


-- | VkRenderingFragmentDensityMapAttachmentInfoEXT - Structure specifying
-- fragment shading rate attachment information
--
-- = Description
--
-- This structure can be included in the @pNext@ chain of
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo' to
-- define a fragment density map. If this structure is not included in the
-- @pNext@ chain, @imageView@ is treated as
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderingFragmentDensityMapAttachmentInfoEXT-imageView-06157#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @layout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT'
--
-- -   #VUID-VkRenderingFragmentDensityMapAttachmentInfoEXT-imageView-06158#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it
--     /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT'
--
-- -   #VUID-VkRenderingFragmentDensityMapAttachmentInfoEXT-imageView-06159#
--     If @imageView@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', it
--     /must/ not have been created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderingFragmentDensityMapAttachmentInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT'
--
-- -   #VUID-VkRenderingFragmentDensityMapAttachmentInfoEXT-imageView-parameter#
--     @imageView@ /must/ be a valid 'Vulkan.Core10.Handles.ImageView'
--     handle
--
-- -   #VUID-VkRenderingFragmentDensityMapAttachmentInfoEXT-imageLayout-parameter#
--     @imageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.Handles.ImageView',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RenderingFragmentDensityMapAttachmentInfoEXT = RenderingFragmentDensityMapAttachmentInfoEXT
  { -- | @imageView@ is the image view that will be used as a fragment shading
    -- rate attachment.
    imageView :: ImageView
  , -- | @imageLayout@ is the layout that @imageView@ will be in during
    -- rendering.
    imageLayout :: ImageLayout
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingFragmentDensityMapAttachmentInfoEXT)
#endif
deriving instance Show RenderingFragmentDensityMapAttachmentInfoEXT

instance ToCStruct RenderingFragmentDensityMapAttachmentInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingFragmentDensityMapAttachmentInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageView)) (imageView)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (imageLayout)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageView)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ImageLayout)) (zero)
    f

instance FromCStruct RenderingFragmentDensityMapAttachmentInfoEXT where
  peekCStruct p = do
    imageView <- peek @ImageView ((p `plusPtr` 16 :: Ptr ImageView))
    imageLayout <- peek @ImageLayout ((p `plusPtr` 24 :: Ptr ImageLayout))
    pure $ RenderingFragmentDensityMapAttachmentInfoEXT
             imageView imageLayout

instance Storable RenderingFragmentDensityMapAttachmentInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderingFragmentDensityMapAttachmentInfoEXT where
  zero = RenderingFragmentDensityMapAttachmentInfoEXT
           zero
           zero


-- | VkAttachmentSampleCountInfoAMD - Structure specifying command buffer
-- inheritance info for dynamic render pass instances
--
-- = Description
--
-- If
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@renderPass@
-- is 'Vulkan.Core10.APIConstants.NULL_HANDLE',
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
-- is specified in
-- 'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@flags@, and the
-- @pNext@ chain of
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' includes
-- 'AttachmentSampleCountInfoAMD', then this structure defines the sample
-- counts of each attachment within the render pass instance. If
-- 'AttachmentSampleCountInfoAMD' is not included, the value of
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'::@rasterizationSamples@
-- is used as the sample count for each attachment. If
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@renderPass@
-- is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', or
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
-- is not specified in
-- 'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@flags@,
-- parameters of this structure are ignored.
--
-- 'AttachmentSampleCountInfoAMD' /can/ also be included in the @pNext@
-- chain of 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'. When a
-- graphics pipeline is created without a
-- 'Vulkan.Core10.Handles.RenderPass', if this structure is present in the
-- @pNext@ chain of 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo', it
-- specifies the sample count of attachments used for rendering. If this
-- structure is not specified, and the pipeline does not include a
-- 'Vulkan.Core10.Handles.RenderPass', the value of
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
-- is used as the sample count for each attachment. If a graphics pipeline
-- is created with a valid 'Vulkan.Core10.Handles.RenderPass', parameters
-- of this structure are ignored.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_mixed_attachment_samples VK_AMD_mixed_attachment_samples>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>,
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AttachmentSampleCountInfoAMD = AttachmentSampleCountInfoAMD
  { -- | @pColorAttachmentSamples@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' values
    -- defining the sample count of color attachments.
    colorAttachmentSamples :: Vector SampleCountFlagBits
  , -- | @depthStencilAttachmentSamples@ is a
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
    -- defining the sample count of a depth\/stencil attachment.
    depthStencilAttachmentSamples :: SampleCountFlagBits
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentSampleCountInfoAMD)
#endif
deriving instance Show AttachmentSampleCountInfoAMD

instance ToCStruct AttachmentSampleCountInfoAMD where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentSampleCountInfoAMD{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorAttachmentSamples)) :: Word32))
    pPColorAttachmentSamples' <- ContT $ allocaBytes @SampleCountFlagBits ((Data.Vector.length (colorAttachmentSamples)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachmentSamples' `plusPtr` (4 * (i)) :: Ptr SampleCountFlagBits) (e)) (colorAttachmentSamples)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr SampleCountFlagBits))) (pPColorAttachmentSamples')
    lift $ poke ((p `plusPtr` 32 :: Ptr SampleCountFlagBits)) (depthStencilAttachmentSamples)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct AttachmentSampleCountInfoAMD where
  peekCStruct p = do
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pColorAttachmentSamples <- peek @(Ptr SampleCountFlagBits) ((p `plusPtr` 24 :: Ptr (Ptr SampleCountFlagBits)))
    pColorAttachmentSamples' <- generateM (fromIntegral colorAttachmentCount) (\i -> peek @SampleCountFlagBits ((pColorAttachmentSamples `advancePtrBytes` (4 * (i)) :: Ptr SampleCountFlagBits)))
    depthStencilAttachmentSamples <- peek @SampleCountFlagBits ((p `plusPtr` 32 :: Ptr SampleCountFlagBits))
    pure $ AttachmentSampleCountInfoAMD
             pColorAttachmentSamples' depthStencilAttachmentSamples

instance Zero AttachmentSampleCountInfoAMD where
  zero = AttachmentSampleCountInfoAMD
           mempty
           zero


-- | VkMultiviewPerViewAttributesInfoNVX - Structure specifying the multiview
-- per-attribute properties
--
-- = Description
--
-- When dynamic render pass instances are being used, instead of specifying
-- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_PER_VIEW_ATTRIBUTES_BIT_NVX'
-- or
-- 'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_PER_VIEW_POSITION_X_ONLY_BIT_NVX'
-- in the subpass description flags, the per-attibute properties of the
-- render pass instance /must/ be specified by the
-- 'MultiviewPerViewAttributesInfoNVX' structure Include the
-- 'MultiviewPerViewAttributesInfoNVX' structure in the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' when creating a
-- graphics pipeline for dynamic rendering,
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'
-- when starting a dynamic render pass instance, and
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' when
-- specifying the dynamic render pass instance parameters for secondary
-- command buffers.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NVX_multiview_per_view_attributes VK_NVX_multiview_per_view_attributes>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MultiviewPerViewAttributesInfoNVX = MultiviewPerViewAttributesInfoNVX
  { -- | @perViewAttributes@ specifies that shaders compiled for this pipeline
    -- write the attributes for all views in a single invocation of each vertex
    -- processing stage. All pipelines executed within a render pass instance
    -- that includes this bit /must/ write per-view attributes to the
    -- @*PerViewNV[]@ shader outputs, in addition to the non-per-view (e.g.
    -- @Position@) outputs.
    perViewAttributes :: Bool
  , -- | @perViewAttributesPositionXOnly@ specifies that shaders compiled for
    -- this pipeline use per-view positions which only differ in value in the x
    -- component. Per-view viewport mask /can/ also be used.
    perViewAttributesPositionXOnly :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MultiviewPerViewAttributesInfoNVX)
#endif
deriving instance Show MultiviewPerViewAttributesInfoNVX

instance ToCStruct MultiviewPerViewAttributesInfoNVX where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MultiviewPerViewAttributesInfoNVX{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_ATTRIBUTES_INFO_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (perViewAttributes))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (perViewAttributesPositionXOnly))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_ATTRIBUTES_INFO_NVX)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct MultiviewPerViewAttributesInfoNVX where
  peekCStruct p = do
    perViewAttributes <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    perViewAttributesPositionXOnly <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ MultiviewPerViewAttributesInfoNVX
             (bool32ToBool perViewAttributes) (bool32ToBool perViewAttributesPositionXOnly)

instance Storable MultiviewPerViewAttributesInfoNVX where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MultiviewPerViewAttributesInfoNVX where
  zero = MultiviewPerViewAttributesInfoNVX
           zero
           zero


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


-- No documentation found for TopLevel "VkAttachmentSampleCountInfoNV"
type AttachmentSampleCountInfoNV = AttachmentSampleCountInfoAMD


type KHR_DYNAMIC_RENDERING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DYNAMIC_RENDERING_SPEC_VERSION"
pattern KHR_DYNAMIC_RENDERING_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DYNAMIC_RENDERING_SPEC_VERSION = 1


type KHR_DYNAMIC_RENDERING_EXTENSION_NAME = "VK_KHR_dynamic_rendering"

-- No documentation found for TopLevel "VK_KHR_DYNAMIC_RENDERING_EXTENSION_NAME"
pattern KHR_DYNAMIC_RENDERING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DYNAMIC_RENDERING_EXTENSION_NAME = "VK_KHR_dynamic_rendering"

