{-# language CPP #-}
-- | = Name
--
-- VK_HUAWEI_subpass_shading - device extension
--
-- = VK_HUAWEI_subpass_shading
--
-- [__Name String__]
--     @VK_HUAWEI_subpass_shading@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     370
--
-- [__Revision__]
--     3
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>
--              or
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/HUAWEI/SPV_HUAWEI_subpass_shading.html SPV_HUAWEI_subpass_shading>
--
-- [__Contact__]
--
--     -   Pan Gao
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_HUAWEI_subpass_shading] @PanGao-h%0A*Here describe the issue or question you have about the VK_HUAWEI_subpass_shading extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-06-01
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/huawei/GLSL_HUAWEI_subpass_shading.txt GL_HUAWEI_subpass_shading>.
--
-- [__Contributors__]
--
--     -   Hueilong Wang
--
--     -   Juntao Li, Huawei
--
--     -   Renmiao Lu, Huawei
--
--     -   Pan Gao, Huawei
--
-- == Description
--
-- This extension allows applications to execute a subpass shading pipeline
-- in a subpass of a render pass in order to save memory bandwidth for
-- algorithms like tile-based deferred rendering and forward plus. A
-- subpass shading pipeline is a pipeline with the compute pipeline
-- ability, allowed to read values from input attachments, and only allowed
-- to be dispatched inside a stand-alone subpass. Its work dimension is
-- defined by the render pass’s render area size. Its workgroup size
-- (width, height) shall be a power-of-two number in width or height, with
-- minimum value from 8, and maximum value shall be decided from the render
-- pass attachments and sample counts but depends on implementation.
--
-- The @GlobalInvocationId.xy@ of a subpass shading pipeline is equal to
-- the @FragCoord.xy@ of a graphic pipeline in the same render pass
-- subtracted the <VkRect2D.html offset> of the
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@.
-- @GlobalInvocationId.z@ is mapped to the Layer if
-- @VK_EXT_shader_viewport_index_layer@ is supported. The
-- @GlobalInvocationId.xy@ is equal to the index of the local workgroup
-- multiplied by the size of the local workgroup plus the
-- @LocalInvocationId@ and the <VkRect2D.html offset> of the
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@.
--
-- This extension allows a subpass’s pipeline bind point to be
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI'.
--
-- == New Commands
--
-- -   'cmdSubpassShadingHUAWEI'
--
-- -   'getDeviceSubpassShadingMaxWorkgroupSizeHUAWEI'
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.ComputePipeline.ComputePipelineCreateInfo':
--
--     -   'SubpassShadingPipelineCreateInfoHUAWEI'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSubpassShadingFeaturesHUAWEI'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceSubpassShadingPropertiesHUAWEI'
--
-- == New Enum Constants
--
-- -   'HUAWEI_SUBPASS_SHADING_EXTENSION_NAME'
--
-- -   'HUAWEI_SUBPASS_SHADING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint':
--
--     -   'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--
--     -   'PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_SUBPASS_SHADING_BIT_HUAWEI'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_FEATURES_HUAWEI'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_PROPERTIES_HUAWEI'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBPASS_SHADING_PIPELINE_CREATE_INFO_HUAWEI'
--
-- == Sample Code
--
-- Example of subpass shading in a GLSL shader
--
-- > #extension GL_HUAWEI_subpass_shading: enable
-- > #extension GL_KHR_shader_subgroup_arithmetic: enable
-- >
-- > layout(constant_id = 0) const uint tileWidth = 8;
-- > layout(constant_id = 1) const uint tileHeight = 8;
-- > layout(local_size_x_id = 0, local_size_y_id = 1, local_size_z = 1) in;
-- > layout(set=0, binding=0, input_attachment_index=0) uniform subpassInput depth;
-- >
-- > void main()
-- > {
-- >   float d = subpassLoad(depth).x;
-- >   float minD = subgroupMin(d);
-- >   float maxD = subgroupMax(d);
-- > }
--
-- Example of subpass shading dispatching in a subpass
--
-- > vkCmdNextSubpass(commandBuffer, VK_SUBPASS_CONTENTS_INLINE);
-- > vkCmdBindPipeline(commandBuffer, VK_PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI, subpassShadingPipeline);
-- > vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI, subpassShadingPipelineLayout,
-- >   firstSet, descriptorSetCount, pDescriptorSets, dynamicOffsetCount, pDynamicOffsets);
-- > vkCmdSubpassShadingHUAWEI(commandBuffer)
-- > vkCmdEndRenderPass(commandBuffer);
--
-- Example of subpass shading render pass creation
--
-- > VkAttachmentDescription2 attachments[] = {
-- >   {
-- >     VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2, NULL,
-- >     0, VK_FORMAT_R8G8B8A8_UNORM, VK_SAMPLE_COUNT_1_BIT,
-- >     VK_ATTACHMENT_LOAD_OP_CLEAR, VK_ATTACHMENT_STORE_OP_DONT_CARE,
-- >     VK_ATTACHMENT_LOAD_OP_DONT_CARE, VK_ATTACHMENT_LOAD_OP_DONT_CARE,
-- >     VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
-- >   },
-- >   {
-- >     VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2, NULL,
-- >     0, VK_FORMAT_R8G8B8A8_UNORM, VK_SAMPLE_COUNT_1_BIT,
-- >     VK_ATTACHMENT_LOAD_OP_CLEAR, VK_ATTACHMENT_STORE_OP_DONT_CARE,
-- >     VK_ATTACHMENT_LOAD_OP_DONT_CARE, VK_ATTACHMENT_LOAD_OP_DONT_CARE,
-- >     VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
-- >   },
-- >   {
-- >     VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2, NULL,
-- >     0, VK_FORMAT_R8G8B8A8_UNORM, VK_SAMPLE_COUNT_1_BIT,
-- >     VK_ATTACHMENT_LOAD_OP_CLEAR, VK_ATTACHMENT_STORE_OP_DONT_CARE,
-- >     VK_ATTACHMENT_LOAD_OP_DONT_CARE, VK_ATTACHMENT_LOAD_OP_DONT_CARE,
-- >     VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
-- >   },
-- >   {
-- >     VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2, NULL,
-- >     0, VK_FORMAT_D24_UNORM_S8_UINT, VK_SAMPLE_COUNT_1_BIT,
-- >     VK_ATTACHMENT_LOAD_OP_CLEAR, VK_ATTACHMENT_STORE_OP_DONT_CARE,
-- >     VK_ATTACHMENT_LOAD_OP_CLEAR, VK_ATTACHMENT_LOAD_OP_DONT_CARE,
-- >     VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
-- >   },
-- >   {
-- >     VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2, NULL,
-- >     0, VK_FORMAT_R8G8B8A8_UNORM, VK_SAMPLE_COUNT_1_BIT,
-- >     VK_ATTACHMENT_LOAD_OP_CLEAR, VK_ATTACHMENT_STORE_OP_STORE,
-- >     VK_ATTACHMENT_LOAD_OP_DONT_CARE, VK_ATTACHMENT_LOAD_OP_DONT_CARE,
-- >     VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
-- >   }
-- > };
-- >
-- > VkAttachmentReference2 gBufferAttachmentReferences[] = {
-- >   { VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2, NULL, 0, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_IMAGE_ASPECT_COLOR_BIT },
-- >   { VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2, NULL, 1, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_IMAGE_ASPECT_COLOR_BIT },
-- >   { VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2, NULL, 2, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_IMAGE_ASPECT_COLOR_BIT }
-- > };
-- > VkAttachmentReference2 gBufferDepthStencilAttachmentReferences =
-- >   { VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2, NULL, 3, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, VK_IMAGE_ASPECT_DEPTH_BIT|VK_IMAGE_ASPECT_STENCIL_BIT };
-- > VkAttachmentReference2 depthInputAttachmentReferences[] = {
-- >   { VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2, NULL, 3, VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL, VK_IMAGE_ASPECT_DEPTH_BIT|VK_IMAGE_ASPECT_STENCIL_BIT };
-- > };
-- > VkAttachmentReference2 preserveAttachmentReferences[] = {
-- >   { VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2, NULL, 0, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_IMAGE_ASPECT_COLOR_BIT },
-- >   { VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2, NULL, 1, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_IMAGE_ASPECT_COLOR_BIT },
-- >   { VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2, NULL, 2, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_IMAGE_ASPECT_COLOR_BIT },
-- >   { VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2, NULL, 3, VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, VK_IMAGE_ASPECT_DEPTH_BIT|VK_IMAGE_ASPECT_STENCIL_BIT }
-- > }; // G buffer including depth/stencil
-- > VkAttachmentReference2 colorAttachmentReferences[] = {
-- >   { VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2, NULL, 4, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_IMAGE_ASPECT_COLOR_BIT }
-- > };
-- > VkAttachmentReference2 resolveAttachmentReference =
-- >   { VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2, NULL, 4, VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL, VK_IMAGE_ASPECT_COLOR_BIT };
-- >
-- > VkSubpassDescription2 subpasses[] = {
-- >   {
-- >     VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2, NULL, 0, VK_PIPELINE_BIND_POINT_GRAPHICS, 0,
-- >     0, NULL, // input
-- >     sizeof(gBufferAttachmentReferences)/sizeof(gBufferAttachmentReferences[0]), gBufferAttachmentReferences, // color
-- >     NULL, &gBufferDepthStencilAttachmentReferences, // resolve & DS
-- >     0, NULL
-- >   },
-- >   {
-- >     VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2, NULL, 0, VK_PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI , 0,
-- >     sizeof(depthInputAttachmentReferences)/sizeof(depthInputAttachmentReferences[0]), depthInputAttachmentReferences, // input
-- >     0, NULL, // color
-- >     NULL, NULL, // resolve & DS
-- >     sizeof(preserveAttachmentReferences)/sizeof(preserveAttachmentReferences[0]), preserveAttachmentReferences,
-- >   },
-- >   {
-- >     VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2, NULL, 0, VK_PIPELINE_BIND_POINT_GRAPHICS, 0,
-- >     sizeof(gBufferAttachmentReferences)/sizeof(gBufferAttachmentReferences[0]), gBufferAttachmentReferences, // input
-- >     sizeof(colorAttachmentReferences)/sizeof(colorAttachmentReferences[0]), colorAttachmentReferences, // color
-- >     &resolveAttachmentReference, &gBufferDepthStencilAttachmentReferences, // resolve & DS
-- >     0, NULL
-- >   },
-- > };
-- >
-- > VkMemoryBarrier2KHR fragmentToSubpassShading = {
-- >   VK_STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR, NULL,
-- >   VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR, VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT|VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT,
-- >   VK_PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI, VK_ACCESS_INPUT_ATTACHMENT_READ_BIT
-- > };
-- >
-- > VkMemoryBarrier2KHR subpassShadingToFragment = {
-- >   VK_STRUCTURE_TYPE_MEMORY_BARRIER_2_KHR, NULL,
-- >   VK_PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI, VK_ACCESS_SHADER_WRITE_BIT,
-- >   VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR, VK_ACCESS_SHADER_READ_BIT
-- > };
-- >
-- > VkSubpassDependency2 dependencies[] = {
-- >   {
-- >     VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2, &fragmentToSubpassShading,
-- >     0, 1,
-- >     0, 0, 0, 0,
-- >     0, 0
-- >   },
-- >   {
-- >     VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2, &subpassShadingToFragment,
-- >     1, 2,
-- >     0, 0, 0, 0,
-- >     0, 0
-- >   },
-- > };
-- >
-- > VkRenderPassCreateInfo2 renderPassCreateInfo = {
-- >   VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2, NULL, 0,
-- >     sizeof(attachments)/sizeof(attachments[0]), attachments,
-- >     sizeof(subpasses)/sizeof(subpasses[0]), subpasses,
-- >     sizeof(dependencies)/sizeof(dependencies[0]), dependencies,
-- >     0, NULL
-- > };
-- > VKRenderPass renderPass;
-- > vkCreateRenderPass2(device, &renderPassCreateInfo, NULL, &renderPass);
--
-- Example of subpass shading pipeline creation
--
-- > VkExtent2D maxWorkgroupSize;
-- >
-- > VkSpecializationMapEntry subpassShadingConstantMapEntries[] = {
-- >   { 0, 0 * sizeof(uint32_t), sizeof(uint32_t) },
-- >   { 1, 1 * sizeof(uint32_t), sizeof(uint32_t) }
-- > };
-- >
-- > VkSpecializationInfo subpassShadingConstants = {
-- >   2, subpassShadingConstantMapEntries,
-- >   sizeof(VkExtent2D), &maxWorkgroupSize
-- > };
-- >
-- > VkSubpassShadingPipelineCreateInfoHUAWEI subpassShadingPipelineCreateInfo {
-- >   VK_STRUCTURE_TYPE_SUBPASSS_SHADING_PIPELINE_CREATE_INFO_HUAWEI, NULL,
-- >   renderPass, 1
-- > };
-- >
-- > VkPipelineShaderStageCreateInfo subpassShadingPipelineStageCreateInfo {
-- >   VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO, NULL,
-- >   0, VK_SHADER_STAGE_SUBPASS_SHADING_BIT_HUAWEI,
-- >   shaderModule, "main",
-- >   &subpassShadingConstants
-- > };
-- >
-- > VkComputePipelineCreateInfo subpassShadingComputePipelineCreateInfo = {
-- >   VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO, &subpassShadingPipelineCreateInfo,
-- >   0, &subpassShadingPipelineStageCreateInfo,
-- >   pipelineLayout, basePipelineHandle, basePipelineIndex
-- > };
-- >
-- > VKPipeline pipeline;
-- >
-- > vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI(device, renderPass, &maxWorkgroupSize);
-- > vkCreateComputePipelines(device, pipelineCache, 1, &subpassShadingComputePipelineCreateInfo, NULL, &pipeline);
--
-- == Version History
--
-- -   Revision 3, 2023-06-19 (Pan Gao)
--
--     -   Rename 'PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI' to
--         'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--         to better aligned with naming of other pipeline stages
--
-- -   Revision 2, 2021-06-28 (Hueilong Wang)
--
--     -   Change vkGetSubpassShadingMaxWorkgroupSizeHUAWEI to
--         vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI to resolve issue
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/1564 pub1564>
--
-- -   Revision 1, 2020-12-15 (Hueilong Wang)
--
--     -   Initial draft.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_HUAWEI_subpass_shading Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_HUAWEI_subpass_shading  ( getDeviceSubpassShadingMaxWorkgroupSizeHUAWEI
                                                    , cmdSubpassShadingHUAWEI
                                                    , pattern PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI
                                                    , SubpassShadingPipelineCreateInfoHUAWEI(..)
                                                    , PhysicalDeviceSubpassShadingPropertiesHUAWEI(..)
                                                    , PhysicalDeviceSubpassShadingFeaturesHUAWEI(..)
                                                    , HUAWEI_SUBPASS_SHADING_SPEC_VERSION
                                                    , pattern HUAWEI_SUBPASS_SHADING_SPEC_VERSION
                                                    , HUAWEI_SUBPASS_SHADING_EXTENSION_NAME
                                                    , pattern HUAWEI_SUBPASS_SHADING_EXTENSION_NAME
                                                    ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSubpassShadingHUAWEI))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.Handles (RenderPass)
import Vulkan.Core10.Handles (RenderPass(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2(PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_FEATURES_HUAWEI))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_PROPERTIES_HUAWEI))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_SHADING_PIPELINE_CREATE_INFO_HUAWEI))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI
  :: FunPtr (Ptr Device_T -> RenderPass -> Ptr Extent2D -> IO Result) -> Ptr Device_T -> RenderPass -> Ptr Extent2D -> IO Result

-- | vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI - Query maximum
-- supported subpass shading workgroup size for a give render pass
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_HUAWEI_subpass_shading VK_HUAWEI_subpass_shading>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Handles.RenderPass'
getDeviceSubpassShadingMaxWorkgroupSizeHUAWEI :: forall io
                                               . (MonadIO io)
                                              => -- | @device@ is a handle to a local device object that was used to create
                                                 -- the given render pass.
                                                 --
                                                 -- #VUID-vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI-device-parameter#
                                                 -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                                 Device
                                              -> -- | @renderpass@ is a handle to a render pass object describing the
                                                 -- environment in which the pipeline will be used. The pipeline /must/ only
                                                 -- be used with a render pass instance compatible with the one provided.
                                                 -- See
                                                 -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-compatibility Render Pass Compatibility>
                                                 -- for more information.
                                                 --
                                                 -- #VUID-vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI-renderpass-parameter#
                                                 -- @renderpass@ /must/ be a valid 'Vulkan.Core10.Handles.RenderPass' handle
                                                 --
                                                 -- #VUID-vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI-renderpass-parent#
                                                 -- @renderpass@ /must/ have been created, allocated, or retrieved from
                                                 -- @device@
                                                 RenderPass
                                              -> io (("maxWorkgroupSize" ::: Extent2D))
getDeviceSubpassShadingMaxWorkgroupSizeHUAWEI device
                                                renderpass = liftIO . evalContT $ do
  let vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEIPtr = pVkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEIPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI is null" Nothing Nothing
  let vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI' = mkVkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEIPtr
  pPMaxWorkgroupSize <- ContT (withZeroCStruct @Extent2D)
  r <- lift $ traceAroundEvent "vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI" (vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI'
                                                                                    (deviceHandle (device))
                                                                                    (renderpass)
                                                                                    (pPMaxWorkgroupSize))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMaxWorkgroupSize <- lift $ peekCStruct @Extent2D pPMaxWorkgroupSize
  pure $ (pMaxWorkgroupSize)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSubpassShadingHUAWEI
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- | vkCmdSubpassShadingHUAWEI - Dispatch compute work items
--
-- = Description
--
-- When the command is executed, a global workgroup consisting of ceil
-- (render area size \/ local workgroup size) local workgroups is
-- assembled.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR',
--     @reductionMode@ equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE',
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-magFilter-09598# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @reductionMode@ equal to either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR',
--     @reductionMode@ equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE',
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-mipmapMode-09599# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @reductionMode@ equal to either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-unnormalizedCoordinates-09635# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s @levelCount@ and @layerCount@ /must/ be 1
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08609# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s @viewType@ /must/ be
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08610# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08611# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-07888# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor is accessed using atomic operations as a result of this
--     command, then the storage texel buffer’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-buffer-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-02693# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--     extension is not enabled and any 'Vulkan.Core10.Handles.ImageView'
--     is sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a
--     result of this command, it /must/ not have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' of
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE', or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-cubicRangeClamp-09212# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-cubicRangeClamp cubicRangeClamp>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ not have a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-reductionMode-09213# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--     as a result of this command /must/ sample with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-selectableCubicWeights-09214# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-selectableCubicWeights selectableCubicWeights>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.SamplerCubicWeightsCreateInfoQCOM'::@cubicWeights@
--     equal to
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08600# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' without the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline without the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a set /n/, a descriptor set
--     /must/ have been bound to /n/ at the same pipeline bind point, with
--     a 'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT' , as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08601# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' without the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline without the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a push constant value, that
--     value /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility compatible for push constants>
--     with the 'Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-10068# For each array of
--     resources that is used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     the indices used to access members of the array /must/ be less than
--     the descriptor count for the identified binding in the descriptor
--     sets used by this command
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-maintenance4-08602# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' without the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline without the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a push constant value, that
--     value /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility compatible for push constants>
--     with the 'Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' and
--     'Vulkan.Core10.PipelineLayout.PushConstantRange' arrays used to
--     create the current 'Vulkan.Extensions.Handles.ShaderEXT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08114# Descriptors in each
--     bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are accessed as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptor-validity descriptor validity>
--     by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command and the bound
--     'Vulkan.Core10.Handles.Pipeline' was not created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-imageLayout-00344# If an image
--     descriptor is accessed by a shader, the
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' /must/ match the
--     subresource accessible from the 'Vulkan.Core10.Handles.ImageView' as
--     defined by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-layouts-matching-rule image layout matching rules>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08115# If the descriptors used
--     by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point were specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', the
--     bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08116# Descriptors in bound
--     descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command and the bound 'Vulkan.Core10.Handles.Pipeline'
--     was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08604# Descriptors in bound
--     descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08117# If the descriptors used
--     by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point were specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     the bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08119# If a descriptor is
--     dynamically used with a 'Vulkan.Core10.Handles.Pipeline' created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08605# If a descriptor is
--     dynamically used with a 'Vulkan.Extensions.Handles.ShaderEXT'
--     created with a 'Vulkan.Core10.Handles.DescriptorSetLayout' that was
--     created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08606# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature is not enabled, a valid pipeline /must/ be bound to the
--     pipeline bind point used by this command
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08608# If a pipeline is bound
--     to the pipeline bind point used by this command, there /must/ not
--     have been any calls to dynamic state setting commands for any state
--     specified statically in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command, since that
--     pipeline was bound
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-uniformBuffers-06935# If any stage
--     of the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a uniform buffer, and that
--     stage was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08612# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a uniform
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-storageBuffers-06936# If any stage
--     of the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a storage buffer, and that
--     stage was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-08613# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a storage
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shaders>
--     /must/ not be a protected resource
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-viewType-07752# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the image view’s @viewType@ /must/ match the @Dim@
--     operand of the @OpTypeImage@ as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-image-dimensions ???>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-format-07753# If a
--     'Vulkan.Core10.Handles.ImageView' or
--     'Vulkan.Core10.Handles.BufferView' is accessed as a result of this
--     command, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-numericformat numeric type>
--     of the view’s @format@ and the @Sampled@ @Type@ operand of the
--     @OpTypeImage@ /must/ match
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageWrite-08795# If a
--     'Vulkan.Core10.Handles.ImageView' created with a format other than
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have at least as many
--     components as the image view’s format
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageWrite-08796# If a
--     'Vulkan.Core10.Handles.ImageView' created with the format
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have four components
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-sparseImageInt64Atomics-04474# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-sparseImageInt64Atomics-04475# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageSampleWeightedQCOM-06971# If
--     @OpImageSampleWeightedQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageSampleWeightedQCOM-06972# If
--     @OpImageSampleWeightedQCOM@ uses a 'Vulkan.Core10.Handles.ImageView'
--     as a sample weight image as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageBlockMatchSSDQCOM-06974# If
--     @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageBlockMatchSADQCOM-06975# If
--     @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageBlockMatchSADQCOM-06976# If
--     @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageSampleWeightedQCOM-06977# If
--     @OpImageSampleWeightedQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchWindowSSDQCOM@, @OpImageBlockMatchWindowSADQCOM@,
--     @OpImageBlockMatchGatherSSDQCOM@, @OpImageBlockMatchGatherSADQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageSampleWeightedQCOM-06978# If
--     any command other than @OpImageSampleWeightedQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchWindowSSDQCOM@,
--     @OpImageBlockMatchWindowSADQCOM@, @OpImageBlockMatchGatherSSDQCOM@,
--     @OpImageBlockMatchGatherSADQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageBlockMatchWindow-09215# If a
--     @OpImageBlockMatchWindow*QCOM@ or @OpImageBlockMatchGather*QCOM@
--     instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageBlockMatchWindow-09216# If a
--     @OpImageBlockMatchWindow*QCOM@ or @OpImageBlockMatchGather*QCOM@
--     instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s format /must/ be a single-component format
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpImageBlockMatchWindow-09217# If a
--     @OpImageBlockMatchWindow*QCOM@ or @OpImageBlockMatchGather*QCOM@
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-07288# Any shader invocation
--     executed by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-09600# If a descriptor with
--     type equal to any of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     is accessed as a result of this command, all image subresources
--     identified by that descriptor /must/ be in the image layout
--     identified when the descriptor was written
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-commandBuffer-10746# The
--     'Vulkan.Core10.Handles.DeviceMemory' object allocated from a
--     'Vulkan.Core10.DeviceInitialization.MemoryHeap' with the
--     'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
--     property that is bound to a resource accessed as a result of this
--     command /must/ be the active bound
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-bind-tile-memory bound tile memory object>
--     in @commandBuffer@
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-10678# If this command is
--     recorded inside a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-tile-shading tile shading render pass>
--     instance, the stages corresponding to the pipeline bind point used
--     by this command /must/ only include
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     and\/or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-10679# If this command is
--     recorded where
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
--     is enabled, there /must/ be no access to any image while the image
--     was be transitioned to the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     layout
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-pDescription-09900# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the
--     underlying 'Vulkan.Extensions.Handles.TensorARM' object /must/ have
--     been created with the
--     'Vulkan.Extensions.VK_ARM_tensors.TENSOR_USAGE_SHADER_BIT_ARM' usage
--     flag set
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-dimensionCount-09905# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the @Rank@
--     of the @OpTypeTensorARM@ of the tensor resource variable /must/ be
--     equal to the @dimensionCount@ provided via
--     'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM'::@pDescription@
--     when creating the underlying 'Vulkan.Extensions.Handles.TensorARM'
--     object
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-OpTypeTensorARM-09906# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the element
--     type of the @OpTypeTensorARM@ of the tensor resource variable /must/
--     be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-tensor-formats compatible>
--     with the 'Vulkan.Core10.Enums.Format.Format' of the
--     'Vulkan.Extensions.Handles.TensorViewARM' used for the access
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11297# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses a @OpTypeStruct@ decorated with @Block@ or
--     @BufferBlock@ using that mapping, the calculated offset for the
--     resource heap /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-bufferDescriptorAlignment bufferDescriptorAlignment>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11298# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses an @OpTypeImage@ or @OpTypeSampledImage@ using
--     that mapping, the calculated offset for the resource heap /must/ be
--     a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-imageDescriptorAlignment imageDescriptorAlignment>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11299# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses an @OpTypeSampler@ or @OpTypeSampledImage@
--     using that mapping, the calculated offset for the sampler heap
--     /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-samplerDescriptorAlignment samplerDescriptorAlignment>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11397# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses an @OpTypeTensorARM@ using that mapping, the
--     calculated offset for the resource heap /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-tensorDescriptorAlignment tensorDescriptorAlignment>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11300# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a
--     multiple of 4
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11301# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' backed by physical
--     memory at every offset specified by each mapping
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11302# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11304# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a
--     multiple of 8
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11305# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' backed by physical
--     memory at every offset specified by each mapping
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11306# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address pointed to by the address in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11308# For each
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps descriptor heap>
--     that is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     either directly or via a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>,
--     a valid descriptor heap /must/ be bound
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11309# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline with the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, execution of this command /must/ not result in any descriptor
--     read accessing data outside of the user range of the respective heap
--     bound by @vkCmdBind*HeapEXT@ commands
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11372# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a uniform buffer or uniform
--     texel buffer through a descriptor in the bound resource heap, that
--     stage was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @uniformBuffers@, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the descriptor specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DeviceAddressRangeEXT'
--     when the descriptor was written
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11373# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a storage buffer or storage
--     texel buffer through a descriptor in the bound resource heap, that
--     stage was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @storageBuffers@, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the descriptor specified by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DeviceAddressRangeEXT'
--     when the descriptor was written
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11374# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a uniform
--     buffer, uniform texel buffer, storage buffer, or storage texel
--     buffer, that shader /must/ not access values outside of the range of
--     the buffer as specified by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DeviceAddressRangeEXT'
--     when the descriptor was written
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-pBindInfo-11375# If any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     uses an embedded sampler via a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>,
--     the value of @pBindInfo->reservedRangeSize@ set for
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.cmdBindSamplerHeapEXT'
--     /must/ be greater than or equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minSamplerHeapReservedRangeWithEmbedded minSamplerHeapReservedRangeWithEmbedded>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11376# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline with the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a push constant value, that
--     value /must/ have been set by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.cmdPushDataEXT'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11398# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the access
--     /must/ not be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-execution-memory-access-bounds out of bounds>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11437# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the buffer from
--     which the address in push data was queried /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11438# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a uniform buffer using that mapping, the
--     address that the uniform buffer is mapped to /must/ have been
--     queried from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11441# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a uniform buffer using that mapping, the
--     address that the uniform buffer is mapped to /must/ be aligned to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minUniformBufferOffsetAlignment minUniformBufferOffsetAlignment>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11439# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a storage buffer using that mapping, the
--     address that the storage buffer is mapped to /must/ have been
--     queried from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11442# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a storage buffer using that mapping, the
--     address that the storage buffer is mapped to /must/ be aligned to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minStorageBufferOffsetAlignment minStorageBufferOffsetAlignment>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-11485# If a pipeline is bound
--     to the pipeline bind point used by this command, or shader is bound
--     to a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses an acceleration structure using that mapping,
--     the address that the acceleration structure is mapped to /must/ be
--     an acceleration structure address retrieved from a
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' object via
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.getAccelerationStructureDeviceAddressKHR'
--     or handle retrieved from a
--     'Vulkan.Extensions.Handles.AccelerationStructureNV' object via
--     'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-index-11450# If a shader uses a
--     sampler descriptor to sample an image as a result of this command,
--     and that sampler descriptor uses a custom border color with an index
--     defined by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.SamplerCustomBorderColorIndexCreateInfoEXT',
--     the value of
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.SamplerCustomBorderColorIndexCreateInfoEXT'::@index@
--     /must/ have been registered before this command was recorded, and
--     still be registered during the sampling operation, with an
--     identically defined color
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-protectedNoFault-11455# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, a pipeline is bound to the pipeline bind point
--     used by this command, or a shader is bound to a shader stage used by
--     this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     the address that the resource is mapped to /must/ have been queried
--     from a buffer created without the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--     create flag set
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-protectedNoFault-11456# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, a pipeline is bound to the pipeline bind point
--     used by this command, or a shader is bound to a shader stage used by
--     this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     the address of the indirect memory /must/ have been queried from a
--     buffer created without the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--     create flag set
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-None-04931# This command /must/ be
--     called in a subpass with bind point
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI'.
--     No draw commands can be called in the same subpass. Only one
--     'cmdSubpassShadingHUAWEI' command can be called in a subpass
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-renderpass# This command /must/ only
--     be called inside of a render pass instance
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-suspended# This command /must/ not
--     be called between suspended render pass instances
--
-- -   #VUID-vkCmdSubpassShadingHUAWEI-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | VK_QUEUE_GRAPHICS_BIT                                                                                                 | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdSubpassShadingHUAWEI is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_HUAWEI_subpass_shading VK_HUAWEI_subpass_shading>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSubpassShadingHUAWEI :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> io ()
cmdSubpassShadingHUAWEI commandBuffer = liftIO $ do
  let vkCmdSubpassShadingHUAWEIPtr = pVkCmdSubpassShadingHUAWEI (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSubpassShadingHUAWEIPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSubpassShadingHUAWEI is null" Nothing Nothing
  let vkCmdSubpassShadingHUAWEI' = mkVkCmdSubpassShadingHUAWEI vkCmdSubpassShadingHUAWEIPtr
  traceAroundEvent "vkCmdSubpassShadingHUAWEI" (vkCmdSubpassShadingHUAWEI'
                                                  (commandBufferHandle (commandBuffer)))
  pure $ ()


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI"
pattern PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI = PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI


-- | VkSubpassShadingPipelineCreateInfoHUAWEI - Structure specifying
-- parameters of a newly created subpass shading pipeline
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_HUAWEI_subpass_shading VK_HUAWEI_subpass_shading>,
-- 'Vulkan.Core10.Handles.RenderPass',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SubpassShadingPipelineCreateInfoHUAWEI = SubpassShadingPipelineCreateInfoHUAWEI
  { -- | @renderPass@ is a handle to a render pass object describing the
    -- environment in which the pipeline will be used. The pipeline /must/ only
    -- be used with a render pass instance compatible with the one provided.
    -- See
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-compatibility Render Pass Compatibility>
    -- for more information. The implementation /must/ not access this object
    -- outside of the duration of the command this structure is passed to.
    --
    -- #VUID-VkSubpassShadingPipelineCreateInfoHUAWEI-renderPass-parameter#
    -- @renderPass@ /must/ be a valid 'Vulkan.Core10.Handles.RenderPass' handle
    renderPass :: RenderPass
  , -- | @subpass@ is the index of the subpass in the render pass where this
    -- pipeline will be used.
    --
    -- #VUID-VkSubpassShadingPipelineCreateInfoHUAWEI-subpass-04946# @subpass@
    -- /must/ be created with
    -- 'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI'
    -- bind point
    subpass :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassShadingPipelineCreateInfoHUAWEI)
#endif
deriving instance Show SubpassShadingPipelineCreateInfoHUAWEI

instance ToCStruct SubpassShadingPipelineCreateInfoHUAWEI where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassShadingPipelineCreateInfoHUAWEI{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_SHADING_PIPELINE_CREATE_INFO_HUAWEI)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RenderPass)) (renderPass)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (subpass)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBPASS_SHADING_PIPELINE_CREATE_INFO_HUAWEI)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RenderPass)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct SubpassShadingPipelineCreateInfoHUAWEI where
  peekCStruct p = do
    renderPass <- peek @RenderPass ((p `plusPtr` 16 :: Ptr RenderPass))
    subpass <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ SubpassShadingPipelineCreateInfoHUAWEI
             renderPass subpass

instance Storable SubpassShadingPipelineCreateInfoHUAWEI where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SubpassShadingPipelineCreateInfoHUAWEI where
  zero = SubpassShadingPipelineCreateInfoHUAWEI
           zero
           zero


-- | VkPhysicalDeviceSubpassShadingPropertiesHUAWEI - Structure describing
-- subpass shading properties supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceSubpassShadingPropertiesHUAWEI' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_HUAWEI_subpass_shading VK_HUAWEI_subpass_shading>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSubpassShadingPropertiesHUAWEI = PhysicalDeviceSubpassShadingPropertiesHUAWEI
  { -- | #limits-maxSubpassShadingWorkgroupSizeAspectRatio#
    -- @maxSubpassShadingWorkgroupSizeAspectRatio@ indicates the maximum ratio
    -- between the width and height of the portion of the subpass shading
    -- shader workgroup size. @maxSubpassShadingWorkgroupSizeAspectRatio@
    -- /must/ be a power-of-two value, and /must/ be less than or equal to
    -- max(@WorkgroupSize.x@ \/ @WorkgroupSize.y@, @WorkgroupSize.y@ \/
    -- @WorkgroupSize.x@).
    maxSubpassShadingWorkgroupSizeAspectRatio :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSubpassShadingPropertiesHUAWEI)
#endif
deriving instance Show PhysicalDeviceSubpassShadingPropertiesHUAWEI

instance ToCStruct PhysicalDeviceSubpassShadingPropertiesHUAWEI where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSubpassShadingPropertiesHUAWEI{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_PROPERTIES_HUAWEI)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxSubpassShadingWorkgroupSizeAspectRatio)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_PROPERTIES_HUAWEI)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceSubpassShadingPropertiesHUAWEI where
  peekCStruct p = do
    maxSubpassShadingWorkgroupSizeAspectRatio <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceSubpassShadingPropertiesHUAWEI
             maxSubpassShadingWorkgroupSizeAspectRatio

instance Storable PhysicalDeviceSubpassShadingPropertiesHUAWEI where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSubpassShadingPropertiesHUAWEI where
  zero = PhysicalDeviceSubpassShadingPropertiesHUAWEI
           zero


-- | VkPhysicalDeviceSubpassShadingFeaturesHUAWEI - Structure describing
-- whether subpass shading is enabled
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceSubpassShadingFeaturesHUAWEI' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceSubpassShadingFeaturesHUAWEI', it /must/ add an instance
-- of the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_HUAWEI_subpass_shading VK_HUAWEI_subpass_shading>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSubpassShadingFeaturesHUAWEI = PhysicalDeviceSubpassShadingFeaturesHUAWEI
  { -- | #features-subpassShading# @subpassShading@ specifies whether subpass
    -- shading is supported.
    subpassShading :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSubpassShadingFeaturesHUAWEI)
#endif
deriving instance Show PhysicalDeviceSubpassShadingFeaturesHUAWEI

instance ToCStruct PhysicalDeviceSubpassShadingFeaturesHUAWEI where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSubpassShadingFeaturesHUAWEI{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_FEATURES_HUAWEI)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (subpassShading))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBPASS_SHADING_FEATURES_HUAWEI)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSubpassShadingFeaturesHUAWEI where
  peekCStruct p = do
    subpassShading <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceSubpassShadingFeaturesHUAWEI
             (bool32ToBool subpassShading)

instance Storable PhysicalDeviceSubpassShadingFeaturesHUAWEI where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSubpassShadingFeaturesHUAWEI where
  zero = PhysicalDeviceSubpassShadingFeaturesHUAWEI
           zero


type HUAWEI_SUBPASS_SHADING_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_HUAWEI_SUBPASS_SHADING_SPEC_VERSION"
pattern HUAWEI_SUBPASS_SHADING_SPEC_VERSION :: forall a . Integral a => a
pattern HUAWEI_SUBPASS_SHADING_SPEC_VERSION = 3


type HUAWEI_SUBPASS_SHADING_EXTENSION_NAME = "VK_HUAWEI_subpass_shading"

-- No documentation found for TopLevel "VK_HUAWEI_SUBPASS_SHADING_EXTENSION_NAME"
pattern HUAWEI_SUBPASS_SHADING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern HUAWEI_SUBPASS_SHADING_EXTENSION_NAME = "VK_HUAWEI_subpass_shading"

