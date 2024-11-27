{-# language CPP #-}
-- | = Name
--
-- VK_HUAWEI_subpass_shading - device extension
--
-- == VK_HUAWEI_subpass_shading
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_create_renderpass2 VK_KHR_create_renderpass2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
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
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/huawei/GLSL_HUAWEI_subpass_shading.txt GL_HUAWEI_subpass_shading>.
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
-- -   Extending 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo':
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
-- 'PhysicalDeviceSubpassShadingFeaturesHUAWEI',
-- 'PhysicalDeviceSubpassShadingPropertiesHUAWEI',
-- 'SubpassShadingPipelineCreateInfoHUAWEI', 'cmdSubpassShadingHUAWEI',
-- 'getDeviceSubpassShadingMaxWorkgroupSizeHUAWEI'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_HUAWEI_subpass_shading Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_HUAWEI_subpass_shading  ( PhysicalDeviceSubpassShadingFeaturesHUAWEI
                                                    , PhysicalDeviceSubpassShadingPropertiesHUAWEI
                                                    , SubpassShadingPipelineCreateInfoHUAWEI
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceSubpassShadingFeaturesHUAWEI

instance ToCStruct PhysicalDeviceSubpassShadingFeaturesHUAWEI
instance Show PhysicalDeviceSubpassShadingFeaturesHUAWEI

instance FromCStruct PhysicalDeviceSubpassShadingFeaturesHUAWEI


data PhysicalDeviceSubpassShadingPropertiesHUAWEI

instance ToCStruct PhysicalDeviceSubpassShadingPropertiesHUAWEI
instance Show PhysicalDeviceSubpassShadingPropertiesHUAWEI

instance FromCStruct PhysicalDeviceSubpassShadingPropertiesHUAWEI


data SubpassShadingPipelineCreateInfoHUAWEI

instance ToCStruct SubpassShadingPipelineCreateInfoHUAWEI
instance Show SubpassShadingPipelineCreateInfoHUAWEI

instance FromCStruct SubpassShadingPipelineCreateInfoHUAWEI

