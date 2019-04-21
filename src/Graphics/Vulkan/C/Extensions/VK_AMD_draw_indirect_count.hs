{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_draw_indirect_count
  ( vkCmdDrawIndexedIndirectCountAMD
  , vkCmdDrawIndirectCountAMD
  , pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count
  ( vkCmdDrawIndexedIndirectCountKHR
  , vkCmdDrawIndirectCountKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | vkCmdDrawIndexedIndirectCountAMD - Perform an indexed indirect draw with
-- the draw count sourced from a buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @buffer@ is the buffer containing draw parameters.
--
-- -   @offset@ is the byte offset into @buffer@ where parameters begin.
--
-- -   @countBuffer@ is the buffer containing the draw count.
--
-- -   @countBufferOffset@ is the byte offset into @countBuffer@ where the
--     draw count begins.
--
-- -   @maxDrawCount@ specifies the maximum number of draws that will be
--     executed. The actual number of executed draw calls is the minimum of
--     the count specified in @countBuffer@ and @maxDrawCount@.
--
-- -   @stride@ is the byte stride between successive sets of draw
--     parameters.
--
-- = Description
--
-- 'vkCmdDrawIndexedIndirectCountAMD' behaves similarly to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect'
-- except that the draw count is read by the device from a buffer during
-- execution. The command will read an unsigned 32-bit integer from
-- @countBuffer@ located at @countBufferOffset@ and use this as the draw
-- count.
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @buffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   If @countBuffer@ is non-sparse then it /must/ be bound completely
--     and contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @countBuffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   @countBufferOffset@ /must/ be a multiple of @4@
--
-- -   @stride@ /must/ be a multiple of @4@ and /must/ be greater than or
--     equal to
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand')
--
-- -   If @maxDrawCount@ is greater than or equal to @1@, (@stride@ ×
--     (@maxDrawCount@ - 1) + @offset@ +
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, all the @firstInstance@ members of the
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'
--     structures accessed by this command /must/ be @0@
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS', a
--     descriptor set /must/ have been bound to /n/ at
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS',
--     with a 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS', a
--     push constant value /must/ have been set for
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS',
--     with a 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the bound
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindPipeline'
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   A valid graphics pipeline /must/ be bound to the current command
--     buffer with
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to 'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
--     requires any dynamic state, that state /must/ have been set on the
--     current command buffer
--
-- -   If count stored in @countBuffer@ is equal to @1@, (@offset@ +
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   If count stored in @countBuffer@ is greater than @1@, (@stride@ ×
--     (@drawCount@ - 1) + @offset@ +
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   @drawCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   If any 'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that is
--     accessed from a shader by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS' uses
--     unnormalized coordinates, it /must/ not be used to sample from any
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If any 'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that is
--     accessed from a shader by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS' uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any 'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that is
--     accessed from a shader by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS' uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of that buffer specified in the bound descriptor set
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of that buffer specified in the bound descriptor set
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'.
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' and the current
--     subpass has a depth\/stencil attachment, then that attachment /must/
--     have been created with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   Any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' created with
--     a 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@
--     containing
--     'Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image.VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSamplerAddressMode' of
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
--
-- Unresolved directive in vkCmdDrawIndexedIndirectCountAMD.txt -
-- include::..\/validity\/protos\/vkCmdDrawIndexedIndirectCountAMD.txt[]
--
-- = See Also
--
-- No cross-references are available
vkCmdDrawIndexedIndirectCountAMD :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
vkCmdDrawIndexedIndirectCountAMD = vkCmdDrawIndexedIndirectCountKHR

-- | vkCmdDrawIndirectCountAMD - Perform an indirect draw with the draw count
-- sourced from a buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @buffer@ is the buffer containing draw parameters.
--
-- -   @offset@ is the byte offset into @buffer@ where parameters begin.
--
-- -   @countBuffer@ is the buffer containing the draw count.
--
-- -   @countBufferOffset@ is the byte offset into @countBuffer@ where the
--     draw count begins.
--
-- -   @maxDrawCount@ specifies the maximum number of draws that will be
--     executed. The actual number of executed draw calls is the minimum of
--     the count specified in @countBuffer@ and @maxDrawCount@.
--
-- -   @stride@ is the byte stride between successive sets of draw
--     parameters.
--
-- = Description
--
-- 'vkCmdDrawIndirectCountAMD' behaves similarly to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect'
-- except that the draw count is read by the device from a buffer during
-- execution. The command will read an unsigned 32-bit integer from
-- @countBuffer@ located at @countBufferOffset@ and use this as the draw
-- count.
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @buffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   If @countBuffer@ is non-sparse then it /must/ be bound completely
--     and contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @countBuffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   @countBufferOffset@ /must/ be a multiple of @4@
--
-- -   @stride@ /must/ be a multiple of @4@ and /must/ be greater than or
--     equal to
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand')
--
-- -   If @maxDrawCount@ is greater than or equal to @1@, (@stride@ ×
--     (@maxDrawCount@ - 1) + @offset@ +
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, all the @firstInstance@ members of the
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'
--     structures accessed by this command /must/ be @0@
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS', a
--     descriptor set /must/ have been bound to /n/ at
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS',
--     with a 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS', a
--     push constant value /must/ have been set for
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS',
--     with a 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the bound
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindPipeline'
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   A valid graphics pipeline /must/ be bound to the current command
--     buffer with
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to 'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
--     requires any dynamic state, that state /must/ have been set on the
--     current command buffer
--
-- -   If the count stored in @countBuffer@ is equal to @1@, (@offset@ +
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   If the count stored in @countBuffer@ is greater than @1@, (@stride@
--     × (@drawCount@ - 1) + @offset@ +
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   The count stored in @countBuffer@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   If any 'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that is
--     accessed from a shader by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS' uses
--     unnormalized coordinates, it /must/ not be used to sample from any
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If any 'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that is
--     accessed from a shader by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS' uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any 'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that is
--     accessed from a shader by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS' uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of that buffer specified in the bound descriptor set
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of that buffer specified in the bound descriptor set
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'.
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' and the current
--     subpass has a depth\/stencil attachment, then that attachment /must/
--     have been created with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   Any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' created with
--     a 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@
--     containing
--     'Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image.VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSamplerAddressMode' of
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
--
-- Unresolved directive in vkCmdDrawIndirectCountAMD.txt -
-- include::..\/validity\/protos\/vkCmdDrawIndirectCountAMD.txt[]
--
-- = See Also
--
-- No cross-references are available
vkCmdDrawIndirectCountAMD :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
vkCmdDrawIndirectCountAMD = vkCmdDrawIndirectCountKHR

-- No documentation found for TopLevel "VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME"
pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_AMD_draw_indirect_count"

-- No documentation found for TopLevel "VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION"
pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION :: Integral a => a
pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1
