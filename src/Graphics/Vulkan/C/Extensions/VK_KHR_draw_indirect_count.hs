{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count
  ( 
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  vkCmdDrawIndexedIndirectCountKHR
  , 
#endif
  FN_vkCmdDrawIndexedIndirectCountKHR
  , PFN_vkCmdDrawIndexedIndirectCountKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdDrawIndirectCountKHR
#endif
  , FN_vkCmdDrawIndirectCountKHR
  , PFN_vkCmdDrawIndirectCountKHR
  , pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
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
import Graphics.Vulkan.NamedType
  ( (:::)
  )


#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCmdDrawIndexedIndirectCountKHR - Perform an indexed indirect draw with
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
-- @vkCmdDrawIndexedIndirectCountKHR@ behaves similarly to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect'
-- except that the draw count is read by the device from a buffer during
-- execution. The command will read an unsigned 32-bit integer from
-- @countBuffer@ located at @countBufferOffset@ and use this as the draw
-- count.
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @buffer@ /must/ have been created with the
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set
--
-- -   If @countBuffer@ is non-sparse then it /must/ be bound completely
--     and contiguously to a single @VkDeviceMemory@ object
--
-- -   @countBuffer@ /must/ have been created with the
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   @countBufferOffset@ /must/ be a multiple of @4@
--
-- -   @stride@ /must/ be a multiple of @4@ and /must/ be greater than or
--     equal to sizeof(@VkDrawIndexedIndirectCommand@)
--
-- -   If @maxDrawCount@ is greater than or equal to @1@, (@stride@ ×
--     (@maxDrawCount@ - 1) + @offset@ +
--     sizeof(@VkDrawIndexedIndirectCommand@)) /must/ be less than or equal
--     to the size of @buffer@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, all the @firstInstance@ members of the
--     @VkDrawIndexedIndirectCommand@ structures accessed by this command
--     /must/ be @0@
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the @VkGraphicsPipelineCreateInfo@
--     structure specified when creating the @VkPipeline@ bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   For each set /n/ that is statically used by the @VkPipeline@ bound
--     to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a descriptor set /must/ have
--     been bound to /n/ at @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for set /n/, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push constant value
--     /must/ have been set for @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for push constants, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   Descriptors in each bound descriptor set, specified via
--     @vkCmdBindDescriptorSets@, /must/ be valid if they are statically
--     used by the bound @VkPipeline@ object, specified via
--     @vkCmdBindPipeline@
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   A valid graphics pipeline /must/ be bound to the current command
--     buffer with @VK_PIPELINE_BIND_POINT_GRAPHICS@
--
-- -   If the @VkPipeline@ object bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@ requires any dynamic state, that
--     state /must/ have been set on the current command buffer
--
-- -   If count stored in @countBuffer@ is equal to @1@, (@offset@ +
--     sizeof(@VkDrawIndexedIndirectCommand@)) /must/ be less than or equal
--     to the size of @buffer@
--
-- -   If count stored in @countBuffer@ is greater than @1@, (@stride@ ×
--     (@drawCount@ - 1) + @offset@ +
--     sizeof(@VkDrawIndexedIndirectCommand@)) /must/ be less than or equal
--     to the size of @buffer@
--
-- -   @drawCount@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxDrawIndirectCount@
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used to sample from any
--     @VkImage@ with a @VkImageView@ of the type @VK_IMAGE_VIEW_TYPE_3D@,
--     @VK_IMAGE_VIEW_TYPE_CUBE@, @VK_IMAGE_VIEW_TYPE_1D_ARRAY@,
--     @VK_IMAGE_VIEW_TYPE_2D_ARRAY@ or @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@, in
--     any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a storage
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If a @VkImageView@ is sampled with @VK_FILTER_LINEAR@ as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@.
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If a @VkImageView@ is sampled with @VK_FILTER_CUBIC_EXT@ as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT@
--
-- -   Any 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_EXT@ as a result of this command /must/ have a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageViewType' and format that
--     supports cubic filtering, as specified by
--     @VkFilterCubicImageViewImageFormatPropertiesEXT@::@filterCubic@
--     returned by @vkGetPhysicalDeviceImageFormatProperties2@
--
-- -   Any 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_EXT@ with a reduction mode of either
--     @VK_SAMPLER_REDUCTION_MODE_MIN_EXT@ or
--     @VK_SAMPLER_REDUCTION_MODE_MAX_EXT@ as a result of this command
--     /must/ have a 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewType'
--     and format that supports cubic filtering together with minmax
--     filtering, as specified by
--     @VkFilterCubicImageViewImageFormatPropertiesEXT@::@filterCubicMinmax@
--     returned by @vkGetPhysicalDeviceImageFormatProperties2@
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to @VK_TRUE@ and the current subpass has a depth\/stencil
--     attachment, then that attachment /must/ have been created with the
--     @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ bit set
--
-- -   Any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' created with
--     a 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@
--     containing @VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV@ sampled as a
--     result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSamplerAddressMode' of
--     @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE@.
--
-- Unresolved directive in vkCmdDrawIndexedIndirectCountKHR.txt -
-- include::..\/validity\/protos\/vkCmdDrawIndexedIndirectCountKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndexedIndirectCountKHR" vkCmdDrawIndexedIndirectCountKHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()

#endif
type FN_vkCmdDrawIndexedIndirectCountKHR = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndexedIndirectCountKHR = FunPtr FN_vkCmdDrawIndexedIndirectCountKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkCmdDrawIndirectCountKHR - Perform an indirect draw with the draw count
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
-- @vkCmdDrawIndirectCountKHR@ behaves similarly to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect'
-- except that the draw count is read by the device from a buffer during
-- execution. The command will read an unsigned 32-bit integer from
-- @countBuffer@ located at @countBufferOffset@ and use this as the draw
-- count.
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @buffer@ /must/ have been created with the
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set
--
-- -   If @countBuffer@ is non-sparse then it /must/ be bound completely
--     and contiguously to a single @VkDeviceMemory@ object
--
-- -   @countBuffer@ /must/ have been created with the
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   @countBufferOffset@ /must/ be a multiple of @4@
--
-- -   @stride@ /must/ be a multiple of @4@ and /must/ be greater than or
--     equal to sizeof(@VkDrawIndirectCommand@)
--
-- -   If @maxDrawCount@ is greater than or equal to @1@, (@stride@ ×
--     (@maxDrawCount@ - 1) + @offset@ + sizeof(@VkDrawIndirectCommand@))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, all the @firstInstance@ members of the
--     @VkDrawIndirectCommand@ structures accessed by this command /must/
--     be @0@
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the @VkGraphicsPipelineCreateInfo@
--     structure specified when creating the @VkPipeline@ bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   For each set /n/ that is statically used by the @VkPipeline@ bound
--     to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a descriptor set /must/ have
--     been bound to /n/ at @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for set /n/, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push constant value
--     /must/ have been set for @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for push constants, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   Descriptors in each bound descriptor set, specified via
--     @vkCmdBindDescriptorSets@, /must/ be valid if they are statically
--     used by the bound @VkPipeline@ object, specified via
--     @vkCmdBindPipeline@
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   A valid graphics pipeline /must/ be bound to the current command
--     buffer with @VK_PIPELINE_BIND_POINT_GRAPHICS@
--
-- -   If the @VkPipeline@ object bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@ requires any dynamic state, that
--     state /must/ have been set on the current command buffer
--
-- -   If the count stored in @countBuffer@ is equal to @1@, (@offset@ +
--     sizeof(@VkDrawIndirectCommand@)) /must/ be less than or equal to the
--     size of @buffer@
--
-- -   If the count stored in @countBuffer@ is greater than @1@, (@stride@
--     × (@drawCount@ - 1) + @offset@ + sizeof(@VkDrawIndirectCommand@))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   The count stored in @countBuffer@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxDrawIndirectCount@
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used to sample from any
--     @VkImage@ with a @VkImageView@ of the type @VK_IMAGE_VIEW_TYPE_3D@,
--     @VK_IMAGE_VIEW_TYPE_CUBE@, @VK_IMAGE_VIEW_TYPE_1D_ARRAY@,
--     @VK_IMAGE_VIEW_TYPE_2D_ARRAY@ or @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@, in
--     any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a storage
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If a @VkImageView@ is sampled with @VK_FILTER_LINEAR@ as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@.
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If a @VkImageView@ is sampled with @VK_FILTER_CUBIC_EXT@ as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT@
--
-- -   Any 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_EXT@ as a result of this command /must/ have a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageViewType' and format that
--     supports cubic filtering, as specified by
--     @VkFilterCubicImageViewImageFormatPropertiesEXT@::@filterCubic@
--     returned by @vkGetPhysicalDeviceImageFormatProperties2@
--
-- -   Any 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_EXT@ with a reduction mode of either
--     @VK_SAMPLER_REDUCTION_MODE_MIN_EXT@ or
--     @VK_SAMPLER_REDUCTION_MODE_MAX_EXT@ as a result of this command
--     /must/ have a 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewType'
--     and format that supports cubic filtering together with minmax
--     filtering, as specified by
--     @VkFilterCubicImageViewImageFormatPropertiesEXT@::@filterCubicMinmax@
--     returned by @vkGetPhysicalDeviceImageFormatProperties2@
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to @VK_TRUE@ and the current subpass has a depth\/stencil
--     attachment, then that attachment /must/ have been created with the
--     @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ bit set
--
-- -   Any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' created with
--     a 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@
--     containing @VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV@ sampled as a
--     result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSamplerAddressMode' of
--     @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE@.
--
-- Unresolved directive in vkCmdDrawIndirectCountKHR.txt -
-- include::..\/validity\/protos\/vkCmdDrawIndirectCountKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndirectCountKHR" vkCmdDrawIndirectCountKHR :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()

#endif
type FN_vkCmdDrawIndirectCountKHR = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndirectCountKHR = FunPtr FN_vkCmdDrawIndirectCountKHR
-- No documentation found for TopLevel "VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME"
pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_KHR_draw_indirect_count"
-- No documentation found for TopLevel "VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION"
pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1
