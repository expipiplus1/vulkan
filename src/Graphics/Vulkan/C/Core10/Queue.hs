{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  , VkFence
  , VkPipelineStageFlagBits(..)
  , pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT
  , pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT
  , pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
  , pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
  , pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
  , pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_TRANSFER_BIT
  , pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
  , pattern VK_PIPELINE_STAGE_HOST_BIT
  , pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT
  , pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT
  , VkPipelineStageFlags
  , VkQueue
  , VkSemaphore
  , VkSubmitInfo(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDeviceWaitIdle
#endif
  , FN_vkDeviceWaitIdle
  , PFN_vkDeviceWaitIdle
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkGetDeviceQueue
#endif
  , FN_vkGetDeviceQueue
  , PFN_vkGetDeviceQueue
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkQueueSubmit
#endif
  , FN_vkQueueSubmit
  , PFN_vkQueueSubmit
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkQueueWaitIdle
#endif
  , FN_vkQueueWaitIdle
  , PFN_vkQueueWaitIdle
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkCommandBuffer_T
-- | VkCommandBuffer - Opaque handle to a command buffer object
--
-- = See Also
--
-- 'VkSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkAllocateCommandBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkBeginCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindPipeline',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearAttachments',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearColorImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatch',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.vkCmdDispatchBase',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDraw',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexed',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndQuery',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndRenderPass',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdExecuteCommands',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdFillBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdNextSubpass',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPushConstants',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetQueryPool',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResolveImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetBlendConstants',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetDepthBias',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetDepthBounds',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.vkCmdSetDeviceMask',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetLineWidth',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetScissor',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilCompareMask',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilReference',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilWriteMask',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetViewport',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdUpdateBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkEndCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkFreeCommandBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkResetCommandBuffer'
type VkCommandBuffer = Ptr VkCommandBuffer_T
-- | Dummy data to tag the 'Ptr' with
data VkFence_T
-- | VkFence - Opaque handle to a fence object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Fence.vkCreateFence',
-- 'Graphics.Vulkan.C.Core10.Fence.vkDestroyFence',
-- 'Graphics.Vulkan.C.Core10.Fence.vkGetFenceStatus',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkQueueBindSparse',
-- 'vkQueueSubmit', 'Graphics.Vulkan.C.Core10.Fence.vkResetFences',
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences'
type VkFence = Ptr VkFence_T
-- ** VkPipelineStageFlagBits

-- | VkPipelineStageFlagBits - Bitmask specifying pipeline stages
--
-- = Description
--
-- -   @VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT@ specifies the stage of the
--     pipeline where any commands are initially received by the queue.
--
-- -   @VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT@ specifies the stage of the
--     pipeline where Draw\/DispatchIndirect data structures are consumed.
--     This stage also includes reading commands written by
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkCmdProcessCommandsNVX'.
--
-- -   @VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV@ specifies the task shader
--     stage.
--
-- -   @VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV@ specifies the mesh shader
--     stage.
--
-- -   @VK_PIPELINE_STAGE_VERTEX_INPUT_BIT@ specifies the stage of the
--     pipeline where vertex and index buffers are consumed.
--
-- -   @VK_PIPELINE_STAGE_VERTEX_SHADER_BIT@ specifies the vertex shader
--     stage.
--
-- -   @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ specifies the
--     tessellation control shader stage.
--
-- -   @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@ specifies the
--     tessellation evaluation shader stage.
--
-- -   @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@ specifies the geometry
--     shader stage.
--
-- -   @VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT@ specifies the fragment
--     shader stage.
--
-- -   @VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT@ specifies the stage of
--     the pipeline where early fragment tests (depth and stencil tests
--     before fragment shading) are performed. This stage also includes
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>
--     for framebuffer attachments with a depth\/stencil format.
--
-- -   @VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT@ specifies the stage of
--     the pipeline where late fragment tests (depth and stencil tests
--     after fragment shading) are performed. This stage also includes
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-load-store-ops subpass store operations>
--     for framebuffer attachments with a depth\/stencil format.
--
-- -   @VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT@ specifies the stage
--     of the pipeline after blending where the final color values are
--     output from the pipeline. This stage also includes
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>
--     and multisample resolve operations for framebuffer attachments with
--     a color or depth\/stencil format.
--
-- -   @VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT@ specifies the execution of a
--     compute shader.
--
-- -   @VK_PIPELINE_STAGE_TRANSFER_BIT@ specifies the execution of copy
--     commands. This includes the operations resulting from all
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#copies copy commands>,
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#clears clear commands>
--     (with the exception of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearAttachments'),
--     and
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults'.
--
-- -   @VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT@ specifies the final stage in
--     the pipeline where operations generated by all commands complete
--     execution.
--
-- -   @VK_PIPELINE_STAGE_HOST_BIT@ specifies a pseudo-stage indicating
--     execution on the host of reads\/writes of device memory. This stage
--     is not invoked by any commands recorded in a command buffer.
--
-- -   @VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV@ specifies the
--     execution of the ray tracing shader stages.
--
-- -   @VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV@ specifies
--     the execution of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdBuildAccelerationStructureNV',
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdCopyAccelerationStructureNV',
--     and
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdWriteAccelerationStructuresPropertiesNV'.
--
-- -   @VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT@ specifies the execution of all
--     graphics pipeline stages, and is equivalent to the logical OR of:
--
--     -   @VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT@
--
--     -   @VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT@
--
--     -   @VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV@
--
--     -   @VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV@
--
--     -   @VK_PIPELINE_STAGE_VERTEX_INPUT_BIT@
--
--     -   @VK_PIPELINE_STAGE_VERTEX_SHADER_BIT@
--
--     -   @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@
--
--     -   @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
--     -   @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
--     -   @VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT@
--
--     -   @VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT@
--
--     -   @VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT@
--
--     -   @VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT@
--
--     -   @VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT@
--
--     -   @VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT@
--
--     -   @VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT@
--
--     -   @VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV@
--
--     -   @VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT@
--
-- -   @VK_PIPELINE_STAGE_ALL_COMMANDS_BIT@ is equivalent to the logical OR
--     of every other pipeline stage flag that is supported on the queue it
--     is used with.
--
-- -   @VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT@ specifies the
--     stage of the pipeline where the predicate of conditional rendering
--     is consumed.
--
-- -   @VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT@ specifies the stage
--     of the pipeline where vertex attribute output values are written to
--     the transform feedback buffers.
--
-- -   @VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX@ specifies the stage of
--     the pipeline where device-side generation of commands via
--     'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkCmdProcessCommandsNVX'
--     is handled.
--
-- -   @VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV@ specifies the stage of
--     the pipeline where the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#primsrast-shading-rate-image shading rate image>
--     is read to determine the shading rate for portions of a rasterized
--     primitive.
--
-- -   @VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT@ specifies the
--     stage of the pipeline where the fragment density map is read to
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fragmentdensitymapops generate the fragment areas>.
--
-- __Note__
--
-- An execution dependency with only @VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT@
-- in the destination stage mask will only prevent that stage from
-- executing in subsequently submitted commands. As this stage does not
-- perform any actual execution, this is not observable - in effect, it
-- does not delay processing of subsequent commands. Similarly an execution
-- dependency with only @VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT@ in the source
-- stage mask will effectively not wait for any prior commands to complete.
--
-- When defining a memory dependency, using only
-- @VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT@ or
-- @VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT@ would never make any accesses
-- available and\/or visible because these stages do not access memory.
--
-- @VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT@ and
-- @VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT@ are useful for accomplishing layout
-- transitions and queue ownership operations when the required execution
-- dependency is satisfied by other means - for example, semaphore
-- operations between queues.
--
-- = See Also
--
-- 'VkPipelineStageFlags',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp'
newtype VkPipelineStageFlagBits = VkPipelineStageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkPipelineStageFlagBits where
  showsPrec _ VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT = showString "VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT"
  showsPrec _ VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT = showString "VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT"
  showsPrec _ VK_PIPELINE_STAGE_VERTEX_INPUT_BIT = showString "VK_PIPELINE_STAGE_VERTEX_INPUT_BIT"
  showsPrec _ VK_PIPELINE_STAGE_VERTEX_SHADER_BIT = showString "VK_PIPELINE_STAGE_VERTEX_SHADER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT = showString "VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = showString "VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT = showString "VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT = showString "VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = showString "VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT"
  showsPrec _ VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = showString "VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT"
  showsPrec _ VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = showString "VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT"
  showsPrec _ VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT = showString "VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_TRANSFER_BIT = showString "VK_PIPELINE_STAGE_TRANSFER_BIT"
  showsPrec _ VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = showString "VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT"
  showsPrec _ VK_PIPELINE_STAGE_HOST_BIT = showString "VK_PIPELINE_STAGE_HOST_BIT"
  showsPrec _ VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT = showString "VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT"
  showsPrec _ VK_PIPELINE_STAGE_ALL_COMMANDS_BIT = showString "VK_PIPELINE_STAGE_ALL_COMMANDS_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkPipelineStageFlagBits 0x08000000) = showString "VK_PIPELINE_STAGE_RESERVED_27_BIT_KHR"
  showsPrec _ (VkPipelineStageFlagBits 0x04000000) = showString "VK_PIPELINE_STAGE_RESERVED_26_BIT_KHR"
  showsPrec _ (VkPipelineStageFlagBits 0x01000000) = showString "VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT"
  showsPrec _ (VkPipelineStageFlagBits 0x00040000) = showString "VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT"
  showsPrec _ (VkPipelineStageFlagBits 0x00020000) = showString "VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX"
  showsPrec _ (VkPipelineStageFlagBits 0x00400000) = showString "VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV"
  showsPrec _ (VkPipelineStageFlagBits 0x00200000) = showString "VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV"
  showsPrec _ (VkPipelineStageFlagBits 0x02000000) = showString "VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV"
  showsPrec _ (VkPipelineStageFlagBits 0x00080000) = showString "VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV"
  showsPrec _ (VkPipelineStageFlagBits 0x00100000) = showString "VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV"
  showsPrec _ (VkPipelineStageFlagBits 0x00800000) = showString "VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT"
  showsPrec p (VkPipelineStageFlagBits x) = showParen (p >= 11) (showString "VkPipelineStageFlagBits " . showsPrec 11 x)

instance Read VkPipelineStageFlagBits where
  readPrec = parens ( choose [ ("VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT",                    pure VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT)
                             , ("VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT",                  pure VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT)
                             , ("VK_PIPELINE_STAGE_VERTEX_INPUT_BIT",                   pure VK_PIPELINE_STAGE_VERTEX_INPUT_BIT)
                             , ("VK_PIPELINE_STAGE_VERTEX_SHADER_BIT",                  pure VK_PIPELINE_STAGE_VERTEX_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT",    pure VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT", pure VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT",                pure VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT",                pure VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT",           pure VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT)
                             , ("VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT",            pure VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT)
                             , ("VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT",        pure VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
                             , ("VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT",                 pure VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT)
                             , ("VK_PIPELINE_STAGE_TRANSFER_BIT",                       pure VK_PIPELINE_STAGE_TRANSFER_BIT)
                             , ("VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT",                 pure VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT)
                             , ("VK_PIPELINE_STAGE_HOST_BIT",                           pure VK_PIPELINE_STAGE_HOST_BIT)
                             , ("VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT",                   pure VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT)
                             , ("VK_PIPELINE_STAGE_ALL_COMMANDS_BIT",                   pure VK_PIPELINE_STAGE_ALL_COMMANDS_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_PIPELINE_STAGE_RESERVED_27_BIT_KHR",                 pure (VkPipelineStageFlagBits 0x08000000))
                             , ("VK_PIPELINE_STAGE_RESERVED_26_BIT_KHR",                 pure (VkPipelineStageFlagBits 0x04000000))
                             , ("VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT",          pure (VkPipelineStageFlagBits 0x01000000))
                             , ("VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT",       pure (VkPipelineStageFlagBits 0x00040000))
                             , ("VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX",             pure (VkPipelineStageFlagBits 0x00020000))
                             , ("VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV",           pure (VkPipelineStageFlagBits 0x00400000))
                             , ("VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV",           pure (VkPipelineStageFlagBits 0x00200000))
                             , ("VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV", pure (VkPipelineStageFlagBits 0x02000000))
                             , ("VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV",                  pure (VkPipelineStageFlagBits 0x00080000))
                             , ("VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV",                  pure (VkPipelineStageFlagBits 0x00100000))
                             , ("VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT",    pure (VkPipelineStageFlagBits 0x00800000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineStageFlagBits")
                        v <- step readPrec
                        pure (VkPipelineStageFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT"
pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT = VkPipelineStageFlagBits 0x00000001

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT"
pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT = VkPipelineStageFlagBits 0x00000002

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_VERTEX_INPUT_BIT"
pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT = VkPipelineStageFlagBits 0x00000004

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_VERTEX_SHADER_BIT"
pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT = VkPipelineStageFlagBits 0x00000008

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT"
pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT = VkPipelineStageFlagBits 0x00000010

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT"
pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = VkPipelineStageFlagBits 0x00000020

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT"
pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT = VkPipelineStageFlagBits 0x00000040

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT"
pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT = VkPipelineStageFlagBits 0x00000080

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT"
pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = VkPipelineStageFlagBits 0x00000100

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT"
pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = VkPipelineStageFlagBits 0x00000200

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT"
pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = VkPipelineStageFlagBits 0x00000400

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT"
pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT = VkPipelineStageFlagBits 0x00000800

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TRANSFER_BIT"
pattern VK_PIPELINE_STAGE_TRANSFER_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_TRANSFER_BIT = VkPipelineStageFlagBits 0x00001000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT"
pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = VkPipelineStageFlagBits 0x00002000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_HOST_BIT"
pattern VK_PIPELINE_STAGE_HOST_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_HOST_BIT = VkPipelineStageFlagBits 0x00004000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT"
pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT = VkPipelineStageFlagBits 0x00008000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_ALL_COMMANDS_BIT"
pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT = VkPipelineStageFlagBits 0x00010000
-- | VkPipelineStageFlags - Bitmask of VkPipelineStageFlagBits
--
-- = Description
--
-- @VkPipelineStageFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkPipelineStageFlagBits'.
--
-- = See Also
--
-- 'VkPipelineStageFlagBits', 'VkSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
type VkPipelineStageFlags = VkPipelineStageFlagBits
-- | Dummy data to tag the 'Ptr' with
data VkQueue_T
-- | VkQueue - Opaque handle to a queue object
--
-- = See Also
--
-- 'vkGetDeviceQueue',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.vkGetDeviceQueue2',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkQueueBindSparse',
-- 'vkQueueSubmit', 'vkQueueWaitIdle'
type VkQueue = Ptr VkQueue_T
-- | Dummy data to tag the 'Ptr' with
data VkSemaphore_T
-- | VkSemaphore - Opaque handle to a semaphore object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo',
-- 'VkSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkCreateSemaphore',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkDestroySemaphore'
type VkSemaphore = Ptr VkSemaphore_T
-- | VkSubmitInfo - Structure specifying a queue submit operation
--
-- = Description
--
-- The order that command buffers appear in @pCommandBuffers@ is used to
-- determine
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order submission order>,
-- and thus all the
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-implicit implicit ordering guarantees>
-- that respect it. Other than these implicit ordering guarantees and any
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization explicit synchronization primitives>,
-- these command buffers /may/ overlap or otherwise execute out of order.
--
-- == Valid Usage
--
-- -   Each element of @pCommandBuffers@ /must/ not have been allocated
--     with @VK_COMMAND_BUFFER_LEVEL_SECONDARY@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   Each element of @pWaitDstStageMask@ /must/ not include
--     @VK_PIPELINE_STAGE_HOST_BIT@.
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain @VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain @VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_SUBMIT_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupSubmitInfo'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkProtectedSubmitInfo'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   If @waitSemaphoreCount@ is not @0@, @pWaitSemaphores@ /must/ be a
--     valid pointer to an array of @waitSemaphoreCount@ valid
--     @VkSemaphore@ handles
--
-- -   If @waitSemaphoreCount@ is not @0@, @pWaitDstStageMask@ /must/ be a
--     valid pointer to an array of @waitSemaphoreCount@ valid combinations
--     of 'VkPipelineStageFlagBits' values
--
-- -   Each element of @pWaitDstStageMask@ /must/ not be @0@
--
-- -   If @commandBufferCount@ is not @0@, @pCommandBuffers@ /must/ be a
--     valid pointer to an array of @commandBufferCount@ valid
--     @VkCommandBuffer@ handles
--
-- -   If @signalSemaphoreCount@ is not @0@, @pSignalSemaphores@ /must/ be
--     a valid pointer to an array of @signalSemaphoreCount@ valid
--     @VkSemaphore@ handles
--
-- -   Each of the elements of @pCommandBuffers@, the elements of
--     @pSignalSemaphores@, and the elements of @pWaitSemaphores@ that are
--     valid handles /must/ have been created, allocated, or retrieved from
--     the same @VkDevice@
--
-- = See Also
--
-- 'VkCommandBuffer', 'VkPipelineStageFlags', 'VkSemaphore',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'vkQueueSubmit'
data VkSubmitInfo = VkSubmitInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @waitSemaphoreCount@ is the number of semaphores upon which to wait
  -- before executing the command buffers for the batch.
  vkWaitSemaphoreCount :: Word32
  , -- | @pWaitSemaphores@ is a pointer to an array of semaphores upon which to
  -- wait before the command buffers for this batch begin execution. If
  -- semaphores to wait on are provided, they define a
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-semaphores-waiting semaphore wait operation>.
  vkPWaitSemaphores :: Ptr VkSemaphore
  , -- | @pWaitDstStageMask@ is a pointer to an array of pipeline stages at which
  -- each corresponding semaphore wait will occur.
  vkPWaitDstStageMask :: Ptr VkPipelineStageFlags
  , -- | @commandBufferCount@ is the number of command buffers to execute in the
  -- batch.
  vkCommandBufferCount :: Word32
  , -- | @pCommandBuffers@ is a pointer to an array of command buffers to execute
  -- in the batch.
  vkPCommandBuffers :: Ptr VkCommandBuffer
  , -- | @signalSemaphoreCount@ is the number of semaphores to be signaled once
  -- the commands specified in @pCommandBuffers@ have completed execution.
  vkSignalSemaphoreCount :: Word32
  , -- | @pSignalSemaphores@ is a pointer to an array of semaphores which will be
  -- signaled when the command buffers for this batch have completed
  -- execution. If semaphores to be signaled are provided, they define a
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>.
  vkPSignalSemaphores :: Ptr VkSemaphore
  }
  deriving (Eq, Show)

instance Storable VkSubmitInfo where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkSubmitInfo <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
                          <*> peek (ptr `plusPtr` 16)
                          <*> peek (ptr `plusPtr` 24)
                          <*> peek (ptr `plusPtr` 32)
                          <*> peek (ptr `plusPtr` 40)
                          <*> peek (ptr `plusPtr` 48)
                          <*> peek (ptr `plusPtr` 56)
                          <*> peek (ptr `plusPtr` 64)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphores (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 32) (vkPWaitDstStageMask (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 40) (vkCommandBufferCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 48) (vkPCommandBuffers (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 56) (vkSignalSemaphoreCount (poked :: VkSubmitInfo))
                *> poke (ptr `plusPtr` 64) (vkPSignalSemaphores (poked :: VkSubmitInfo))

instance Zero VkSubmitInfo where
  zero = VkSubmitInfo zero
                      zero
                      zero
                      zero
                      zero
                      zero
                      zero
                      zero
                      zero
#if defined(EXPOSE_CORE10_COMMANDS)
-- | vkDeviceWaitIdle - Wait for a device to become idle
--
-- = Parameters
--
-- -   @device@ is the logical device to idle.
--
-- = Description
--
-- @vkDeviceWaitIdle@ is equivalent to calling @vkQueueWaitIdle@ for all
-- queues owned by @device@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- == Host Synchronization
--
-- -   Host access to all @VkQueue@ objects created from @device@ /must/ be
--     externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_DEVICE_LOST@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDeviceWaitIdle" vkDeviceWaitIdle :: ("device" ::: VkDevice) -> IO VkResult

#endif
type FN_vkDeviceWaitIdle = ("device" ::: VkDevice) -> IO VkResult
type PFN_vkDeviceWaitIdle = FunPtr FN_vkDeviceWaitIdle
#if defined(EXPOSE_CORE10_COMMANDS)
-- | vkGetDeviceQueue - Get a queue handle from a device
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the queue.
--
-- -   @queueFamilyIndex@ is the index of the queue family to which the
--     queue belongs.
--
-- -   @queueIndex@ is the index within this queue family of the queue to
--     retrieve.
--
-- -   @pQueue@ is a pointer to a 'VkQueue' object that will be filled with
--     the handle for the requested queue.
--
-- == Valid Usage
--
-- -   @queueFamilyIndex@ /must/ be one of the queue family indices
--     specified when @device@ was created, via the
--     @VkDeviceQueueCreateInfo@ structure
--
-- -   @queueIndex@ /must/ be less than the number of queues created for
--     the specified queue family index when @device@ was created, via the
--     @queueCount@ member of the @VkDeviceQueueCreateInfo@ structure
--
-- -   'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateInfo'::@flags@
--     /must/ have been set to zero when @device@ was created
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pQueue@ /must/ be a valid pointer to a @VkQueue@ handle
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice', 'VkQueue'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceQueue" vkGetDeviceQueue :: ("device" ::: VkDevice) -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr VkQueue) -> IO ()

#endif
type FN_vkGetDeviceQueue = ("device" ::: VkDevice) -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr VkQueue) -> IO ()
type PFN_vkGetDeviceQueue = FunPtr FN_vkGetDeviceQueue
#if defined(EXPOSE_CORE10_COMMANDS)
-- | vkQueueSubmit - Submits a sequence of semaphores or command buffers to a
-- queue
--
-- = Parameters
--
-- -   @queue@ is the queue that the command buffers will be submitted to.
--
-- -   @submitCount@ is the number of elements in the @pSubmits@ array.
--
-- -   @pSubmits@ is a pointer to an array of 'VkSubmitInfo' structures,
--     each specifying a command buffer submission batch.
--
-- -   @fence@ is an /optional/ handle to a fence to be signaled once all
--     submitted command buffers have completed execution. If @fence@ is
--     not 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', it defines
--     a
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-fences-signaling fence signal operation>.
--
-- = Description
--
-- __Note__
--
-- Submission can be a high overhead operation, and applications /should/
-- attempt to batch work together into as few calls to @vkQueueSubmit@ as
-- possible.
--
-- @vkQueueSubmit@ is a
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#devsandqueues-submission queue submission command>,
-- with each batch defined by an element of @pSubmits@ as an instance of
-- the 'VkSubmitInfo' structure. Batches begin execution in the order they
-- appear in @pSubmits@, but /may/ complete out of order.
--
-- Fence and semaphore operations submitted with 'vkQueueSubmit' have
-- additional ordering constraints compared to other submission commands,
-- with dependencies involving previous and subsequent queue operations.
-- Information about these additional constraints can be found in the
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-semaphores semaphore>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-fences fence>
-- sections of
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization the synchronization chapter>.
--
-- Details on the interaction of @pWaitDstStageMask@ with synchronization
-- are described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-semaphores-waiting semaphore wait operation>
-- section of
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization the synchronization chapter>.
--
-- The order that batches appear in @pSubmits@ is used to determine
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order submission order>,
-- and thus all the
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-implicit implicit ordering guarantees>
-- that respect it. Other than these implicit ordering guarantees and any
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization explicit synchronization primitives>,
-- these batches /may/ overlap or otherwise execute out of order.
--
-- If any command buffer submitted to this queue is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>,
-- it is moved to the
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
-- Once execution of all submissions of a command buffer complete, it moves
-- from the
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>,
-- back to the
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>.
-- If a command buffer was recorded with the
-- @VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT@ flag, it instead moves
-- back to the
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle invalid state>.
--
-- If @vkQueueSubmit@ fails, it /may/ return @VK_ERROR_OUT_OF_HOST_MEMORY@
-- or @VK_ERROR_OUT_OF_DEVICE_MEMORY@. If it does, the implementation
-- /must/ ensure that the state and contents of any resources or
-- synchronization primitives referenced by the submitted command buffers
-- and any semaphores referenced by @pSubmits@ is unaffected by the call or
-- its failure. If @vkQueueSubmit@ fails in such a way that the
-- implementation is unable to make that guarantee, the implementation
-- /must/ return @VK_ERROR_DEVICE_LOST@. See
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>.
--
-- == Valid Usage
--
-- -   If @fence@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @fence@ /must/
--     be unsignaled
--
-- -   If @fence@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @fence@ /must/
--     not be associated with any other queue command that has not yet
--     completed execution on that queue
--
-- -   Any calls to
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent',
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent' or
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
--     that have been recorded into any of the command buffer elements of
--     the @pCommandBuffers@ member of any element of @pSubmits@, /must/
--     not reference any 'Graphics.Vulkan.C.Core10.Event.VkEvent' that is
--     referenced by any of those commands in a command buffer that has
--     been submitted to another queue and is still in the /pending state/.
--
-- -   Any stage flag included in any element of the @pWaitDstStageMask@
--     member of any element of @pSubmits@ /must/ be a pipeline stage
--     supported by one of the capabilities of @queue@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>.
--
-- -   Each element of the @pSignalSemaphores@ member of any element of
--     @pSubmits@ /must/ be unsignaled when the semaphore signal operation
--     it defines is executed on the device
--
-- -   When a semaphore unsignal operation defined by any element of the
--     @pWaitSemaphores@ member of any element of @pSubmits@ executes on
--     @queue@, no other queue /must/ be waiting on the same semaphore.
--
-- -   All elements of the @pWaitSemaphores@ member of all elements of
--     @pSubmits@ /must/ be semaphores that are signaled, or have
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operations>
--     previously submitted for execution.
--
-- -   Each element of the @pCommandBuffers@ member of each element of
--     @pSubmits@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>.
--
-- -   If any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ was not recorded with the
--     @VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT@, it /must/ not be in
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
--
-- -   Any
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-secondary secondary command buffers recorded>
--     into any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>.
--
-- -   If any
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-secondary secondary command buffers recorded>
--     into any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ was not recorded with the
--     @VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT@, it /must/ not be in
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
--
-- -   Each element of the @pCommandBuffers@ member of each element of
--     @pSubmits@ /must/ have been allocated from a @VkCommandPool@ that
--     was created for the same queue family @queue@ belongs to.
--
-- -   If any element of @pSubmits@->@pCommandBuffers@ includes a
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers-acquire Queue Family Transfer Acquire Operation>,
--     there /must/ exist a previously submitted
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers-release Queue Family Transfer Release Operation>
--     on a queue in the queue family identified by the acquire operation,
--     with parameters matching the acquire operation as defined in the
--     definition of such
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers-acquire acquire operations>,
--     and which happens before the acquire operation.
--
-- == Valid Usage (Implicit)
--
-- -   @queue@ /must/ be a valid @VkQueue@ handle
--
-- -   If @submitCount@ is not @0@, @pSubmits@ /must/ be a valid pointer to
--     an array of @submitCount@ valid @VkSubmitInfo@ structures
--
-- -   If @fence@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @fence@ /must/
--     be a valid @VkFence@ handle
--
-- -   Both of @fence@, and @queue@ that are valid handles /must/ have been
--     created, allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @queue@ /must/ be externally synchronized
--
-- -   Host access to @pSubmits@[].pWaitSemaphores[] /must/ be externally
--     synchronized
--
-- -   Host access to @pSubmits@[].pSignalSemaphores[] /must/ be externally
--     synchronized
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | -               | -               | Any             | -               |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_DEVICE_LOST@
--
-- = See Also
--
-- 'VkFence', 'VkQueue', 'VkSubmitInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkQueueSubmit" vkQueueSubmit :: ("queue" ::: VkQueue) -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr VkSubmitInfo) -> ("fence" ::: VkFence) -> IO VkResult

#endif
type FN_vkQueueSubmit = ("queue" ::: VkQueue) -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr VkSubmitInfo) -> ("fence" ::: VkFence) -> IO VkResult
type PFN_vkQueueSubmit = FunPtr FN_vkQueueSubmit
#if defined(EXPOSE_CORE10_COMMANDS)
-- | vkQueueWaitIdle - Wait for a queue to become idle
--
-- = Parameters
--
-- -   @queue@ is the queue on which to wait.
--
-- = Description
--
-- @vkQueueWaitIdle@ is equivalent to submitting a fence to a queue and
-- waiting with an infinite timeout for that fence to signal.
--
-- == Valid Usage (Implicit)
--
-- -   @queue@ /must/ be a valid @VkQueue@ handle
--
-- == Host Synchronization
--
-- -   Host access to @queue@ /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | -               | -               | Any             | -               |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_DEVICE_LOST@
--
-- = See Also
--
-- 'VkQueue'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkQueueWaitIdle" vkQueueWaitIdle :: ("queue" ::: VkQueue) -> IO VkResult

#endif
type FN_vkQueueWaitIdle = ("queue" ::: VkQueue) -> IO VkResult
type PFN_vkQueueWaitIdle = FunPtr FN_vkQueueWaitIdle
