{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Queue
  ( VkPipelineStageFlagBits(..)
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
  , VkQueue
  , VkCommandBuffer
  , VkFence
  , VkSemaphore
  , vkGetDeviceQueue
  , vkQueueSubmit
  , vkQueueWaitIdle
  , vkDeviceWaitIdle
  , VkSubmitInfo(..)
  , VkPipelineStageFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
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
import Graphics.Vulkan.NamedType
  ( (:::)
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


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  )


-- ** VkPipelineStageFlagBits

-- | VkPipelineStageFlagBits - Bitmask specifying pipeline stages
--
-- = Description
--
-- -   @VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT@ specifies the stage of the
--     pipeline where any commands are initially received by the queue.
--
-- -   @VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX@ specifies the stage of
--     the pipeline where device-side generation of commands via
--     'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkCmdProcessCommandsNVX'
--     is handled.
--
-- -   @VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT@ specifies the stage of the
--     pipeline where Draw\/DispatchIndirect data structures are consumed.
--     This stage also includes reading commands written by
--     'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkCmdProcessCommandsNVX'.
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
--     [subpass load
--     operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-load-store-ops)
--     for framebuffer attachments with a depth\/stencil format.
--
-- -   @VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT@ specifies the stage of
--     the pipeline where late fragment tests (depth and stencil tests
--     after fragment shading) are performed. This stage also includes
--     [subpass store
--     operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-load-store-ops)
--     for framebuffer attachments with a depth\/stencil format.
--
-- -   @VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT@ specifies the stage
--     of the pipeline after blending where the final color values are
--     output from the pipeline. This stage also includes [subpass load and
--     store
--     operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-load-store-ops)
--     and multisample resolve operations for framebuffer attachments with
--     a color format.
--
-- -   @VK_PIPELINE_STAGE_TRANSFER_BIT@ specifies the execution of copy
--     commands. This includes the operations resulting from all [copy
--     commands](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#copies),
--     [clear
--     commands](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#clears)
--     (with the exception of
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdClearAttachments'),
--     and
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults'.
--
-- -   @VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT@ specifies the execution of a
--     compute shader.
--
-- -   @VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT@ specifies the final stage in
--     the pipeline where operations generated by all commands complete
--     execution.
--
-- -   @VK_PIPELINE_STAGE_HOST_BIT@ specifies a pseudo-stage indicating
--     execution on the host of reads\/writes of device memory. This stage
--     is not invoked by any commands recorded in a command buffer.
--
-- -   @VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT@ specifies the execution of all
--     graphics pipeline stages, and is equivalent to the logical OR of:
--
--     -   @VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT@
--
--     -   @VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT@
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
-- -   @VK_PIPELINE_STAGE_ALL_COMMANDS_BIT@ is equivalent to the logical OR
--     of every other pipeline stage flag that is supported on the queue it
--     is used with.
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
-- 'Graphics.Vulkan.Extensions.VK_AMD_buffer_marker.vkCmdWriteBufferMarkerAMD',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdWriteTimestamp'
newtype VkPipelineStageFlagBits = VkPipelineStageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

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
  showsPrec _ (VkPipelineStageFlagBits 0x00020000) = showString "VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX"
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
                               ("VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX", pure (VkPipelineStageFlagBits 0x00020000))
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
-- | Dummy data to tag the 'Ptr' with
data VkQueue_T
-- | VkQueue - Opaque handle to a queue object
--
-- = See Also
--
-- 'vkGetDeviceQueue',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory.vkGetDeviceQueue2',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkQueueBeginDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.vkQueueBindSparse',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkQueueEndDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkQueueInsertDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkQueuePresentKHR',
-- 'vkQueueSubmit', 'vkQueueWaitIdle'
type VkQueue = Ptr VkQueue_T
-- | Dummy data to tag the 'Ptr' with
data VkCommandBuffer_T
-- | VkCommandBuffer - Opaque handle to a command buffer object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkCmdProcessCommandsInfoNVX',
-- 'VkSubmitInfo',
-- 'Graphics.Vulkan.Core10.CommandBuffer.vkAllocateCommandBuffers',
-- 'Graphics.Vulkan.Core10.CommandBuffer.vkBeginCommandBuffer',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkCmdBeginDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBeginQuery',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBeginRenderPass',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindPipeline',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBlitImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdClearAttachments',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdClearColorImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerBeginEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerEndEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.vkCmdDebugMarkerInsertEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDispatch',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.vkCmdDispatchBase',
-- 'Graphics.Vulkan.Extensions.VK_KHR_device_group.vkCmdDispatchBaseKHR',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDispatchIndirect',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDraw',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDrawIndexed',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.vkCmdDrawIndexedIndirectCountAMD',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDrawIndirect',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.vkCmdDrawIndirectCountAMD',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkCmdEndDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdEndQuery',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdEndRenderPass',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdExecuteCommands',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdFillBuffer',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.vkCmdInsertDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdNextSubpass',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkCmdProcessCommandsNVX',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdPushConstants',
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetWithTemplateKHR',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.vkCmdReserveSpaceForCommandsNVX',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdResetQueryPool',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdResolveImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetBlendConstants',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetDepthBias',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetDepthBounds',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.vkCmdSetDeviceMask',
-- 'Graphics.Vulkan.Extensions.VK_KHR_device_group.vkCmdSetDeviceMaskKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.vkCmdSetDiscardRectangleEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetLineWidth',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.vkCmdSetSampleLocationsEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetScissor',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetStencilCompareMask',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetStencilReference',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetStencilWriteMask',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetViewport',
-- 'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.vkCmdSetViewportWScalingNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdUpdateBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdWaitEvents',
-- 'Graphics.Vulkan.Extensions.VK_AMD_buffer_marker.vkCmdWriteBufferMarkerAMD',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdWriteTimestamp',
-- 'Graphics.Vulkan.Core10.CommandBuffer.vkEndCommandBuffer',
-- 'Graphics.Vulkan.Core10.CommandBuffer.vkFreeCommandBuffers',
-- 'Graphics.Vulkan.Core10.CommandBuffer.vkResetCommandBuffer'
type VkCommandBuffer = Ptr VkCommandBuffer_T
-- | Dummy data to tag the 'Ptr' with
data VkFence_T
-- | VkFence - Opaque handle to a fence object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkAcquireNextImageInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd.VkFenceGetFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.VkFenceGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd.VkImportFenceFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.VkImportFenceWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR',
-- 'Graphics.Vulkan.Core10.Fence.vkCreateFence',
-- 'Graphics.Vulkan.Core10.Fence.vkDestroyFence',
-- 'Graphics.Vulkan.Core10.Fence.vkGetFenceStatus',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.vkQueueBindSparse',
-- 'vkQueueSubmit',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.vkRegisterDeviceEventEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.vkRegisterDisplayEventEXT',
-- 'Graphics.Vulkan.Core10.Fence.vkResetFences',
-- 'Graphics.Vulkan.Core10.Fence.vkWaitForFences'
type VkFence = Ptr VkFence_T
-- | Dummy data to tag the 'Ptr' with
data VkSemaphore_T
-- | VkSemaphore - Opaque handle to a semaphore object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkAcquireNextImageInfoKHR',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkBindSparseInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd.VkImportSemaphoreFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.VkImportSemaphoreWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkPresentInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd.VkSemaphoreGetFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.VkSemaphoreGetWin32HandleInfoKHR',
-- 'VkSubmitInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.vkCreateSemaphore',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.vkDestroySemaphore'
type VkSemaphore = Ptr VkSemaphore_T
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
-- -   @pQueue@ is a pointer to a @VkQueue@ object that will be filled with
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
-- -   'Graphics.Vulkan.Core10.Device.VkDeviceQueueCreateInfo'::@flags@
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
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkQueue'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceQueue" vkGetDeviceQueue :: ("device" ::: VkDevice) -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr VkQueue) -> IO ()
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
--     not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', it defines a
--     [fence signal
--     operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-fences-signaling).
--
-- = Description
--
-- __Note__
--
-- Submission can be a high overhead operation, and applications /should/
-- attempt to batch work together into as few calls to @vkQueueSubmit@ as
-- possible.
--
-- @vkQueueSubmit@ is a [queue submission
-- command](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#devsandqueues-submission),
-- with each batch defined by an element of @pSubmits@ as an instance of
-- the 'VkSubmitInfo' structure. Batches begin execution in the order they
-- appear in @pSubmits@, but /may/ complete out of order.
--
-- Fence and semaphore operations submitted with 'vkQueueSubmit' have
-- additional ordering constraints compared to other submission commands,
-- with dependencies involving previous and subsequent queue operations.
-- Information about these additional constraints can be found in the
-- [semaphore](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-semaphores)
-- and
-- [fence](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-fences)
-- sections of [the synchronization
-- chapter](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization).
--
-- Details on the interaction of @pWaitDstStageMask@ with synchronization
-- are described in the [semaphore wait
-- operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-semaphores-waiting)
-- section of [the synchronization
-- chapter](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization).
--
-- The order that batches appear in @pSubmits@ is used to determine
-- [submission
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order),
-- and thus all the [implicit ordering
-- guarantees](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-implicit)
-- that respect it. Other than these implicit ordering guarantees and any
-- [explicit synchronization
-- primitives](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization),
-- these batches /may/ overlap or otherwise execute out of order.
--
-- If any command buffer submitted to this queue is in the [executable
-- state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle),
-- it is moved to the [pending
-- state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
-- Once execution of all submissions of a command buffer complete, it moves
-- from the [pending
-- state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle),
-- back to the [executable
-- state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
-- If a command buffer was recorded with the
-- @VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT@ flag, it instead moves
-- back to the [invalid
-- state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- If @vkQueueSubmit@ fails, it /may/ return @VK_ERROR_OUT_OF_HOST_MEMORY@
-- or @VK_ERROR_OUT_OF_DEVICE_MEMORY@. If it does, the implementation
-- /must/ ensure that the state and contents of any resources or
-- synchronization primitives referenced by the submitted command buffers
-- and any semaphores referenced by @pSubmits@ is unaffected by the call or
-- its failure. If @vkQueueSubmit@ fails in such a way that the
-- implementation is unable to make that guarantee, the implementation
-- /must/ return @VK_ERROR_DEVICE_LOST@. See [Lost
-- Device](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#devsandqueues-lost-device).
--
-- == Valid Usage
--
-- -   If @fence@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @fence@ /must/ be unsignaled
--
-- -   If @fence@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @fence@ /must/ not be associated with any other queue command that
--     has not yet completed execution on that queue
--
-- -   Any calls to
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetEvent',
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdResetEvent' or
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdWaitEvents' that
--     have been recorded into any of the command buffer elements of the
--     @pCommandBuffers@ member of any element of @pSubmits@, /must/ not
--     reference any 'Graphics.Vulkan.Core10.Event.VkEvent' that is
--     referenced by any of those commands in a command buffer that has
--     been submitted to another queue and is still in the /pending state/.
--
-- -   Any stage flag included in any element of the @pWaitDstStageMask@
--     member of any element of @pSubmits@ /must/ be a pipeline stage
--     supported by one of the capabilities of @queue@, as specified in the
--     [table of supported pipeline
--     stages](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-supported).
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
--     [semaphore signal
--     operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-semaphores-signaling)
--     previously submitted for execution.
--
-- -   Each element of the @pCommandBuffers@ member of each element of
--     @pSubmits@ /must/ be in the [pending or executable
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- -   If any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ was not recorded with the
--     @VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT@, it /must/ not be in
--     the [pending
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- -   Any [secondary command buffers
--     recorded](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-secondary)
--     into any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ /must/ be in the [pending or executable
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- -   If any [secondary command buffers
--     recorded](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-secondary)
--     into any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ was not recorded with the
--     @VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT@, it /must/ not be in
--     the [pending
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- -   Each element of the @pCommandBuffers@ member of each element of
--     @pSubmits@ /must/ have been allocated from a @VkCommandPool@ that
--     was created for the same queue family @queue@ belongs to.
--
-- == Valid Usage (Implicit)
--
-- -   @queue@ /must/ be a valid @VkQueue@ handle
--
-- -   If @submitCount@ is not @0@, @pSubmits@ /must/ be a valid pointer to
--     an array of @submitCount@ valid @VkSubmitInfo@ structures
--
-- -   If @fence@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @fence@ /must/ be a valid @VkFence@ handle
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
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | -                                                                                                           | -                                                                                                          | Any                                                                                                   | -                                                                                                                          |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
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
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | -                                                                                                           | -                                                                                                          | Any                                                                                                   | -                                                                                                                          |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
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
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_DEVICE_LOST@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDeviceWaitIdle" vkDeviceWaitIdle :: ("device" ::: VkDevice) -> IO VkResult
-- | VkSubmitInfo - Structure specifying a queue submit operation
--
-- = Description
--
-- The order that command buffers appear in @pCommandBuffers@ is used to
-- determine [submission
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order),
-- and thus all the [implicit ordering
-- guarantees](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-implicit)
-- that respect it. Other than these implicit ordering guarantees and any
-- [explicit synchronization
-- primitives](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization),
-- these command buffers /may/ overlap or otherwise execute out of order.
--
-- == Valid Usage
--
-- -   Each element of @pCommandBuffers@ /must/ not have been allocated
--     with @VK_COMMAND_BUFFER_LEVEL_SECONDARY@
--
-- -   If the [geometry
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-geometryShader)
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the [tessellation
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-tessellationShader)
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   Each element of @pWaitDstStageMask@ /must/ not include
--     @VK_PIPELINE_STAGE_HOST_BIT@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_SUBMIT_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.VkD3D12FenceSubmitInfoKHR',
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupSubmitInfo',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory.VkProtectedSubmitInfo',
--     'Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex.VkWin32KeyedMutexAcquireReleaseInfoKHR',
--     or
--     'Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex.VkWin32KeyedMutexAcquireReleaseInfoNV'
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
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkQueueSubmit'
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
  -- semaphores to wait on are provided, they define a [semaphore wait
  -- operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-semaphores-waiting).
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
  -- [semaphore signal
  -- operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-semaphores-signaling).
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
-- 'Graphics.Vulkan.Core10.Pass.VkSubpassDependency',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdWaitEvents'
type VkPipelineStageFlags = VkPipelineStageFlagBits
