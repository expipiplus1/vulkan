{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_AMD_buffer_marker
  ( pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION
  , pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME
  , vkCmdWriteBufferMarkerAMD
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  )


-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_SPEC_VERSION"
pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION :: Integral a => a
pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_EXTENSION_NAME"
pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"
-- | vkCmdWriteBufferMarkerAMD - Execute a pipelined write of a marker value
-- into a buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pipelineStage@ is one of the
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' values,
--     specifying the pipeline stage whose completion triggers the marker
--     write.
--
-- -   @dstBuffer@ is the buffer where the marker will be written to.
--
-- -   @dstOffset@ is the byte offset into the buffer where the marker will
--     be written to.
--
-- -   @marker@ is the 32-bit value of the marker.
--
-- = Description
--
-- The command will write the 32-bit marker value into the buffer only
-- after all preceding commands have finished executing up to at least the
-- specified pipeline stage. This includes the completion of other
-- preceding @vkCmdWriteBufferMarkerAMD@ commands so long as their
-- specified pipeline stages occur either at the same time or earlier than
-- this command’s specified @pipelineStage@.
--
-- While consecutive buffer marker writes with the same @pipelineStage@
-- parameter are implicitly complete in submission order, memory and
-- execution dependencies between buffer marker writes and other operations
-- must still be explicitly ordered using synchronization commands. The
-- access scope for buffer marker writes falls under the
-- @VK_ACCESS_TRANSFER_WRITE_BIT@, and the pipeline stages for identifying
-- the synchronization scope /must/ include both @pipelineStage@ and
-- @VK_PIPELINE_STAGE_TRANSFER_BIT@.
--
-- __Note__
--
-- Similar to @vkCmdWriteTimestamp@, if an implementation is unable to
-- write a marker at any specific pipeline stage, it /may/ instead do so at
-- any logically later stage.
--
-- __Note__
--
-- Implementations /may/ only support a limited number of pipelined marker
-- write operations in flight at a given time, thus excessive number of
-- marker write operations /may/ degrade command execution performance.
--
-- == Valid Usage
--
-- -   @dstOffset@ /must/ be less than or equal to the size of @dstBuffer@
--     minus @4@.
--
-- -   @dstBuffer@ /must/ have been created with
--     @VK_BUFFER_USAGE_TRANSFER_DST_BIT@ usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @dstOffset@ /must/ be a multiple of @4@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pipelineStage@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' value
--
-- -   @dstBuffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support transfer, graphics, or compute operations
--
-- -   Both of @commandBuffer@, and @dstBuffer@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Transfer                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            | Graphics                                                                                              |                                                                                                                            |
-- |                                                                                                             |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@,
-- 'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdWriteBufferMarkerAMD" vkCmdWriteBufferMarkerAMD :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ()
