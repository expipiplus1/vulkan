{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_buffer_marker
  ( cmdWriteBufferMarkerAMD
  , pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION
  , pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME
  ) where

import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker
  ( vkCmdWriteBufferMarkerAMD
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  , PipelineStageFlagBits
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker
  ( pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME
  , pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION
  )



-- | vkCmdWriteBufferMarkerAMD - Execute a pipelined write of a marker value
-- into a buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pipelineStage@ is one of the
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' values,
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
-- preceding
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker.vkCmdWriteBufferMarkerAMD'
-- commands so long as their specified pipeline stages occur either at the
-- same time or earlier than this command’s specified @pipelineStage@.
--
-- While consecutive buffer marker writes with the same @pipelineStage@
-- parameter are implicitly complete in submission order, memory and
-- execution dependencies between buffer marker writes and other operations
-- must still be explicitly ordered using synchronization commands. The
-- access scope for buffer marker writes falls under the
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_TRANSFER_WRITE_BIT', and the
-- pipeline stages for identifying the synchronization scope /must/ include
-- both @pipelineStage@ and
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TRANSFER_BIT'.
--
-- __Note__
--
-- Similar to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp', if
-- an implementation is unable to write a marker at any specific pipeline
-- stage, it /may/ instead do so at any logically later stage.
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
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @dstOffset@ /must/ be a multiple of @4@
--
-- Unresolved directive in vkCmdWriteBufferMarkerAMD.txt -
-- include::{generated}\/validity\/protos\/vkCmdWriteBufferMarkerAMD.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdWriteBufferMarkerAMD :: CommandBuffer ->  PipelineStageFlagBits ->  Buffer ->  DeviceSize ->  Word32 ->  IO ()
cmdWriteBufferMarkerAMD = \(CommandBuffer commandBuffer' commandTable) -> \pipelineStage' -> \dstBuffer' -> \dstOffset' -> \marker' -> vkCmdWriteBufferMarkerAMD commandTable commandBuffer' pipelineStage' dstBuffer' dstOffset' marker' *> (pure ())
