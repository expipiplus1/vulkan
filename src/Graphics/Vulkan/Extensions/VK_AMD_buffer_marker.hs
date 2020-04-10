{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_AMD_buffer_marker  ( cmdWriteBufferMarkerAMD
                                                        , AMD_BUFFER_MARKER_SPEC_VERSION
                                                        , pattern AMD_BUFFER_MARKER_SPEC_VERSION
                                                        , AMD_BUFFER_MARKER_EXTENSION_NAME
                                                        , pattern AMD_BUFFER_MARKER_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.Core10.Handles (Buffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer_T)
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdWriteBufferMarkerAMD))
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits)
import Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteBufferMarkerAMD
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlagBits -> Buffer -> DeviceSize -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlagBits -> Buffer -> DeviceSize -> Word32 -> IO ()

-- | vkCmdWriteBufferMarkerAMD - Execute a pipelined write of a marker value
-- into a buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pipelineStage@ is one of the
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values, specifying the pipeline stage whose completion triggers the
--     marker write.
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
-- preceding 'cmdWriteBufferMarkerAMD' commands so long as their specified
-- pipeline stages occur either at the same time or earlier than this
-- command’s specified @pipelineStage@.
--
-- While consecutive buffer marker writes with the same @pipelineStage@
-- parameter are implicitly complete in submission order, memory and
-- execution dependencies between buffer marker writes and other operations
-- must still be explicitly ordered using synchronization commands. The
-- access scope for buffer marker writes falls under the
-- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFER_WRITE_BIT',
-- and the pipeline stages for identifying the synchronization scope /must/
-- include both @pipelineStage@ and
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFER_BIT'.
--
-- Note
--
-- Similar to
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp', if an
-- implementation is unable to write a marker at any specific pipeline
-- stage, it /may/ instead do so at any logically later stage.
--
-- Note
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
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @dstOffset@ /must/ be a multiple of @4@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pipelineStage@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     value
--
-- -   @dstBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ support transfer,
--     graphics, or compute operations
--
-- -   Both of @commandBuffer@, and @dstBuffer@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
cmdWriteBufferMarkerAMD :: CommandBuffer -> PipelineStageFlagBits -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> ("marker" ::: Word32) -> IO ()
cmdWriteBufferMarkerAMD commandBuffer pipelineStage dstBuffer dstOffset marker = do
  let vkCmdWriteBufferMarkerAMD' = mkVkCmdWriteBufferMarkerAMD (pVkCmdWriteBufferMarkerAMD (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdWriteBufferMarkerAMD' (commandBufferHandle (commandBuffer)) (pipelineStage) (dstBuffer) (dstOffset) (marker)
  pure $ ()


type AMD_BUFFER_MARKER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_SPEC_VERSION"
pattern AMD_BUFFER_MARKER_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_BUFFER_MARKER_SPEC_VERSION = 1


type AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"

-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_EXTENSION_NAME"
pattern AMD_BUFFER_MARKER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"

