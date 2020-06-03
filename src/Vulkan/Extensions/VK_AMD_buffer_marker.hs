{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_buffer_marker  ( cmdWriteBufferMarkerAMD
                                               , AMD_BUFFER_MARKER_SPEC_VERSION
                                               , pattern AMD_BUFFER_MARKER_SPEC_VERSION
                                               , AMD_BUFFER_MARKER_EXTENSION_NAME
                                               , pattern AMD_BUFFER_MARKER_EXTENSION_NAME
                                               ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteBufferMarkerAMD))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteBufferMarkerAMD
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlagBits -> Buffer -> DeviceSize -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlagBits -> Buffer -> DeviceSize -> Word32 -> IO ()

-- | vkCmdWriteBufferMarkerAMD - Execute a pipelined write of a marker value
-- into a buffer
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
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFER_WRITE_BIT', and the
-- pipeline stages for identifying the synchronization scope /must/ include
-- both @pipelineStage@ and
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFER_BIT'.
--
-- Note
--
-- Similar to 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp', if
-- an implementation is unable to write a marker at any specific pipeline
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
-- -   @pipelineStage@ /must/ be a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported valid stage>
--     for the queue family that was used to create the command pool that
--     @commandBuffer@ was allocated from
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   @dstOffset@ /must/ be less than or equal to the size of @dstBuffer@
--     minus @4@
--
-- -   @dstBuffer@ /must/ have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @dstOffset@ /must/ be a multiple of @4@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pipelineStage@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     value
--
-- -   @dstBuffer@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   Both of @commandBuffer@, and @dstBuffer@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
cmdWriteBufferMarkerAMD :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @pipelineStage@ is one of the
                           -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                           -- values, specifying the pipeline stage whose completion triggers the
                           -- marker write.
                           PipelineStageFlagBits
                        -> -- | @dstBuffer@ is the buffer where the marker will be written to.
                           ("dstBuffer" ::: Buffer)
                        -> -- | @dstOffset@ is the byte offset into the buffer where the marker will be
                           -- written to.
                           ("dstOffset" ::: DeviceSize)
                        -> -- | @marker@ is the 32-bit value of the marker.
                           ("marker" ::: Word32)
                        -> io ()
cmdWriteBufferMarkerAMD commandBuffer pipelineStage dstBuffer dstOffset marker = liftIO $ do
  let vkCmdWriteBufferMarkerAMDPtr = pVkCmdWriteBufferMarkerAMD (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdWriteBufferMarkerAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteBufferMarkerAMD is null" Nothing Nothing
  let vkCmdWriteBufferMarkerAMD' = mkVkCmdWriteBufferMarkerAMD vkCmdWriteBufferMarkerAMDPtr
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

