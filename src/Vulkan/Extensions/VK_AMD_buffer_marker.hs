{-# language CPP #-}
-- | = Name
--
-- VK_AMD_buffer_marker - device extension
--
-- == VK_AMD_buffer_marker
--
-- [__Name String__]
--     @VK_AMD_buffer_marker@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     180
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_buffer_marker] @drakos-amd%0A<<Here describe the issue or question you have about the VK_AMD_buffer_marker extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-01-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Jaakko Konttinen, AMD
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- This extension adds a new operation to execute pipelined writes of small
-- marker values into a 'Vulkan.Core10.Handles.Buffer' object.
--
-- The primary purpose of these markers is to facilitate the development of
-- debugging tools for tracking which pipelined command contributed to
-- device loss.
--
-- == New Commands
--
-- -   'cmdWriteBufferMarkerAMD'
--
-- == New Enum Constants
--
-- -   'AMD_BUFFER_MARKER_EXTENSION_NAME'
--
-- -   'AMD_BUFFER_MARKER_SPEC_VERSION'
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2018-01-26 (Jaakko Konttinen)
--
--     -   Initial revision
--
-- == See Also
--
-- 'cmdWriteBufferMarkerAMD'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_buffer_marker Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_buffer_marker  ( cmdWriteBufferMarkerAMD
                                               , AMD_BUFFER_MARKER_SPEC_VERSION
                                               , pattern AMD_BUFFER_MARKER_SPEC_VERSION
                                               , AMD_BUFFER_MARKER_EXTENSION_NAME
                                               , pattern AMD_BUFFER_MARKER_EXTENSION_NAME
                                               ) where

import Vulkan.Internal.Utils (traceAroundEvent)
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
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
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
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04074# @pipelineStage@
--     /must/ be a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported valid stage>
--     for the queue family that was used to create the command pool that
--     @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04075# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04076# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04077# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04078# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04079# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04080# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04081# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-synchronization2-06489# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.PIPELINE_STAGE_NONE_KHR'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-dstOffset-01798# @dstOffset@ /must/
--     be less than or equal to the size of @dstBuffer@ minus @4@
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-dstBuffer-01799# @dstBuffer@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-dstBuffer-01800# If @dstBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-dstOffset-01801# @dstOffset@ /must/
--     be a multiple of @4@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-parameter# If
--     @pipelineStage@ is not @0@, @pipelineStage@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     value
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-dstBuffer-parameter# @dstBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-commonparent# Both of
--     @commandBuffer@, and @dstBuffer@ /must/ have been created,
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Transfer                                                                                                              |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_buffer_marker VK_AMD_buffer_marker>,
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
cmdWriteBufferMarkerAMD :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @pipelineStage@ is a
                           -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits' value
                           -- specifying the pipeline stage whose completion triggers the marker
                           -- write.
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
  let vkCmdWriteBufferMarkerAMDPtr = pVkCmdWriteBufferMarkerAMD (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdWriteBufferMarkerAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteBufferMarkerAMD is null" Nothing Nothing
  let vkCmdWriteBufferMarkerAMD' = mkVkCmdWriteBufferMarkerAMD vkCmdWriteBufferMarkerAMDPtr
  traceAroundEvent "vkCmdWriteBufferMarkerAMD" (vkCmdWriteBufferMarkerAMD' (commandBufferHandle (commandBuffer)) (pipelineStage) (dstBuffer) (dstOffset) (marker))
  pure $ ()


type AMD_BUFFER_MARKER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_SPEC_VERSION"
pattern AMD_BUFFER_MARKER_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_BUFFER_MARKER_SPEC_VERSION = 1


type AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"

-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_EXTENSION_NAME"
pattern AMD_BUFFER_MARKER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"

