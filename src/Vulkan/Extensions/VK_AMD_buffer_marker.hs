{-# language CPP #-}
-- | = Name
--
-- VK_AMD_buffer_marker - device extension
--
-- = VK_AMD_buffer_marker
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
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_3
--
--     -   Interacts with VK_KHR_synchronization2
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_buffer_marker] @drakos-amd%0A*Here describe the issue or question you have about the VK_AMD_buffer_marker extension* >
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
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
-- is supported:
--
-- -   'cmdWriteBufferMarker2AMD'
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
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_AMD_buffer_marker Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_buffer_marker  ( cmdWriteBufferMarkerAMD
                                               , cmdWriteBufferMarker2AMD
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
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteBufferMarker2AMD))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteBufferMarkerAMD))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(..))
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2(..))
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
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
-- When 'cmdWriteBufferMarkerAMD' is submitted to a queue, it defines an
-- execution dependency between prior operations and writing the marker
-- value, as well as a memory dependency from earlier
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#copies-buffer-markers buffer marker write commands>.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes operations performed by operations that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-submission-order submission order>
-- in the pipeline stage identified by @pipelineStage@. It additionally
-- includes other
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#copies-buffer-markers buffer marker write commands>
-- that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-submission-order submission order>
-- that specified either the same @pipelineStage@ or a stage that is
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-pipeline-stages-order logically earlier>.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the buffer marker write.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- includes only accesses performed by other
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#copies-buffer-markers buffer marker write commands>.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is empty.
--
-- The access scope for buffer marker writes falls under the
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFER_WRITE_BIT' flag, and
-- is performed by either @pipelineStage@ or
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFER_BIT'.
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization Synchronization commands>
-- should specify this access flag and both pipeline stages when defining
-- dependencies with this command.
--
-- Similar to 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp', if
-- an implementation is unable to write a marker at any specific pipeline
-- stage, it /may/ instead do so at any logically later stage.
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04076# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04077# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04078# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04079# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-04080# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-pipelineStage-07077# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-shadingRateImage-07314# If neither
--     of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     features are enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-synchronization2-06489# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_NONE'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-rayTracingPipeline-07943# If neither
--     of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline>
--     feature are enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-dstOffset-01798# @dstOffset@ /must/
--     be less than or equal to the size of @dstBuffer@ minus @4@
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-dstBuffer-01799# @dstBuffer@ /must/
--     have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag set
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
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_TRANSFER_BIT' operations
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-suspended# This command /must/ not
--     be called between suspended render pass instances
--
-- -   #VUID-vkCmdWriteBufferMarkerAMD-videocoding# This command /must/
--     only be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | VK_QUEUE_TRANSFER_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdWriteBufferMarkerAMD is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
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
cmdWriteBufferMarkerAMD commandBuffer
                          pipelineStage
                          dstBuffer
                          dstOffset
                          marker = liftIO $ do
  let vkCmdWriteBufferMarkerAMDPtr = pVkCmdWriteBufferMarkerAMD (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdWriteBufferMarkerAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteBufferMarkerAMD is null" Nothing Nothing
  let vkCmdWriteBufferMarkerAMD' = mkVkCmdWriteBufferMarkerAMD vkCmdWriteBufferMarkerAMDPtr
  traceAroundEvent "vkCmdWriteBufferMarkerAMD" (vkCmdWriteBufferMarkerAMD'
                                                  (commandBufferHandle (commandBuffer))
                                                  (pipelineStage)
                                                  (dstBuffer)
                                                  (dstOffset)
                                                  (marker))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteBufferMarker2AMD
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlags2 -> Buffer -> DeviceSize -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlags2 -> Buffer -> DeviceSize -> Word32 -> IO ()

-- | vkCmdWriteBufferMarker2AMD - Execute a pipelined write of a marker value
-- into a buffer
--
-- = Description
--
-- When 'cmdWriteBufferMarker2AMD' is submitted to a queue, it defines an
-- execution dependency between prior operations and writing the marker
-- value, as well as a memory dependency from earlier
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#copies-buffer-markers buffer marker write commands>.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes operations performed by operations that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-submission-order submission order>
-- in the pipeline stage identified by @pipelineStage@. It additionally
-- includes other
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#copies-buffer-markers buffer marker write commands>
-- that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-submission-order submission order>
-- that specified either the same @pipelineStage@ or a stage that is
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-pipeline-stages-order logically earlier>.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the buffer marker write.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- includes only accesses performed by other
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#copies-buffer-markers buffer marker write commands>.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is empty.
--
-- The access scope for buffer marker writes falls under the
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFER_WRITE_BIT' flag, and
-- is performed by either @pipelineStage@ or
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFER_BIT'.
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization Synchronization commands>
-- should specify this access flag and both pipeline stages when defining
-- dependencies with this command.
--
-- Similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWriteTimestamp2',
-- if an implementation is unable to write a marker at any specific
-- pipeline stage, it /may/ instead do so at any logically later stage.
--
-- Implementations /may/ only support a limited number of pipelined marker
-- write operations in flight at a given time. Thus an excessive number of
-- marker write operations /may/ degrade command execution performance.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-07316# If neither of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     features are enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-04957# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subpassShading subpassShading>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocationMask>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-07946# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline>
--     feature are enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-10751# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-accelerationStructure accelerationStructure>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-10752# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingMaintenance1 rayTracingMaintenance1>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-10753# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-micromap micromap>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-synchronization2-03893# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03894# @stage@ /must/ include
--     only a single pipeline stage
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-03895# @stage@ /must/ include
--     only stages that are valid for the queue family that was used to
--     create the command pool that @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-dstOffset-03896# @dstOffset@ /must/
--     be less than or equal to the size of @dstBuffer@ minus @4@
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-dstBuffer-03897# @dstBuffer@ /must/
--     have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-dstBuffer-03898# If @dstBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-dstOffset-03899# @dstOffset@ /must/
--     be a multiple of @4@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-stage-parameter# @stage@ /must/ be
--     a valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-dstBuffer-parameter# @dstBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_TRANSFER_BIT' operations
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-suspended# This command /must/ not
--     be called between suspended render pass instances
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdWriteBufferMarker2AMD-commonparent# Both of
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | VK_QUEUE_TRANSFER_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdWriteBufferMarker2AMD is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_buffer_marker VK_AMD_buffer_marker>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2'
cmdWriteBufferMarker2AMD :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @stage@ specifies the pipeline stage whose completion triggers the
                            -- marker write.
                            PipelineStageFlags2
                         -> -- | @dstBuffer@ is the buffer where the marker will be written.
                            ("dstBuffer" ::: Buffer)
                         -> -- | @dstOffset@ is the byte offset into the buffer where the marker will be
                            -- written.
                            ("dstOffset" ::: DeviceSize)
                         -> -- | @marker@ is the 32-bit value of the marker.
                            ("marker" ::: Word32)
                         -> io ()
cmdWriteBufferMarker2AMD commandBuffer
                           stage
                           dstBuffer
                           dstOffset
                           marker = liftIO $ do
  let vkCmdWriteBufferMarker2AMDPtr = pVkCmdWriteBufferMarker2AMD (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdWriteBufferMarker2AMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteBufferMarker2AMD is null" Nothing Nothing
  let vkCmdWriteBufferMarker2AMD' = mkVkCmdWriteBufferMarker2AMD vkCmdWriteBufferMarker2AMDPtr
  traceAroundEvent "vkCmdWriteBufferMarker2AMD" (vkCmdWriteBufferMarker2AMD'
                                                   (commandBufferHandle (commandBuffer))
                                                   (stage)
                                                   (dstBuffer)
                                                   (dstOffset)
                                                   (marker))
  pure $ ()


type AMD_BUFFER_MARKER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_SPEC_VERSION"
pattern AMD_BUFFER_MARKER_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_BUFFER_MARKER_SPEC_VERSION = 1


type AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"

-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_EXTENSION_NAME"
pattern AMD_BUFFER_MARKER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"

