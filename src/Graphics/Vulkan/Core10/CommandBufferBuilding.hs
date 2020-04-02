{-# language CPP #-}
module Graphics.Vulkan.Core10.CommandBufferBuilding  ( cmdBindPipeline
                                                     , cmdSetViewport
                                                     , cmdSetScissor
                                                     , cmdSetLineWidth
                                                     , cmdSetDepthBias
                                                     , cmdSetBlendConstants
                                                     , cmdSetDepthBounds
                                                     , cmdSetStencilCompareMask
                                                     , cmdSetStencilWriteMask
                                                     , cmdSetStencilReference
                                                     , cmdBindDescriptorSets
                                                     , cmdBindIndexBuffer
                                                     , cmdBindVertexBuffers
                                                     , cmdDraw
                                                     , cmdDrawIndexed
                                                     , cmdDrawIndirect
                                                     , cmdDrawIndexedIndirect
                                                     , cmdDispatch
                                                     , cmdDispatchIndirect
                                                     , cmdCopyBuffer
                                                     , cmdCopyImage
                                                     , cmdBlitImage
                                                     , cmdCopyBufferToImage
                                                     , cmdCopyImageToBuffer
                                                     , cmdUpdateBuffer
                                                     , cmdFillBuffer
                                                     , cmdClearColorImage
                                                     , cmdClearDepthStencilImage
                                                     , cmdClearAttachments
                                                     , cmdResolveImage
                                                     , cmdSetEvent
                                                     , cmdResetEvent
                                                     , cmdWaitEvents
                                                     , cmdPipelineBarrier
                                                     , cmdBeginQuery
                                                     , cmdWithQuery
                                                     , cmdEndQuery
                                                     , cmdResetQueryPool
                                                     , cmdWriteTimestamp
                                                     , cmdCopyQueryPoolResults
                                                     , cmdPushConstants
                                                     , cmdBeginRenderPass
                                                     , cmdNextSubpass
                                                     , cmdEndRenderPass
                                                     , cmdExecuteCommands
                                                     , Viewport(..)
                                                     , Rect2D(..)
                                                     , ClearRect(..)
                                                     , BufferCopy(..)
                                                     , ImageCopy(..)
                                                     , ImageBlit(..)
                                                     , BufferImageCopy(..)
                                                     , ImageResolve(..)
                                                     , RenderPassBeginInfo(..)
                                                     , ClearAttachment(..)
                                                     ) where

import Control.Exception.Base (bracket_)
import Control.Monad (unless)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import qualified Data.Vector.Storable.Sized (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.CStruct.Utils (lowerArrayPtr)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.Core10.Handles (Buffer(..))
import Graphics.Vulkan.Core10.OtherTypes (BufferMemoryBarrier)
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.Core10.SharedTypes (ClearColorValue)
import Graphics.Vulkan.Core10.SharedTypes (ClearDepthStencilValue)
import Graphics.Vulkan.Core10.SharedTypes (ClearValue)
import Graphics.Vulkan.Core10.Handles (CommandBuffer)
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer_T)
import Graphics.Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Graphics.Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Graphics.Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlagBits(..))
import Graphics.Vulkan.Core10.Handles (DescriptorSet)
import Graphics.Vulkan.Core10.Handles (DescriptorSet(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdBeginQuery))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdBeginRenderPass))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdBindDescriptorSets))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdBindIndexBuffer))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdBindPipeline))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdBindVertexBuffers))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdBlitImage))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdClearAttachments))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdClearColorImage))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdClearDepthStencilImage))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdCopyBuffer))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdCopyBufferToImage))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdCopyImage))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdCopyImageToBuffer))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdCopyQueryPoolResults))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdDispatch))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdDispatchIndirect))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdDraw))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndexed))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndexedIndirect))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndirect))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdEndQuery))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdEndRenderPass))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdExecuteCommands))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdFillBuffer))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdNextSubpass))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdPipelineBarrier))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdPushConstants))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdResetEvent))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdResetQueryPool))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdResolveImage))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetBlendConstants))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthBias))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthBounds))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetEvent))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetLineWidth))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetScissor))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilCompareMask))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilReference))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilWriteMask))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewport))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdUpdateBuffer))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdWaitEvents))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCmdWriteTimestamp))
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupRenderPassBeginInfo)
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core10.Handles (Event)
import Graphics.Vulkan.Core10.Handles (Event(..))
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.Core10.SharedTypes (Extent2D)
import Graphics.Vulkan.Core10.SharedTypes (Extent3D)
import Graphics.Vulkan.Core10.Enums.Filter (Filter)
import Graphics.Vulkan.Core10.Enums.Filter (Filter(..))
import Graphics.Vulkan.Core10.Handles (Framebuffer)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Image)
import Graphics.Vulkan.Core10.Handles (Image(..))
import Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Graphics.Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Graphics.Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Graphics.Vulkan.Core10.OtherTypes (ImageMemoryBarrier)
import Graphics.Vulkan.Core10.SharedTypes (ImageSubresourceLayers)
import Graphics.Vulkan.Core10.SharedTypes (ImageSubresourceRange)
import Graphics.Vulkan.Core10.Enums.IndexType (IndexType)
import Graphics.Vulkan.Core10.Enums.IndexType (IndexType(..))
import Graphics.Vulkan.Core10.OtherTypes (MemoryBarrier)
import Graphics.Vulkan.Core10.SharedTypes (Offset2D)
import Graphics.Vulkan.Core10.SharedTypes (Offset3D)
import Graphics.Vulkan.Core10.Handles (Pipeline)
import Graphics.Vulkan.Core10.Handles (Pipeline(..))
import Graphics.Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Graphics.Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Graphics.Vulkan.Core10.Handles (PipelineLayout)
import Graphics.Vulkan.Core10.Handles (PipelineLayout(..))
import Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits)
import Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(..))
import Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(..))
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlags)
import Graphics.Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlags)
import Graphics.Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlagBits(..))
import Graphics.Vulkan.Core10.Handles (QueryPool)
import Graphics.Vulkan.Core10.Handles (QueryPool(..))
import Graphics.Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlags)
import Graphics.Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlags)
import Graphics.Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlagBits(..))
import Graphics.Vulkan.Core10.Handles (RenderPass)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (RenderPassAttachmentBeginInfo)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_sample_locations (RenderPassSampleLocationsBeginInfoEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform (RenderPassTransformBeginInfoQCOM)
import Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(..))
import Graphics.Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlags)
import Graphics.Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlags)
import Graphics.Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlagBits(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.Core10.Enums.SubpassContents (SubpassContents)
import Graphics.Vulkan.Core10.Enums.SubpassContents (SubpassContents(..))
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindPipeline
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> IO ()

-- | vkCmdBindPipeline - Bind a pipeline object to a command buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     that the pipeline will be bound to.
--
-- -   'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     is a
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     value specifying whether to bind to the compute or graphics bind
--     point. Binding one does not disturb the other.
--
-- -   'Graphics.Vulkan.Core10.Handles.Pipeline' is the pipeline to be
--     bound.
--
-- = Description
--
-- Once bound, a pipeline binding affects subsequent graphics or compute
-- commands in the command buffer until a different pipeline is bound to
-- the bind point. The pipeline bound to
-- 'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'
-- controls the behavior of 'cmdDispatch' and 'cmdDispatchIndirect'. The
-- pipeline bound to
-- 'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
-- controls the behavior of all
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing drawing commands>.
-- The pipeline bound to
-- 'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_RAY_TRACING_NV'
-- controls the behavior of
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV'. No other
-- commands are affected by the pipeline state.
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     is
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE',
--     the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support compute operations
--
-- -   If
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     is
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS',
--     the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   If
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     is
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE',
--     'Graphics.Vulkan.Core10.Handles.Pipeline' /must/ be a compute
--     pipeline
--
-- -   If
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     is
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS',
--     'Graphics.Vulkan.Core10.Handles.Pipeline' /must/ be a graphics
--     pipeline
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-variableMultisampleRate variable multisample rate>
--     feature is not supported, 'Graphics.Vulkan.Core10.Handles.Pipeline'
--     is a graphics pipeline, the current subpass has no attachments, and
--     this is not the first call to this function with a graphics pipeline
--     after transitioning to the current subpass, then the sample count
--     specified by this pipeline /must/ match that set in the previous
--     pipeline
--
-- -   If
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PhysicalDeviceSampleLocationsPropertiesEXT'::@variableSampleLocations@
--     is 'Graphics.Vulkan.Core10.BaseType.FALSE', and
--     'Graphics.Vulkan.Core10.Handles.Pipeline' is a graphics pipeline
--     created with a
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     structure having its @sampleLocationsEnable@ member set to
--     'Graphics.Vulkan.Core10.BaseType.TRUE' but without
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     enabled then the current render pass instance /must/ have been begun
--     by specifying a
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.RenderPassSampleLocationsBeginInfoEXT'
--     structure whose @pPostSubpassSampleLocations@ member contains an
--     element with a @subpassIndex@ matching the current subpass index and
--     the @sampleLocationsInfo@ member of that element /must/ match the
--     @sampleLocationsInfo@ specified in
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     when the pipeline was created
--
-- -   This command /must/ not be recorded when transform feedback is
--     active
--
-- -   If
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     is
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_RAY_TRACING_NV',
--     the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support compute operations
--
-- -   If
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     is
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_RAY_TRACING_NV',
--     the 'Graphics.Vulkan.Core10.Handles.Pipeline' /must/ be a ray
--     tracing pipeline
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     value
--
-- -   'Graphics.Vulkan.Core10.Handles.Pipeline' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Pipeline' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics, or compute operations
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and
--     'Graphics.Vulkan.Core10.Handles.Pipeline' /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.Pipeline',
-- 'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
cmdBindPipeline :: CommandBuffer -> PipelineBindPoint -> Pipeline -> IO ()
cmdBindPipeline commandBuffer pipelineBindPoint pipeline = do
  let vkCmdBindPipeline' = mkVkCmdBindPipeline (pVkCmdBindPipeline (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdBindPipeline' (commandBufferHandle (commandBuffer)) (pipelineBindPoint) (pipeline)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewport
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Viewport -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Viewport -> IO ()

-- | vkCmdSetViewport - Set the viewport on a command buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @firstViewport@ is the index of the first viewport whose parameters
--     are updated by the command.
--
-- -   @viewportCount@ is the number of viewports whose parameters are
--     updated by the command.
--
-- -   @pViewports@ is a pointer to an array of 'Viewport' structures
--     specifying viewport parameters.
--
-- = Description
--
-- The viewport parameters taken from element i of @pViewports@ replace the
-- current state for the viewport index @firstViewport@ + i, for i in [0,
-- @viewportCount@).
--
-- == Valid Usage
--
-- -   @firstViewport@ /must/ be less than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstViewport@ and @viewportCount@ /must/ be between @1@
--     and
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @firstViewport@ /must/ be @0@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pViewports@ /must/ be a valid pointer to an array of
--     @viewportCount@ valid 'Viewport' structures
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   @viewportCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer', 'Viewport'
cmdSetViewport :: CommandBuffer -> ("firstViewport" ::: Word32) -> ("viewports" ::: Vector Viewport) -> IO ()
cmdSetViewport commandBuffer firstViewport viewports = evalContT $ do
  let vkCmdSetViewport' = mkVkCmdSetViewport (pVkCmdSetViewport (deviceCmds (commandBuffer :: CommandBuffer)))
  pPViewports <- ContT $ allocaBytesAligned @Viewport ((Data.Vector.length (viewports)) * 24) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPViewports `plusPtr` (24 * (i)) :: Ptr Viewport) (e) . ($ ())) (viewports)
  lift $ vkCmdSetViewport' (commandBufferHandle (commandBuffer)) (firstViewport) ((fromIntegral (Data.Vector.length $ (viewports)) :: Word32)) (pPViewports)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetScissor
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()

-- | vkCmdSetScissor - Set the dynamic scissor rectangles on a command buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @firstScissor@ is the index of the first scissor whose state is
--     updated by the command.
--
-- -   @scissorCount@ is the number of scissors whose rectangles are
--     updated by the command.
--
-- -   @pScissors@ is a pointer to an array of 'Rect2D' structures defining
--     scissor rectangles.
--
-- = Description
--
-- The scissor rectangles taken from element i of @pScissors@ replace the
-- current state for the scissor index @firstScissor@ + i, for i in [0,
-- @scissorCount@).
--
-- Each scissor rectangle is described by a 'Rect2D' structure, with the
-- @offset.x@ and @offset.y@ values determining the upper left corner of
-- the scissor rectangle, and the @extent.width@ and @extent.height@ values
-- determining the size in pixels.
--
-- If a render pass transform is enabled, the (@offset.x@ and @offset.y@)
-- and (@extent.width@ and @extent.height@) values are transformed as
-- described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-renderpass-transform render pass transform>
-- before participating in the scissor test.
--
-- == Valid Usage
--
-- -   @firstScissor@ /must/ be less than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstScissor@ and @scissorCount@ /must/ be between @1@
--     and
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @firstScissor@ /must/ be @0@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @scissorCount@ /must/ be @1@
--
-- -   The @x@ and @y@ members of @offset@ member of any element of
--     @pScissors@ /must/ be greater than or equal to @0@
--
-- -   Evaluation of (@offset.x@ + @extent.width@) /must/ not cause a
--     signed integer addition overflow for any element of @pScissors@
--
-- -   Evaluation of (@offset.y@ + @extent.height@) /must/ not cause a
--     signed integer addition overflow for any element of @pScissors@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pScissors@ /must/ be a valid pointer to an array of @scissorCount@
--     'Rect2D' structures
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   @scissorCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer', 'Rect2D'
cmdSetScissor :: CommandBuffer -> ("firstScissor" ::: Word32) -> ("scissors" ::: Vector Rect2D) -> IO ()
cmdSetScissor commandBuffer firstScissor scissors = evalContT $ do
  let vkCmdSetScissor' = mkVkCmdSetScissor (pVkCmdSetScissor (deviceCmds (commandBuffer :: CommandBuffer)))
  pPScissors <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (scissors)) * 16) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e) . ($ ())) (scissors)
  lift $ vkCmdSetScissor' (commandBufferHandle (commandBuffer)) (firstScissor) ((fromIntegral (Data.Vector.length $ (scissors)) :: Word32)) (pPScissors)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetLineWidth
  :: FunPtr (Ptr CommandBuffer_T -> CFloat -> IO ()) -> Ptr CommandBuffer_T -> CFloat -> IO ()

-- | vkCmdSetLineWidth - Set the dynamic line width state
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @lineWidth@ is the width of rasterized line segments.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-wideLines wide lines>
--     feature is not enabled, @lineWidth@ /must/ be @1.0@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdSetLineWidth :: CommandBuffer -> ("lineWidth" ::: Float) -> IO ()
cmdSetLineWidth commandBuffer lineWidth = do
  let vkCmdSetLineWidth' = mkVkCmdSetLineWidth (pVkCmdSetLineWidth (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdSetLineWidth' (commandBufferHandle (commandBuffer)) (CFloat (lineWidth))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBias
  :: FunPtr (Ptr CommandBuffer_T -> CFloat -> CFloat -> CFloat -> IO ()) -> Ptr CommandBuffer_T -> CFloat -> CFloat -> CFloat -> IO ()

-- | vkCmdSetDepthBias - Set the depth bias dynamic state
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @depthBiasConstantFactor@ is a scalar factor controlling the
--     constant depth value added to each fragment.
--
-- -   @depthBiasClamp@ is the maximum (or minimum) depth bias of a
--     fragment.
--
-- -   @depthBiasSlopeFactor@ is a scalar factor applied to a fragment’s
--     slope in depth bias calculations.
--
-- = Description
--
-- If @depthBiasEnable@ is 'Graphics.Vulkan.Core10.BaseType.FALSE', no
-- depth bias is applied and the fragment’s depth values are unchanged.
--
-- @depthBiasSlopeFactor@ scales the maximum depth slope of the polygon,
-- and @depthBiasConstantFactor@ scales an implementation-dependent
-- constant that relates to the usable resolution of the depth buffer. The
-- resulting values are summed to produce the depth bias value which is
-- then clamped to a minimum or maximum value specified by
-- @depthBiasClamp@. @depthBiasSlopeFactor@, @depthBiasConstantFactor@, and
-- @depthBiasClamp@ /can/ each be positive, negative, or zero.
--
-- The maximum depth slope m of a triangle is
--
-- \[m = \sqrt{ \left({{\partial z_f} \over {\partial x_f}}\right)^2
--         +  \left({{\partial z_f} \over {\partial y_f}}\right)^2}\]
--
-- where (xf, yf, zf) is a point on the triangle. m /may/ be approximated
-- as
--
-- \[m = \max\left( \left| { {\partial z_f} \over {\partial x_f} } \right|,
--                \left| { {\partial z_f} \over {\partial y_f} } \right|
--        \right).\]
--
-- The minimum resolvable difference r is an implementation-dependent
-- parameter that depends on the depth buffer representation. It is the
-- smallest difference in framebuffer coordinate z values that is
-- guaranteed to remain distinct throughout polygon rasterization and in
-- the depth buffer. All pairs of fragments generated by the rasterization
-- of two polygons with otherwise identical vertices, but @z@f values that
-- differ by r, will have distinct depth values.
--
-- For fixed-point depth buffer representations, r is constant throughout
-- the range of the entire depth buffer. For floating-point depth buffers,
-- there is no single minimum resolvable difference. In this case, the
-- minimum resolvable difference for a given polygon is dependent on the
-- maximum exponent, e, in the range of z values spanned by the primitive.
-- If n is the number of bits in the floating-point mantissa, the minimum
-- resolvable difference, r, for the given primitive is defined as
--
-- -   r = 2e-n
--
-- If a triangle is rasterized using the
-- 'Graphics.Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL_RECTANGLE_NV'
-- polygon mode, then this minimum resolvable difference /may/ not be
-- resolvable for samples outside of the triangle, where the depth is
-- extrapolated.
--
-- If no depth buffer is present, r is undefined.
--
-- The bias value o for a polygon is
--
-- \[\begin{aligned}
-- o &= \mathrm{dbclamp}( m \times \mathtt{depthBiasSlopeFactor} + r \times \mathtt{depthBiasConstantFactor} ) \\
-- \text{where} &\quad \mathrm{dbclamp}(x) =
-- \begin{cases}
--     x                                 & \mathtt{depthBiasClamp} = 0 \ \text{or}\ \texttt{NaN} \\
--     \min(x, \mathtt{depthBiasClamp})  & \mathtt{depthBiasClamp} > 0 \\
--     \max(x, \mathtt{depthBiasClamp})  & \mathtt{depthBiasClamp} < 0 \\
-- \end{cases}
-- \end{aligned}\]
--
-- m is computed as described above. If the depth buffer uses a fixed-point
-- representation, m is a function of depth values in the range [0,1], and
-- o is applied to depth values in the same range.
--
-- For fixed-point depth buffers, fragment depth values are always limited
-- to the range [0,1] by clamping after depth bias addition is performed.
-- Unless the
-- @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_EXT_depth_range_unrestricted@
-- extension is enabled, fragment depth values are clamped even when the
-- depth buffer uses a floating-point representation.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthBiasClamp depth bias clamping>
--     feature is not enabled, @depthBiasClamp@ /must/ be @0.0@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthBias :: CommandBuffer -> ("depthBiasConstantFactor" ::: Float) -> ("depthBiasClamp" ::: Float) -> ("depthBiasSlopeFactor" ::: Float) -> IO ()
cmdSetDepthBias commandBuffer depthBiasConstantFactor depthBiasClamp depthBiasSlopeFactor = do
  let vkCmdSetDepthBias' = mkVkCmdSetDepthBias (pVkCmdSetDepthBias (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdSetDepthBias' (commandBufferHandle (commandBuffer)) (CFloat (depthBiasConstantFactor)) (CFloat (depthBiasClamp)) (CFloat (depthBiasSlopeFactor))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetBlendConstants
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (Data.Vector.Storable.Sized.Vector 4 CFloat) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (Data.Vector.Storable.Sized.Vector 4 CFloat) -> IO ()

-- | vkCmdSetBlendConstants - Set the values of blend constants
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @blendConstants@ is a pointer to an array of four values specifying
--     the R, G, B, and A components of the blend constant color used in
--     blending, depending on the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blendfactors blend factor>.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdSetBlendConstants :: CommandBuffer -> ("blendConstants" ::: (Float, Float, Float, Float)) -> IO ()
cmdSetBlendConstants commandBuffer blendConstants = evalContT $ do
  let vkCmdSetBlendConstants' = mkVkCmdSetBlendConstants (pVkCmdSetBlendConstants (deviceCmds (commandBuffer :: CommandBuffer)))
  pBlendConstants <- ContT $ allocaBytesAligned @(Data.Vector.Storable.Sized.Vector 4 CFloat) 16 4
  let pBlendConstants' = lowerArrayPtr pBlendConstants
  lift $ case (blendConstants) of
    (e0, e1, e2, e3) -> do
      poke (pBlendConstants' :: Ptr CFloat) (CFloat (e0))
      poke (pBlendConstants' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
      poke (pBlendConstants' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
      poke (pBlendConstants' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
  lift $ vkCmdSetBlendConstants' (commandBufferHandle (commandBuffer)) (pBlendConstants)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBounds
  :: FunPtr (Ptr CommandBuffer_T -> CFloat -> CFloat -> IO ()) -> Ptr CommandBuffer_T -> CFloat -> CFloat -> IO ()

-- | vkCmdSetDepthBounds - Set the depth bounds test values for a command
-- buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @minDepthBounds@ is the lower bound of the range of depth values
--     used in the depth bounds test.
--
-- -   @maxDepthBounds@ is the upper bound of the range.
--
-- == Valid Usage
--
-- -   Unless the
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_EXT_depth_range_unrestricted@
--     extension is enabled @minDepthBounds@ /must/ be between @0.0@ and
--     @1.0@, inclusive
--
-- -   Unless the
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_EXT_depth_range_unrestricted@
--     extension is enabled @maxDepthBounds@ /must/ be between @0.0@ and
--     @1.0@, inclusive
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthBounds :: CommandBuffer -> ("minDepthBounds" ::: Float) -> ("maxDepthBounds" ::: Float) -> IO ()
cmdSetDepthBounds commandBuffer minDepthBounds maxDepthBounds = do
  let vkCmdSetDepthBounds' = mkVkCmdSetDepthBounds (pVkCmdSetDepthBounds (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdSetDepthBounds' (commandBufferHandle (commandBuffer)) (CFloat (minDepthBounds)) (CFloat (maxDepthBounds))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilCompareMask
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()

-- | vkCmdSetStencilCompareMask - Set the stencil compare mask dynamic state
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @faceMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits'
--     specifying the set of stencil state for which to update the compare
--     mask.
--
-- -   @compareMask@ is the new value to use as the stencil compare mask.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @faceMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits'
--     values
--
-- -   @faceMask@ /must/ not be @0@
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlags'
cmdSetStencilCompareMask :: CommandBuffer -> ("faceMask" ::: StencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ()
cmdSetStencilCompareMask commandBuffer faceMask compareMask = do
  let vkCmdSetStencilCompareMask' = mkVkCmdSetStencilCompareMask (pVkCmdSetStencilCompareMask (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdSetStencilCompareMask' (commandBufferHandle (commandBuffer)) (faceMask) (compareMask)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilWriteMask
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()

-- | vkCmdSetStencilWriteMask - Set the stencil write mask dynamic state
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @faceMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits'
--     specifying the set of stencil state for which to update the write
--     mask, as described above for 'cmdSetStencilCompareMask'.
--
-- -   @writeMask@ is the new value to use as the stencil write mask.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @faceMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits'
--     values
--
-- -   @faceMask@ /must/ not be @0@
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlags'
cmdSetStencilWriteMask :: CommandBuffer -> ("faceMask" ::: StencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ()
cmdSetStencilWriteMask commandBuffer faceMask writeMask = do
  let vkCmdSetStencilWriteMask' = mkVkCmdSetStencilWriteMask (pVkCmdSetStencilWriteMask (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdSetStencilWriteMask' (commandBufferHandle (commandBuffer)) (faceMask) (writeMask)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilReference
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()

-- | vkCmdSetStencilReference - Set the stencil reference dynamic state
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @faceMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits'
--     specifying the set of stencil state for which to update the
--     reference value, as described above for 'cmdSetStencilCompareMask'.
--
-- -   @reference@ is the new value to use as the stencil reference value.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @faceMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits'
--     values
--
-- -   @faceMask@ /must/ not be @0@
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlags'
cmdSetStencilReference :: CommandBuffer -> ("faceMask" ::: StencilFaceFlags) -> ("reference" ::: Word32) -> IO ()
cmdSetStencilReference commandBuffer faceMask reference = do
  let vkCmdSetStencilReference' = mkVkCmdSetStencilReference (pVkCmdSetStencilReference (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdSetStencilReference' (commandBufferHandle (commandBuffer)) (faceMask) (reference)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindDescriptorSets
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr DescriptorSet -> Word32 -> Ptr Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr DescriptorSet -> Word32 -> Ptr Word32 -> IO ()

-- | vkCmdBindDescriptorSets - Binds descriptor sets to a command buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     that the descriptor sets will be bound to.
--
-- -   'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     is a
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     indicating whether the descriptors will be used by graphics
--     pipelines or compute pipelines. There is a separate set of bind
--     points for each of graphics and compute, so binding one does not
--     disturb the other.
--
-- -   @layout@ is a 'Graphics.Vulkan.Core10.Handles.PipelineLayout' object
--     used to program the bindings.
--
-- -   @firstSet@ is the set number of the first descriptor set to be
--     bound.
--
-- -   @descriptorSetCount@ is the number of elements in the
--     @pDescriptorSets@ array.
--
-- -   @pDescriptorSets@ is a pointer to an array of handles to
--     'Graphics.Vulkan.Core10.Handles.DescriptorSet' objects describing
--     the descriptor sets to write to.
--
-- -   @dynamicOffsetCount@ is the number of dynamic offsets in the
--     @pDynamicOffsets@ array.
--
-- -   @pDynamicOffsets@ is a pointer to an array of @uint32_t@ values
--     specifying dynamic offsets.
--
-- = Description
--
-- 'cmdBindDescriptorSets' causes the sets numbered [@firstSet@..
-- @firstSet@+@descriptorSetCount@-1] to use the bindings stored in
-- @pDescriptorSets@[0..descriptorSetCount-1] for subsequent rendering
-- commands (either compute or graphics, according to the
-- 'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'). Any
-- bindings that were previously applied via these sets are no longer
-- valid.
--
-- Once bound, a descriptor set affects rendering of subsequent graphics or
-- compute commands in the command buffer until a different set is bound to
-- the same set number, or else until the set is disturbed as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>.
--
-- A compatible descriptor set /must/ be bound for all set numbers that any
-- shaders in a pipeline access, at the time that a draw or dispatch
-- command is recorded to execute using that pipeline. However, if none of
-- the shaders in a pipeline statically use any bindings with a particular
-- set number, then no descriptor set need be bound for that set number,
-- even if the pipeline layout includes a non-trivial descriptor set layout
-- for that set number.
--
-- If any of the sets being bound include dynamic uniform or storage
-- buffers, then @pDynamicOffsets@ includes one element for each array
-- element in each dynamic descriptor type binding in each set. Values are
-- taken from @pDynamicOffsets@ in an order such that all entries for set N
-- come before set N+1; within a set, entries are ordered by the binding
-- numbers in the descriptor set layouts; and within a binding array,
-- elements are in order. @dynamicOffsetCount@ /must/ equal the total
-- number of dynamic descriptors in the sets being bound.
--
-- The effective offset used for dynamic uniform and storage buffer
-- bindings is the sum of the relative offset taken from @pDynamicOffsets@,
-- and the base address of the buffer plus base offset in the descriptor
-- set. The range of the dynamic uniform and storage buffer bindings is the
-- buffer range as specified in the descriptor set.
--
-- Each of the @pDescriptorSets@ /must/ be compatible with the pipeline
-- layout specified by @layout@. The layout used to program the bindings
-- /must/ also be compatible with the pipeline used in subsequent graphics
-- or compute commands, as defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>
-- section.
--
-- The descriptor set contents bound by a call to 'cmdBindDescriptorSets'
-- /may/ be consumed at the following times:
--
-- -   For descriptor bindings created with the
--     'Graphics.Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
--     bit set, the contents /may/ be consumed when the command buffer is
--     submitted to a queue, or during shader execution of the resulting
--     draws and dispatches, or any time in between. Otherwise,
--
-- -   during host execution of the command, or during shader execution of
--     the resulting draws and dispatches, or any time in between.
--
-- Thus, the contents of a descriptor set binding /must/ not be altered
-- (overwritten by an update command, or freed) between the first point in
-- time that it /may/ be consumed, and when the command completes executing
-- on the queue.
--
-- The contents of @pDynamicOffsets@ are consumed immediately during
-- execution of 'cmdBindDescriptorSets'. Once all pending uses have
-- completed, it is legal to update and reuse a descriptor set.
--
-- == Valid Usage
--
-- -   Each element of @pDescriptorSets@ /must/ have been allocated with a
--     'Graphics.Vulkan.Core10.Handles.DescriptorSetLayout' that matches
--     (is the same as, or identically defined as) the
--     'Graphics.Vulkan.Core10.Handles.DescriptorSetLayout' at set /n/ in
--     @layout@, where /n/ is the sum of @firstSet@ and the index into
--     @pDescriptorSets@
--
-- -   @dynamicOffsetCount@ /must/ be equal to the total number of dynamic
--     descriptors in @pDescriptorSets@
--
-- -   The sum of @firstSet@ and @descriptorSetCount@ /must/ be less than
--     or equal to
--     'Graphics.Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     /must/ be supported by the
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer'’s parent
--     'Graphics.Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- -   Each element of @pDynamicOffsets@ which corresponds to a descriptor
--     binding with type
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     /must/ be a multiple of
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minUniformBufferOffsetAlignment@
--
-- -   Each element of @pDynamicOffsets@ which corresponds to a descriptor
--     binding with type
--     'Graphics.Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     /must/ be a multiple of
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minStorageBufferOffsetAlignment@
--
-- -   For each dynamic uniform or storage buffer binding in
--     @pDescriptorSets@, the sum of the effective offset, as defined
--     above, and the range of the binding /must/ be less than or equal to
--     the size of the buffer
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
--     value
--
-- -   @layout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   @pDescriptorSets@ /must/ be a valid pointer to an array of
--     @descriptorSetCount@ valid
--     'Graphics.Vulkan.Core10.Handles.DescriptorSet' handles
--
-- -   If @dynamicOffsetCount@ is not @0@, @pDynamicOffsets@ /must/ be a
--     valid pointer to an array of @dynamicOffsetCount@ @uint32_t@ values
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics, or compute operations
--
-- -   @descriptorSetCount@ /must/ be greater than @0@
--
-- -   Each of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', @layout@,
--     and the elements of @pDescriptorSets@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.DescriptorSet',
-- 'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Graphics.Vulkan.Core10.Handles.PipelineLayout'
cmdBindDescriptorSets :: CommandBuffer -> PipelineBindPoint -> PipelineLayout -> ("firstSet" ::: Word32) -> ("descriptorSets" ::: Vector DescriptorSet) -> ("dynamicOffsets" ::: Vector Word32) -> IO ()
cmdBindDescriptorSets commandBuffer pipelineBindPoint layout firstSet descriptorSets dynamicOffsets = evalContT $ do
  let vkCmdBindDescriptorSets' = mkVkCmdBindDescriptorSets (pVkCmdBindDescriptorSets (deviceCmds (commandBuffer :: CommandBuffer)))
  pPDescriptorSets <- ContT $ allocaBytesAligned @DescriptorSet ((Data.Vector.length (descriptorSets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorSets `plusPtr` (8 * (i)) :: Ptr DescriptorSet) (e)) (descriptorSets)
  pPDynamicOffsets <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (dynamicOffsets)) * 4) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDynamicOffsets `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (dynamicOffsets)
  lift $ vkCmdBindDescriptorSets' (commandBufferHandle (commandBuffer)) (pipelineBindPoint) (layout) (firstSet) ((fromIntegral (Data.Vector.length $ (descriptorSets)) :: Word32)) (pPDescriptorSets) ((fromIntegral (Data.Vector.length $ (dynamicOffsets)) :: Word32)) (pPDynamicOffsets)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindIndexBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> IndexType -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> IndexType -> IO ()

-- | vkCmdBindIndexBuffer - Bind an index buffer to a command buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command is recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' is the buffer being bound.
--
-- -   @offset@ is the starting offset in bytes within
--     'Graphics.Vulkan.Core10.Handles.Buffer' used in index buffer address
--     calculations.
--
-- -   'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' is a
--     'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' value specifying
--     whether indices are treated as 16 bits or 32 bits.
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of
--     'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   The sum of @offset@ and the address of the range of
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object that is backing
--     'Graphics.Vulkan.Core10.Handles.Buffer', /must/ be a multiple of the
--     type indicated by 'Graphics.Vulkan.Core10.Enums.IndexType.IndexType'
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ have been created
--     with the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDEX_BUFFER_BIT'
--     flag
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' /must/ not be
--     'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_NV'.
--
-- -   If 'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' is
--     'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8_EXT', the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-indexTypeUint8 indexTypeUint8>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.Buffer', and
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Enums.IndexType.IndexType'
cmdBindIndexBuffer :: CommandBuffer -> Buffer -> ("offset" ::: DeviceSize) -> IndexType -> IO ()
cmdBindIndexBuffer commandBuffer buffer offset indexType = do
  let vkCmdBindIndexBuffer' = mkVkCmdBindIndexBuffer (pVkCmdBindIndexBuffer (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdBindIndexBuffer' (commandBufferHandle (commandBuffer)) (buffer) (offset) (indexType)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindVertexBuffers
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()

-- | vkCmdBindVertexBuffers - Bind vertex buffers to a command buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command is recorded.
--
-- -   @firstBinding@ is the index of the first vertex input binding whose
--     state is updated by the command.
--
-- -   @bindingCount@ is the number of vertex input bindings whose state is
--     updated by the command.
--
-- -   @pBuffers@ is a pointer to an array of buffer handles.
--
-- -   @pOffsets@ is a pointer to an array of buffer offsets.
--
-- = Description
--
-- The values taken from elements i of @pBuffers@ and @pOffsets@ replace
-- the current state for the vertex input binding @firstBinding@ + i, for i
-- in [0, @bindingCount@). The vertex input binding is updated to start at
-- the offset indicated by @pOffsets@[i] from the start of the buffer
-- @pBuffers@[i]. All vertex input attributes that use each of these
-- bindings will use these updated addresses in their address calculations
-- for subsequent draw commands.
--
-- == Valid Usage
--
-- -   @firstBinding@ /must/ be less than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   The sum of @firstBinding@ and @bindingCount@ /must/ be less than or
--     equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   All elements of @pOffsets@ /must/ be less than the size of the
--     corresponding element in @pBuffers@
--
-- -   All elements of @pBuffers@ /must/ have been created with the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_VERTEX_BUFFER_BIT'
--     flag
--
-- -   Each element of @pBuffers@ that is non-sparse /must/ be bound
--     completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pBuffers@ /must/ be a valid pointer to an array of @bindingCount@
--     valid 'Graphics.Vulkan.Core10.Handles.Buffer' handles
--
-- -   @pOffsets@ /must/ be a valid pointer to an array of @bindingCount@
--     'Graphics.Vulkan.Core10.BaseType.DeviceSize' values
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   @bindingCount@ /must/ be greater than @0@
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and the
--     elements of @pBuffers@ /must/ have been created, allocated, or
--     retrieved from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize'
cmdBindVertexBuffers :: CommandBuffer -> ("firstBinding" ::: Word32) -> ("buffers" ::: Vector Buffer) -> ("offsets" ::: Vector DeviceSize) -> IO ()
cmdBindVertexBuffers commandBuffer firstBinding buffers offsets = evalContT $ do
  let vkCmdBindVertexBuffers' = mkVkCmdBindVertexBuffers (pVkCmdBindVertexBuffers (deviceCmds (commandBuffer :: CommandBuffer)))
  let pBuffersLength = Data.Vector.length $ (buffers)
  let pOffsetsLength = Data.Vector.length $ (offsets)
  lift $ unless (pOffsetsLength == pBuffersLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pOffsets and pBuffers must have the same length" Nothing Nothing
  pPBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (buffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (buffers)
  pPOffsets <- ContT $ allocaBytesAligned @DeviceSize ((Data.Vector.length (offsets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (offsets)
  lift $ vkCmdBindVertexBuffers' (commandBufferHandle (commandBuffer)) (firstBinding) ((fromIntegral pBuffersLength :: Word32)) (pPBuffers) (pPOffsets)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDraw
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

-- | vkCmdDraw - Draw primitives
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command is recorded.
--
-- -   @vertexCount@ is the number of vertices to draw.
--
-- -   @instanceCount@ is the number of instances to draw.
--
-- -   @firstVertex@ is the index of the first vertex to draw.
--
-- -   @firstInstance@ is the instance ID of the first instance to draw.
--
-- = Description
--
-- When the command is executed, primitives are assembled using the current
-- primitive topology and @vertexCount@ consecutive vertex indices with the
-- first @vertexIndex@ value equal to @firstVertex@. The primitives are
-- drawn @instanceCount@ times with @instanceIndex@ starting with
-- @firstInstance@ and increasing sequentially for each instance. The
-- assembled primitives execute the bound graphics pipeline.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is accessed using
--     atomic operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering, as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT'
--     with a reduction mode of either
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering together with minmax filtering,
--     as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.Image' created with a
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
--     of
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a descriptor set /must/ have been bound
--     to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for set /n/, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a push constant value /must/ have been
--     set for the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for push constants, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'cmdBindDescriptorSets', /must/ be valid if they are statically used
--     by the 'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command requires any dynamic state,
--     that state /must/ have been set for
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer', and done so after
--     any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   There /must/ not have been any calls to dynamic state setting
--     commands for any state not specified as dynamic in the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command, since that pipeline was
--     bound
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.Core10.Handles.Image' with a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of the type
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, any resource accessed by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the 'Graphics.Vulkan.Core10.Handles.RenderPass' member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Graphics.Vulkan.Core10.BaseType.TRUE' and the current
--     subpass has a depth\/stencil attachment, then that attachment /must/
--     have been created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, any resource written to by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be an
--     unprotected resource
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, pipeline stages other than the framebuffer-space and
--     compute stages in the 'Graphics.Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point /must/ not write to any
--     resource
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdDraw :: CommandBuffer -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ()
cmdDraw commandBuffer vertexCount instanceCount firstVertex firstInstance = do
  let vkCmdDraw' = mkVkCmdDraw (pVkCmdDraw (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdDraw' (commandBufferHandle (commandBuffer)) (vertexCount) (instanceCount) (firstVertex) (firstInstance)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndexed
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()

-- | vkCmdDrawIndexed - Issue an indexed draw into a command buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command is recorded.
--
-- -   @indexCount@ is the number of vertices to draw.
--
-- -   @instanceCount@ is the number of instances to draw.
--
-- -   @firstIndex@ is the base index within the index buffer.
--
-- -   @vertexOffset@ is the value added to the vertex index before
--     indexing into the vertex buffer.
--
-- -   @firstInstance@ is the instance ID of the first instance to draw.
--
-- = Description
--
-- When the command is executed, primitives are assembled using the current
-- primitive topology and @indexCount@ vertices whose indices are retrieved
-- from the index buffer. The index buffer is treated as an array of
-- tightly packed unsigned integers of size defined by the
-- 'cmdBindIndexBuffer'::'Graphics.Vulkan.Core10.Enums.IndexType.IndexType'
-- parameter with which the buffer was bound.
--
-- The first vertex index is at an offset of @firstIndex@ * @indexSize@ +
-- @offset@ within the bound index buffer, where @offset@ is the offset
-- specified by 'cmdBindIndexBuffer' and @indexSize@ is the byte size of
-- the type specified by
-- 'Graphics.Vulkan.Core10.Enums.IndexType.IndexType'. Subsequent index
-- values are retrieved from consecutive locations in the index buffer.
-- Indices are first compared to the primitive restart value, then zero
-- extended to 32 bits (if the
-- 'Graphics.Vulkan.Core10.Enums.IndexType.IndexType' is
-- 'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8_EXT' or
-- 'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16') and have
-- @vertexOffset@ added to them, before being supplied as the @vertexIndex@
-- value.
--
-- The primitives are drawn @instanceCount@ times with @instanceIndex@
-- starting with @firstInstance@ and increasing sequentially for each
-- instance. The assembled primitives execute the bound graphics pipeline.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is accessed using
--     atomic operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering, as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT'
--     with a reduction mode of either
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering together with minmax filtering,
--     as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.Image' created with a
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
--     of
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a descriptor set /must/ have been bound
--     to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for set /n/, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a push constant value /must/ have been
--     set for the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for push constants, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'cmdBindDescriptorSets', /must/ be valid if they are statically used
--     by the 'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command requires any dynamic state,
--     that state /must/ have been set for
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer', and done so after
--     any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   There /must/ not have been any calls to dynamic state setting
--     commands for any state not specified as dynamic in the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command, since that pipeline was
--     bound
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.Core10.Handles.Image' with a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of the type
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, any resource accessed by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the 'Graphics.Vulkan.Core10.Handles.RenderPass' member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Graphics.Vulkan.Core10.BaseType.TRUE' and the current
--     subpass has a depth\/stencil attachment, then that attachment /must/
--     have been created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, any resource written to by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be an
--     unprotected resource
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, pipeline stages other than the framebuffer-space and
--     compute stages in the 'Graphics.Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point /must/ not write to any
--     resource
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   (@indexSize@ * (@firstIndex@ + @indexCount@) + @offset@) /must/ be
--     less than or equal to the size of the bound index buffer, with
--     @indexSize@ being based on the type specified by
--     'Graphics.Vulkan.Core10.Enums.IndexType.IndexType', where the index
--     buffer, 'Graphics.Vulkan.Core10.Enums.IndexType.IndexType', and
--     @offset@ are specified via 'cmdBindIndexBuffer'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdDrawIndexed :: CommandBuffer -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ()
cmdDrawIndexed commandBuffer indexCount instanceCount firstIndex vertexOffset firstInstance = do
  let vkCmdDrawIndexed' = mkVkCmdDrawIndexed (pVkCmdDrawIndexed (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdDrawIndexed' (commandBufferHandle (commandBuffer)) (indexCount) (instanceCount) (firstIndex) (vertexOffset) (firstInstance)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirect
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- | vkCmdDrawIndirect - Issue an indirect draw into a command buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command is recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' is the buffer containing
--     draw parameters.
--
-- -   @offset@ is the byte offset into
--     'Graphics.Vulkan.Core10.Handles.Buffer' where parameters begin.
--
-- -   @drawCount@ is the number of draws to execute, and /can/ be zero.
--
-- -   @stride@ is the byte stride between successive sets of draw
--     parameters.
--
-- = Description
--
-- 'cmdDrawIndirect' behaves similarly to 'cmdDraw' except that the
-- parameters are read by the device from a buffer during execution.
-- @drawCount@ draws are executed by the command, with parameters taken
-- from 'Graphics.Vulkan.Core10.Handles.Buffer' starting at @offset@ and
-- increasing by @stride@ bytes for each successive draw. The parameters of
-- each draw are encoded in an array of
-- 'Graphics.Vulkan.Core10.OtherTypes.DrawIndirectCommand' structures. If
-- @drawCount@ is less than or equal to one, @stride@ is ignored.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is accessed using
--     atomic operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering, as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT'
--     with a reduction mode of either
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering together with minmax filtering,
--     as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.Image' created with a
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
--     of
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a descriptor set /must/ have been bound
--     to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for set /n/, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a push constant value /must/ have been
--     set for the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for push constants, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'cmdBindDescriptorSets', /must/ be valid if they are statically used
--     by the 'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command requires any dynamic state,
--     that state /must/ have been set for
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer', and done so after
--     any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   There /must/ not have been any calls to dynamic state setting
--     commands for any state not specified as dynamic in the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command, since that pipeline was
--     bound
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.Core10.Handles.Image' with a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of the type
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, any resource accessed by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the 'Graphics.Vulkan.Core10.Handles.RenderPass' member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Graphics.Vulkan.Core10.BaseType.TRUE' and the current
--     subpass has a depth\/stencil attachment, then that attachment /must/
--     have been created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ have been created
--     with the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ not be a
--     protected command buffer
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiDrawIndirect multi-draw indirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   @drawCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, all the @firstInstance@ members of the
--     'Graphics.Vulkan.Core10.OtherTypes.DrawIndirectCommand' structures
--     accessed by this command /must/ be @0@
--
-- -   If @drawCount@ is greater than @1@, @stride@ /must/ be a multiple of
--     @4@ and /must/ be greater than or equal to
--     @sizeof@('Graphics.Vulkan.Core10.OtherTypes.DrawIndirectCommand')
--
-- -   If @drawCount@ is equal to @1@, (@offset@ +
--     @sizeof@('Graphics.Vulkan.Core10.OtherTypes.DrawIndirectCommand'))
--     /must/ be less than or equal to the size of
--     'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   If @drawCount@ is greater than @1@, (@stride@ × (@drawCount@ - 1) +
--     @offset@ +
--     @sizeof@('Graphics.Vulkan.Core10.OtherTypes.DrawIndirectCommand'))
--     /must/ be less than or equal to the size of
--     'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.Buffer', and
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize'
cmdDrawIndirect :: CommandBuffer -> Buffer -> ("offset" ::: DeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
cmdDrawIndirect commandBuffer buffer offset drawCount stride = do
  let vkCmdDrawIndirect' = mkVkCmdDrawIndirect (pVkCmdDrawIndirect (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdDrawIndirect' (commandBufferHandle (commandBuffer)) (buffer) (offset) (drawCount) (stride)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndexedIndirect
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- | vkCmdDrawIndexedIndirect - Perform an indexed indirect draw
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command is recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' is the buffer containing
--     draw parameters.
--
-- -   @offset@ is the byte offset into
--     'Graphics.Vulkan.Core10.Handles.Buffer' where parameters begin.
--
-- -   @drawCount@ is the number of draws to execute, and /can/ be zero.
--
-- -   @stride@ is the byte stride between successive sets of draw
--     parameters.
--
-- = Description
--
-- 'cmdDrawIndexedIndirect' behaves similarly to 'cmdDrawIndexed' except
-- that the parameters are read by the device from a buffer during
-- execution. @drawCount@ draws are executed by the command, with
-- parameters taken from 'Graphics.Vulkan.Core10.Handles.Buffer' starting
-- at @offset@ and increasing by @stride@ bytes for each successive draw.
-- The parameters of each draw are encoded in an array of
-- 'Graphics.Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand'
-- structures. If @drawCount@ is less than or equal to one, @stride@ is
-- ignored.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is accessed using
--     atomic operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering, as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT'
--     with a reduction mode of either
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering together with minmax filtering,
--     as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.Image' created with a
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
--     of
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a descriptor set /must/ have been bound
--     to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for set /n/, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a push constant value /must/ have been
--     set for the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for push constants, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'cmdBindDescriptorSets', /must/ be valid if they are statically used
--     by the 'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command requires any dynamic state,
--     that state /must/ have been set for
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer', and done so after
--     any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   There /must/ not have been any calls to dynamic state setting
--     commands for any state not specified as dynamic in the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command, since that pipeline was
--     bound
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.Core10.Handles.Image' with a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of the type
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, any resource accessed by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the 'Graphics.Vulkan.Core10.Handles.RenderPass' member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to
--     'Graphics.Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Graphics.Vulkan.Core10.BaseType.TRUE' and the current
--     subpass has a depth\/stencil attachment, then that attachment /must/
--     have been created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ have been created
--     with the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ not be a
--     protected command buffer
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiDrawIndirect multi-draw indirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   @drawCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   If @drawCount@ is greater than @1@, @stride@ /must/ be a multiple of
--     @4@ and /must/ be greater than or equal to
--     @sizeof@('Graphics.Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand')
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, all the @firstInstance@ members of the
--     'Graphics.Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand'
--     structures accessed by this command /must/ be @0@
--
-- -   If @drawCount@ is equal to @1@, (@offset@ +
--     @sizeof@('Graphics.Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand'))
--     /must/ be less than or equal to the size of
--     'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   If @drawCount@ is greater than @1@, (@stride@ × (@drawCount@ - 1) +
--     @offset@ +
--     @sizeof@('Graphics.Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand'))
--     /must/ be less than or equal to the size of
--     'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-drawIndirectCount drawIndirectCount>
--     is not enabled this function /must/ not be used
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.Buffer', and
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize'
cmdDrawIndexedIndirect :: CommandBuffer -> Buffer -> ("offset" ::: DeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
cmdDrawIndexedIndirect commandBuffer buffer offset drawCount stride = do
  let vkCmdDrawIndexedIndirect' = mkVkCmdDrawIndexedIndirect (pVkCmdDrawIndexedIndirect (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdDrawIndexedIndirect' (commandBufferHandle (commandBuffer)) (buffer) (offset) (drawCount) (stride)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatch
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> IO ()

-- | vkCmdDispatch - Dispatch compute work items
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @groupCountX@ is the number of local workgroups to dispatch in the X
--     dimension.
--
-- -   @groupCountY@ is the number of local workgroups to dispatch in the Y
--     dimension.
--
-- -   @groupCountZ@ is the number of local workgroups to dispatch in the Z
--     dimension.
--
-- = Description
--
-- When the command is executed, a global workgroup consisting of
-- @groupCountX@ × @groupCountY@ × @groupCountZ@ local workgroups is
-- assembled.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is accessed using
--     atomic operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering, as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT'
--     with a reduction mode of either
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering together with minmax filtering,
--     as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.Image' created with a
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
--     of
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a descriptor set /must/ have been bound
--     to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for set /n/, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a push constant value /must/ have been
--     set for the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for push constants, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'cmdBindDescriptorSets', /must/ be valid if they are statically used
--     by the 'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command requires any dynamic state,
--     that state /must/ have been set for
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer', and done so after
--     any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   There /must/ not have been any calls to dynamic state setting
--     commands for any state not specified as dynamic in the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command, since that pipeline was
--     bound
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.Core10.Handles.Image' with a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of the type
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, any resource accessed by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, any resource written to by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be an
--     unprotected resource
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, pipeline stages other than the framebuffer-space and
--     compute stages in the 'Graphics.Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point /must/ not write to any
--     resource
--
-- -   @groupCountX@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   @groupCountY@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   @groupCountZ@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               | Compute                                                                                                                             |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdDispatch :: CommandBuffer -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
cmdDispatch commandBuffer groupCountX groupCountY groupCountZ = do
  let vkCmdDispatch' = mkVkCmdDispatch (pVkCmdDispatch (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdDispatch' (commandBufferHandle (commandBuffer)) (groupCountX) (groupCountY) (groupCountZ)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchIndirect
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> IO ()

-- | vkCmdDispatchIndirect - Dispatch compute work items using indirect
-- parameters
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' is the buffer containing
--     dispatch parameters.
--
-- -   @offset@ is the byte offset into
--     'Graphics.Vulkan.Core10.Handles.Buffer' where parameters begin.
--
-- = Description
--
-- 'cmdDispatchIndirect' behaves similarly to 'cmdDispatch' except that the
-- parameters are read by the device from a buffer during execution. The
-- parameters of the dispatch are encoded in a
-- 'Graphics.Vulkan.Core10.OtherTypes.DispatchIndirectCommand' structure
-- taken from 'Graphics.Vulkan.Core10.Handles.Buffer' starting at @offset@.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is accessed using
--     atomic operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   If a 'Graphics.Vulkan.Core10.Handles.ImageView' is sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering, as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.ImageView' being sampled with
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT'
--     with a reduction mode of either
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Graphics.Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.ImageViewType' and
--     format that supports cubic filtering together with minmax filtering,
--     as specified by
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Graphics.Vulkan.Core10.Handles.Image' created with a
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
--     of
--     'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a descriptor set /must/ have been bound
--     to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for set /n/, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a push constant value /must/ have been
--     set for the same pipeline bind point, with a
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' that is compatible
--     for push constants, with the
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Graphics.Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'cmdBindDescriptorSets', /must/ be valid if they are statically used
--     by the 'Graphics.Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command requires any dynamic state,
--     that state /must/ have been set for
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer', and done so after
--     any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   There /must/ not have been any calls to dynamic state setting
--     commands for any state not specified as dynamic in the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command, since that pipeline was
--     bound
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.Core10.Handles.Image' with a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of the type
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, any resource accessed by the
--     'Graphics.Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ have been created
--     with the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ not be a
--     protected command buffer
--
-- -   The sum of @offset@ and the size of
--     'Graphics.Vulkan.Core10.OtherTypes.DispatchIndirectCommand' /must/
--     be less than or equal to the size of
--     'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.Buffer', and
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               | Compute                                                                                                                             |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize'
cmdDispatchIndirect :: CommandBuffer -> Buffer -> ("offset" ::: DeviceSize) -> IO ()
cmdDispatchIndirect commandBuffer buffer offset = do
  let vkCmdDispatchIndirect' = mkVkCmdDispatchIndirect (pVkCmdDispatchIndirect (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdDispatchIndirect' (commandBufferHandle (commandBuffer)) (buffer) (offset)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> Buffer -> Word32 -> Ptr BufferCopy -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> Buffer -> Word32 -> Ptr BufferCopy -> IO ()

-- | vkCmdCopyBuffer - Copy data between buffer regions
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @srcBuffer@ is the source buffer.
--
-- -   @dstBuffer@ is the destination buffer.
--
-- -   @regionCount@ is the number of regions to copy.
--
-- -   @pRegions@ is a pointer to an array of 'BufferCopy' structures
--     specifying the regions to copy.
--
-- = Description
--
-- Each region in @pRegions@ is copied from the source buffer to the same
-- region of the destination buffer. @srcBuffer@ and @dstBuffer@ /can/ be
-- the same buffer or alias the same memory, but the resulting values are
-- undefined if the copy regions overlap in memory.
--
-- == Valid Usage
--
-- -   The @srcOffset@ member of each element of @pRegions@ /must/ be less
--     than the size of @srcBuffer@
--
-- -   The @dstOffset@ member of each element of @pRegions@ /must/ be less
--     than the size of @dstBuffer@
--
-- -   The @size@ member of each element of @pRegions@ /must/ be less than
--     or equal to the size of @srcBuffer@ minus @srcOffset@
--
-- -   The @size@ member of each element of @pRegions@ /must/ be less than
--     or equal to the size of @dstBuffer@ minus @dstOffset@
--
-- -   The union of the source regions, and the union of the destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   @srcBuffer@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   If @srcBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @dstBuffer@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @srcBuffer@ /must/ not be a protected buffer
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @dstBuffer@ /must/ not be a protected buffer
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, then @dstBuffer@ /must/ not be an unprotected buffer
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @srcBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   @dstBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid 'BufferCopy' structures
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support transfer, graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', @dstBuffer@,
--     and @srcBuffer@ /must/ have been created, allocated, or retrieved
--     from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer', 'BufferCopy',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdCopyBuffer :: CommandBuffer -> ("srcBuffer" ::: Buffer) -> ("dstBuffer" ::: Buffer) -> ("regions" ::: Vector BufferCopy) -> IO ()
cmdCopyBuffer commandBuffer srcBuffer dstBuffer regions = evalContT $ do
  let vkCmdCopyBuffer' = mkVkCmdCopyBuffer (pVkCmdCopyBuffer (deviceCmds (commandBuffer :: CommandBuffer)))
  pPRegions <- ContT $ allocaBytesAligned @BufferCopy ((Data.Vector.length (regions)) * 24) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPRegions `plusPtr` (24 * (i)) :: Ptr BufferCopy) (e) . ($ ())) (regions)
  lift $ vkCmdCopyBuffer' (commandBufferHandle (commandBuffer)) (srcBuffer) (dstBuffer) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageCopy -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageCopy -> IO ()

-- | vkCmdCopyImage - Copy data between images
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @srcImage@ is the source image.
--
-- -   @srcImageLayout@ is the current layout of the source image
--     subresource.
--
-- -   @dstImage@ is the destination image.
--
-- -   @dstImageLayout@ is the current layout of the destination image
--     subresource.
--
-- -   @regionCount@ is the number of regions to copy.
--
-- -   @pRegions@ is a pointer to an array of 'ImageCopy' structures
--     specifying the regions to copy.
--
-- = Description
--
-- Each region in @pRegions@ is copied from the source image to the same
-- region of the destination image. @srcImage@ and @dstImage@ /can/ be the
-- same image or alias the same memory.
--
-- The formats of @srcImage@ and @dstImage@ /must/ be compatible. Formats
-- are compatible if they share the same class, as shown in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility Compatible Formats>
-- table. Depth\/stencil formats /must/ match exactly.
--
-- If the format of @srcImage@ or @dstImage@ is a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
-- regions of each plane to be copied /must/ be specified separately using
-- the @srcSubresource@ and @dstSubresource@ members of the 'ImageCopy'
-- structure. In this case, the @aspectMask@ of the @srcSubresource@ or
-- @dstSubresource@ that refers to the multi-planar image /must/ be
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
-- or
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'.
-- For the purposes of 'cmdCopyImage', each plane of a multi-planar image
-- is treated as having the format listed in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes>
-- for the plane identified by the @aspectMask@ of the corresponding
-- subresource. This applies both to
-- 'Graphics.Vulkan.Core10.Enums.Format.Format' and to coordinates used in
-- the copy, which correspond to texels in the /plane/ rather than how
-- these texels map to coordinates in the image as a whole.
--
-- Note
--
-- For example, the
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
-- plane of a
-- 'Graphics.Vulkan.Core10.Enums.Format.FORMAT_G8_B8R8_2PLANE_420_UNORM'
-- image is compatible with an image of format
-- 'Graphics.Vulkan.Core10.Enums.Format.FORMAT_R8G8_UNORM' and (less
-- usefully) with the
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
-- plane of an image of format
-- 'Graphics.Vulkan.Core10.Enums.Format.FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16',
-- as each texel is 2 bytes in size.
--
-- 'cmdCopyImage' allows copying between /size-compatible/ compressed and
-- uncompressed internal formats. Formats are size-compatible if the texel
-- block size of the uncompressed format is equal to the texel block size
-- of the compressed format. Such a copy does not perform on-the-fly
-- compression or decompression. When copying from an uncompressed format
-- to a compressed format, each texel of uncompressed data of the source
-- image is copied as a raw value to the corresponding compressed texel
-- block of the destination image. When copying from a compressed format to
-- an uncompressed format, each compressed texel block of the source image
-- is copied as a raw value to the corresponding texel of uncompressed data
-- in the destination image. Thus, for example, it is legal to copy between
-- a 128-bit uncompressed format and a compressed format which has a
-- 128-bit sized compressed texel block representing 4×4 texels (using 8
-- bits per texel), or between a 64-bit uncompressed format and a
-- compressed format which has a 64-bit sized compressed texel block
-- representing 4×4 texels (using 4 bits per texel).
--
-- When copying between compressed and uncompressed formats the @extent@
-- members represent the texel dimensions of the source image and not the
-- destination. When copying from a compressed image to an uncompressed
-- image the image texel dimensions written to the uncompressed image will
-- be source extent divided by the compressed texel block dimensions. When
-- copying from an uncompressed image to a compressed image the image texel
-- dimensions written to the compressed image will be the source extent
-- multiplied by the compressed texel block dimensions. In both cases the
-- number of bytes read and the number of bytes written will be identical.
--
-- Copying to or from block-compressed images is typically done in
-- multiples of the compressed texel block size. For this reason the
-- @extent@ /must/ be a multiple of the compressed texel block dimension.
-- There is one exception to this rule which is /required/ to handle
-- compressed images created with dimensions that are not a multiple of the
-- compressed texel block dimensions: if the @srcImage@ is compressed,
-- then:
--
-- -   If @extent.width@ is not a multiple of the compressed texel block
--     width, then (@extent.width@ + @srcOffset.x@) /must/ equal the image
--     subresource width.
--
-- -   If @extent.height@ is not a multiple of the compressed texel block
--     height, then (@extent.height@ + @srcOffset.y@) /must/ equal the
--     image subresource height.
--
-- -   If @extent.depth@ is not a multiple of the compressed texel block
--     depth, then (@extent.depth@ + @srcOffset.z@) /must/ equal the image
--     subresource depth.
--
-- Similarly, if the @dstImage@ is compressed, then:
--
-- -   If @extent.width@ is not a multiple of the compressed texel block
--     width, then (@extent.width@ + @dstOffset.x@) /must/ equal the image
--     subresource width.
--
-- -   If @extent.height@ is not a multiple of the compressed texel block
--     height, then (@extent.height@ + @dstOffset.y@) /must/ equal the
--     image subresource height.
--
-- -   If @extent.depth@ is not a multiple of the compressed texel block
--     depth, then (@extent.depth@ + @dstOffset.z@) /must/ equal the image
--     subresource depth.
--
-- This allows the last compressed texel block of the image in each
-- non-multiple dimension to be included as a source or destination of the
-- copy.
--
-- “@_422@” image formats that are not
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
-- are treated as having a 2×1 compressed texel block for the purposes of
-- these rules.
--
-- 'cmdCopyImage' /can/ be used to copy image data between multisample
-- images, but both images /must/ have the same number of samples.
--
-- == Valid Usage
--
-- -   The source region specified by each element of @pRegions@ /must/ be
--     a region that is contained within @srcImage@ if the @srcImage@’s
--     'Graphics.Vulkan.Core10.Enums.Format.Format' is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     and /must/ be a region that is contained within the plane being
--     copied if the @srcImage@’s
--     'Graphics.Vulkan.Core10.Enums.Format.Format' is a multi-planar
--     format
--
-- -   The destination region specified by each element of @pRegions@
--     /must/ be a region that is contained within @dstImage@ if the
--     @dstImage@’s 'Graphics.Vulkan.Core10.Enums.Format.Format' is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     and /must/ be a region that is contained within the plane being
--     copied to if the @dstImage@’s
--     'Graphics.Vulkan.Core10.Enums.Format.Format' is a multi-planar
--     format
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_SRC_BIT'.
--
-- -   @srcImage@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   If @srcImage@ is non-sparse then the image or /disjoint/ plane to be
--     copied /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a 'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   @srcImageLayout@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'.
--
-- -   @dstImage@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstImage@ is non-sparse then the image or /disjoint/ plane that
--     is the destination of the copy /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a 'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   @dstImageLayout@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.Format.Format' of each of
--     @srcImage@ and @dstImage@ is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     the 'Graphics.Vulkan.Core10.Enums.Format.Format' of each of
--     @srcImage@ and @dstImage@ /must/ be compatible, as defined
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-images-format-compatibility above>
--
-- -   In a copy to or from a plane of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image>,
--     the 'Graphics.Vulkan.Core10.Enums.Format.Format' of the image and
--     plane /must/ be compatible according to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes the description of compatible planes>
--     for the plane being copied
--
-- -   When a copy is performed to or from an image with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     the @aspectMask@ of the @srcSubresource@ and\/or @dstSubresource@
--     that refers to the multi-planar image /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--     (with
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--     valid only for a 'Graphics.Vulkan.Core10.Enums.Format.Format' with
--     three planes)
--
-- -   The sample count of @srcImage@ and @dstImage@ /must/ match
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @srcImage@ /must/ not be a protected image
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @dstImage@ /must/ not be a protected image
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, then @dstImage@ /must/ not be an unprotected image
--
-- -   The @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was
--     created
--
-- -   The @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was
--     created
--
-- -   The @srcOffset@ and @extent@ members of each element of @pRegions@
--     /must/ respect the image transfer granularity requirements of
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer'’s command pool’s
--     queue family, as described in
--     'Graphics.Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   The @dstOffset@ and @extent@ members of each element of @pRegions@
--     /must/ respect the image transfer granularity requirements of
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer'’s command pool’s
--     queue family, as described in
--     'Graphics.Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   @dstImage@ and @srcImage@ /must/ not have been created with
--     'Graphics.Vulkan.Core10.BaseType.Flags' containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @srcImage@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image'
--     handle
--
-- -   @srcImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   @dstImage@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image'
--     handle
--
-- -   @dstImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid 'ImageCopy' structures
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support transfer, graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', @dstImage@,
--     and @srcImage@ /must/ have been created, allocated, or retrieved
--     from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.Image', 'ImageCopy',
-- 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout'
cmdCopyImage :: CommandBuffer -> ("srcImage" ::: Image) -> ("srcImageLayout" ::: ImageLayout) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("regions" ::: Vector ImageCopy) -> IO ()
cmdCopyImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout regions = evalContT $ do
  let vkCmdCopyImage' = mkVkCmdCopyImage (pVkCmdCopyImage (deviceCmds (commandBuffer :: CommandBuffer)))
  pPRegions <- ContT $ allocaBytesAligned @ImageCopy ((Data.Vector.length (regions)) * 68) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPRegions `plusPtr` (68 * (i)) :: Ptr ImageCopy) (e) . ($ ())) (regions)
  lift $ vkCmdCopyImage' (commandBufferHandle (commandBuffer)) (srcImage) (srcImageLayout) (dstImage) (dstImageLayout) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBlitImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageBlit -> Filter -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageBlit -> Filter -> IO ()

-- | vkCmdBlitImage - Copy regions of an image, potentially performing format
-- conversion,
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @srcImage@ is the source image.
--
-- -   @srcImageLayout@ is the layout of the source image subresources for
--     the blit.
--
-- -   @dstImage@ is the destination image.
--
-- -   @dstImageLayout@ is the layout of the destination image subresources
--     for the blit.
--
-- -   @regionCount@ is the number of regions to blit.
--
-- -   @pRegions@ is a pointer to an array of 'ImageBlit' structures
--     specifying the regions to blit.
--
-- -   'Graphics.Vulkan.Core10.Enums.Filter.Filter' is a
--     'Graphics.Vulkan.Core10.Enums.Filter.Filter' specifying the filter
--     to apply if the blits require scaling.
--
-- = Description
--
-- 'cmdBlitImage' /must/ not be used for multisampled source or destination
-- images. Use 'cmdResolveImage' for this purpose.
--
-- As the sizes of the source and destination extents /can/ differ in any
-- dimension, texels in the source extent are scaled and filtered to the
-- destination extent. Scaling occurs via the following operations:
--
-- -   For each destination texel, the integer coordinate of that texel is
--     converted to an unnormalized texture coordinate, using the effective
--     inverse of the equations described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-unnormalized-to-integer unnormalized to integer conversion>:
--
--     -   ubase = i + ½
--
--     -   vbase = j + ½
--
--     -   wbase = k + ½
--
-- -   These base coordinates are then offset by the first destination
--     offset:
--
--     -   uoffset = ubase - xdst0
--
--     -   voffset = vbase - ydst0
--
--     -   woffset = wbase - zdst0
--
--     -   aoffset = a - @baseArrayCount@dst
--
-- -   The scale is determined from the source and destination regions, and
--     applied to the offset coordinates:
--
--     -   scale_u = (xsrc1 - xsrc0) \/ (xdst1 - xdst0)
--
--     -   scale_v = (ysrc1 - ysrc0) \/ (ydst1 - ydst0)
--
--     -   scale_w = (zsrc1 - zsrc0) \/ (zdst1 - zdst0)
--
--     -   uscaled = uoffset * scaleu
--
--     -   vscaled = voffset * scalev
--
--     -   wscaled = woffset * scalew
--
-- -   Finally the source offset is added to the scaled coordinates, to
--     determine the final unnormalized coordinates used to sample from
--     @srcImage@:
--
--     -   u = uscaled + xsrc0
--
--     -   v = vscaled + ysrc0
--
--     -   w = wscaled + zsrc0
--
--     -   q = @mipLevel@
--
--     -   a = aoffset + @baseArrayCount@src
--
-- These coordinates are used to sample from the source image, as described
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures Image Operations chapter>,
-- with the filter mode equal to that of
-- 'Graphics.Vulkan.Core10.Enums.Filter.Filter', a mipmap mode of
-- 'Graphics.Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_NEAREST'
-- and an address mode of
-- 'Graphics.Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
-- Implementations /must/ clamp at the edge of the source image, and /may/
-- additionally clamp to the edge of the source region.
--
-- Note
--
-- Due to allowable rounding errors in the generation of the source texture
-- coordinates, it is not always possible to guarantee exactly which source
-- texels will be sampled for a given blit. As rounding errors are
-- implementation dependent, the exact results of a blitting operation are
-- also implementation dependent.
--
-- Blits are done layer by layer starting with the @baseArrayLayer@ member
-- of @srcSubresource@ for the source and @dstSubresource@ for the
-- destination. @layerCount@ layers are blitted to the destination image.
--
-- 3D textures are blitted slice by slice. Slices in the source region
-- bounded by @srcOffsets@[0].z and @srcOffsets@[1].z are copied to slices
-- in the destination region bounded by @dstOffsets@[0].z and
-- @dstOffsets@[1].z. For each destination slice, a source __z__ coordinate
-- is linearly interpolated between @srcOffsets@[0].z and
-- @srcOffsets@[1].z. If the 'Graphics.Vulkan.Core10.Enums.Filter.Filter'
-- parameter is 'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR' then
-- the value sampled from the source image is taken by doing linear
-- filtering using the interpolated __z__ coordinate. If
-- 'Graphics.Vulkan.Core10.Enums.Filter.Filter' parameter is
-- 'Graphics.Vulkan.Core10.Enums.Filter.FILTER_NEAREST' then the value
-- sampled from the source image is taken from the single nearest slice,
-- with an implementation-dependent arithmetic rounding mode.
--
-- The following filtering and conversion rules apply:
--
-- -   Integer formats /can/ only be converted to other integer formats
--     with the same signedness.
--
-- -   No format conversion is supported between depth\/stencil images. The
--     formats /must/ match.
--
-- -   Format conversions on unorm, snorm, unscaled and packed float
--     formats of the copied aspect of the image are performed by first
--     converting the pixels to float values.
--
-- -   For sRGB source formats, nonlinear RGB values are converted to
--     linear representation prior to filtering.
--
-- -   After filtering, the float values are first clamped and then cast to
--     the destination image format. In case of sRGB destination format,
--     linear RGB values are converted to nonlinear representation before
--     writing the pixel to the image.
--
-- Signed and unsigned integers are converted by first clamping to the
-- representable range of the destination format, then casting the value.
--
-- == Valid Usage
--
-- -   The source region specified by each element of @pRegions@ /must/ be
--     a region that is contained within @srcImage@
--
-- -   The destination region specified by each element of @pRegions@
--     /must/ be a region that is contained within @dstImage@
--
-- -   The union of all destination regions, specified by the elements of
--     @pRegions@, /must/ not overlap in memory with any texel that /may/
--     be sampled during the blit operation
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'.
--
-- -   @srcImage@ /must/ not use a format listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>
--
-- -   @srcImage@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   If @srcImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a 'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   @srcImageLayout@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_DST_BIT'.
--
-- -   @dstImage@ /must/ not use a format listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>
--
-- -   @dstImage@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a 'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   @dstImageLayout@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   The sample count of @srcImage@ and @dstImage@ /must/ both be equal
--     to
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   If either of @srcImage@ or @dstImage@ was created with a signed
--     integer 'Graphics.Vulkan.Core10.Enums.Format.Format', the other
--     /must/ also have been created with a signed integer
--     'Graphics.Vulkan.Core10.Enums.Format.Format'
--
-- -   If either of @srcImage@ or @dstImage@ was created with an unsigned
--     integer 'Graphics.Vulkan.Core10.Enums.Format.Format', the other
--     /must/ also have been created with an unsigned integer
--     'Graphics.Vulkan.Core10.Enums.Format.Format'
--
-- -   If either of @srcImage@ or @dstImage@ was created with a
--     depth\/stencil format, the other /must/ have exactly the same format
--
-- -   If @srcImage@ was created with a depth\/stencil format,
--     'Graphics.Vulkan.Core10.Enums.Filter.Filter' /must/ be
--     'Graphics.Vulkan.Core10.Enums.Filter.FILTER_NEAREST'
--
-- -   @srcImage@ /must/ have been created with a @samples@ value of
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   @dstImage@ /must/ have been created with a @samples@ value of
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   If 'Graphics.Vulkan.Core10.Enums.Filter.Filter' is
--     'Graphics.Vulkan.Core10.Enums.Filter.FILTER_LINEAR', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'.
--
-- -   If 'Graphics.Vulkan.Core10.Enums.Filter.Filter' is
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'.
--
-- -   If 'Graphics.Vulkan.Core10.Enums.Filter.Filter' is
--     'Graphics.Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT',
--     @srcImage@ /must/ have a
--     'Graphics.Vulkan.Core10.Enums.ImageType.ImageType' of
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @srcImage@ /must/ not be a protected image
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @dstImage@ /must/ not be a protected image
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, then @dstImage@ /must/ not be an unprotected image
--
-- -   The @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was
--     created
--
-- -   The @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was
--     created
--
-- -   @dstImage@ and @srcImage@ /must/ not have been created with
--     'Graphics.Vulkan.Core10.BaseType.Flags' containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @srcImage@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image'
--     handle
--
-- -   @srcImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   @dstImage@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image'
--     handle
--
-- -   @dstImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid 'ImageBlit' structures
--
-- -   'Graphics.Vulkan.Core10.Enums.Filter.Filter' /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.Filter.Filter' value
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', @dstImage@,
--     and @srcImage@ /must/ have been created, allocated, or retrieved
--     from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Enums.Filter.Filter',
-- 'Graphics.Vulkan.Core10.Handles.Image', 'ImageBlit',
-- 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout'
cmdBlitImage :: CommandBuffer -> ("srcImage" ::: Image) -> ("srcImageLayout" ::: ImageLayout) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("regions" ::: Vector ImageBlit) -> Filter -> IO ()
cmdBlitImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout regions filter' = evalContT $ do
  let vkCmdBlitImage' = mkVkCmdBlitImage (pVkCmdBlitImage (deviceCmds (commandBuffer :: CommandBuffer)))
  pPRegions <- ContT $ allocaBytesAligned @ImageBlit ((Data.Vector.length (regions)) * 80) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPRegions `plusPtr` (80 * (i)) :: Ptr ImageBlit) (e) . ($ ())) (regions)
  lift $ vkCmdBlitImage' (commandBufferHandle (commandBuffer)) (srcImage) (srcImageLayout) (dstImage) (dstImageLayout) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions) (filter')
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBufferToImage
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> Image -> ImageLayout -> Word32 -> Ptr BufferImageCopy -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> Image -> ImageLayout -> Word32 -> Ptr BufferImageCopy -> IO ()

-- | vkCmdCopyBufferToImage - Copy data from a buffer into an image
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @srcBuffer@ is the source buffer.
--
-- -   @dstImage@ is the destination image.
--
-- -   @dstImageLayout@ is the layout of the destination image subresources
--     for the copy.
--
-- -   @regionCount@ is the number of regions to copy.
--
-- -   @pRegions@ is a pointer to an array of 'BufferImageCopy' structures
--     specifying the regions to copy.
--
-- = Description
--
-- Each region in @pRegions@ is copied from the specified region of the
-- source buffer to the specified region of the destination image.
--
-- If the format of @dstImage@ is a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>),
-- regions of each plane to be a target of a copy /must/ be specified
-- separately using the @pRegions@ member of the 'BufferImageCopy'
-- structure. In this case, the @aspectMask@ of
-- 'Graphics.Vulkan.Core10.Image.ImageSubresource' /must/ be
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
-- or
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'.
-- For the purposes of 'cmdCopyBufferToImage', each plane of a multi-planar
-- image is treated as having the format listed in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes>
-- for the plane identified by the @aspectMask@ of the corresponding
-- subresource. This applies both to
-- 'Graphics.Vulkan.Core10.Enums.Format.Format' and to coordinates used in
-- the copy, which correspond to texels in the /plane/ rather than how
-- these texels map to coordinates in the image as a whole.
--
-- == Valid Usage
--
-- -   @srcBuffer@ /must/ be large enough to contain all buffer locations
--     that are accessed according to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   The image region specified by each element of @pRegions@ /must/ be a
--     region that is contained within @dstImage@ if the @dstImage@’s
--     'Graphics.Vulkan.Core10.Enums.Format.Format' is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     and /must/ be a region that is contained within the plane being
--     copied to if the @dstImage@’s
--     'Graphics.Vulkan.Core10.Enums.Format.Format' is a multi-planar
--     format
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   @srcBuffer@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'.
--
-- -   If @srcBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @dstImage@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @dstImage@ /must/ have a sample count equal to
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a 'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   @dstImageLayout@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @srcBuffer@ /must/ not be a protected buffer
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @dstImage@ /must/ not be a protected image
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, then @dstImage@ /must/ not be an unprotected image
--
-- -   The @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was
--     created
--
-- -   The @imageSubresource.baseArrayLayer@ +
--     @imageSubresource.layerCount@ of each element of @pRegions@ /must/
--     be less than or equal to the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was
--     created
--
-- -   The @imageOffset@ and @imageExtent@ members of each element of
--     @pRegions@ /must/ respect the image transfer granularity
--     requirements of 'Graphics.Vulkan.Core10.Handles.CommandBuffer'’s
--     command pool’s queue family, as described in
--     'Graphics.Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   @dstImage@ /must/ not have been created with
--     'Graphics.Vulkan.Core10.BaseType.Flags' containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @srcBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   @dstImage@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image'
--     handle
--
-- -   @dstImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid 'BufferImageCopy' structures
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support transfer, graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', @dstImage@,
--     and @srcBuffer@ /must/ have been created, allocated, or retrieved
--     from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer', 'BufferImageCopy',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.Image',
-- 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout'
cmdCopyBufferToImage :: CommandBuffer -> ("srcBuffer" ::: Buffer) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("regions" ::: Vector BufferImageCopy) -> IO ()
cmdCopyBufferToImage commandBuffer srcBuffer dstImage dstImageLayout regions = evalContT $ do
  let vkCmdCopyBufferToImage' = mkVkCmdCopyBufferToImage (pVkCmdCopyBufferToImage (deviceCmds (commandBuffer :: CommandBuffer)))
  pPRegions <- ContT $ allocaBytesAligned @BufferImageCopy ((Data.Vector.length (regions)) * 56) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPRegions `plusPtr` (56 * (i)) :: Ptr BufferImageCopy) (e) . ($ ())) (regions)
  lift $ vkCmdCopyBufferToImage' (commandBufferHandle (commandBuffer)) (srcBuffer) (dstImage) (dstImageLayout) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImageToBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Buffer -> Word32 -> Ptr BufferImageCopy -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Buffer -> Word32 -> Ptr BufferImageCopy -> IO ()

-- | vkCmdCopyImageToBuffer - Copy image data into a buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @srcImage@ is the source image.
--
-- -   @srcImageLayout@ is the layout of the source image subresources for
--     the copy.
--
-- -   @dstBuffer@ is the destination buffer.
--
-- -   @regionCount@ is the number of regions to copy.
--
-- -   @pRegions@ is a pointer to an array of 'BufferImageCopy' structures
--     specifying the regions to copy.
--
-- = Description
--
-- Each region in @pRegions@ is copied from the specified region of the
-- source image to the specified region of the destination buffer.
--
-- If the 'Graphics.Vulkan.Core10.Enums.Format.Format' of @srcImage@ is a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
-- regions of each plane to be a source of a copy /must/ be specified
-- separately using the @pRegions@ member of the 'BufferImageCopy'
-- structure. In this case, the @aspectMask@ of
-- 'Graphics.Vulkan.Core10.Image.ImageSubresource' /must/ be
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
-- or
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'.
-- For the purposes of 'cmdCopyBufferToImage', each plane of a multi-planar
-- image is treated as having the format listed in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes>
-- for the plane identified by the @aspectMask@ of the corresponding
-- subresource. This applies both to
-- 'Graphics.Vulkan.Core10.Enums.Format.Format' and to coordinates used in
-- the copy, which correspond to texels in the /plane/ rather than how
-- these texels map to coordinates in the image as a whole.
--
-- == Valid Usage
--
-- -   The image region specified by each element of @pRegions@ /must/ be a
--     region that is contained within @srcImage@ if the @srcImage@’s
--     'Graphics.Vulkan.Core10.Enums.Format.Format' is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     and /must/ be a region that is contained within the plane being
--     copied if the @srcImage@’s
--     'Graphics.Vulkan.Core10.Enums.Format.Format' is a multi-planar
--     format
--
-- -   @dstBuffer@ /must/ be large enough to contain all buffer locations
--     that are accessed according to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_SRC_BIT'.
--
-- -   @srcImage@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   If @srcImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @srcImage@ /must/ have a sample count equal to
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a 'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   @srcImageLayout@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   @dstBuffer@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @srcImage@ /must/ not be a protected image
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @dstBuffer@ /must/ not be a protected buffer
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, then @dstBuffer@ /must/ not be an unprotected buffer
--
-- -   The @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @imageSubresource.baseArrayLayer@ +
--     @imageSubresource.layerCount@ of each element of @pRegions@ /must/
--     be less than or equal to the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @imageOffset@ and @imageExtent@ members of each element of
--     @pRegions@ /must/ respect the image transfer granularity
--     requirements of 'Graphics.Vulkan.Core10.Handles.CommandBuffer'’s
--     command pool’s queue family, as described in
--     'Graphics.Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   @srcImage@ /must/ not have been created with
--     'Graphics.Vulkan.Core10.BaseType.Flags' containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @srcImage@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image'
--     handle
--
-- -   @srcImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   @dstBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid 'BufferImageCopy' structures
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support transfer, graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', @dstBuffer@,
--     and @srcImage@ /must/ have been created, allocated, or retrieved
--     from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer', 'BufferImageCopy',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.Image',
-- 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout'
cmdCopyImageToBuffer :: CommandBuffer -> ("srcImage" ::: Image) -> ("srcImageLayout" ::: ImageLayout) -> ("dstBuffer" ::: Buffer) -> ("regions" ::: Vector BufferImageCopy) -> IO ()
cmdCopyImageToBuffer commandBuffer srcImage srcImageLayout dstBuffer regions = evalContT $ do
  let vkCmdCopyImageToBuffer' = mkVkCmdCopyImageToBuffer (pVkCmdCopyImageToBuffer (deviceCmds (commandBuffer :: CommandBuffer)))
  pPRegions <- ContT $ allocaBytesAligned @BufferImageCopy ((Data.Vector.length (regions)) * 56) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPRegions `plusPtr` (56 * (i)) :: Ptr BufferImageCopy) (e) . ($ ())) (regions)
  lift $ vkCmdCopyImageToBuffer' (commandBufferHandle (commandBuffer)) (srcImage) (srcImageLayout) (dstBuffer) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdUpdateBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> Ptr () -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> Ptr () -> IO ()

-- | vkCmdUpdateBuffer - Update a buffer’s contents from host memory
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @dstBuffer@ is a handle to the buffer to be updated.
--
-- -   @dstOffset@ is the byte offset into the buffer to start updating,
--     and /must/ be a multiple of 4.
--
-- -   @dataSize@ is the number of bytes to update, and /must/ be a
--     multiple of 4.
--
-- -   @pData@ is a pointer to the source data for the buffer update, and
--     /must/ be at least @dataSize@ bytes in size.
--
-- = Description
--
-- @dataSize@ /must/ be less than or equal to 65536 bytes. For larger
-- updates, applications /can/ use buffer to buffer
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-buffers copies>.
--
-- Note
--
-- Buffer updates performed with 'cmdUpdateBuffer' first copy the data into
-- command buffer memory when the command is recorded (which requires
-- additional storage and may incur an additional allocation), and then
-- copy the data from the command buffer into @dstBuffer@ when the command
-- is executed on a device.
--
-- The additional cost of this functionality compared to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-buffers buffer to buffer copies>
-- means it is only recommended for very small amounts of data, and is why
-- it is limited to only 65536 bytes.
--
-- Applications /can/ work around this by issuing multiple
-- 'cmdUpdateBuffer' commands to different ranges of the same buffer, but
-- it is strongly recommended that they /should/ not.
--
-- The source data is copied from the user pointer to the command buffer
-- when the command is called.
--
-- 'cmdUpdateBuffer' is only allowed outside of a render pass. This command
-- is treated as “transfer” operation, for the purposes of synchronization
-- barriers. The
-- 'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
-- /must/ be specified in @usage@ of
-- 'Graphics.Vulkan.Core10.Buffer.BufferCreateInfo' in order for the buffer
-- to be compatible with 'cmdUpdateBuffer'.
--
-- == Valid Usage
--
-- -   @dstOffset@ /must/ be less than the size of @dstBuffer@
--
-- -   @dataSize@ /must/ be less than or equal to the size of @dstBuffer@
--     minus @dstOffset@
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
-- -   @dataSize@ /must/ be less than or equal to @65536@
--
-- -   @dataSize@ /must/ be a multiple of @4@
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @dstBuffer@ /must/ not be a protected buffer
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, then @dstBuffer@ /must/ not be an unprotected buffer
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @dstBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support transfer, graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @dataSize@ /must/ be greater than @0@
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and
--     @dstBuffer@ /must/ have been created, allocated, or retrieved from
--     the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize'
cmdUpdateBuffer :: CommandBuffer -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> ("dataSize" ::: DeviceSize) -> ("data" ::: Ptr ()) -> IO ()
cmdUpdateBuffer commandBuffer dstBuffer dstOffset dataSize data' = do
  let vkCmdUpdateBuffer' = mkVkCmdUpdateBuffer (pVkCmdUpdateBuffer (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdUpdateBuffer' (commandBufferHandle (commandBuffer)) (dstBuffer) (dstOffset) (dataSize) (data')
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdFillBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> Word32 -> IO ()

-- | vkCmdFillBuffer - Fill a region of a buffer with a fixed value
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @dstBuffer@ is the buffer to be filled.
--
-- -   @dstOffset@ is the byte offset into the buffer at which to start
--     filling, and /must/ be a multiple of 4.
--
-- -   @size@ is the number of bytes to fill, and /must/ be either a
--     multiple of 4, or 'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE'
--     to fill the range from @offset@ to the end of the buffer. If
--     'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE' is used and the
--     remaining size of the buffer is not a multiple of 4, then the
--     nearest smaller multiple is used.
--
-- -   @data@ is the 4-byte word written repeatedly to the buffer to fill
--     @size@ bytes of data. The data word is written to memory according
--     to the host endianness.
--
-- = Description
--
-- 'cmdFillBuffer' is treated as “transfer” operation for the purposes of
-- synchronization barriers. The
-- 'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
-- /must/ be specified in @usage@ of
-- 'Graphics.Vulkan.Core10.Buffer.BufferCreateInfo' in order for the buffer
-- to be compatible with 'cmdFillBuffer'.
--
-- == Valid Usage
--
-- -   @dstOffset@ /must/ be less than the size of @dstBuffer@
--
-- -   @dstOffset@ /must/ be a multiple of @4@
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be
--     greater than @0@
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be
--     less than or equal to the size of @dstBuffer@ minus @dstOffset@
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be a
--     multiple of @4@
--
-- -   @dstBuffer@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @dstBuffer@ /must/ not be a protected buffer
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, then @dstBuffer@ /must/ not be an unprotected buffer
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @dstBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support transfer, graphics or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and
--     @dstBuffer@ /must/ have been created, allocated, or retrieved from
--     the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize'
cmdFillBuffer :: CommandBuffer -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> DeviceSize -> ("data" ::: Word32) -> IO ()
cmdFillBuffer commandBuffer dstBuffer dstOffset size data' = do
  let vkCmdFillBuffer' = mkVkCmdFillBuffer (pVkCmdFillBuffer (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdFillBuffer' (commandBufferHandle (commandBuffer)) (dstBuffer) (dstOffset) (size) (data')
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdClearColorImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Ptr ClearColorValue -> Word32 -> Ptr ImageSubresourceRange -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Ptr ClearColorValue -> Word32 -> Ptr ImageSubresourceRange -> IO ()

-- | vkCmdClearColorImage - Clear regions of a color image
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' is the image to be cleared.
--
-- -   'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' specifies the
--     current layout of the image subresource ranges to be cleared, and
--     /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'.
--
-- -   @pColor@ is a pointer to a
--     'Graphics.Vulkan.Core10.SharedTypes.ClearColorValue' structure
--     containing the values that the image subresource ranges will be
--     cleared to (see
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears-values>
--     below).
--
-- -   @rangeCount@ is the number of image subresource range structures in
--     @pRanges@.
--
-- -   @pRanges@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'
--     structures describing a range of mipmap levels, array layers, and
--     aspects to be cleared, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views Image Views>.
--
-- = Description
--
-- Each specified range in @pRanges@ is cleared to the value specified by
-- @pColor@.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of 'Graphics.Vulkan.Core10.Handles.Image' /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'.
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ not use a format
--     listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' /must/
--     specify the layout of the image subresource ranges of
--     'Graphics.Vulkan.Core10.Handles.Image' specified in @pRanges@ at the
--     time this command is executed on a
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   The
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'::@aspectMask@
--     members of the elements of the @pRanges@ array /must/ each only
--     include
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   The
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'::@baseMipLevel@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   For each 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'
--     element of @pRanges@, if the @levelCount@ member is not
--     'Graphics.Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS', then
--     @baseMipLevel@ + @levelCount@ /must/ be less than the @mipLevels@
--     specified in 'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   The
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'::@baseArrayLayer@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   For each 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'
--     element of @pRanges@, if the @layerCount@ member is not
--     'Graphics.Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', then
--     @baseArrayLayer@ + @layerCount@ /must/ be less than the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ not have a compressed
--     or depth\/stencil format
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then 'Graphics.Vulkan.Core10.Handles.Image' /must/
--     not be a protected image
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, then 'Graphics.Vulkan.Core10.Handles.Image' /must/
--     not be an unprotected image
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Image' handle
--
-- -   'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' /must/ be a
--     valid 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   @pColor@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.Core10.SharedTypes.ClearColorValue' union
--
-- -   @pRanges@ /must/ be a valid pointer to an array of @rangeCount@
--     valid 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'
--     structures
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @rangeCount@ /must/ be greater than @0@
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and
--     'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SharedTypes.ClearColorValue',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.Image',
-- 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'
cmdClearColorImage :: CommandBuffer -> Image -> ImageLayout -> ClearColorValue -> ("ranges" ::: Vector ImageSubresourceRange) -> IO ()
cmdClearColorImage commandBuffer image imageLayout color ranges = evalContT $ do
  let vkCmdClearColorImage' = mkVkCmdClearColorImage (pVkCmdClearColorImage (deviceCmds (commandBuffer :: CommandBuffer)))
  pColor <- ContT $ withCStruct (color)
  pPRanges <- ContT $ allocaBytesAligned @ImageSubresourceRange ((Data.Vector.length (ranges)) * 20) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPRanges `plusPtr` (20 * (i)) :: Ptr ImageSubresourceRange) (e) . ($ ())) (ranges)
  lift $ vkCmdClearColorImage' (commandBufferHandle (commandBuffer)) (image) (imageLayout) pColor ((fromIntegral (Data.Vector.length $ (ranges)) :: Word32)) (pPRanges)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdClearDepthStencilImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Ptr ClearDepthStencilValue -> Word32 -> Ptr ImageSubresourceRange -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Ptr ClearDepthStencilValue -> Word32 -> Ptr ImageSubresourceRange -> IO ()

-- | vkCmdClearDepthStencilImage - Fill regions of a combined depth\/stencil
-- image
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' is the image to be cleared.
--
-- -   'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' specifies the
--     current layout of the image subresource ranges to be cleared, and
--     /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'.
--
-- -   @pDepthStencil@ is a pointer to a
--     'Graphics.Vulkan.Core10.SharedTypes.ClearDepthStencilValue'
--     structure containing the values that the depth and stencil image
--     subresource ranges will be cleared to (see
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears-values>
--     below).
--
-- -   @rangeCount@ is the number of image subresource range structures in
--     @pRanges@.
--
-- -   @pRanges@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'
--     structures describing a range of mipmap levels, array layers, and
--     aspects to be cleared, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views Image Views>.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of 'Graphics.Vulkan.Core10.Handles.Image' /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'.
--
-- -   If any element of @pRanges.aspect@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     and 'Graphics.Vulkan.Core10.Handles.Image' was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     /must/ have been included in the
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     used to create 'Graphics.Vulkan.Core10.Handles.Image'
--
-- -   If any element of @pRanges.aspect@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     and 'Graphics.Vulkan.Core10.Handles.Image' was not created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     /must/ have been included in the
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@usage@ used to
--     create 'Graphics.Vulkan.Core10.Handles.Image'
--
-- -   If any element of @pRanges.aspect@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     /must/ have been included in the
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@usage@ used to
--     create 'Graphics.Vulkan.Core10.Handles.Image'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' /must/
--     specify the layout of the image subresource ranges of
--     'Graphics.Vulkan.Core10.Handles.Image' specified in @pRanges@ at the
--     time this command is executed on a
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' /must/ be
--     either of
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   The
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'::@aspectMask@
--     member of each element of the @pRanges@ array /must/ not include
--     bits other than
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Image'’s format does not have
--     a stencil component, then the
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'::@aspectMask@
--     member of each element of the @pRanges@ array /must/ not include the
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--     bit
--
-- -   If the 'Graphics.Vulkan.Core10.Handles.Image'’s format does not have
--     a depth component, then the
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'::@aspectMask@
--     member of each element of the @pRanges@ array /must/ not include the
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--     bit
--
-- -   The
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'::@baseMipLevel@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   For each 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'
--     element of @pRanges@, if the @levelCount@ member is not
--     'Graphics.Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS', then
--     @baseMipLevel@ + @levelCount@ /must/ be less than the @mipLevels@
--     specified in 'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   The
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'::@baseArrayLayer@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   For each 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'
--     element of @pRanges@, if the @layerCount@ member is not
--     'Graphics.Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', then
--     @baseArrayLayer@ + @layerCount@ /must/ be less than the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when
--     'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ have a depth\/stencil
--     format
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then 'Graphics.Vulkan.Core10.Handles.Image' /must/
--     not be a protected image
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, then 'Graphics.Vulkan.Core10.Handles.Image' /must/
--     not be an unprotected image
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Image' handle
--
-- -   'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' /must/ be a
--     valid 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   @pDepthStencil@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.Core10.SharedTypes.ClearDepthStencilValue'
--     structure
--
-- -   @pRanges@ /must/ be a valid pointer to an array of @rangeCount@
--     valid 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'
--     structures
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @rangeCount@ /must/ be greater than @0@
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and
--     'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SharedTypes.ClearDepthStencilValue',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.Image',
-- 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceRange'
cmdClearDepthStencilImage :: CommandBuffer -> Image -> ImageLayout -> ClearDepthStencilValue -> ("ranges" ::: Vector ImageSubresourceRange) -> IO ()
cmdClearDepthStencilImage commandBuffer image imageLayout depthStencil ranges = evalContT $ do
  let vkCmdClearDepthStencilImage' = mkVkCmdClearDepthStencilImage (pVkCmdClearDepthStencilImage (deviceCmds (commandBuffer :: CommandBuffer)))
  pDepthStencil <- ContT $ withCStruct (depthStencil)
  pPRanges <- ContT $ allocaBytesAligned @ImageSubresourceRange ((Data.Vector.length (ranges)) * 20) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPRanges `plusPtr` (20 * (i)) :: Ptr ImageSubresourceRange) (e) . ($ ())) (ranges)
  lift $ vkCmdClearDepthStencilImage' (commandBufferHandle (commandBuffer)) (image) (imageLayout) pDepthStencil ((fromIntegral (Data.Vector.length $ (ranges)) :: Word32)) (pPRanges)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdClearAttachments
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr ClearAttachment -> Word32 -> Ptr ClearRect -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr ClearAttachment -> Word32 -> Ptr ClearRect -> IO ()

-- | vkCmdClearAttachments - Clear regions within bound framebuffer
-- attachments
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @attachmentCount@ is the number of entries in the @pAttachments@
--     array.
--
-- -   @pAttachments@ is a pointer to an array of 'ClearAttachment'
--     structures defining the attachments to clear and the clear values to
--     use. If any attachment to be cleared in the current subpass is
--     'Graphics.Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', then the
--     clear has no effect on that attachment.
--
-- -   @rectCount@ is the number of entries in the @pRects@ array.
--
-- -   @pRects@ is a pointer to an array of 'ClearRect' structures defining
--     regions within each selected attachment to clear.
--
-- = Description
--
-- 'cmdClearAttachments' /can/ clear multiple regions of each attachment
-- used in the current subpass of a render pass instance. This command
-- /must/ be called only inside a render pass instance, and implicitly
-- selects the images to clear based on the current framebuffer attachments
-- and the command parameters.
--
-- If the render pass has a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-fragmentdensitymapattachment fragment density map attachment>,
-- clears follow the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragmentdensitymapops operations of fragment density maps>
-- as if each clear region was a primitive which generates fragments. The
-- clear color is applied to all pixels inside each fragment’s area
-- regardless if the pixels lie outside of the clear region. Clears /may/
-- have a different set of supported fragment areas than draws.
--
-- Unlike other
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears clear commands>,
-- 'cmdClearAttachments' executes as a drawing command, rather than a
-- transfer command, with writes performed by it executing in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primrast-order rasterization order>.
-- Clears to color attachments are executed as color attachment writes, by
-- the
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
-- stage. Clears to depth\/stencil attachments are executed as
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth depth writes>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil writes>
-- by the
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT'
-- and
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT'
-- stages.
--
-- == Valid Usage
--
-- -   If the @aspectMask@ member of any element of @pAttachments@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
--     then the @colorAttachment@ member of that element /must/ either
--     refer to a color attachment which is
--     'Graphics.Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', or /must/
--     be a valid color attachment.
--
-- -   If the @aspectMask@ member of any element of @pAttachments@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT',
--     then the current subpass\' depth\/stencil attachment /must/ either
--     be 'Graphics.Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', or
--     /must/ have a depth component
--
-- -   If the @aspectMask@ member of any element of @pAttachments@ contains
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     then the current subpass\' depth\/stencil attachment /must/ either
--     be 'Graphics.Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', or
--     /must/ have a stencil component
--
-- -   The @rect@ member of each element of @pRects@ /must/ have an
--     @extent.width@ greater than @0@
--
-- -   The @rect@ member of each element of @pRects@ /must/ have an
--     @extent.height@ greater than @0@
--
-- -   The rectangular region specified by each element of @pRects@ /must/
--     be contained within the render area of the current render pass
--     instance
--
-- -   The layers specified by each element of @pRects@ /must/ be contained
--     within every attachment that @pAttachments@ refers to
--
-- -   The @layerCount@ member of each element of @pRects@ /must/ not be
--     @0@
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then each attachment to be cleared /must/ not be a
--     protected image.
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, then each attachment to be cleared /must/ not be an
--     unprotected image.
--
-- -   If the render pass instance this is recorded in uses multiview, then
--     @baseArrayLayer@ /must/ be zero and @layerCount@ /must/ be one.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pAttachments@ /must/ be a valid pointer to an array of
--     @attachmentCount@ valid 'ClearAttachment' structures
--
-- -   @pRects@ /must/ be a valid pointer to an array of @rectCount@
--     'ClearRect' structures
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   @attachmentCount@ /must/ be greater than @0@
--
-- -   @rectCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'ClearAttachment', 'ClearRect',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdClearAttachments :: CommandBuffer -> ("attachments" ::: Vector ClearAttachment) -> ("rects" ::: Vector ClearRect) -> IO ()
cmdClearAttachments commandBuffer attachments rects = evalContT $ do
  let vkCmdClearAttachments' = mkVkCmdClearAttachments (pVkCmdClearAttachments (deviceCmds (commandBuffer :: CommandBuffer)))
  pPAttachments <- ContT $ allocaBytesAligned @ClearAttachment ((Data.Vector.length (attachments)) * 24) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachments `plusPtr` (24 * (i)) :: Ptr ClearAttachment) (e) . ($ ())) (attachments)
  pPRects <- ContT $ allocaBytesAligned @ClearRect ((Data.Vector.length (rects)) * 24) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPRects `plusPtr` (24 * (i)) :: Ptr ClearRect) (e) . ($ ())) (rects)
  lift $ vkCmdClearAttachments' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (attachments)) :: Word32)) (pPAttachments) ((fromIntegral (Data.Vector.length $ (rects)) :: Word32)) (pPRects)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResolveImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageResolve -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageResolve -> IO ()

-- | vkCmdResolveImage - Resolve regions of an image
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @srcImage@ is the source image.
--
-- -   @srcImageLayout@ is the layout of the source image subresources for
--     the resolve.
--
-- -   @dstImage@ is the destination image.
--
-- -   @dstImageLayout@ is the layout of the destination image subresources
--     for the resolve.
--
-- -   @regionCount@ is the number of regions to resolve.
--
-- -   @pRegions@ is a pointer to an array of 'ImageResolve' structures
--     specifying the regions to resolve.
--
-- = Description
--
-- During the resolve the samples corresponding to each pixel location in
-- the source are converted to a single sample before being written to the
-- destination. If the source formats are floating-point or normalized
-- types, the sample values for each pixel are resolved in an
-- implementation-dependent manner. If the source formats are integer
-- types, a single sample’s value is selected for each pixel.
--
-- @srcOffset@ and @dstOffset@ select the initial @x@, @y@, and @z@ offsets
-- in texels of the sub-regions of the source and destination image data.
-- @extent@ is the size in texels of the source image to resolve in
-- @width@, @height@ and @depth@.
--
-- Resolves are done layer by layer starting with @baseArrayLayer@ member
-- of @srcSubresource@ for the source and @dstSubresource@ for the
-- destination. @layerCount@ layers are resolved to the destination image.
--
-- == Valid Usage
--
-- -   The source region specified by each element of @pRegions@ /must/ be
--     a region that is contained within @srcImage@
--
-- -   The destination region specified by each element of @pRegions@
--     /must/ be a region that is contained within @dstImage@
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   If @srcImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @srcImage@ /must/ have a sample count equal to any valid sample
--     count value other than
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   If @dstImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @dstImage@ /must/ have a sample count equal to
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a 'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   @srcImageLayout@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a 'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   @dstImageLayout@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'.
--
-- -   @srcImage@ and @dstImage@ /must/ have been created with the same
--     image format
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @srcImage@ /must/ not be a protected image
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then @dstImage@ /must/ not be a protected image
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, then @dstImage@ /must/ not be an unprotected image
--
-- -   The @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was
--     created
--
-- -   The @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was
--     created
--
-- -   @dstImage@ and @srcImage@ /must/ not have been created with
--     'Graphics.Vulkan.Core10.BaseType.Flags' containing
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @srcImage@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image'
--     handle
--
-- -   @srcImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   @dstImage@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Image'
--     handle
--
-- -   @dstImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid 'ImageResolve' structures
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', @dstImage@,
--     and @srcImage@ /must/ have been created, allocated, or retrieved
--     from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.Image',
-- 'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout', 'ImageResolve'
cmdResolveImage :: CommandBuffer -> ("srcImage" ::: Image) -> ("srcImageLayout" ::: ImageLayout) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("regions" ::: Vector ImageResolve) -> IO ()
cmdResolveImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout regions = evalContT $ do
  let vkCmdResolveImage' = mkVkCmdResolveImage (pVkCmdResolveImage (deviceCmds (commandBuffer :: CommandBuffer)))
  pPRegions <- ContT $ allocaBytesAligned @ImageResolve ((Data.Vector.length (regions)) * 68) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPRegions `plusPtr` (68 * (i)) :: Ptr ImageResolve) (e) . ($ ())) (regions)
  lift $ vkCmdResolveImage' (commandBufferHandle (commandBuffer)) (srcImage) (srcImageLayout) (dstImage) (dstImageLayout) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetEvent
  :: FunPtr (Ptr CommandBuffer_T -> Event -> PipelineStageFlags -> IO ()) -> Ptr CommandBuffer_T -> Event -> PipelineStageFlags -> IO ()

-- | vkCmdSetEvent - Set an event object to signaled state
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command is recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.Event' is the event that will be
--     signaled.
--
-- -   @stageMask@ specifies the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>
--     used to determine when the 'Graphics.Vulkan.Core10.Handles.Event' is
--     signaled.
--
-- = Description
--
-- When 'cmdSetEvent' is submitted to a queue, it defines an execution
-- dependency on commands that were submitted before it, and defines an
-- event signal operation which sets the event to the signaled state.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- The synchronization scope is limited to operations on the pipeline
-- stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @stageMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the event signal operation.
--
-- If 'Graphics.Vulkan.Core10.Handles.Event' is already in the signaled
-- state when 'cmdSetEvent' is executed on the device, then 'cmdSetEvent'
-- has no effect, no event signal operation occurs, and no execution
-- dependency is generated.
--
-- == Valid Usage
--
-- -   @stageMask@ /must/ not include
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer'’s current device mask
--     /must/ include exactly one physical device.
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Event' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Event' handle
--
-- -   @stageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   @stageMask@ /must/ not be @0@
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and
--     'Graphics.Vulkan.Core10.Handles.Event' /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.Event',
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags'
cmdSetEvent :: CommandBuffer -> Event -> ("stageMask" ::: PipelineStageFlags) -> IO ()
cmdSetEvent commandBuffer event stageMask = do
  let vkCmdSetEvent' = mkVkCmdSetEvent (pVkCmdSetEvent (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdSetEvent' (commandBufferHandle (commandBuffer)) (event) (stageMask)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResetEvent
  :: FunPtr (Ptr CommandBuffer_T -> Event -> PipelineStageFlags -> IO ()) -> Ptr CommandBuffer_T -> Event -> PipelineStageFlags -> IO ()

-- | vkCmdResetEvent - Reset an event object to non-signaled state
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command is recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.Event' is the event that will be
--     unsignaled.
--
-- -   @stageMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     specifying the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>
--     used to determine when the 'Graphics.Vulkan.Core10.Handles.Event' is
--     unsignaled.
--
-- = Description
--
-- When 'cmdResetEvent' is submitted to a queue, it defines an execution
-- dependency on commands that were submitted before it, and defines an
-- event unsignal operation which resets the event to the unsignaled state.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- The synchronization scope is limited to operations on the pipeline
-- stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @stageMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the event unsignal operation.
--
-- If 'Graphics.Vulkan.Core10.Handles.Event' is already in the unsignaled
-- state when 'cmdResetEvent' is executed on the device, then
-- 'cmdResetEvent' has no effect, no event unsignal operation occurs, and
-- no execution dependency is generated.
--
-- == Valid Usage
--
-- -   @stageMask@ /must/ not include
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   When this command executes, 'Graphics.Vulkan.Core10.Handles.Event'
--     /must/ not be waited on by a 'cmdWaitEvents' command that is
--     currently executing
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer'’s current device mask
--     /must/ include exactly one physical device.
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Event' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Event' handle
--
-- -   @stageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   @stageMask@ /must/ not be @0@
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and
--     'Graphics.Vulkan.Core10.Handles.Event' /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.Event',
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags'
cmdResetEvent :: CommandBuffer -> Event -> ("stageMask" ::: PipelineStageFlags) -> IO ()
cmdResetEvent commandBuffer event stageMask = do
  let vkCmdResetEvent' = mkVkCmdResetEvent (pVkCmdResetEvent (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdResetEvent' (commandBufferHandle (commandBuffer)) (event) (stageMask)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWaitEvents
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (ImageMemoryBarrier a) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (ImageMemoryBarrier a) -> IO ()

-- | vkCmdWaitEvents - Wait for one or more events and insert a set of memory
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command is recorded.
--
-- -   @eventCount@ is the length of the @pEvents@ array.
--
-- -   @pEvents@ is a pointer to an array of event object handles to wait
--     on.
--
-- -   @srcStageMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     specifying the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>.
--
-- -   @dstStageMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     specifying the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages destination stage mask>.
--
-- -   @memoryBarrierCount@ is the length of the @pMemoryBarriers@ array.
--
-- -   @pMemoryBarriers@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.OtherTypes.MemoryBarrier' structures.
--
-- -   @bufferMemoryBarrierCount@ is the length of the
--     @pBufferMemoryBarriers@ array.
--
-- -   @pBufferMemoryBarriers@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.OtherTypes.BufferMemoryBarrier' structures.
--
-- -   @imageMemoryBarrierCount@ is the length of the
--     @pImageMemoryBarriers@ array.
--
-- -   @pImageMemoryBarriers@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.OtherTypes.ImageMemoryBarrier' structures.
--
-- = Description
--
-- When 'cmdWaitEvents' is submitted to a queue, it defines a memory
-- dependency between prior event signal operations on the same queue or
-- the host, and subsequent commands. 'cmdWaitEvents' /must/ not be used to
-- wait on event signal operations occurring on other queues.
--
-- The first synchronization scope only includes event signal operations
-- that operate on members of @pEvents@, and the operations that
-- happened-before the event signal operations. Event signal operations
-- performed by 'cmdSetEvent' that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- are included in the first synchronization scope, if the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically latest>
-- pipeline stage in their @stageMask@ parameter is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earlier>
-- than or equal to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically latest>
-- pipeline stage in @srcStageMask@. Event signal operations performed by
-- 'Graphics.Vulkan.Core10.Event.setEvent' are only included in the first
-- synchronization scope if
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'
-- is included in @srcStageMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur later in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- The second synchronization scope is limited to operations on the
-- pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@. Within that, the first access scope only
-- includes the first access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the first access scope
-- includes no accesses.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@. Within that, the second access scope only
-- includes the second access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the second access scope
-- includes no accesses.
--
-- Note
--
-- 'cmdWaitEvents' is used with 'cmdSetEvent' to define a memory dependency
-- between two sets of action commands, roughly in the same way as pipeline
-- barriers, but split into two commands such that work between the two
-- /may/ execute unhindered.
--
-- Unlike 'cmdPipelineBarrier', a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
-- /cannot/ be performed using 'cmdWaitEvents'.
--
-- Note
--
-- Applications /should/ be careful to avoid race conditions when using
-- events. There is no direct ordering guarantee between a 'cmdResetEvent'
-- command and a 'cmdWaitEvents' command submitted after it, so some other
-- execution dependency /must/ be included between these commands (e.g. a
-- semaphore).
--
-- == Valid Usage
--
-- -   @srcStageMask@ /must/ be the bitwise OR of the @stageMask@ parameter
--     used in previous calls to 'cmdSetEvent' with any of the members of
--     @pEvents@ and
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'
--     if any of the members of @pEvents@ was set using
--     'Graphics.Vulkan.Core10.Event.setEvent'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   If @pEvents@ includes one or more events that will be signaled by
--     'Graphics.Vulkan.Core10.Event.setEvent' after
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' has been submitted to
--     a queue, then 'cmdWaitEvents' /must/ not be called inside a render
--     pass instance
--
-- -   Any pipeline stage included in @srcStageMask@ or @dstStageMask@
--     /must/ be supported by the capabilities of the queue family
--     specified by the @queueFamilyIndex@ member of the
--     'Graphics.Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure
--     that was used to create the
--     'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from,
--     as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>.
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ or
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @srcAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>.
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ or
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @dstAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>.
--
-- -   The @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ members of any
--     element of @pBufferMemoryBarriers@ or @pImageMemoryBarriers@ /must/
--     be equal.
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer'’s current device mask
--     /must/ include exactly one physical device.
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   The @srcAccessMask@ member of each element of @pMemoryBarriers@
--     /must/ only include access flags that are supported by one or more
--     of the pipeline stages in @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   The @dstAccessMask@ member of each element of @pMemoryBarriers@
--     /must/ only include access flags that are supported by one or more
--     of the pipeline stages in @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   The @srcAccessMask@ member of each element of
--     @pBufferMemoryBarriers@ /must/ only include access flags that are
--     supported by one or more of the pipeline stages in @srcStageMask@,
--     as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   The @dstAccessMask@ member of each element of
--     @pBufferMemoryBarriers@ /must/ only include access flags that are
--     supported by one or more of the pipeline stages in @dstStageMask@,
--     as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   The @srcAccessMask@ member of each element of @pImageMemoryBarriers@
--     /must/ only include access flags that are supported by one or more
--     of the pipeline stages in @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   The @dstAccessMask@ member of any element of @pImageMemoryBarriers@
--     /must/ only include access flags that are supported by one or more
--     of the pipeline stages in @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pEvents@ /must/ be a valid pointer to an array of @eventCount@
--     valid 'Graphics.Vulkan.Core10.Handles.Event' handles
--
-- -   @srcStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   @srcStageMask@ /must/ not be @0@
--
-- -   @dstStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   @dstStageMask@ /must/ not be @0@
--
-- -   If @memoryBarrierCount@ is not @0@, @pMemoryBarriers@ /must/ be a
--     valid pointer to an array of @memoryBarrierCount@ valid
--     'Graphics.Vulkan.Core10.OtherTypes.MemoryBarrier' structures
--
-- -   If @bufferMemoryBarrierCount@ is not @0@, @pBufferMemoryBarriers@
--     /must/ be a valid pointer to an array of @bufferMemoryBarrierCount@
--     valid 'Graphics.Vulkan.Core10.OtherTypes.BufferMemoryBarrier'
--     structures
--
-- -   If @imageMemoryBarrierCount@ is not @0@, @pImageMemoryBarriers@
--     /must/ be a valid pointer to an array of @imageMemoryBarrierCount@
--     valid 'Graphics.Vulkan.Core10.OtherTypes.ImageMemoryBarrier'
--     structures
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics, or compute operations
--
-- -   @eventCount@ /must/ be greater than @0@
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and the
--     elements of @pEvents@ /must/ have been created, allocated, or
--     retrieved from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.Event',
-- 'Graphics.Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
-- 'Graphics.Vulkan.Core10.OtherTypes.MemoryBarrier',
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags'
cmdWaitEvents :: PokeChain a => CommandBuffer -> ("events" ::: Vector Event) -> ("srcStageMask" ::: PipelineStageFlags) -> ("dstStageMask" ::: PipelineStageFlags) -> ("memoryBarriers" ::: Vector MemoryBarrier) -> ("bufferMemoryBarriers" ::: Vector BufferMemoryBarrier) -> ("imageMemoryBarriers" ::: Vector (ImageMemoryBarrier a)) -> IO ()
cmdWaitEvents commandBuffer events srcStageMask dstStageMask memoryBarriers bufferMemoryBarriers imageMemoryBarriers = evalContT $ do
  let vkCmdWaitEvents' = mkVkCmdWaitEvents (pVkCmdWaitEvents (deviceCmds (commandBuffer :: CommandBuffer)))
  pPEvents <- ContT $ allocaBytesAligned @Event ((Data.Vector.length (events)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPEvents `plusPtr` (8 * (i)) :: Ptr Event) (e)) (events)
  pPMemoryBarriers <- ContT $ allocaBytesAligned @MemoryBarrier ((Data.Vector.length (memoryBarriers)) * 24) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPMemoryBarriers `plusPtr` (24 * (i)) :: Ptr MemoryBarrier) (e) . ($ ())) (memoryBarriers)
  pPBufferMemoryBarriers <- ContT $ allocaBytesAligned @BufferMemoryBarrier ((Data.Vector.length (bufferMemoryBarriers)) * 56) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPBufferMemoryBarriers `plusPtr` (56 * (i)) :: Ptr BufferMemoryBarrier) (e) . ($ ())) (bufferMemoryBarriers)
  pPImageMemoryBarriers <- ContT $ allocaBytesAligned @(ImageMemoryBarrier _) ((Data.Vector.length (imageMemoryBarriers)) * 72) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPImageMemoryBarriers `plusPtr` (72 * (i)) :: Ptr (ImageMemoryBarrier _)) (e) . ($ ())) (imageMemoryBarriers)
  lift $ vkCmdWaitEvents' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (events)) :: Word32)) (pPEvents) (srcStageMask) (dstStageMask) ((fromIntegral (Data.Vector.length $ (memoryBarriers)) :: Word32)) (pPMemoryBarriers) ((fromIntegral (Data.Vector.length $ (bufferMemoryBarriers)) :: Word32)) (pPBufferMemoryBarriers) ((fromIntegral (Data.Vector.length $ (imageMemoryBarriers)) :: Word32)) (pPImageMemoryBarriers)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPipelineBarrier
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlags -> PipelineStageFlags -> DependencyFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (ImageMemoryBarrier a) -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlags -> PipelineStageFlags -> DependencyFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (ImageMemoryBarrier a) -> IO ()

-- | vkCmdPipelineBarrier - Insert a memory dependency
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command is recorded.
--
-- -   @srcStageMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     specifying the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>.
--
-- -   @dstStageMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     specifying the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>.
--
-- -   'Graphics.Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags' is
--     a bitmask of
--     'Graphics.Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits'
--     specifying how execution and memory dependencies are formed.
--
-- -   @memoryBarrierCount@ is the length of the @pMemoryBarriers@ array.
--
-- -   @pMemoryBarriers@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.OtherTypes.MemoryBarrier' structures.
--
-- -   @bufferMemoryBarrierCount@ is the length of the
--     @pBufferMemoryBarriers@ array.
--
-- -   @pBufferMemoryBarriers@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.OtherTypes.BufferMemoryBarrier' structures.
--
-- -   @imageMemoryBarrierCount@ is the length of the
--     @pImageMemoryBarriers@ array.
--
-- -   @pImageMemoryBarriers@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.OtherTypes.ImageMemoryBarrier' structures.
--
-- = Description
--
-- When 'cmdPipelineBarrier' is submitted to a queue, it defines a memory
-- dependency between commands that were submitted before it, and those
-- submitted after it.
--
-- If 'cmdPipelineBarrier' was recorded outside a render pass instance, the
-- first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- If 'cmdPipelineBarrier' was recorded inside a render pass instance, the
-- first synchronization scope includes only commands that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- within the same subpass. In either case, the first synchronization scope
-- is limited to operations on the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@.
--
-- If 'cmdPipelineBarrier' was recorded outside a render pass instance, the
-- second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur later in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- If 'cmdPipelineBarrier' was recorded inside a render pass instance, the
-- second synchronization scope includes only commands that occur later in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- within the same subpass. In either case, the second synchronization
-- scope is limited to operations on the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@. Within that, the first access scope only
-- includes the first access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the first access scope
-- includes no accesses.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@. Within that, the second access scope only
-- includes the second access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the second access scope
-- includes no accesses.
--
-- If 'Graphics.Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags'
-- includes
-- 'Graphics.Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_BY_REGION_BIT',
-- then any dependency between
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space>
-- pipeline stages is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-local>
-- - otherwise it is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-global>.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   If 'cmdPipelineBarrier' is called within a render pass instance, the
--     render pass /must/ have been created with at least one
--     'Graphics.Vulkan.Core10.Pass.SubpassDependency' instance in
--     'Graphics.Vulkan.Core10.Pass.RenderPassCreateInfo'::@pDependencies@
--     that expresses a dependency from the current subpass to itself, and
--     for which @srcStageMask@ contains a subset of the bit values in
--     'Graphics.Vulkan.Core10.Pass.SubpassDependency'::@srcStageMask@,
--     @dstStageMask@ contains a subset of the bit values in
--     'Graphics.Vulkan.Core10.Pass.SubpassDependency'::@dstStageMask@,
--     'Graphics.Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags' is
--     equal to
--     'Graphics.Vulkan.Core10.Pass.SubpassDependency'::'Graphics.Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags',
--     @srcAccessMask@ member of each element of @pMemoryBarriers@ and
--     @pImageMemoryBarriers@ contains a subset of the bit values in
--     'Graphics.Vulkan.Core10.Pass.SubpassDependency'::@srcAccessMask@,
--     and @dstAccessMask@ member of each element of @pMemoryBarriers@ and
--     @pImageMemoryBarriers@ contains a subset of the bit values in
--     'Graphics.Vulkan.Core10.Pass.SubpassDependency'::@dstAccessMask@
--
-- -   If 'cmdPipelineBarrier' is called within a render pass instance,
--     @bufferMemoryBarrierCount@ /must/ be @0@
--
-- -   If 'cmdPipelineBarrier' is called within a render pass instance, the
--     'Graphics.Vulkan.Core10.Handles.Image' member of any element of
--     @pImageMemoryBarriers@ /must/ be equal to one of the elements of
--     @pAttachments@ that the current
--     'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with, that
--     is also referred to by one of the elements of the
--     @pColorAttachments@, @pResolveAttachments@ or
--     @pDepthStencilAttachment@ members of the
--     'Graphics.Vulkan.Core10.Pass.SubpassDescription' instance or by the
--     @pDepthStencilResolveAttachment@ member of the
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'
--     structure that the current subpass was created with
--
-- -   If 'cmdPipelineBarrier' is called within a render pass instance, the
--     @oldLayout@ and @newLayout@ members of any element of
--     @pImageMemoryBarriers@ /must/ be equal to the @layout@ member of an
--     element of the @pColorAttachments@, @pResolveAttachments@ or
--     @pDepthStencilAttachment@ members of the
--     'Graphics.Vulkan.Core10.Pass.SubpassDescription' instance or by the
--     @pDepthStencilResolveAttachment@ member of the
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'
--     structure that the current subpass was created with, that refers to
--     the same 'Graphics.Vulkan.Core10.Handles.Image'
--
-- -   If 'cmdPipelineBarrier' is called within a render pass instance, the
--     @oldLayout@ and @newLayout@ members of an element of
--     @pImageMemoryBarriers@ /must/ be equal
--
-- -   If 'cmdPipelineBarrier' is called within a render pass instance, the
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ members of any
--     element of @pImageMemoryBarriers@ /must/ be
--     'Graphics.Vulkan.Core10.APIConstants.QUEUE_FAMILY_IGNORED'
--
-- -   Any pipeline stage included in @srcStageMask@ or @dstStageMask@
--     /must/ be supported by the capabilities of the queue family
--     specified by the @queueFamilyIndex@ member of the
--     'Graphics.Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure
--     that was used to create the
--     'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from,
--     as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   If 'cmdPipelineBarrier' is called outside of a render pass instance,
--     'Graphics.Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags'
--     /must/ not include
--     'Graphics.Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   The @srcAccessMask@ member of each element of @pMemoryBarriers@
--     /must/ only include access flags that are supported by one or more
--     of the pipeline stages in @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   The @dstAccessMask@ member of each element of @pMemoryBarriers@
--     /must/ only include access flags that are supported by one or more
--     of the pipeline stages in @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   For any element of @pBufferMemoryBarriers@, if its
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ members are equal,
--     or if its @srcQueueFamilyIndex@ is the queue family index that was
--     used to create the command pool that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from,
--     then its @srcAccessMask@ member /must/ only contain access flags
--     that are supported by one or more of the pipeline stages in
--     @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   For any element of @pBufferMemoryBarriers@, if its
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ members are equal,
--     or if its @dstQueueFamilyIndex@ is the queue family index that was
--     used to create the command pool that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from,
--     then its @dstAccessMask@ member /must/ only contain access flags
--     that are supported by one or more of the pipeline stages in
--     @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   For any element of @pImageMemoryBarriers@, if its
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ members are equal,
--     or if its @srcQueueFamilyIndex@ is the queue family index that was
--     used to create the command pool that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from,
--     then its @srcAccessMask@ member /must/ only contain access flags
--     that are supported by one or more of the pipeline stages in
--     @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   For any element of @pImageMemoryBarriers@, if its
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ members are equal,
--     or if its @dstQueueFamilyIndex@ is the queue family index that was
--     used to create the command pool that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from,
--     then its @dstAccessMask@ member /must/ only contain access flags
--     that are supported by one or more of the pipeline stages in
--     @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @srcStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   @srcStageMask@ /must/ not be @0@
--
-- -   @dstStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   @dstStageMask@ /must/ not be @0@
--
-- -   'Graphics.Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags'
--     /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits'
--     values
--
-- -   If @memoryBarrierCount@ is not @0@, @pMemoryBarriers@ /must/ be a
--     valid pointer to an array of @memoryBarrierCount@ valid
--     'Graphics.Vulkan.Core10.OtherTypes.MemoryBarrier' structures
--
-- -   If @bufferMemoryBarrierCount@ is not @0@, @pBufferMemoryBarriers@
--     /must/ be a valid pointer to an array of @bufferMemoryBarrierCount@
--     valid 'Graphics.Vulkan.Core10.OtherTypes.BufferMemoryBarrier'
--     structures
--
-- -   If @imageMemoryBarrierCount@ is not @0@, @pImageMemoryBarriers@
--     /must/ be a valid pointer to an array of @imageMemoryBarrierCount@
--     valid 'Graphics.Vulkan.Core10.OtherTypes.ImageMemoryBarrier'
--     structures
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support transfer, graphics, or compute operations
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Transfer                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags',
-- 'Graphics.Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
-- 'Graphics.Vulkan.Core10.OtherTypes.MemoryBarrier',
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags'
cmdPipelineBarrier :: PokeChain a => CommandBuffer -> ("srcStageMask" ::: PipelineStageFlags) -> ("dstStageMask" ::: PipelineStageFlags) -> DependencyFlags -> ("memoryBarriers" ::: Vector MemoryBarrier) -> ("bufferMemoryBarriers" ::: Vector BufferMemoryBarrier) -> ("imageMemoryBarriers" ::: Vector (ImageMemoryBarrier a)) -> IO ()
cmdPipelineBarrier commandBuffer srcStageMask dstStageMask dependencyFlags memoryBarriers bufferMemoryBarriers imageMemoryBarriers = evalContT $ do
  let vkCmdPipelineBarrier' = mkVkCmdPipelineBarrier (pVkCmdPipelineBarrier (deviceCmds (commandBuffer :: CommandBuffer)))
  pPMemoryBarriers <- ContT $ allocaBytesAligned @MemoryBarrier ((Data.Vector.length (memoryBarriers)) * 24) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPMemoryBarriers `plusPtr` (24 * (i)) :: Ptr MemoryBarrier) (e) . ($ ())) (memoryBarriers)
  pPBufferMemoryBarriers <- ContT $ allocaBytesAligned @BufferMemoryBarrier ((Data.Vector.length (bufferMemoryBarriers)) * 56) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPBufferMemoryBarriers `plusPtr` (56 * (i)) :: Ptr BufferMemoryBarrier) (e) . ($ ())) (bufferMemoryBarriers)
  pPImageMemoryBarriers <- ContT $ allocaBytesAligned @(ImageMemoryBarrier _) ((Data.Vector.length (imageMemoryBarriers)) * 72) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPImageMemoryBarriers `plusPtr` (72 * (i)) :: Ptr (ImageMemoryBarrier _)) (e) . ($ ())) (imageMemoryBarriers)
  lift $ vkCmdPipelineBarrier' (commandBufferHandle (commandBuffer)) (srcStageMask) (dstStageMask) (dependencyFlags) ((fromIntegral (Data.Vector.length $ (memoryBarriers)) :: Word32)) (pPMemoryBarriers) ((fromIntegral (Data.Vector.length $ (bufferMemoryBarriers)) :: Word32)) (pPBufferMemoryBarriers) ((fromIntegral (Data.Vector.length $ (imageMemoryBarriers)) :: Word32)) (pPImageMemoryBarriers)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginQuery
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> QueryControlFlags -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> QueryControlFlags -> IO ()

-- | vkCmdBeginQuery - Begin a query
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which this command will be recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' is the query pool that
--     will manage the results of the query.
--
-- -   @query@ is the query index within the query pool that will contain
--     the results.
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlagBits'
--     specifying constraints on the types of queries that /can/ be
--     performed.
--
-- = Description
--
-- If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' of the pool is
-- 'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION' and
-- 'Graphics.Vulkan.Core10.BaseType.Flags' contains
-- 'Graphics.Vulkan.Core10.Enums.QueryControlFlagBits.QUERY_CONTROL_PRECISE_BIT',
-- an implementation /must/ return a result that matches the actual number
-- of samples passed. This is described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-occlusion Occlusion Queries>.
--
-- Calling 'cmdBeginQuery' is equivalent to calling
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginQueryIndexedEXT'
-- with the @index@ parameter set to zero.
--
-- After beginning a query, that query is considered /active/ within the
-- command buffer it was called in until that same query is ended. Queries
-- active in a primary command buffer when secondary command buffers are
-- executed are considered active for those secondary command buffers.
--
-- == Valid Usage
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ have been created
--     with a 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' that
--     differs from that of any queries that are
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
--     within 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--
-- -   All queries used by the command /must/ be unavailable
--
-- -   The 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ not be
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-occlusionQueryPrecise precise occlusion queries>
--     feature is not enabled, or the
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to create
--     'Graphics.Vulkan.Core10.Handles.QueryPool' was not
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION',
--     'Graphics.Vulkan.Core10.BaseType.Flags' /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.QueryControlFlagBits.QUERY_CONTROL_PRECISE_BIT'
--
-- -   @query@ /must/ be less than the number of queries in
--     'Graphics.Vulkan.Core10.Handles.QueryPool'
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION', the
--     'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS'
--     and any of the @pipelineStatistics@ indicate graphics operations,
--     the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS'
--     and any of the @pipelineStatistics@ indicate compute operations, the
--     'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support compute operations
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ not be a
--     protected command buffer
--
-- -   If called within a render pass instance, the sum of @query@ and the
--     number of bits set in the current subpass’s view mask /must/ be less
--     than or equal to the number of queries in
--     'Graphics.Vulkan.Core10.Handles.QueryPool'
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     then
--     'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackQueries@
--     /must/ be supported
--
-- -   If 'Graphics.Vulkan.Core10.Handles.QueryPool' was created with a
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' of
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#profiling-lock profiling lock>
--     /must/ have been held before
--     'Graphics.Vulkan.Core10.CommandBuffer.beginCommandBuffer' was called
--     on 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.QueryPool' was created with a
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' of
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR'
--     and one of the counters used to create
--     'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Extensions.VK_KHR_performance_query.PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR',
--     the query begin /must/ be the first recorded command in
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.QueryPool' was created with a
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' of
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR'
--     and one of the counters used to create
--     'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Extensions.VK_KHR_performance_query.PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR',
--     the begin command /must/ not be recorded within a render pass
--     instance
--
-- -   If 'Graphics.Vulkan.Core10.Handles.QueryPool' was created with a
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' of
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR'
--     and another query pool with a
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType'
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR'
--     has been used within 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
--     its parent primary command buffer or secondary command buffer
--     recorded within the same parent primary command buffer as
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer', the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-features-performanceCounterMultipleQueryPools performanceCounterMultipleQueryPools>
--     feature /must/ be enabled
--
-- -   If 'Graphics.Vulkan.Core10.Handles.QueryPool' was created with a
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' of
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     this command /must/ not be recorded in a command buffer that, either
--     directly or through secondary command buffers, also contains a
--     'cmdResetQueryPool' command affecting the same query.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.QueryPool' handle
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be a valid
--     combination of
--     'Graphics.Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlagBits'
--     values
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics, or compute operations
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and
--     'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlags',
-- 'Graphics.Vulkan.Core10.Handles.QueryPool'
cmdBeginQuery :: CommandBuffer -> QueryPool -> ("query" ::: Word32) -> QueryControlFlags -> IO ()
cmdBeginQuery commandBuffer queryPool query flags = do
  let vkCmdBeginQuery' = mkVkCmdBeginQuery (pVkCmdBeginQuery (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdBeginQuery' (commandBufferHandle (commandBuffer)) (queryPool) (query) (flags)
  pure $ ()

-- | A safe wrapper for 'cmdBeginQuery' and 'cmdEndQuery' using 'bracket_'
cmdWithQuery :: CommandBuffer -> QueryPool -> Word32 -> QueryControlFlags -> IO r -> IO r
cmdWithQuery commandBuffer queryPool query flags =
  bracket_
    (cmdBeginQuery commandBuffer queryPool query flags)
    (cmdEndQuery commandBuffer queryPool query)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndQuery
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> IO ()

-- | vkCmdEndQuery - Ends a query
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which this command will be recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' is the query pool that is
--     managing the results of the query.
--
-- -   @query@ is the query index within the query pool where the result is
--     stored.
--
-- = Description
--
-- Calling 'cmdEndQuery' is equivalent to calling
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndQueryIndexedEXT'
-- with the @index@ parameter set to zero.
--
-- As queries operate asynchronously, ending a query does not immediately
-- set the query’s status to available. A query is considered /finished/
-- when the final results of the query are ready to be retrieved by
-- 'Graphics.Vulkan.Core10.Query.getQueryPoolResults' and
-- 'cmdCopyQueryPoolResults', and this is when the query’s status is set to
-- available.
--
-- Once a query is ended the query /must/ finish in finite time, unless the
-- state of the query is changed using other commands, e.g. by issuing a
-- reset of the query.
--
-- == Valid Usage
--
-- -   All queries used by the command /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
--
-- -   @query@ /must/ be less than the number of queries in
--     'Graphics.Vulkan.Core10.Handles.QueryPool'
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ not be a
--     protected command buffer
--
-- -   If 'cmdEndQuery' is called within a render pass instance, the sum of
--     @query@ and the number of bits set in the current subpass’s view
--     mask /must/ be less than or equal to the number of queries in
--     'Graphics.Vulkan.Core10.Handles.QueryPool'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.QueryPool' was created with a
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' of
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR'
--     and one or more of the counters used to create
--     'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Extensions.VK_KHR_performance_query.PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR',
--     the 'cmdEndQuery' /must/ be the last recorded command in
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.QueryPool' was created with a
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' of
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR'
--     and one or more of the counters used to create
--     'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Extensions.VK_KHR_performance_query.PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR',
--     the 'cmdEndQuery' /must/ not be recorded within a render pass
--     instance
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.QueryPool' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics, or compute operations
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and
--     'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.QueryPool'
cmdEndQuery :: CommandBuffer -> QueryPool -> ("query" ::: Word32) -> IO ()
cmdEndQuery commandBuffer queryPool query = do
  let vkCmdEndQuery' = mkVkCmdEndQuery (pVkCmdEndQuery (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdEndQuery' (commandBufferHandle (commandBuffer)) (queryPool) (query)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResetQueryPool
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> IO ()

-- | vkCmdResetQueryPool - Reset queries in a query pool
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which this command will be recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' is the handle of the
--     query pool managing the queries being reset.
--
-- -   @firstQuery@ is the initial query index to reset.
--
-- -   @queryCount@ is the number of queries to reset.
--
-- = Description
--
-- When executed on a queue, this command sets the status of query indices
-- [@firstQuery@, @firstQuery@ + @queryCount@ - 1] to unavailable.
--
-- If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to create
-- 'Graphics.Vulkan.Core10.Handles.QueryPool' was
-- 'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
-- this command sets the status of query indices [@firstQuery@,
-- @firstQuery@ + @queryCount@ - 1] to unavailable for each pass of
-- 'Graphics.Vulkan.Core10.Handles.QueryPool', as indicated by a call to
-- 'Graphics.Vulkan.Extensions.VK_KHR_performance_query.getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'.
--
-- Note
--
-- Because 'cmdResetQueryPool' resets all the passes of the indicated
-- queries, applications must not record a 'cmdResetQueryPool' command for
-- a 'Graphics.Vulkan.Core10.Handles.QueryPool' created with
-- 'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR'
-- in a command buffer that needs to be submitted multiple times as
-- indicated by a call to
-- 'Graphics.Vulkan.Extensions.VK_KHR_performance_query.getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'.
-- Otherwise applications will never be able to complete the recorded
-- queries.
--
-- == Valid Usage
--
-- -   @firstQuery@ /must/ be less than the number of queries in
--     'Graphics.Vulkan.Core10.Handles.QueryPool'
--
-- -   The sum of @firstQuery@ and @queryCount@ /must/ be less than or
--     equal to the number of queries in
--     'Graphics.Vulkan.Core10.Handles.QueryPool'
--
-- -   All queries used by the command /must/ not be active
--
-- -   If 'Graphics.Vulkan.Core10.Handles.QueryPool' was created with
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     this command /must/ not be recorded in a command buffer that, either
--     directly or through secondary command buffers, also contains begin
--     commands for a query from the set of queries [@firstQuery@,
--     @firstQuery@ + @queryCount@ - 1]
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.QueryPool' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and
--     'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.QueryPool'
cmdResetQueryPool :: CommandBuffer -> QueryPool -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()
cmdResetQueryPool commandBuffer queryPool firstQuery queryCount = do
  let vkCmdResetQueryPool' = mkVkCmdResetQueryPool (pVkCmdResetQueryPool (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdResetQueryPool' (commandBufferHandle (commandBuffer)) (queryPool) (firstQuery) (queryCount)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteTimestamp
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlagBits -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlagBits -> QueryPool -> Word32 -> IO ()

-- | vkCmdWriteTimestamp - Write a device timestamp into a query object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which the command will be recorded.
--
-- -   @pipelineStage@ is one of the
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits',
--     specifying a stage of the pipeline.
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' is the query pool that
--     will manage the timestamp.
--
-- -   @query@ is the query within the query pool that will contain the
--     timestamp.
--
-- = Description
--
-- 'cmdWriteTimestamp' latches the value of the timer when all previous
-- commands have completed executing as far as the specified pipeline
-- stage, and writes the timestamp value to memory. When the timestamp
-- value is written, the availability status of the query is set to
-- available.
--
-- Note
--
-- If an implementation is unable to detect completion and latch the timer
-- at any specific stage of the pipeline, it /may/ instead do so at any
-- logically later stage.
--
-- 'cmdCopyQueryPoolResults' /can/ then be called to copy the timestamp
-- value from the query pool into buffer memory, with ordering and
-- synchronization behavior equivalent to how other queries operate.
-- Timestamp values /can/ also be retrieved from the query pool using
-- 'Graphics.Vulkan.Core10.Query.getQueryPoolResults'. As with other
-- queries, the query /must/ be reset using 'cmdResetQueryPool' or
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.resetQueryPool'
-- before requesting the timestamp value be written to it.
--
-- While 'cmdWriteTimestamp' /can/ be called inside or outside of a render
-- pass instance, 'cmdCopyQueryPoolResults' /must/ only be called outside
-- of a render pass instance.
--
-- Timestamps /may/ only be meaningfully compared if they are written by
-- commands submitted to the same queue.
--
-- Note
--
-- An example of such a comparison is determining the execution time of a
-- sequence of commands.
--
-- If 'cmdWriteTimestamp' is called while executing a render pass instance
-- that has multiview enabled, the timestamp uses N consecutive query
-- indices in the query pool (starting at @query@) where N is the number of
-- bits set in the view mask of the subpass the command is executed in. The
-- resulting query values are determined by an implementation-dependent
-- choice of one of the following behaviors:
--
-- -   The first query is a timestamp value and (if more than one bit is
--     set in the view mask) zero is written to the remaining queries. If
--     two timestamps are written in the same subpass, the sum of the
--     execution time of all views between those commands is the difference
--     between the first query written by each command.
--
-- -   All N queries are timestamp values. If two timestamps are written in
--     the same subpass, the sum of the execution time of all views between
--     those commands is the sum of the difference between corresponding
--     queries written by each command. The difference between
--     corresponding queries /may/ be the execution time of a single view.
--
-- In either case, the application /can/ sum the differences between all N
-- queries to determine the total execution time.
--
-- == Valid Usage
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ have been created
--     with a 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' of
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP'
--
-- -   The query identified by 'Graphics.Vulkan.Core10.Handles.QueryPool'
--     and @query@ /must/ be /unavailable/
--
-- -   The command pool’s queue family /must/ support a non-zero
--     @timestampValidBits@
--
-- -   All queries used by the command /must/ be unavailable
--
-- -   If 'cmdWriteTimestamp' is called within a render pass instance, the
--     sum of @query@ and the number of bits set in the current subpass’s
--     view mask /must/ be less than or equal to the number of queries in
--     'Graphics.Vulkan.Core10.Handles.QueryPool'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pipelineStage@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     value
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.QueryPool' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support transfer, graphics, or compute operations
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and
--     'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
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
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits',
-- 'Graphics.Vulkan.Core10.Handles.QueryPool'
cmdWriteTimestamp :: CommandBuffer -> PipelineStageFlagBits -> QueryPool -> ("query" ::: Word32) -> IO ()
cmdWriteTimestamp commandBuffer pipelineStage queryPool query = do
  let vkCmdWriteTimestamp' = mkVkCmdWriteTimestamp (pVkCmdWriteTimestamp (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdWriteTimestamp' (commandBufferHandle (commandBuffer)) (pipelineStage) (queryPool) (query)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyQueryPoolResults
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> Buffer -> DeviceSize -> DeviceSize -> QueryResultFlags -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> Buffer -> DeviceSize -> DeviceSize -> QueryResultFlags -> IO ()

-- | vkCmdCopyQueryPoolResults - Copy the results of queries in a query pool
-- to a buffer object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     into which this command will be recorded.
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' is the query pool
--     managing the queries containing the desired results.
--
-- -   @firstQuery@ is the initial query index.
--
-- -   @queryCount@ is the number of queries. @firstQuery@ and @queryCount@
--     together define a range of queries.
--
-- -   @dstBuffer@ is a 'Graphics.Vulkan.Core10.Handles.Buffer' object that
--     will receive the results of the copy command.
--
-- -   @dstOffset@ is an offset into @dstBuffer@.
--
-- -   @stride@ is the stride in bytes between results for individual
--     queries within @dstBuffer@. The required size of the backing memory
--     for @dstBuffer@ is determined as described above for
--     'Graphics.Vulkan.Core10.Query.getQueryPoolResults'.
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlagBits'
--     specifying how and when results are returned.
--
-- = Description
--
-- 'cmdCopyQueryPoolResults' is guaranteed to see the effect of previous
-- uses of 'cmdResetQueryPool' in the same queue, without any additional
-- synchronization. Thus, the results will always reflect the most recent
-- use of the query.
--
-- 'Graphics.Vulkan.Core10.BaseType.Flags' has the same possible values
-- described above for the 'Graphics.Vulkan.Core10.BaseType.Flags'
-- parameter of 'Graphics.Vulkan.Core10.Query.getQueryPoolResults', but the
-- different style of execution causes some subtle behavioral differences.
-- Because 'cmdCopyQueryPoolResults' executes in order with respect to
-- other query commands, there is less ambiguity about which use of a query
-- is being requested.
--
-- Results for all requested occlusion queries, pipeline statistics
-- queries, transform feedback queries, and timestamp queries are written
-- as 64-bit unsigned integer values if
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
-- is set or 32-bit unsigned integer values otherwise. Performance queries
-- store results in a tightly packed array whose type is determined by the
-- @unit@ member of the corresponding
-- 'Graphics.Vulkan.Extensions.VK_KHR_performance_query.PerformanceCounterKHR'.
--
-- If neither of
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT'
-- and
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- are set, results are only written out for queries in the available
-- state.
--
-- If
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT'
-- is set, the implementation will wait for each query’s status to be in
-- the available state before retrieving the numerical results for that
-- query. This is guaranteed to reflect the most recent use of the query on
-- the same queue, assuming that the query is not being simultaneously used
-- by other queues. If the query does not become available in a finite
-- amount of time (e.g. due to not issuing a query since the last reset), a
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST' error /may/
-- occur.
--
-- Similarly, if
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is set and
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT'
-- is not set, the availability is guaranteed to reflect the most recent
-- use of the query on the same queue, assuming that the query is not being
-- simultaneously used by other queues. As with
-- 'Graphics.Vulkan.Core10.Query.getQueryPoolResults', implementations
-- /must/ guarantee that if they return a non-zero availability value, then
-- the numerical results are valid.
--
-- If
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
-- is set,
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT'
-- is not set, and the query’s status is unavailable, an intermediate
-- result value between zero and the final result value is written for that
-- query.
--
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
-- /must/ not be used if the pool’s
-- 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' is
-- 'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP'.
--
-- 'cmdCopyQueryPoolResults' is considered to be a transfer operation, and
-- its writes to buffer memory /must/ be synchronized using
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFER_BIT'
-- and
-- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFER_WRITE_BIT'
-- before using the results.
--
-- == Valid Usage
--
-- -   @dstOffset@ /must/ be less than the size of @dstBuffer@
--
-- -   @firstQuery@ /must/ be less than the number of queries in
--     'Graphics.Vulkan.Core10.Handles.QueryPool'
--
-- -   The sum of @firstQuery@ and @queryCount@ /must/ be less than or
--     equal to the number of queries in
--     'Graphics.Vulkan.Core10.Handles.QueryPool'
--
-- -   If
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
--     is not set in 'Graphics.Vulkan.Core10.BaseType.Flags' then
--     @dstOffset@ and @stride@ /must/ be multiples of @4@
--
-- -   If
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
--     is set in 'Graphics.Vulkan.Core10.BaseType.Flags' then @dstOffset@
--     and @stride@ /must/ be multiples of @8@
--
-- -   @dstBuffer@ /must/ have enough storage, from @dstOffset@, to contain
--     the result of each query, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-memorylayout here>
--
-- -   @dstBuffer@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP',
--     'Graphics.Vulkan.Core10.BaseType.Flags' /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     'Graphics.Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryPropertiesKHR'::@allowCommandBufferQueryCopies@
--     /must/ be 'Graphics.Vulkan.Core10.BaseType.TRUE'
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     'Graphics.Vulkan.Core10.BaseType.Flags' /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT',
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
--
-- -   If the 'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to
--     create 'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the 'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ have been
--     submitted once for each pass as retrieved via a call to
--     'Graphics.Vulkan.Extensions.VK_KHR_performance_query.getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'
--
-- -   'cmdCopyQueryPoolResults' /must/ not be called if the
--     'Graphics.Vulkan.Core10.Enums.QueryType.QueryType' used to create
--     'Graphics.Vulkan.Core10.Handles.QueryPool' was
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_INTEL'
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.QueryPool' handle
--
-- -   @dstBuffer@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be a valid
--     combination of
--     'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlagBits'
--     values
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Each of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', @dstBuffer@,
--     and 'Graphics.Vulkan.Core10.Handles.QueryPool' /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Handles.QueryPool',
-- 'Graphics.Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlags'
cmdCopyQueryPoolResults :: CommandBuffer -> QueryPool -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> ("stride" ::: DeviceSize) -> QueryResultFlags -> IO ()
cmdCopyQueryPoolResults commandBuffer queryPool firstQuery queryCount dstBuffer dstOffset stride flags = do
  let vkCmdCopyQueryPoolResults' = mkVkCmdCopyQueryPoolResults (pVkCmdCopyQueryPoolResults (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdCopyQueryPoolResults' (commandBufferHandle (commandBuffer)) (queryPool) (firstQuery) (queryCount) (dstBuffer) (dstOffset) (stride) (flags)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushConstants
  :: FunPtr (Ptr CommandBuffer_T -> PipelineLayout -> ShaderStageFlags -> Word32 -> Word32 -> Ptr () -> IO ()) -> Ptr CommandBuffer_T -> PipelineLayout -> ShaderStageFlags -> Word32 -> Word32 -> Ptr () -> IO ()

-- | vkCmdPushConstants - Update the values of push constants
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     in which the push constant update will be recorded.
--
-- -   @layout@ is the pipeline layout used to program the push constant
--     updates.
--
-- -   @stageFlags@ is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits'
--     specifying the shader stages that will use the push constants in the
--     updated range.
--
-- -   @offset@ is the start offset of the push constant range to update,
--     in units of bytes.
--
-- -   @size@ is the size of the push constant range to update, in units of
--     bytes.
--
-- -   @pValues@ is a pointer to an array of @size@ bytes containing the
--     new push constant values.
--
-- = Description
--
-- Note
--
-- As @stageFlags@ needs to include all flags the relevant push constant
-- ranges were created with, any flags that are not supported by the queue
-- family that the 'Graphics.Vulkan.Core10.Handles.CommandPool' used to
-- allocate 'Graphics.Vulkan.Core10.Handles.CommandBuffer' was created on
-- are ignored.
--
-- == Valid Usage
--
-- -   For each byte in the range specified by @offset@ and @size@ and for
--     each shader stage in @stageFlags@, there /must/ be a push constant
--     range in @layout@ that includes that byte and that stage
--
-- -   For each byte in the range specified by @offset@ and @size@ and for
--     each push constant range that overlaps that byte, @stageFlags@
--     /must/ include all stages in that push constant range’s
--     'Graphics.Vulkan.Core10.PipelineLayout.PushConstantRange'::@stageFlags@
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   @size@ /must/ be a multiple of @4@
--
-- -   @offset@ /must/ be less than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPushConstantsSize@
--
-- -   @size@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPushConstantsSize@
--     minus @offset@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @layout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   @stageFlags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits'
--     values
--
-- -   @stageFlags@ /must/ not be @0@
--
-- -   @pValues@ /must/ be a valid pointer to an array of @size@ bytes
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics, or compute operations
--
-- -   @size@ /must/ be greater than @0@
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and @layout@
--     /must/ have been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Handles.PipelineLayout',
-- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags'
cmdPushConstants :: CommandBuffer -> PipelineLayout -> ShaderStageFlags -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("values" ::: Ptr ()) -> IO ()
cmdPushConstants commandBuffer layout stageFlags offset size values = do
  let vkCmdPushConstants' = mkVkCmdPushConstants (pVkCmdPushConstants (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdPushConstants' (commandBufferHandle (commandBuffer)) (layout) (stageFlags) (offset) (size) (values)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginRenderPass
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (RenderPassBeginInfo a) -> SubpassContents -> IO ()) -> Ptr CommandBuffer_T -> Ptr (RenderPassBeginInfo a) -> SubpassContents -> IO ()

-- | vkCmdBeginRenderPass - Begin a new render pass
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     in which to record the command.
--
-- -   @pRenderPassBegin@ is a pointer to a 'RenderPassBeginInfo' structure
--     specifying the render pass to begin an instance of, and the
--     framebuffer the instance uses.
--
-- -   @contents@ is a
--     'Graphics.Vulkan.Core10.Enums.SubpassContents.SubpassContents' value
--     specifying how the commands in the first subpass will be provided.
--
-- = Description
--
-- After beginning a render pass instance, the command buffer is ready to
-- record the commands for the first subpass of that render pass.
--
-- == Valid Usage
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentDescription' structures or
--     the @layout@ member of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentReference' structures
--     specified when creating the render pass specified in the
--     'Graphics.Vulkan.Core10.Handles.RenderPass' member of
--     @pRenderPassBegin@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the 'Graphics.Vulkan.Core10.Handles.Framebuffer' member
--     of @pRenderPassBegin@ /must/ have been created with a @usage@ value
--     including
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentDescription' structures or
--     the @layout@ member of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentReference' structures
--     specified when creating the render pass specified in the
--     'Graphics.Vulkan.Core10.Handles.RenderPass' member of
--     @pRenderPassBegin@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the 'Graphics.Vulkan.Core10.Handles.Framebuffer' member
--     of @pRenderPassBegin@ /must/ have been created with a @usage@ value
--     including
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentDescription' structures or
--     the @layout@ member of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentReference' structures
--     specified when creating the render pass specified in the
--     'Graphics.Vulkan.Core10.Handles.RenderPass' member of
--     @pRenderPassBegin@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the 'Graphics.Vulkan.Core10.Handles.Framebuffer' member
--     of @pRenderPassBegin@ /must/ have been created with a @usage@ value
--     including
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   If any of the @stencilInitialLayout@ or @stencilFinalLayout@ member
--     of the
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
--     structures or the @stencilLayout@ member of the
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentReferenceStencilLayout'
--     structures specified when creating the render pass specified in the
--     'Graphics.Vulkan.Core10.Handles.RenderPass' member of
--     @pRenderPassBegin@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the 'Graphics.Vulkan.Core10.Handles.Framebuffer' member
--     of @pRenderPassBegin@ /must/ have been created with a @usage@ value
--     including
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentDescription' structures or
--     the @layout@ member of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentReference' structures
--     specified when creating the render pass specified in the
--     'Graphics.Vulkan.Core10.Handles.RenderPass' member of
--     @pRenderPassBegin@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the 'Graphics.Vulkan.Core10.Handles.Framebuffer' member
--     of @pRenderPassBegin@ /must/ have been created with a @usage@ value
--     including
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentDescription' structures or
--     the @layout@ member of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentReference' structures
--     specified when creating the render pass specified in the
--     'Graphics.Vulkan.Core10.Handles.RenderPass' member of
--     @pRenderPassBegin@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the 'Graphics.Vulkan.Core10.Handles.Framebuffer' member
--     of @pRenderPassBegin@ /must/ have been created with a @usage@ value
--     including
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentDescription' structures or
--     the @layout@ member of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentReference' structures
--     specified when creating the render pass specified in the
--     'Graphics.Vulkan.Core10.Handles.RenderPass' member of
--     @pRenderPassBegin@ is
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the 'Graphics.Vulkan.Core10.Handles.Framebuffer' member
--     of @pRenderPassBegin@ /must/ have been created with a @usage@ value
--     including
--     'Graphics.Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--
-- -   If any of the @initialLayout@ members of the
--     'Graphics.Vulkan.Core10.Pass.AttachmentDescription' structures
--     specified when creating the render pass specified in the
--     'Graphics.Vulkan.Core10.Handles.RenderPass' member of
--     @pRenderPassBegin@ is not
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED',
--     then each such @initialLayout@ /must/ be equal to the current layout
--     of the corresponding attachment image subresource of the framebuffer
--     specified in the 'Graphics.Vulkan.Core10.Handles.Framebuffer' member
--     of @pRenderPassBegin@
--
-- -   The @srcStageMask@ and @dstStageMask@ members of any element of the
--     @pDependencies@ member of
--     'Graphics.Vulkan.Core10.Pass.RenderPassCreateInfo' used to create
--     'Graphics.Vulkan.Core10.Handles.RenderPass' /must/ be supported by
--     the capabilities of the queue family identified by the
--     @queueFamilyIndex@ member of the
--     'Graphics.Vulkan.Core10.CommandPool.CommandPoolCreateInfo' used to
--     create the command pool which
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--
-- -   For any attachment in 'Graphics.Vulkan.Core10.Handles.Framebuffer'
--     that is used by 'Graphics.Vulkan.Core10.Handles.RenderPass' and is
--     bound to memory locations that are also bound to another attachment
--     used by 'Graphics.Vulkan.Core10.Handles.RenderPass', and if at least
--     one of those uses causes either attachment to be written to, both
--     attachments /must/ have had the
--     'Graphics.Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT'
--     set
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pRenderPassBegin@ /must/ be a valid pointer to a valid
--     'RenderPassBeginInfo' structure
--
-- -   @contents@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.SubpassContents.SubpassContents' value
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a primary
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Graphics                                                                                                                            |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer', 'RenderPassBeginInfo',
-- 'Graphics.Vulkan.Core10.Enums.SubpassContents.SubpassContents'
cmdBeginRenderPass :: PokeChain a => CommandBuffer -> RenderPassBeginInfo a -> SubpassContents -> IO ()
cmdBeginRenderPass commandBuffer renderPassBegin contents = evalContT $ do
  let vkCmdBeginRenderPass' = mkVkCmdBeginRenderPass (pVkCmdBeginRenderPass (deviceCmds (commandBuffer :: CommandBuffer)))
  pRenderPassBegin <- ContT $ withCStruct (renderPassBegin)
  lift $ vkCmdBeginRenderPass' (commandBufferHandle (commandBuffer)) pRenderPassBegin (contents)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdNextSubpass
  :: FunPtr (Ptr CommandBuffer_T -> SubpassContents -> IO ()) -> Ptr CommandBuffer_T -> SubpassContents -> IO ()

-- | vkCmdNextSubpass - Transition to the next subpass of a render pass
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     in which to record the command.
--
-- -   @contents@ specifies how the commands in the next subpass will be
--     provided, in the same fashion as the corresponding parameter of
--     'cmdBeginRenderPass'.
--
-- = Description
--
-- The subpass index for a render pass begins at zero when
-- 'cmdBeginRenderPass' is recorded, and increments each time
-- 'cmdNextSubpass' is recorded.
--
-- Moving to the next subpass automatically performs any multisample
-- resolve operations in the subpass being ended. End-of-subpass
-- multisample resolves are treated as color attachment writes for the
-- purposes of synchronization. This applies to resolve operations for both
-- color and depth\/stencil attachments. That is, they are considered to
-- execute in the
-- 'Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
-- pipeline stage and their writes are synchronized with
-- 'Graphics.Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
-- Synchronization between rendering within a subpass and any resolve
-- operations at the end of the subpass occurs automatically, without need
-- for explicit dependencies or pipeline barriers. However, if the resolve
-- attachment is also used in a different subpass, an explicit dependency
-- is needed.
--
-- After transitioning to the next subpass, the application /can/ record
-- the commands for that subpass.
--
-- == Valid Usage
--
-- -   The current subpass index /must/ be less than the number of
--     subpasses in the render pass minus one
--
-- -   This command /must/ not be recorded when transform feedback is
--     active
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @contents@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.SubpassContents.SubpassContents' value
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a primary
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer',
-- 'Graphics.Vulkan.Core10.Enums.SubpassContents.SubpassContents'
cmdNextSubpass :: CommandBuffer -> SubpassContents -> IO ()
cmdNextSubpass commandBuffer contents = do
  let vkCmdNextSubpass' = mkVkCmdNextSubpass (pVkCmdNextSubpass (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdNextSubpass' (commandBufferHandle (commandBuffer)) (contents)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndRenderPass
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- | vkCmdEndRenderPass - End the current render pass
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is the command buffer
--     in which to end the current render pass instance.
--
-- = Description
--
-- Ending a render pass instance performs any multisample resolve
-- operations on the final subpass.
--
-- == Valid Usage
--
-- -   The current subpass index /must/ be equal to the number of subpasses
--     in the render pass minus one
--
-- -   This command /must/ not be recorded when transform feedback is
--     active
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a primary
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdEndRenderPass :: CommandBuffer -> IO ()
cmdEndRenderPass commandBuffer = do
  let vkCmdEndRenderPass' = mkVkCmdEndRenderPass (pVkCmdEndRenderPass (deviceCmds (commandBuffer :: CommandBuffer)))
  vkCmdEndRenderPass' (commandBufferHandle (commandBuffer))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdExecuteCommands
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr (Ptr CommandBuffer_T) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr (Ptr CommandBuffer_T) -> IO ()

-- | vkCmdExecuteCommands - Execute a secondary command buffer from a primary
-- command buffer
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a handle to a
--     primary command buffer that the secondary command buffers are
--     executed in.
--
-- -   @commandBufferCount@ is the length of the @pCommandBuffers@ array.
--
-- -   @pCommandBuffers@ is a pointer to an array of @commandBufferCount@
--     secondary command buffer handles, which are recorded to execute in
--     the primary command buffer in the order they are listed in the
--     array.
--
-- = Description
--
-- If any element of @pCommandBuffers@ was not recorded with the
-- 'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
-- flag, and it was recorded into any other primary command buffer which is
-- currently in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle executable or recording state>,
-- that primary command buffer becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   Each element of @pCommandBuffers@ /must/ have been allocated with a
--     @level@ of
--     'Graphics.Vulkan.Core10.Enums.CommandBufferLevel.COMMAND_BUFFER_LEVEL_SECONDARY'
--
-- -   Each element of @pCommandBuffers@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--     flag, it /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--     flag, it /must/ not have already been recorded to
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--     flag, it /must/ not appear more than once in @pCommandBuffers@
--
-- -   Each element of @pCommandBuffers@ /must/ have been allocated from a
--     'Graphics.Vulkan.Core10.Handles.CommandPool' that was created for
--     the same queue family as the
--     'Graphics.Vulkan.Core10.Handles.CommandPool' from which
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated
--
-- -   If 'cmdExecuteCommands' is being called within a render pass
--     instance, that render pass instance /must/ have been begun with the
--     @contents@ parameter of 'cmdBeginRenderPass' set to
--     'Graphics.Vulkan.Core10.Enums.SubpassContents.SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS'
--
-- -   If 'cmdExecuteCommands' is being called within a render pass
--     instance, each element of @pCommandBuffers@ /must/ have been
--     recorded with the
--     'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
--
-- -   If 'cmdExecuteCommands' is being called within a render pass
--     instance, each element of @pCommandBuffers@ /must/ have been
--     recorded with
--     'Graphics.Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@subpass@
--     set to the index of the subpass which the given command buffer will
--     be executed in
--
-- -   If 'cmdExecuteCommands' is being called within a render pass
--     instance, the render passes specified in the
--     @pBeginInfo->pInheritanceInfo->renderPass@ members of the
--     'Graphics.Vulkan.Core10.CommandBuffer.beginCommandBuffer' commands
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the current render pass
--
-- -   If 'cmdExecuteCommands' is being called within a render pass
--     instance, and any element of @pCommandBuffers@ was recorded with
--     'Graphics.Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::'Graphics.Vulkan.Core10.Handles.Framebuffer'
--     not equal to 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', that
--     'Graphics.Vulkan.Core10.Handles.Framebuffer' /must/ match the
--     'Graphics.Vulkan.Core10.Handles.Framebuffer' used in the current
--     render pass instance
--
-- -   If 'cmdExecuteCommands' is being called within a render pass
--     instance that included
--     'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'
--     in the @pNext@ chain of 'RenderPassBeginInfo', then each element of
--     @pCommandBuffers@ /must/ have been recorded with
--     'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM'
--     in the @pNext@ chain of VkCommandBufferBeginInfo
--
-- -   If 'cmdExecuteCommands' is being called within a render pass
--     instance that included
--     'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'
--     in the @pNext@ chain of 'RenderPassBeginInfo', then each element of
--     @pCommandBuffers@ /must/ have been recorded with
--     'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM'::@transform@
--     identical to
--     'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'::@transform@
--
-- -   If 'cmdExecuteCommands' is being called within a render pass
--     instance that included
--     'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'
--     in the @pNext@ chain of 'RenderPassBeginInfo', then each element of
--     @pCommandBuffers@ /must/ have been recorded with
--     'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM'::@renderArea@
--     identical to 'RenderPassBeginInfo'::@renderArea@
--
-- -   If 'cmdExecuteCommands' is not being called within a render pass
--     instance, each element of @pCommandBuffers@ /must/ not have been
--     recorded with the
--     'Graphics.Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-inheritedQueries inherited queries>
--     feature is not enabled,
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ not have any
--     queries
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' has a
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION' query
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>,
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with
--     'Graphics.Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@occlusionQueryEnable@
--     set to 'Graphics.Vulkan.Core10.BaseType.TRUE'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' has a
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION' query
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>,
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with
--     'Graphics.Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@queryFlags@
--     having all bits set that are set for the query
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' has a
--     'Graphics.Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS'
--     query
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>,
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with
--     'Graphics.Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@pipelineStatistics@
--     having all bits set that are set in the
--     'Graphics.Vulkan.Core10.Handles.QueryPool' the query uses
--
-- -   Each element of @pCommandBuffers@ /must/ not begin any query types
--     that are
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
--     in 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is a protected
--     command buffer, then each element of @pCommandBuffers@ /must/ be a
--     protected command buffer
--
-- -   If 'Graphics.Vulkan.Core10.Handles.CommandBuffer' is an unprotected
--     command buffer, then each element of @pCommandBuffers@ /must/ be an
--     unprotected command buffer
--
-- -   This command /must/ not be recorded when transform feedback is
--     active
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pCommandBuffers@ /must/ be a valid pointer to an array of
--     @commandBufferCount@ valid
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' handles
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ support transfer, graphics, or compute operations
--
-- -   'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/ be a primary
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer'
--
-- -   @commandBufferCount@ /must/ be greater than @0@
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.CommandBuffer', and the
--     elements of @pCommandBuffers@ /must/ have been created, allocated,
--     or retrieved from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.CommandBuffer' /must/
--     be externally synchronized
--
-- -   Host access to the 'Graphics.Vulkan.Core10.Handles.CommandPool' that
--     'Graphics.Vulkan.Core10.Handles.CommandBuffer' was allocated from
--     /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Transfer                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandBuffer'
cmdExecuteCommands :: CommandBuffer -> ("commandBuffers" ::: Vector CommandBuffer) -> IO ()
cmdExecuteCommands commandBuffer commandBuffers = evalContT $ do
  let vkCmdExecuteCommands' = mkVkCmdExecuteCommands (pVkCmdExecuteCommands (deviceCmds (commandBuffer :: CommandBuffer)))
  pPCommandBuffers <- ContT $ allocaBytesAligned @(Ptr CommandBuffer_T) ((Data.Vector.length (commandBuffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBuffers `plusPtr` (8 * (i)) :: Ptr (Ptr CommandBuffer_T)) (commandBufferHandle (e))) (commandBuffers)
  lift $ vkCmdExecuteCommands' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (commandBuffers)) :: Word32)) (pPCommandBuffers)
  pure $ ()


-- | VkViewport - Structure specifying a viewport
--
-- = Description
--
-- The framebuffer depth coordinate @z@f /may/ be represented using either
-- a fixed-point or floating-point representation. However, a
-- floating-point representation /must/ be used if the depth\/stencil
-- attachment has a floating-point depth component. If an m-bit fixed-point
-- representation is used, we assume that it represents each value
-- \(\frac{k}{2^m - 1}\), where k ∈ { 0, 1, …​, 2m-1 }, as k (e.g. 1.0 is
-- represented in binary as a string of all ones).
--
-- The viewport parameters shown in the above equations are found from
-- these values as
--
-- -   ox = @x@ + @width@ \/ 2
--
-- -   oy = @y@ + @height@ \/ 2
--
-- -   oz = @minDepth@
--
-- -   px = @width@
--
-- -   py = @height@
--
-- -   pz = @maxDepth@ - @minDepth@.
--
-- If a render pass transform is enabled, the values (px,py) and (ox, oy)
-- defining the viewport are transformed as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-renderpass-transform render pass transform>
-- before participating in the viewport transform.
--
-- The application /can/ specify a negative term for @height@, which has
-- the effect of negating the y coordinate in clip space before performing
-- the transform. When using a negative @height@, the application /should/
-- also adjust the @y@ value to point to the lower left corner of the
-- viewport instead of the upper left corner. Using the negative @height@
-- allows the application to avoid having to negate the y component of the
-- @Position@ output from the last vertex processing stage in shaders that
-- also target other graphics APIs.
--
-- The width and height of the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxViewportDimensions implementation-dependent maximum viewport dimensions>
-- /must/ be greater than or equal to the width and height of the largest
-- image which /can/ be created and attached to a framebuffer.
--
-- The floating-point viewport bounds are represented with an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-viewportSubPixelBits implementation-dependent precision>.
--
-- == Valid Usage
--
-- -   @width@ /must/ be greater than @0.0@
--
-- -   @width@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewportDimensions@[0]
--
-- -   The absolute value of @height@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewportDimensions@[1]
--
-- -   @x@ /must/ be greater than or equal to @viewportBoundsRange@[0]
--
-- -   (@x@ + @width@) /must/ be less than or equal to
--     @viewportBoundsRange@[1]
--
-- -   @y@ /must/ be greater than or equal to @viewportBoundsRange@[0]
--
-- -   @y@ /must/ be less than or equal to @viewportBoundsRange@[1]
--
-- -   (@y@ + @height@) /must/ be greater than or equal to
--     @viewportBoundsRange@[0]
--
-- -   (@y@ + @height@) /must/ be less than or equal to
--     @viewportBoundsRange@[1]
--
-- -   Unless
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_EXT_depth_range_unrestricted@
--     extension is enabled @minDepth@ /must/ be between @0.0@ and @1.0@,
--     inclusive
--
-- -   Unless
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_EXT_depth_range_unrestricted@
--     extension is enabled @maxDepth@ /must/ be between @0.0@ and @1.0@,
--     inclusive
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo',
-- 'cmdSetViewport'
data Viewport = Viewport
  { -- | @x@ and @y@ are the viewport’s upper left corner (x,y).
    x :: Float
  , -- No documentation found for Nested "VkViewport" "y"
    y :: Float
  , -- | @width@ and @height@ are the viewport’s width and height, respectively.
    width :: Float
  , -- No documentation found for Nested "VkViewport" "height"
    height :: Float
  , -- | @minDepth@ and @maxDepth@ are the depth range for the viewport. It is
    -- valid for @minDepth@ to be greater than or equal to @maxDepth@.
    minDepth :: Float
  , -- No documentation found for Nested "VkViewport" "maxDepth"
    maxDepth :: Float
  }
  deriving (Typeable)
deriving instance Show Viewport

instance ToCStruct Viewport where
  withCStruct x f = allocaBytesAligned 24 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Viewport{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (x))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (y))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (width))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (height))
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (minDepth))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (maxDepth))
    f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct Viewport where
  peekCStruct p = do
    x <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    y <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    width <- peek @CFloat ((p `plusPtr` 8 :: Ptr CFloat))
    height <- peek @CFloat ((p `plusPtr` 12 :: Ptr CFloat))
    minDepth <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    maxDepth <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    pure $ Viewport
             ((\(CFloat a) -> a) x) ((\(CFloat a) -> a) y) ((\(CFloat a) -> a) width) ((\(CFloat a) -> a) height) ((\(CFloat a) -> a) minDepth) ((\(CFloat a) -> a) maxDepth)

instance Storable Viewport where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Viewport where
  zero = Viewport
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkRect2D - Structure specifying a two-dimensional subregion
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo',
-- 'ClearRect',
-- 'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display_swapchain.DisplayPresentInfoKHR',
-- 'Graphics.Vulkan.Core10.SharedTypes.Extent2D',
-- 'Graphics.Vulkan.Core10.SharedTypes.Offset2D',
-- 'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV',
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo',
-- 'RenderPassBeginInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorNV',
-- 'cmdSetScissor',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.getPhysicalDevicePresentRectanglesKHR'
data Rect2D = Rect2D
  { -- | @offset@ is a 'Graphics.Vulkan.Core10.SharedTypes.Offset2D' specifying
    -- the rectangle offset.
    offset :: Offset2D
  , -- | @extent@ is a 'Graphics.Vulkan.Core10.SharedTypes.Extent2D' specifying
    -- the rectangle extent.
    extent :: Extent2D
  }
  deriving (Typeable)
deriving instance Show Rect2D

instance ToCStruct Rect2D where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Rect2D{..} f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr Offset2D)) (offset) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr Extent2D)) (extent) . ($ ())
    lift $ f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr Offset2D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr Extent2D)) (zero) . ($ ())
    lift $ f

instance FromCStruct Rect2D where
  peekCStruct p = do
    offset <- peekCStruct @Offset2D ((p `plusPtr` 0 :: Ptr Offset2D))
    extent <- peekCStruct @Extent2D ((p `plusPtr` 8 :: Ptr Extent2D))
    pure $ Rect2D
             offset extent

instance Zero Rect2D where
  zero = Rect2D
           zero
           zero


-- | VkClearRect - Structure specifying a clear rectangle
--
-- = Description
--
-- The layers [@baseArrayLayer@, @baseArrayLayer@ + @layerCount@) counting
-- from the base layer of the attachment image view are cleared.
--
-- = See Also
--
-- 'Rect2D', 'cmdClearAttachments'
data ClearRect = ClearRect
  { -- | @rect@ is the two-dimensional region to be cleared.
    rect :: Rect2D
  , -- | @baseArrayLayer@ is the first layer to be cleared.
    baseArrayLayer :: Word32
  , -- | @layerCount@ is the number of layers to clear.
    layerCount :: Word32
  }
  deriving (Typeable)
deriving instance Show ClearRect

instance ToCStruct ClearRect where
  withCStruct x f = allocaBytesAligned 24 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ClearRect{..} f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr Rect2D)) (rect) . ($ ())
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (baseArrayLayer)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (layerCount)
    lift $ f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr Rect2D)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ f

instance FromCStruct ClearRect where
  peekCStruct p = do
    rect <- peekCStruct @Rect2D ((p `plusPtr` 0 :: Ptr Rect2D))
    baseArrayLayer <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    layerCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ ClearRect
             rect baseArrayLayer layerCount

instance Zero ClearRect where
  zero = ClearRect
           zero
           zero
           zero


-- | VkBufferCopy - Structure specifying a buffer copy operation
--
-- == Valid Usage
--
-- -   The @size@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize', 'cmdCopyBuffer'
data BufferCopy = BufferCopy
  { -- | @srcOffset@ is the starting offset in bytes from the start of
    -- @srcBuffer@.
    srcOffset :: DeviceSize
  , -- | @dstOffset@ is the starting offset in bytes from the start of
    -- @dstBuffer@.
    dstOffset :: DeviceSize
  , -- | @size@ is the number of bytes to copy.
    size :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show BufferCopy

instance ToCStruct BufferCopy where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCopy{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (srcOffset)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (dstOffset)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BufferCopy where
  peekCStruct p = do
    srcOffset <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    dstOffset <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ BufferCopy
             srcOffset dstOffset size

instance Storable BufferCopy where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferCopy where
  zero = BufferCopy
           zero
           zero
           zero


-- | VkImageCopy - Structure specifying an image copy operation
--
-- = Description
--
-- For 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D' images,
-- copies are performed slice by slice starting with the @z@ member of the
-- @srcOffset@ or @dstOffset@, and copying @depth@ slices. For images with
-- multiple layers, copies are performed layer by layer starting with the
-- @baseArrayLayer@ member of the @srcSubresource@ or @dstSubresource@ and
-- copying @layerCount@ layers. Image data /can/ be copied between images
-- with different image types. If one image is
-- 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D' and the other
-- image is 'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' with
-- multiple layers, then each slice is copied to or from a different layer.
--
-- Copies involving a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
-- specify the region to be copied in terms of the /plane/ to be copied,
-- not the coordinates of the multi-planar image. This means that copies
-- accessing the R\/B planes of “@_422@” format images /must/ fit the
-- copied region within half the @width@ of the parent image, and that
-- copies accessing the R\/B planes of “@_420@” format images /must/ fit
-- the copied region within half the @width@ and @height@ of the parent
-- image.
--
-- == Valid Usage
--
-- -   If neither the calling command’s @srcImage@ nor the calling
--     command’s @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
--     then the @aspectMask@ member of @srcSubresource@ and
--     @dstSubresource@ /must/ match
--
-- -   If the calling command’s @srcImage@ has a
--     'Graphics.Vulkan.Core10.Enums.Format.Format' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion two planes>
--     then the @srcSubresource@ @aspectMask@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--
-- -   If the calling command’s @srcImage@ has a
--     'Graphics.Vulkan.Core10.Enums.Format.Format' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion three planes>
--     then the @srcSubresource@ @aspectMask@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   If the calling command’s @dstImage@ has a
--     'Graphics.Vulkan.Core10.Enums.Format.Format' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion two planes>
--     then the @dstSubresource@ @aspectMask@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--
-- -   If the calling command’s @dstImage@ has a
--     'Graphics.Vulkan.Core10.Enums.Format.Format' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion three planes>
--     then the @dstSubresource@ @aspectMask@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   If the calling command’s @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
--     and the @dstImage@ does not have a multi-planar image format, the
--     @dstSubresource@ @aspectMask@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   If the calling command’s @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
--     and the @srcImage@ does not have a multi-planar image format, the
--     @srcSubresource@ @aspectMask@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   The number of slices of the @extent@ (for 3D) or layers of the
--     @srcSubresource@ (for non-3D) /must/ match the number of slices of
--     the @extent@ (for 3D) or layers of the @dstSubresource@ (for non-3D)
--
-- -   If either of the calling command’s @srcImage@ or @dstImage@
--     parameters are of 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType'
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', the
--     @baseArrayLayer@ and @layerCount@ members of the corresponding
--     subresource /must/ be @0@ and @1@, respectively
--
-- -   The @aspectMask@ member of @srcSubresource@ /must/ specify aspects
--     present in the calling command’s @srcImage@
--
-- -   The @aspectMask@ member of @dstSubresource@ /must/ specify aspects
--     present in the calling command’s @dstImage@
--
-- -   @srcOffset.x@ and (@extent.width@ + @srcOffset.x@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource width
--
-- -   @srcOffset.y@ and (@extent.height@ + @srcOffset.y@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource height
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then
--     @srcOffset.y@ /must/ be @0@ and @extent.height@ /must/ be @1@.
--
-- -   @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource depth
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then
--     @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/ be @1@.
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then
--     @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/ be @1@.
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then
--     @srcOffset.z@ /must/ be @0@.
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then
--     @dstOffset.z@ /must/ be @0@.
--
-- -   If both @srcImage@ and @dstImage@ are of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' then
--     @extent.depth@ /must/ be @1@.
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and the
--     @dstImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then
--     @extent.depth@ /must/ equal to the @layerCount@ member of
--     @srcSubresource@.
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and the
--     @srcImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then
--     @extent.depth@ /must/ equal to the @layerCount@ member of
--     @dstSubresource@.
--
-- -   @dstOffset.x@ and (@extent.width@ + @dstOffset.x@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource width
--
-- -   @dstOffset.y@ and (@extent.height@ + @dstOffset.y@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource height
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then
--     @dstOffset.y@ /must/ be @0@ and @extent.height@ /must/ be @1@.
--
-- -   @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource depth
--
-- -   If the calling command’s @srcImage@ is a compressed image, or a
--     /single-plane/, “@_422@” image format, all members of @srcOffset@
--     /must/ be a multiple of the corresponding dimensions of the
--     compressed texel block
--
-- -   If the calling command’s @srcImage@ is a compressed image, or a
--     /single-plane/, “@_422@” image format, @extent.width@ /must/ be a
--     multiple of the compressed texel block width or (@extent.width@ +
--     @srcOffset.x@) /must/ equal the source image subresource width
--
-- -   If the calling command’s @srcImage@ is a compressed image, or a
--     /single-plane/, “@_422@” image format, @extent.height@ /must/ be a
--     multiple of the compressed texel block height or (@extent.height@ +
--     @srcOffset.y@) /must/ equal the source image subresource height
--
-- -   If the calling command’s @srcImage@ is a compressed image, or a
--     /single-plane/, “@_422@” image format, @extent.depth@ /must/ be a
--     multiple of the compressed texel block depth or (@extent.depth@ +
--     @srcOffset.z@) /must/ equal the source image subresource depth
--
-- -   If the calling command’s @dstImage@ is a compressed format image, or
--     a /single-plane/, “@_422@” image format, all members of @dstOffset@
--     /must/ be a multiple of the corresponding dimensions of the
--     compressed texel block
--
-- -   If the calling command’s @dstImage@ is a compressed format image, or
--     a /single-plane/, “@_422@” image format, @extent.width@ /must/ be a
--     multiple of the compressed texel block width or (@extent.width@ +
--     @dstOffset.x@) /must/ equal the destination image subresource width
--
-- -   If the calling command’s @dstImage@ is a compressed format image, or
--     a /single-plane/, “@_422@” image format, @extent.height@ /must/ be a
--     multiple of the compressed texel block height or (@extent.height@ +
--     @dstOffset.y@) /must/ equal the destination image subresource height
--
-- -   If the calling command’s @dstImage@ is a compressed format image, or
--     a /single-plane/, “@_422@” image format, @extent.depth@ /must/ be a
--     multiple of the compressed texel block depth or (@extent.depth@ +
--     @dstOffset.z@) /must/ equal the destination image subresource depth
--
-- == Valid Usage (Implicit)
--
-- -   @srcSubresource@ /must/ be a valid
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers'
--     structure
--
-- -   @dstSubresource@ /must/ be a valid
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SharedTypes.Extent3D',
-- 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers',
-- 'Graphics.Vulkan.Core10.SharedTypes.Offset3D', 'cmdCopyImage'
data ImageCopy = ImageCopy
  { -- | @srcSubresource@ and @dstSubresource@ are
    -- 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers' structures
    -- specifying the image subresources of the images used for the source and
    -- destination image data, respectively.
    srcSubresource :: ImageSubresourceLayers
  , -- | @srcOffset@ and @dstOffset@ select the initial @x@, @y@, and @z@ offsets
    -- in texels of the sub-regions of the source and destination image data.
    srcOffset :: Offset3D
  , -- No documentation found for Nested "VkImageCopy" "dstSubresource"
    dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageCopy" "dstOffset"
    dstOffset :: Offset3D
  , -- | @extent@ is the size in texels of the image to copy in @width@, @height@
    -- and @depth@.
    extent :: Extent3D
  }
  deriving (Typeable)
deriving instance Show ImageCopy

instance ToCStruct ImageCopy where
  withCStruct x f = allocaBytesAligned 68 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCopy{..} f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (srcSubresource) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Offset3D)) (srcOffset) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers)) (dstSubresource) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 44 :: Ptr Offset3D)) (dstOffset) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr Extent3D)) (extent) . ($ ())
    lift $ f
  cStructSize = 68
  cStructAlignment = 4
  pokeZeroCStruct p f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Offset3D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 44 :: Ptr Offset3D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr Extent3D)) (zero) . ($ ())
    lift $ f

instance FromCStruct ImageCopy where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers))
    srcOffset <- peekCStruct @Offset3D ((p `plusPtr` 16 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers))
    dstOffset <- peekCStruct @Offset3D ((p `plusPtr` 44 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 56 :: Ptr Extent3D))
    pure $ ImageCopy
             srcSubresource srcOffset dstSubresource dstOffset extent

instance Zero ImageCopy where
  zero = ImageCopy
           zero
           zero
           zero
           zero
           zero


-- | VkImageBlit - Structure specifying an image blit operation
--
-- = Description
--
-- For each element of the @pRegions@ array, a blit operation is performed
-- the specified source and destination regions.
--
-- == Valid Usage
--
-- -   The @aspectMask@ member of @srcSubresource@ and @dstSubresource@
--     /must/ match
--
-- -   The @layerCount@ member of @srcSubresource@ and @dstSubresource@
--     /must/ match
--
-- -   If either of the calling command’s @srcImage@ or @dstImage@
--     parameters are of 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType'
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', the
--     @baseArrayLayer@ and @layerCount@ members of both @srcSubresource@
--     and @dstSubresource@ /must/ be @0@ and @1@, respectively
--
-- -   The @aspectMask@ member of @srcSubresource@ /must/ specify aspects
--     present in the calling command’s @srcImage@
--
-- -   The @aspectMask@ member of @dstSubresource@ /must/ specify aspects
--     present in the calling command’s @dstImage@
--
-- -   @srcOffset@[0].x and @srcOffset@[1].x /must/ both be greater than or
--     equal to @0@ and less than or equal to the source image subresource
--     width
--
-- -   @srcOffset@[0].y and @srcOffset@[1].y /must/ both be greater than or
--     equal to @0@ and less than or equal to the source image subresource
--     height
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then
--     @srcOffset@[0].y /must/ be @0@ and @srcOffset@[1].y /must/ be @1@.
--
-- -   @srcOffset@[0].z and @srcOffset@[1].z /must/ both be greater than or
--     equal to @0@ and less than or equal to the source image subresource
--     depth
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then
--     @srcOffset@[0].z /must/ be @0@ and @srcOffset@[1].z /must/ be @1@.
--
-- -   @dstOffset@[0].x and @dstOffset@[1].x /must/ both be greater than or
--     equal to @0@ and less than or equal to the destination image
--     subresource width
--
-- -   @dstOffset@[0].y and @dstOffset@[1].y /must/ both be greater than or
--     equal to @0@ and less than or equal to the destination image
--     subresource height
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then
--     @dstOffset@[0].y /must/ be @0@ and @dstOffset@[1].y /must/ be @1@.
--
-- -   @dstOffset@[0].z and @dstOffset@[1].z /must/ both be greater than or
--     equal to @0@ and less than or equal to the destination image
--     subresource depth
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then
--     @dstOffset@[0].z /must/ be @0@ and @dstOffset@[1].z /must/ be @1@.
--
-- == Valid Usage (Implicit)
--
-- -   @srcSubresource@ /must/ be a valid
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers'
--     structure
--
-- -   @dstSubresource@ /must/ be a valid
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers',
-- 'Graphics.Vulkan.Core10.SharedTypes.Offset3D', 'cmdBlitImage'
data ImageBlit = ImageBlit
  { -- | @srcSubresource@ is the subresource to blit from.
    srcSubresource :: ImageSubresourceLayers
  , -- | @srcOffsets@ is a pointer to an array of two
    -- 'Graphics.Vulkan.Core10.SharedTypes.Offset3D' structures specifying the
    -- bounds of the source region within @srcSubresource@.
    srcOffsets :: (Offset3D, Offset3D)
  , -- | @dstSubresource@ is the subresource to blit into.
    dstSubresource :: ImageSubresourceLayers
  , -- | @dstOffsets@ is a pointer to an array of two
    -- 'Graphics.Vulkan.Core10.SharedTypes.Offset3D' structures specifying the
    -- bounds of the destination region within @dstSubresource@.
    dstOffsets :: (Offset3D, Offset3D)
  }
  deriving (Typeable)
deriving instance Show ImageBlit

instance ToCStruct ImageBlit where
  withCStruct x f = allocaBytesAligned 80 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageBlit{..} f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (srcSubresource) . ($ ())
    let pSrcOffsets' = lowerArrayPtr ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector 2 Offset3D)))
    case (srcOffsets) of
      (e0, e1) -> do
        ContT $ pokeCStruct (pSrcOffsets' :: Ptr Offset3D) (e0) . ($ ())
        ContT $ pokeCStruct (pSrcOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr ImageSubresourceLayers)) (dstSubresource) . ($ ())
    let pDstOffsets' = lowerArrayPtr ((p `plusPtr` 56 :: Ptr (Data.Vector.Storable.Sized.Vector 2 Offset3D)))
    case (dstOffsets) of
      (e0, e1) -> do
        ContT $ pokeCStruct (pDstOffsets' :: Ptr Offset3D) (e0) . ($ ())
        ContT $ pokeCStruct (pDstOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1) . ($ ())
    lift $ f
  cStructSize = 80
  cStructAlignment = 4
  pokeZeroCStruct p f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (zero) . ($ ())
    let pSrcOffsets' = lowerArrayPtr ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector 2 Offset3D)))
    case ((zero, zero)) of
      (e0, e1) -> do
        ContT $ pokeCStruct (pSrcOffsets' :: Ptr Offset3D) (e0) . ($ ())
        ContT $ pokeCStruct (pSrcOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr ImageSubresourceLayers)) (zero) . ($ ())
    let pDstOffsets' = lowerArrayPtr ((p `plusPtr` 56 :: Ptr (Data.Vector.Storable.Sized.Vector 2 Offset3D)))
    case ((zero, zero)) of
      (e0, e1) -> do
        ContT $ pokeCStruct (pDstOffsets' :: Ptr Offset3D) (e0) . ($ ())
        ContT $ pokeCStruct (pDstOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1) . ($ ())
    lift $ f

instance FromCStruct ImageBlit where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers))
    let psrcOffsets = lowerArrayPtr @Offset3D ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector 2 Offset3D)))
    srcOffsets0 <- peekCStruct @Offset3D ((psrcOffsets `advancePtrBytes` 0 :: Ptr Offset3D))
    srcOffsets1 <- peekCStruct @Offset3D ((psrcOffsets `advancePtrBytes` 12 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 40 :: Ptr ImageSubresourceLayers))
    let pdstOffsets = lowerArrayPtr @Offset3D ((p `plusPtr` 56 :: Ptr (Data.Vector.Storable.Sized.Vector 2 Offset3D)))
    dstOffsets0 <- peekCStruct @Offset3D ((pdstOffsets `advancePtrBytes` 0 :: Ptr Offset3D))
    dstOffsets1 <- peekCStruct @Offset3D ((pdstOffsets `advancePtrBytes` 12 :: Ptr Offset3D))
    pure $ ImageBlit
             srcSubresource ((srcOffsets0, srcOffsets1)) dstSubresource ((dstOffsets0, dstOffsets1))

instance Zero ImageBlit where
  zero = ImageBlit
           zero
           (zero, zero)
           zero
           (zero, zero)


-- | VkBufferImageCopy - Structure specifying a buffer image copy operation
--
-- = Description
--
-- When copying to or from a depth or stencil aspect, the data in buffer
-- memory uses a layout that is a (mostly) tightly packed representation of
-- the depth or stencil data. Specifically:
--
-- -   data copied to or from the stencil aspect of any depth\/stencil
--     format is tightly packed with one
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_S8_UINT' value per
--     texel.
--
-- -   data copied to or from the depth aspect of a
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_D16_UNORM' or
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_D16_UNORM_S8_UINT'
--     format is tightly packed with one
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_D16_UNORM' value per
--     texel.
--
-- -   data copied to or from the depth aspect of a
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_D32_SFLOAT' or
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_D32_SFLOAT_S8_UINT'
--     format is tightly packed with one
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_D32_SFLOAT' value per
--     texel.
--
-- -   data copied to or from the depth aspect of a
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_X8_D24_UNORM_PACK32' or
--     'Graphics.Vulkan.Core10.Enums.Format.FORMAT_D24_UNORM_S8_UINT'
--     format is packed with one 32-bit word per texel with the D24 value
--     in the LSBs of the word, and undefined values in the eight MSBs.
--
-- Note
--
-- To copy both the depth and stencil aspects of a depth\/stencil format,
-- two entries in @pRegions@ /can/ be used, where one specifies the depth
-- aspect in 'Graphics.Vulkan.Core10.Image.ImageSubresource', and the other
-- specifies the stencil aspect.
--
-- Because depth or stencil aspect buffer to image copies /may/ require
-- format conversions on some implementations, they are not supported on
-- queues that do not support graphics.
--
-- When copying to a depth aspect, and the
-- @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_EXT_depth_range_unrestricted@
-- extension is not enabled, the data in buffer memory /must/ be in the
-- range [0,1], or the resulting values are undefined.
--
-- Copies are done layer by layer starting with image layer
-- @baseArrayLayer@ member of
-- 'Graphics.Vulkan.Core10.Image.ImageSubresource'. @layerCount@ layers are
-- copied from the source image or to the destination image.
--
-- == Valid Usage
--
-- -   If the calling command’s 'Graphics.Vulkan.Core10.Handles.Image'
--     parameter’s format is not a depth\/stencil format or a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then @bufferOffset@ /must/ be a multiple of the format’s texel block
--     size.
--
-- -   If the calling command’s 'Graphics.Vulkan.Core10.Handles.Image'
--     parameter’s format is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then @bufferOffset@ /must/ be a multiple of the element size of the
--     compatible format for the format and the @aspectMask@ of the
--     'Graphics.Vulkan.Core10.Image.ImageSubresource' as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes>
--
-- -   @bufferOffset@ /must/ be a multiple of @4@
--
-- -   @bufferRowLength@ /must/ be @0@, or greater than or equal to the
--     @width@ member of @imageExtent@
--
-- -   @bufferImageHeight@ /must/ be @0@, or greater than or equal to the
--     @height@ member of @imageExtent@
--
-- -   @imageOffset.x@ and (@imageExtent.width@ + @imageOffset.x@) /must/
--     both be greater than or equal to @0@ and less than or equal to the
--     image subresource width where this refers to the width of the
--     /plane/ of the image involved in the copy in the case of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   @imageOffset.y@ and (imageExtent.height + @imageOffset.y@) /must/
--     both be greater than or equal to @0@ and less than or equal to the
--     image subresource height where this refers to the height of the
--     /plane/ of the image involved in the copy in the case of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   If the calling command’s @srcImage@ ('cmdCopyImageToBuffer') or
--     @dstImage@ ('cmdCopyBufferToImage') is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then
--     @imageOffset.y@ /must/ be @0@ and @imageExtent.height@ /must/ be
--     @1@.
--
-- -   @imageOffset.z@ and (imageExtent.depth + @imageOffset.z@) /must/
--     both be greater than or equal to @0@ and less than or equal to the
--     image subresource depth
--
-- -   If the calling command’s @srcImage@ ('cmdCopyImageToBuffer') or
--     @dstImage@ ('cmdCopyBufferToImage') is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then
--     @imageOffset.z@ /must/ be @0@ and @imageExtent.depth@ /must/ be @1@
--
-- -   If the calling command’s 'Graphics.Vulkan.Core10.Handles.Image'
--     parameter is a compressed image, or a /single-plane/, “@_422@” image
--     format, @bufferRowLength@ /must/ be a multiple of the compressed
--     texel block width
--
-- -   If the calling command’s 'Graphics.Vulkan.Core10.Handles.Image'
--     parameter is a compressed image, or a /single-plane/, “@_422@” image
--     format, @bufferImageHeight@ /must/ be a multiple of the compressed
--     texel block height
--
-- -   If the calling command’s 'Graphics.Vulkan.Core10.Handles.Image'
--     parameter is a compressed image, or a /single-plane/, “@_422@” image
--     format, all members of @imageOffset@ /must/ be a multiple of the
--     corresponding dimensions of the compressed texel block
--
-- -   If the calling command’s 'Graphics.Vulkan.Core10.Handles.Image'
--     parameter is a compressed image, or a /single-plane/, “@_422@” image
--     format, @bufferOffset@ /must/ be a multiple of the compressed texel
--     block size in bytes
--
-- -   If the calling command’s 'Graphics.Vulkan.Core10.Handles.Image'
--     parameter is a compressed image, or a /single-plane/, “@_422@” image
--     format, @imageExtent.width@ /must/ be a multiple of the compressed
--     texel block width or (@imageExtent.width@ + @imageOffset.x@) /must/
--     equal the image subresource width
--
-- -   If the calling command’s 'Graphics.Vulkan.Core10.Handles.Image'
--     parameter is a compressed image, or a /single-plane/, “@_422@” image
--     format, @imageExtent.height@ /must/ be a multiple of the compressed
--     texel block height or (@imageExtent.height@ + @imageOffset.y@)
--     /must/ equal the image subresource height
--
-- -   If the calling command’s 'Graphics.Vulkan.Core10.Handles.Image'
--     parameter is a compressed image, or a /single-plane/, “@_422@” image
--     format, @imageExtent.depth@ /must/ be a multiple of the compressed
--     texel block depth or (@imageExtent.depth@ + @imageOffset.z@) /must/
--     equal the image subresource depth
--
-- -   The @aspectMask@ member of
--     'Graphics.Vulkan.Core10.Image.ImageSubresource' /must/ specify
--     aspects present in the calling command’s
--     'Graphics.Vulkan.Core10.Handles.Image' parameter
--
-- -   If the calling command’s 'Graphics.Vulkan.Core10.Handles.Image'
--     parameter’s format is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then the @aspectMask@ member of
--     'Graphics.Vulkan.Core10.Image.ImageSubresource' /must/ be
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--     (with
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--     valid only for image formats with three planes)
--
-- -   The @aspectMask@ member of
--     'Graphics.Vulkan.Core10.Image.ImageSubresource' /must/ only have a
--     single bit set
--
-- -   If the calling command’s 'Graphics.Vulkan.Core10.Handles.Image'
--     parameter is of 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType'
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', the
--     @baseArrayLayer@ and @layerCount@ members of
--     'Graphics.Vulkan.Core10.Image.ImageSubresource' /must/ be @0@ and
--     @1@, respectively
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Image.ImageSubresource' /must/ be a valid
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.SharedTypes.Extent3D',
-- 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers',
-- 'Graphics.Vulkan.Core10.SharedTypes.Offset3D', 'cmdCopyBufferToImage',
-- 'cmdCopyImageToBuffer'
data BufferImageCopy = BufferImageCopy
  { -- | @bufferOffset@ is the offset in bytes from the start of the buffer
    -- object where the image data is copied from or to.
    bufferOffset :: DeviceSize
  , -- | @bufferRowLength@ and @bufferImageHeight@ specify in texels a subregion
    -- of a larger two- or three-dimensional image in buffer memory, and
    -- control the addressing calculations. If either of these values is zero,
    -- that aspect of the buffer memory is considered to be tightly packed
    -- according to the @imageExtent@.
    bufferRowLength :: Word32
  , -- No documentation found for Nested "VkBufferImageCopy" "bufferImageHeight"
    bufferImageHeight :: Word32
  , -- | 'Graphics.Vulkan.Core10.Image.ImageSubresource' is a
    -- 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers' used to
    -- specify the specific image subresources of the image used for the source
    -- or destination image data.
    imageSubresource :: ImageSubresourceLayers
  , -- | @imageOffset@ selects the initial @x@, @y@, @z@ offsets in texels of the
    -- sub-region of the source or destination image data.
    imageOffset :: Offset3D
  , -- | @imageExtent@ is the size in texels of the image to copy in @width@,
    -- @height@ and @depth@.
    imageExtent :: Extent3D
  }
  deriving (Typeable)
deriving instance Show BufferImageCopy

instance ToCStruct BufferImageCopy where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferImageCopy{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (bufferOffset)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) (bufferRowLength)
    lift $ poke ((p `plusPtr` 12 :: Ptr Word32)) (bufferImageHeight)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (imageSubresource) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 32 :: Ptr Offset3D)) (imageOffset) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 44 :: Ptr Extent3D)) (imageExtent) . ($ ())
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 32 :: Ptr Offset3D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 44 :: Ptr Extent3D)) (zero) . ($ ())
    lift $ f

instance FromCStruct BufferImageCopy where
  peekCStruct p = do
    bufferOffset <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    bufferRowLength <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    bufferImageHeight <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    imageSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers))
    imageOffset <- peekCStruct @Offset3D ((p `plusPtr` 32 :: Ptr Offset3D))
    imageExtent <- peekCStruct @Extent3D ((p `plusPtr` 44 :: Ptr Extent3D))
    pure $ BufferImageCopy
             bufferOffset bufferRowLength bufferImageHeight imageSubresource imageOffset imageExtent

instance Zero BufferImageCopy where
  zero = BufferImageCopy
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkImageResolve - Structure specifying an image resolve operation
--
-- == Valid Usage
--
-- -   The @aspectMask@ member of @srcSubresource@ and @dstSubresource@
--     /must/ only contain
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   The @layerCount@ member of @srcSubresource@ and @dstSubresource@
--     /must/ match
--
-- -   If either of the calling command’s @srcImage@ or @dstImage@
--     parameters are of 'Graphics.Vulkan.Core10.Enums.ImageType.ImageType'
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', the
--     @baseArrayLayer@ and @layerCount@ members of both @srcSubresource@
--     and @dstSubresource@ /must/ be @0@ and @1@, respectively
--
-- -   @srcOffset.x@ and (@extent.width@ + @srcOffset.x@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource width
--
-- -   @srcOffset.y@ and (@extent.height@ + @srcOffset.y@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource height
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then
--     @srcOffset.y@ /must/ be @0@ and @extent.height@ /must/ be @1@.
--
-- -   @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource depth
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then
--     @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/ be @1@.
--
-- -   @dstOffset.x@ and (@extent.width@ + @dstOffset.x@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource width
--
-- -   @dstOffset.y@ and (@extent.height@ + @dstOffset.y@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource height
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then
--     @dstOffset.y@ /must/ be @0@ and @extent.height@ /must/ be @1@.
--
-- -   @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource depth
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Graphics.Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then
--     @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/ be @1@.
--
-- == Valid Usage (Implicit)
--
-- -   @srcSubresource@ /must/ be a valid
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers'
--     structure
--
-- -   @dstSubresource@ /must/ be a valid
--     'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SharedTypes.Extent3D',
-- 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers',
-- 'Graphics.Vulkan.Core10.SharedTypes.Offset3D', 'cmdResolveImage'
data ImageResolve = ImageResolve
  { -- | @srcSubresource@ and @dstSubresource@ are
    -- 'Graphics.Vulkan.Core10.SharedTypes.ImageSubresourceLayers' structures
    -- specifying the image subresources of the images used for the source and
    -- destination image data, respectively. Resolve of depth\/stencil images
    -- is not supported.
    srcSubresource :: ImageSubresourceLayers
  , -- | @srcOffset@ and @dstOffset@ select the initial @x@, @y@, and @z@ offsets
    -- in texels of the sub-regions of the source and destination image data.
    srcOffset :: Offset3D
  , -- No documentation found for Nested "VkImageResolve" "dstSubresource"
    dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageResolve" "dstOffset"
    dstOffset :: Offset3D
  , -- | @extent@ is the size in texels of the source image to resolve in
    -- @width@, @height@ and @depth@.
    extent :: Extent3D
  }
  deriving (Typeable)
deriving instance Show ImageResolve

instance ToCStruct ImageResolve where
  withCStruct x f = allocaBytesAligned 68 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageResolve{..} f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (srcSubresource) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Offset3D)) (srcOffset) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers)) (dstSubresource) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 44 :: Ptr Offset3D)) (dstOffset) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr Extent3D)) (extent) . ($ ())
    lift $ f
  cStructSize = 68
  cStructAlignment = 4
  pokeZeroCStruct p f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 16 :: Ptr Offset3D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 44 :: Ptr Offset3D)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 56 :: Ptr Extent3D)) (zero) . ($ ())
    lift $ f

instance FromCStruct ImageResolve where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers))
    srcOffset <- peekCStruct @Offset3D ((p `plusPtr` 16 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers))
    dstOffset <- peekCStruct @Offset3D ((p `plusPtr` 44 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 56 :: Ptr Extent3D))
    pure $ ImageResolve
             srcSubresource srcOffset dstSubresource dstOffset extent

instance Zero ImageResolve where
  zero = ImageResolve
           zero
           zero
           zero
           zero
           zero


-- | VkRenderPassBeginInfo - Structure specifying render pass begin info
--
-- = Description
--
-- @renderArea@ is the render area that is affected by the render pass
-- instance. The effects of attachment load, store and multisample resolve
-- operations are restricted to the pixels whose x and y coordinates fall
-- within the render area on all attachments. The render area extends to
-- all layers of 'Graphics.Vulkan.Core10.Handles.Framebuffer'. The
-- application /must/ ensure (using scissor if necessary) that all
-- rendering is contained within the render area. The render area, after
-- any transform specified by
-- 'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'::@transform@
-- is applied, /must/ be contained within the framebuffer dimensions.
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-renderpass-transform render pass transform>
-- is enabled, then @renderArea@ must equal the framebuffer pre-transformed
-- dimensions. After @renderArea@ has been transformed by
-- 'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'::@transform@,
-- the resulting render area /must/ be equal to the framebuffer dimensions.
--
-- When multiview is enabled, the resolve operation at the end of a subpass
-- applies to all views in the view mask.
--
-- Note
--
-- There /may/ be a performance cost for using a render area smaller than
-- the framebuffer, unless it matches the render area granularity for the
-- render pass.
--
-- == Valid Usage
--
-- -   @clearValueCount@ /must/ be greater than the largest attachment
--     index in 'Graphics.Vulkan.Core10.Handles.RenderPass' that specifies
--     a @loadOp@ (or @stencilLoadOp@, if the attachment has a
--     depth\/stencil format) of
--     'Graphics.Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'
--
-- -   'Graphics.Vulkan.Core10.Handles.RenderPass' /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the 'Graphics.Vulkan.Core10.Handles.RenderPass' member of the
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo' structure
--     specified when creating 'Graphics.Vulkan.Core10.Handles.Framebuffer'
--
-- -   If the @pNext@ chain does not contain
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.x@ /must/ be greater than or equal to 0
--
-- -   If the @pNext@ chain does not contain
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.y@ /must/ be greater than or equal to 0
--
-- -   If the @pNext@ chain does not contain
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.x@ + @renderArea.offset.width@ /must/ be less
--     than or equal to
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::@width@ the
--     'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with
--
-- -   If the @pNext@ chain does not contain
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.y@ + @renderArea.offset.height@ /must/ be less
--     than or equal to
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::@height@ the
--     'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with
--
-- -   If the @pNext@ chain contains
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     the @offset.x@ member of each element of @pDeviceRenderAreas@ /must/
--     be greater than or equal to 0
--
-- -   If the @pNext@ chain contains
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     the @offset.y@ member of each element of @pDeviceRenderAreas@ /must/
--     be greater than or equal to 0
--
-- -   If the @pNext@ chain contains
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     @offset.x@ + @offset.width@ of each element of @pDeviceRenderAreas@
--     /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::@width@ the
--     'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with
--
-- -   If the @pNext@ chain contains
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     @offset.y@ + @offset.height@ of each element of @pDeviceRenderAreas@
--     /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::@height@ the
--     'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with a
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     value that did not include
--     'Graphics.Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     and the @pNext@ chain includes a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure, its @attachmentCount@ /must/ be zero
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with a
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     value that included
--     'Graphics.Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @attachmentCount@ of a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be equal to the value
--     of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@attachmentImageInfoCount@
--     used to create 'Graphics.Vulkan.Core10.Handles.Framebuffer'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with a
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     value that included
--     'Graphics.Vulkan.Extensions.VK_KHR_imageless_framebuffer.FRAMEBUFFER_CREATE_IMAGELESS_BIT_KHR',
--     each element of the @pAttachments@ member of a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ have been created on
--     the same 'Graphics.Vulkan.Core10.Handles.Device' as
--     'Graphics.Vulkan.Core10.Handles.Framebuffer' and
--     'Graphics.Vulkan.Core10.Handles.RenderPass'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with a
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     value that included
--     'Graphics.Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of an image created with
--     a value of
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     equal to the 'Graphics.Vulkan.Core10.BaseType.Flags' member of the
--     corresponding element of
--     'Graphics.Vulkan.Extensions.VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfoKHR'::@pAttachments@
--     used to create 'Graphics.Vulkan.Core10.Handles.Framebuffer'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with a
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     value that included
--     'Graphics.Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of an image created with
--     a value of 'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@usage@
--     equal to the @usage@ member of the corresponding element of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachments@
--     used to create 'Graphics.Vulkan.Core10.Handles.Framebuffer'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with a
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     value that included
--     'Graphics.Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Graphics.Vulkan.Core10.Handles.ImageView' with a width equal to the
--     @width@ member of the corresponding element of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachments@
--     used to create 'Graphics.Vulkan.Core10.Handles.Framebuffer'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with a
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     value that included
--     'Graphics.Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Graphics.Vulkan.Core10.Handles.ImageView' with a height equal to
--     the @height@ member of the corresponding element of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachments@
--     used to create 'Graphics.Vulkan.Core10.Handles.Framebuffer'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with a
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     value that included
--     'Graphics.Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of an image created with
--     a value of
--     'Graphics.Vulkan.Core10.ImageView.ImageViewCreateInfo'::@subresourceRange.layerCount@
--     equal to the @layerCount@ member of the corresponding element of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachments@
--     used to create 'Graphics.Vulkan.Core10.Handles.Framebuffer'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with a
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     value that included
--     'Graphics.Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of an image created with
--     a value of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@viewFormatCount@
--     equal to the @viewFormatCount@ member of the corresponding element
--     of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachments@
--     used to create 'Graphics.Vulkan.Core10.Handles.Framebuffer'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with a
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     value that included
--     'Graphics.Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of an image created with
--     a set of elements in
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@pViewFormats@
--     equal to the set of elements in the @pViewFormats@ member of the
--     corresponding element of
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachments@
--     used to create 'Graphics.Vulkan.Core10.Handles.Framebuffer'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with a
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     value that included
--     'Graphics.Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of an image created with
--     a value of
--     'Graphics.Vulkan.Core10.ImageView.ImageViewCreateInfo'::'Graphics.Vulkan.Core10.Enums.Format.Format'
--     equal to the corresponding value of
--     'Graphics.Vulkan.Core10.Pass.AttachmentDescription'::'Graphics.Vulkan.Core10.Enums.Format.Format'
--     in 'Graphics.Vulkan.Core10.Handles.RenderPass'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Framebuffer' was created with a
--     'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo'::'Graphics.Vulkan.Core10.BaseType.Flags'
--     value that included
--     'Graphics.Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Graphics.Vulkan.Core10.Handles.ImageView' of an image created with
--     a value of 'Graphics.Vulkan.Core10.Image.ImageCreateInfo'::@samples@
--     equal to the corresponding value of
--     'Graphics.Vulkan.Core10.Pass.AttachmentDescription'::@samples@ in
--     'Graphics.Vulkan.Core10.Handles.RenderPass'
--
-- -   If the @pNext@ chain includes
--     'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM',
--     @renderArea@::@offset@ /must/ equal (0,0).
--
-- -   If the @pNext@ chain includes
--     'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM',
--     @renderArea@::@extent@ transformed by
--     'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'::@transform@
--     /must/ equal the 'Graphics.Vulkan.Core10.Handles.Framebuffer'
--     dimensions.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo',
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.RenderPassSampleLocationsBeginInfoEXT',
--     or
--     'Graphics.Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   'Graphics.Vulkan.Core10.Handles.RenderPass' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.RenderPass' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Framebuffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Framebuffer' handle
--
-- -   If @clearValueCount@ is not @0@, @pClearValues@ /must/ be a valid
--     pointer to an array of @clearValueCount@
--     'Graphics.Vulkan.Core10.SharedTypes.ClearValue' unions
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.Framebuffer', and
--     'Graphics.Vulkan.Core10.Handles.RenderPass' /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SharedTypes.ClearValue',
-- 'Graphics.Vulkan.Core10.Handles.Framebuffer', 'Rect2D',
-- 'Graphics.Vulkan.Core10.Handles.RenderPass',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBeginRenderPass',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.cmdBeginRenderPass2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2.cmdBeginRenderPass2KHR'
data RenderPassBeginInfo (es :: [Type]) = RenderPassBeginInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | 'Graphics.Vulkan.Core10.Handles.RenderPass' is the render pass to begin
    -- an instance of.
    renderPass :: RenderPass
  , -- | 'Graphics.Vulkan.Core10.Handles.Framebuffer' is the framebuffer
    -- containing the attachments that are used with the render pass.
    framebuffer :: Framebuffer
  , -- | @renderArea@ is the render area that is affected by the render pass
    -- instance, and is described in more detail below.
    renderArea :: Rect2D
  , -- | @pClearValues@ is a pointer to an array of @clearValueCount@
    -- 'Graphics.Vulkan.Core10.SharedTypes.ClearValue' structures that contains
    -- clear values for each attachment, if the attachment uses a @loadOp@
    -- value of
    -- 'Graphics.Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'
    -- or if the attachment has a depth\/stencil format and uses a
    -- @stencilLoadOp@ value of
    -- 'Graphics.Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'.
    -- The array is indexed by attachment number. Only elements corresponding
    -- to cleared attachments are used. Other elements of @pClearValues@ are
    -- ignored.
    clearValues :: Vector ClearValue
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (RenderPassBeginInfo es)

instance Extensible RenderPassBeginInfo where
  extensibleType = STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
  setNext x next = x{next = next}
  getNext RenderPassBeginInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RenderPassBeginInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @RenderPassTransformBeginInfoQCOM = Just f
    | Just Refl <- eqT @e @RenderPassAttachmentBeginInfo = Just f
    | Just Refl <- eqT @e @RenderPassSampleLocationsBeginInfoEXT = Just f
    | Just Refl <- eqT @e @DeviceGroupRenderPassBeginInfo = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (RenderPassBeginInfo es) where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassBeginInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderPass)) (renderPass)
    lift $ poke ((p `plusPtr` 24 :: Ptr Framebuffer)) (framebuffer)
    ContT $ pokeCStruct ((p `plusPtr` 32 :: Ptr Rect2D)) (renderArea) . ($ ())
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (clearValues)) :: Word32))
    pPClearValues' <- ContT $ allocaBytesAligned @ClearValue ((Data.Vector.length (clearValues)) * 16) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPClearValues' `plusPtr` (16 * (i)) :: Ptr ClearValue) (e) . ($ ())) (clearValues)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr ClearValue))) (pPClearValues')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderPass)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Framebuffer)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 32 :: Ptr Rect2D)) (zero) . ($ ())
    pPClearValues' <- ContT $ allocaBytesAligned @ClearValue ((Data.Vector.length (mempty)) * 16) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPClearValues' `plusPtr` (16 * (i)) :: Ptr ClearValue) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr ClearValue))) (pPClearValues')
    lift $ f

instance es ~ '[] => Zero (RenderPassBeginInfo es) where
  zero = RenderPassBeginInfo
           ()
           zero
           zero
           zero
           mempty


-- | VkClearAttachment - Structure specifying a clear attachment
--
-- = Description
--
-- No memory barriers are needed between 'cmdClearAttachments' and
-- preceding or subsequent draw or attachment clear commands in the same
-- subpass.
--
-- The 'cmdClearAttachments' command is not affected by the bound pipeline
-- state.
--
-- Attachments /can/ also be cleared at the beginning of a render pass
-- instance by setting @loadOp@ (or @stencilLoadOp@) of
-- 'Graphics.Vulkan.Core10.Pass.AttachmentDescription' to
-- 'Graphics.Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR',
-- as described for 'Graphics.Vulkan.Core10.Pass.createRenderPass'.
--
-- == Valid Usage
--
-- -   If @aspectMask@ includes
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
--     it /must/ not include
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   @aspectMask@ /must/ not include
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_METADATA_BIT'
--
-- -   @aspectMask@ /must/ not include
--     @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ for any index @i@.
--
-- -   'Graphics.Vulkan.Core10.SharedTypes.ClearValue' /must/ be a valid
--     'Graphics.Vulkan.Core10.SharedTypes.ClearValue' union
--
-- == Valid Usage (Implicit)
--
-- -   @aspectMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits'
--     values
--
-- -   @aspectMask@ /must/ not be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.SharedTypes.ClearValue',
-- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- 'cmdClearAttachments'
data ClearAttachment = ClearAttachment
  { -- | @aspectMask@ is a mask selecting the color, depth and\/or stencil
    -- aspects of the attachment to be cleared.
    aspectMask :: ImageAspectFlags
  , -- | @colorAttachment@ is only meaningful if
    -- 'Graphics.Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
    -- is set in @aspectMask@, in which case it is an index to the
    -- @pColorAttachments@ array in the
    -- 'Graphics.Vulkan.Core10.Pass.SubpassDescription' structure of the
    -- current subpass which selects the color attachment to clear.
    colorAttachment :: Word32
  , -- | 'Graphics.Vulkan.Core10.SharedTypes.ClearValue' is the color or
    -- depth\/stencil value to clear the attachment to, as described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears-values Clear Values>
    -- below.
    clearValue :: ClearValue
  }
  deriving (Typeable)
deriving instance Show ClearAttachment

instance ToCStruct ClearAttachment where
  withCStruct x f = allocaBytesAligned 24 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ClearAttachment{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (aspectMask)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (colorAttachment)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr ClearValue)) (clearValue) . ($ ())
    lift $ f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (zero)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr ClearValue)) (zero) . ($ ())
    lift $ f

instance Zero ClearAttachment where
  zero = ClearAttachment
           zero
           zero
           zero

