{-# language CPP #-}
-- No documentation found for Chapter "CommandBufferBuilding"
module Vulkan.Core10.CommandBufferBuilding  ( cmdBindPipeline
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
                                            , cmdWaitEventsSafe
                                            , cmdPipelineBarrier
                                            , cmdBeginQuery
                                            , cmdUseQuery
                                            , cmdEndQuery
                                            , cmdResetQueryPool
                                            , cmdWriteTimestamp
                                            , cmdCopyQueryPoolResults
                                            , cmdPushConstants
                                            , cmdBeginRenderPass
                                            , cmdUseRenderPass
                                            , cmdNextSubpass
                                            , cmdEndRenderPass
                                            , cmdExecuteCommands
                                            , ClearRect(..)
                                            , ImageSubresourceLayers(..)
                                            , BufferCopy(..)
                                            , ImageCopy(..)
                                            , ImageBlit(..)
                                            , BufferImageCopy(..)
                                            , ImageResolve(..)
                                            , RenderPassBeginInfo(..)
                                            , ClearDepthStencilValue(..)
                                            , ClearAttachment(..)
                                            , ClearColorValue(..)
                                            , ClearValue(..)
                                            , IndexType(..)
                                            , SubpassContents(..)
                                            , StencilFaceFlagBits(..)
                                            , StencilFaceFlags
                                            ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CFloat(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.OtherTypes (BufferMemoryBarrier)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlagBits(..))
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core10.Handles (DescriptorSet)
import Vulkan.Core10.Handles (DescriptorSet(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginQuery))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginRenderPass))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindDescriptorSets))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindIndexBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindPipeline))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindVertexBuffers))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBlitImage))
import Vulkan.Dynamic (DeviceCmds(pVkCmdClearAttachments))
import Vulkan.Dynamic (DeviceCmds(pVkCmdClearColorImage))
import Vulkan.Dynamic (DeviceCmds(pVkCmdClearDepthStencilImage))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyBufferToImage))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyImage))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyImageToBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyQueryPoolResults))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDispatch))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDispatchIndirect))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDraw))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndexed))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndexedIndirect))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndirect))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndQuery))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndRenderPass))
import Vulkan.Dynamic (DeviceCmds(pVkCmdExecuteCommands))
import Vulkan.Dynamic (DeviceCmds(pVkCmdFillBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkCmdNextSubpass))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPipelineBarrier))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushConstants))
import Vulkan.Dynamic (DeviceCmds(pVkCmdResetEvent))
import Vulkan.Dynamic (DeviceCmds(pVkCmdResetQueryPool))
import Vulkan.Dynamic (DeviceCmds(pVkCmdResolveImage))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetBlendConstants))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthBias))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthBounds))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetEvent))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetLineWidth))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetScissor))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilCompareMask))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilReference))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilWriteMask))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewport))
import Vulkan.Dynamic (DeviceCmds(pVkCmdUpdateBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWaitEvents))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteTimestamp))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupRenderPassBeginInfo)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Event)
import Vulkan.Core10.Handles (Event(..))
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core10.Enums.Filter (Filter)
import Vulkan.Core10.Enums.Filter (Filter(..))
import Vulkan.Core10.Handles (Framebuffer)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Vulkan.Core10.OtherTypes (ImageMemoryBarrier)
import Vulkan.Core10.ImageView (ImageSubresourceRange)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Core10.Enums.IndexType (IndexType(..))
import Vulkan.Core10.OtherTypes (MemoryBarrier)
import Vulkan.Core10.FundamentalTypes (Offset3D)
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Handles (PipelineLayout(..))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(..))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlagBits(..))
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlags)
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlagBits(..))
import Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlags)
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Handles (RenderPass)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (RenderPassAttachmentBeginInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (RenderPassSampleLocationsBeginInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_render_pass_transform (RenderPassTransformBeginInfoQCOM)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlagBits(..))
import Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.SubpassContents (SubpassContents)
import Vulkan.Core10.Enums.SubpassContents (SubpassContents(..))
import Vulkan.Core10.Pipeline (Viewport)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO))
import Vulkan.Core10.Enums.IndexType (IndexType(..))
import Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlagBits(..))
import Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlags)
import Vulkan.Core10.Enums.SubpassContents (SubpassContents(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindPipeline
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> IO ()

-- | vkCmdBindPipeline - Bind a pipeline object to a command buffer
--
-- = Description
--
-- Once bound, a pipeline binding affects subsequent commands that interact
-- with the given pipeline type in the command buffer until a different
-- pipeline of the same type is bound to the bind point. Commands that do
-- not interact with the given pipeline type /must/ not be affected by the
-- pipeline state.
--
-- -   The pipeline bound to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE'
--     controls the behavior of all
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#dispatch dispatching commands>.
--
-- -   The pipeline bound to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--     controls the behavior of all
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing drawing commands>.
--
-- -   The pipeline bound to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_RAY_TRACING_KHR'
--     controls the behavior of
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysKHR' and
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysIndirectKHR'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindPipeline-pipelineBindPoint-00777# If
--     @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE',
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdBindPipeline-pipelineBindPoint-00778# If
--     @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS',
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBindPipeline-pipelineBindPoint-00779# If
--     @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE',
--     @pipeline@ /must/ be a compute pipeline
--
-- -   #VUID-vkCmdBindPipeline-pipelineBindPoint-00780# If
--     @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS',
--     @pipeline@ /must/ be a graphics pipeline
--
-- -   #VUID-vkCmdBindPipeline-pipeline-00781# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-variableMultisampleRate variable multisample rate>
--     feature is not supported, @pipeline@ is a graphics pipeline, the
--     current subpass
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-noattachments uses no attachments>,
--     and this is not the first call to this function with a graphics
--     pipeline after transitioning to the current subpass, then the sample
--     count specified by this pipeline /must/ match that set in the
--     previous pipeline
--
-- -   #VUID-vkCmdBindPipeline-variableSampleLocations-01525# If
--     'Vulkan.Extensions.VK_EXT_sample_locations.PhysicalDeviceSampleLocationsPropertiesEXT'::@variableSampleLocations@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', and @pipeline@ is a
--     graphics pipeline created with a
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     structure having its @sampleLocationsEnable@ member set to
--     'Vulkan.Core10.FundamentalTypes.TRUE' but without
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     enabled then the current render pass instance /must/ have been begun
--     by specifying a
--     'Vulkan.Extensions.VK_EXT_sample_locations.RenderPassSampleLocationsBeginInfoEXT'
--     structure whose @pPostSubpassSampleLocations@ member contains an
--     element with a @subpassIndex@ matching the current subpass index and
--     the @sampleLocationsInfo@ member of that element /must/ match the
--     @sampleLocationsInfo@ specified in
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     when the pipeline was created
--
-- -   #VUID-vkCmdBindPipeline-None-02323# This command /must/ not be
--     recorded when transform feedback is active
--
-- -   #VUID-vkCmdBindPipeline-pipelineBindPoint-02391# If
--     @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_RAY_TRACING_KHR',
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdBindPipeline-pipelineBindPoint-02392# If
--     @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_RAY_TRACING_KHR',
--     the @pipeline@ /must/ be a ray tracing pipeline
--
-- -   #VUID-vkCmdBindPipeline-pipeline-03382# The @pipeline@ /must/ not
--     have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--     set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindPipeline-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindPipeline-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-vkCmdBindPipeline-pipeline-parameter# @pipeline@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-vkCmdBindPipeline-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindPipeline-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdBindPipeline-commonparent# Both of @commandBuffer@, and
--     @pipeline@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint'
cmdBindPipeline :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer that the pipeline will be bound
                   -- to.
                   CommandBuffer
                -> -- | @pipelineBindPoint@ is a
                   -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
                   -- specifying to which bind point the pipeline is bound. Binding one does
                   -- not disturb the others.
                   PipelineBindPoint
                -> -- | @pipeline@ is the pipeline to be bound.
                   Pipeline
                -> io ()
cmdBindPipeline commandBuffer pipelineBindPoint pipeline = liftIO $ do
  let vkCmdBindPipelinePtr = pVkCmdBindPipeline (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBindPipelinePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindPipeline is null" Nothing Nothing
  let vkCmdBindPipeline' = mkVkCmdBindPipeline vkCmdBindPipelinePtr
  traceAroundEvent "vkCmdBindPipeline" (vkCmdBindPipeline' (commandBufferHandle (commandBuffer)) (pipelineBindPoint) (pipeline))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewport
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Viewport -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Viewport -> IO ()

-- | vkCmdSetViewport - Set the viewport on a command buffer
--
-- = Description
--
-- The viewport parameters taken from element i of @pViewports@ replace the
-- current state for the viewport index @firstViewport@ + i, for i in [0,
-- @viewportCount@).
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetViewport-firstViewport-01223# The sum of
--     @firstViewport@ and @viewportCount@ /must/ be between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   #VUID-vkCmdSetViewport-firstViewport-01224# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @firstViewport@ /must/ be @0@
--
-- -   #VUID-vkCmdSetViewport-viewportCount-01225# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetViewport-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetViewport-pViewports-parameter# @pViewports@ /must/ be
--     a valid pointer to an array of @viewportCount@ valid
--     'Vulkan.Core10.Pipeline.Viewport' structures
--
-- -   #VUID-vkCmdSetViewport-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetViewport-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetViewport-viewportCount-arraylength# @viewportCount@
--     /must/ be greater than @0@
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Pipeline.Viewport'
cmdSetViewport :: forall io
                . (MonadIO io)
               => -- | @commandBuffer@ is the command buffer into which the command will be
                  -- recorded.
                  CommandBuffer
               -> -- | @firstViewport@ is the index of the first viewport whose parameters are
                  -- updated by the command.
                  ("firstViewport" ::: Word32)
               -> -- | @pViewports@ is a pointer to an array of
                  -- 'Vulkan.Core10.Pipeline.Viewport' structures specifying viewport
                  -- parameters.
                  ("viewports" ::: Vector Viewport)
               -> io ()
cmdSetViewport commandBuffer firstViewport viewports = liftIO . evalContT $ do
  let vkCmdSetViewportPtr = pVkCmdSetViewport (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetViewportPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetViewport is null" Nothing Nothing
  let vkCmdSetViewport' = mkVkCmdSetViewport vkCmdSetViewportPtr
  pPViewports <- ContT $ allocaBytesAligned @Viewport ((Data.Vector.length (viewports)) * 24) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPViewports `plusPtr` (24 * (i)) :: Ptr Viewport) (e)) (viewports)
  lift $ traceAroundEvent "vkCmdSetViewport" (vkCmdSetViewport' (commandBufferHandle (commandBuffer)) (firstViewport) ((fromIntegral (Data.Vector.length $ (viewports)) :: Word32)) (pPViewports))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetScissor
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()

-- | vkCmdSetScissor - Set the dynamic scissor rectangles on a command buffer
--
-- = Description
--
-- The scissor rectangles taken from element i of @pScissors@ replace the
-- current state for the scissor index @firstScissor@ + i, for i in [0,
-- @scissorCount@).
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetScissor-firstScissor-00592# The sum of @firstScissor@
--     and @scissorCount@ /must/ be between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   #VUID-vkCmdSetScissor-firstScissor-00593# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @firstScissor@ /must/ be @0@
--
-- -   #VUID-vkCmdSetScissor-scissorCount-00594# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @scissorCount@ /must/ be @1@
--
-- -   #VUID-vkCmdSetScissor-x-00595# The @x@ and @y@ members of @offset@
--     member of any element of @pScissors@ /must/ be greater than or equal
--     to @0@
--
-- -   #VUID-vkCmdSetScissor-offset-00596# Evaluation of (@offset.x@ +
--     @extent.width@) /must/ not cause a signed integer addition overflow
--     for any element of @pScissors@
--
-- -   #VUID-vkCmdSetScissor-offset-00597# Evaluation of (@offset.y@ +
--     @extent.height@) /must/ not cause a signed integer addition overflow
--     for any element of @pScissors@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetScissor-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetScissor-pScissors-parameter# @pScissors@ /must/ be a
--     valid pointer to an array of @scissorCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- -   #VUID-vkCmdSetScissor-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetScissor-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetScissor-scissorCount-arraylength# @scissorCount@
--     /must/ be greater than @0@
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.Rect2D'
cmdSetScissor :: forall io
               . (MonadIO io)
              => -- | @commandBuffer@ is the command buffer into which the command will be
                 -- recorded.
                 CommandBuffer
              -> -- | @firstScissor@ is the index of the first scissor whose state is updated
                 -- by the command.
                 ("firstScissor" ::: Word32)
              -> -- | @pScissors@ is a pointer to an array of
                 -- 'Vulkan.Core10.FundamentalTypes.Rect2D' structures defining scissor
                 -- rectangles.
                 ("scissors" ::: Vector Rect2D)
              -> io ()
cmdSetScissor commandBuffer firstScissor scissors = liftIO . evalContT $ do
  let vkCmdSetScissorPtr = pVkCmdSetScissor (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetScissorPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetScissor is null" Nothing Nothing
  let vkCmdSetScissor' = mkVkCmdSetScissor vkCmdSetScissorPtr
  pPScissors <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (scissors)) * 16) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (scissors)
  lift $ traceAroundEvent "vkCmdSetScissor" (vkCmdSetScissor' (commandBufferHandle (commandBuffer)) (firstScissor) ((fromIntegral (Data.Vector.length $ (scissors)) :: Word32)) (pPScissors))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetLineWidth
  :: FunPtr (Ptr CommandBuffer_T -> CFloat -> IO ()) -> Ptr CommandBuffer_T -> CFloat -> IO ()

-- | vkCmdSetLineWidth - Set the dynamic line width state
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetLineWidth-lineWidth-00788# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-wideLines wide lines>
--     feature is not enabled, @lineWidth@ /must/ be @1.0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetLineWidth-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetLineWidth-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetLineWidth-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetLineWidth :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer into which the command will be
                   -- recorded.
                   CommandBuffer
                -> -- | @lineWidth@ is the width of rasterized line segments.
                   ("lineWidth" ::: Float)
                -> io ()
cmdSetLineWidth commandBuffer lineWidth = liftIO $ do
  let vkCmdSetLineWidthPtr = pVkCmdSetLineWidth (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetLineWidthPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetLineWidth is null" Nothing Nothing
  let vkCmdSetLineWidth' = mkVkCmdSetLineWidth vkCmdSetLineWidthPtr
  traceAroundEvent "vkCmdSetLineWidth" (vkCmdSetLineWidth' (commandBufferHandle (commandBuffer)) (CFloat (lineWidth)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBias
  :: FunPtr (Ptr CommandBuffer_T -> CFloat -> CFloat -> CFloat -> IO ()) -> Ptr CommandBuffer_T -> CFloat -> CFloat -> CFloat -> IO ()

-- | vkCmdSetDepthBias - Set the depth bias dynamic state
--
-- = Description
--
-- If @depthBiasEnable@ is 'Vulkan.Core10.FundamentalTypes.FALSE', no depth
-- bias is applied and the fragment’s depth values are unchanged.
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
-- 'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL_RECTANGLE_NV' polygon
-- mode, then this minimum resolvable difference /may/ not be resolvable
-- for samples outside of the triangle, where the depth is extrapolated.
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
-- Unless the @VK_EXT_depth_range_unrestricted@ extension is enabled,
-- fragment depth values are clamped even when the depth buffer uses a
-- floating-point representation.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDepthBias-depthBiasClamp-00790# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthBiasClamp depth bias clamping>
--     feature is not enabled, @depthBiasClamp@ /must/ be @0.0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthBias-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthBias-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthBias-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthBias :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer into which the command will be
                   -- recorded.
                   CommandBuffer
                -> -- | @depthBiasConstantFactor@ is a scalar factor controlling the constant
                   -- depth value added to each fragment.
                   ("depthBiasConstantFactor" ::: Float)
                -> -- | @depthBiasClamp@ is the maximum (or minimum) depth bias of a fragment.
                   ("depthBiasClamp" ::: Float)
                -> -- | @depthBiasSlopeFactor@ is a scalar factor applied to a fragment’s slope
                   -- in depth bias calculations.
                   ("depthBiasSlopeFactor" ::: Float)
                -> io ()
cmdSetDepthBias commandBuffer depthBiasConstantFactor depthBiasClamp depthBiasSlopeFactor = liftIO $ do
  let vkCmdSetDepthBiasPtr = pVkCmdSetDepthBias (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDepthBiasPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthBias is null" Nothing Nothing
  let vkCmdSetDepthBias' = mkVkCmdSetDepthBias vkCmdSetDepthBiasPtr
  traceAroundEvent "vkCmdSetDepthBias" (vkCmdSetDepthBias' (commandBufferHandle (commandBuffer)) (CFloat (depthBiasConstantFactor)) (CFloat (depthBiasClamp)) (CFloat (depthBiasSlopeFactor)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetBlendConstants
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (FixedArray 4 CFloat) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (FixedArray 4 CFloat) -> IO ()

-- | vkCmdSetBlendConstants - Set the values of blend constants
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetBlendConstants-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetBlendConstants-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetBlendConstants-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetBlendConstants :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer into which the command will be
                        -- recorded.
                        CommandBuffer
                     -> -- | @blendConstants@ is a pointer to an array of four values specifying the
                        -- R, G, B, and A components of the blend constant color used in blending,
                        -- depending on the
                        -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blendfactors blend factor>.
                        ("blendConstants" ::: (Float, Float, Float, Float))
                     -> io ()
cmdSetBlendConstants commandBuffer blendConstants = liftIO . evalContT $ do
  let vkCmdSetBlendConstantsPtr = pVkCmdSetBlendConstants (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetBlendConstantsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetBlendConstants is null" Nothing Nothing
  let vkCmdSetBlendConstants' = mkVkCmdSetBlendConstants vkCmdSetBlendConstantsPtr
  pBlendConstants <- ContT $ allocaBytesAligned @(FixedArray 4 CFloat) 16 4
  let pBlendConstants' = lowerArrayPtr pBlendConstants
  lift $ case (blendConstants) of
    (e0, e1, e2, e3) -> do
      poke (pBlendConstants' :: Ptr CFloat) (CFloat (e0))
      poke (pBlendConstants' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
      poke (pBlendConstants' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
      poke (pBlendConstants' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
  lift $ traceAroundEvent "vkCmdSetBlendConstants" (vkCmdSetBlendConstants' (commandBufferHandle (commandBuffer)) (pBlendConstants))
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
-- = Description
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDepthBounds-minDepthBounds-00600# Unless the
--     @VK_EXT_depth_range_unrestricted@ extension is enabled
--     @minDepthBounds@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- -   #VUID-vkCmdSetDepthBounds-maxDepthBounds-00601# Unless the
--     @VK_EXT_depth_range_unrestricted@ extension is enabled
--     @maxDepthBounds@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthBounds-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthBounds-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthBounds-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthBounds :: forall io
                   . (MonadIO io)
                  => -- | @commandBuffer@ is the command buffer into which the command will be
                     -- recorded.
                     CommandBuffer
                  -> -- | @minDepthBounds@ is the minimum depth bound.
                     ("minDepthBounds" ::: Float)
                  -> -- | @maxDepthBounds@ is the maximum depth bound.
                     ("maxDepthBounds" ::: Float)
                  -> io ()
cmdSetDepthBounds commandBuffer minDepthBounds maxDepthBounds = liftIO $ do
  let vkCmdSetDepthBoundsPtr = pVkCmdSetDepthBounds (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDepthBoundsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthBounds is null" Nothing Nothing
  let vkCmdSetDepthBounds' = mkVkCmdSetDepthBounds vkCmdSetDepthBoundsPtr
  traceAroundEvent "vkCmdSetDepthBounds" (vkCmdSetDepthBounds' (commandBufferHandle (commandBuffer)) (CFloat (minDepthBounds)) (CFloat (maxDepthBounds)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilCompareMask
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()

-- | vkCmdSetStencilCompareMask - Set the stencil compare mask dynamic state
--
-- = Description
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_COMPARE_MASK'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetStencilCompareMask-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetStencilCompareMask-faceMask-parameter# @faceMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits' values
--
-- -   #VUID-vkCmdSetStencilCompareMask-faceMask-requiredbitmask#
--     @faceMask@ /must/ not be @0@
--
-- -   #VUID-vkCmdSetStencilCompareMask-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetStencilCompareMask-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlags'
cmdSetStencilCompareMask :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @faceMask@ is a bitmask of
                            -- 'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits' specifying
                            -- the set of stencil state for which to update the compare mask.
                            ("faceMask" ::: StencilFaceFlags)
                         -> -- | @compareMask@ is the new value to use as the stencil compare mask.
                            ("compareMask" ::: Word32)
                         -> io ()
cmdSetStencilCompareMask commandBuffer faceMask compareMask = liftIO $ do
  let vkCmdSetStencilCompareMaskPtr = pVkCmdSetStencilCompareMask (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetStencilCompareMaskPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetStencilCompareMask is null" Nothing Nothing
  let vkCmdSetStencilCompareMask' = mkVkCmdSetStencilCompareMask vkCmdSetStencilCompareMaskPtr
  traceAroundEvent "vkCmdSetStencilCompareMask" (vkCmdSetStencilCompareMask' (commandBufferHandle (commandBuffer)) (faceMask) (compareMask))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilWriteMask
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()

-- | vkCmdSetStencilWriteMask - Set the stencil write mask dynamic state
--
-- = Description
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_WRITE_MASK' set
-- in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetStencilWriteMask-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetStencilWriteMask-faceMask-parameter# @faceMask@ /must/
--     be a valid combination of
--     'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits' values
--
-- -   #VUID-vkCmdSetStencilWriteMask-faceMask-requiredbitmask# @faceMask@
--     /must/ not be @0@
--
-- -   #VUID-vkCmdSetStencilWriteMask-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetStencilWriteMask-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlags'
cmdSetStencilWriteMask :: forall io
                        . (MonadIO io)
                       => -- | @commandBuffer@ is the command buffer into which the command will be
                          -- recorded.
                          CommandBuffer
                       -> -- | @faceMask@ is a bitmask of
                          -- 'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits' specifying
                          -- the set of stencil state for which to update the write mask, as
                          -- described above for 'cmdSetStencilCompareMask'.
                          ("faceMask" ::: StencilFaceFlags)
                       -> -- | @writeMask@ is the new value to use as the stencil write mask.
                          ("writeMask" ::: Word32)
                       -> io ()
cmdSetStencilWriteMask commandBuffer faceMask writeMask = liftIO $ do
  let vkCmdSetStencilWriteMaskPtr = pVkCmdSetStencilWriteMask (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetStencilWriteMaskPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetStencilWriteMask is null" Nothing Nothing
  let vkCmdSetStencilWriteMask' = mkVkCmdSetStencilWriteMask vkCmdSetStencilWriteMaskPtr
  traceAroundEvent "vkCmdSetStencilWriteMask" (vkCmdSetStencilWriteMask' (commandBufferHandle (commandBuffer)) (faceMask) (writeMask))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilReference
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()

-- | vkCmdSetStencilReference - Set the stencil reference dynamic state
--
-- = Description
--
-- This command sets the state for a given draw when the graphics pipeline
-- is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_REFERENCE' set
-- in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetStencilReference-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetStencilReference-faceMask-parameter# @faceMask@ /must/
--     be a valid combination of
--     'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits' values
--
-- -   #VUID-vkCmdSetStencilReference-faceMask-requiredbitmask# @faceMask@
--     /must/ not be @0@
--
-- -   #VUID-vkCmdSetStencilReference-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetStencilReference-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlags'
cmdSetStencilReference :: forall io
                        . (MonadIO io)
                       => -- | @commandBuffer@ is the command buffer into which the command will be
                          -- recorded.
                          CommandBuffer
                       -> -- | @faceMask@ is a bitmask of
                          -- 'Vulkan.Core10.Enums.StencilFaceFlagBits.StencilFaceFlagBits' specifying
                          -- the set of stencil state for which to update the reference value, as
                          -- described above for 'cmdSetStencilCompareMask'.
                          ("faceMask" ::: StencilFaceFlags)
                       -> -- | @reference@ is the new value to use as the stencil reference value.
                          ("reference" ::: Word32)
                       -> io ()
cmdSetStencilReference commandBuffer faceMask reference = liftIO $ do
  let vkCmdSetStencilReferencePtr = pVkCmdSetStencilReference (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetStencilReferencePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetStencilReference is null" Nothing Nothing
  let vkCmdSetStencilReference' = mkVkCmdSetStencilReference vkCmdSetStencilReferencePtr
  traceAroundEvent "vkCmdSetStencilReference" (vkCmdSetStencilReference' (commandBufferHandle (commandBuffer)) (faceMask) (reference))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindDescriptorSets
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr DescriptorSet -> Word32 -> Ptr Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr DescriptorSet -> Word32 -> Ptr Word32 -> IO ()

-- | vkCmdBindDescriptorSets - Binds descriptor sets to a command buffer
--
-- = Description
--
-- 'cmdBindDescriptorSets' causes the sets numbered [@firstSet@..
-- @firstSet@+@descriptorSetCount@-1] to use the bindings stored in
-- @pDescriptorSets@[0..descriptorSetCount-1] for subsequent
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipeline-bindpoint-commands bound pipeline commands>
-- set by @pipelineBindPoint@. Any bindings that were previously applied
-- via these sets are no longer valid.
--
-- Once bound, a descriptor set affects rendering of subsequent commands
-- that interact with the given pipeline type in the command buffer until
-- either a different set is bound to the same set number, or the set is
-- disturbed as described in
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
-- /must/ also be compatible with the pipeline used in subsequent
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipeline-bindpoint-commands bound pipeline commands>
-- with that pipeline type, as defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>
-- section.
--
-- The descriptor set contents bound by a call to 'cmdBindDescriptorSets'
-- /may/ be consumed at the following times:
--
-- -   For descriptor bindings created with the
--     'Vulkan.Core12.Enums.DescriptorBindingFlagBits.DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT'
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
-- -   #VUID-vkCmdBindDescriptorSets-pDescriptorSets-00358# Each element of
--     @pDescriptorSets@ /must/ have been allocated with a
--     'Vulkan.Core10.Handles.DescriptorSetLayout' that matches (is the
--     same as, or identically defined as) the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' at set /n/ in @layout@,
--     where /n/ is the sum of @firstSet@ and the index into
--     @pDescriptorSets@
--
-- -   #VUID-vkCmdBindDescriptorSets-dynamicOffsetCount-00359#
--     @dynamicOffsetCount@ /must/ be equal to the total number of dynamic
--     descriptors in @pDescriptorSets@
--
-- -   #VUID-vkCmdBindDescriptorSets-firstSet-00360# The sum of @firstSet@
--     and @descriptorSetCount@ /must/ be less than or equal to
--     'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   #VUID-vkCmdBindDescriptorSets-pipelineBindPoint-00361#
--     @pipelineBindPoint@ /must/ be supported by the @commandBuffer@’s
--     parent 'Vulkan.Core10.Handles.CommandPool'’s queue family
--
-- -   #VUID-vkCmdBindDescriptorSets-pDynamicOffsets-01971# Each element of
--     @pDynamicOffsets@ which corresponds to a descriptor binding with
--     type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     /must/ be a multiple of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minUniformBufferOffsetAlignment@
--
-- -   #VUID-vkCmdBindDescriptorSets-pDynamicOffsets-01972# Each element of
--     @pDynamicOffsets@ which corresponds to a descriptor binding with
--     type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     /must/ be a multiple of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minStorageBufferOffsetAlignment@
--
-- -   #VUID-vkCmdBindDescriptorSets-pDescriptorSets-01979# For each
--     dynamic uniform or storage buffer binding in @pDescriptorSets@, the
--     sum of the effective offset, as defined above, and the range of the
--     binding /must/ be less than or equal to the size of the buffer
--
-- -   #VUID-vkCmdBindDescriptorSets-pDescriptorSets-04616# Each element of
--     @pDescriptorSets@ /must/ not have been allocated from a
--     'Vulkan.Core10.Handles.DescriptorPool' with the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_VALVE'
--     flag set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindDescriptorSets-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindDescriptorSets-pipelineBindPoint-parameter#
--     @pipelineBindPoint@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' value
--
-- -   #VUID-vkCmdBindDescriptorSets-layout-parameter# @layout@ /must/ be a
--     valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-vkCmdBindDescriptorSets-pDescriptorSets-parameter#
--     @pDescriptorSets@ /must/ be a valid pointer to an array of
--     @descriptorSetCount@ valid 'Vulkan.Core10.Handles.DescriptorSet'
--     handles
--
-- -   #VUID-vkCmdBindDescriptorSets-pDynamicOffsets-parameter# If
--     @dynamicOffsetCount@ is not @0@, @pDynamicOffsets@ /must/ be a valid
--     pointer to an array of @dynamicOffsetCount@ @uint32_t@ values
--
-- -   #VUID-vkCmdBindDescriptorSets-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindDescriptorSets-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdBindDescriptorSets-descriptorSetCount-arraylength#
--     @descriptorSetCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdBindDescriptorSets-commonparent# Each of @commandBuffer@,
--     @layout@, and the elements of @pDescriptorSets@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Handles.DescriptorSet',
-- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint',
-- 'Vulkan.Core10.Handles.PipelineLayout'
cmdBindDescriptorSets :: forall io
                       . (MonadIO io)
                      => -- | @commandBuffer@ is the command buffer that the descriptor sets will be
                         -- bound to.
                         CommandBuffer
                      -> -- | @pipelineBindPoint@ is a
                         -- 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint' indicating the
                         -- type of the pipeline that will use the descriptors. There is a separate
                         -- set of bind points for each pipeline type, so binding one does not
                         -- disturb the others.
                         PipelineBindPoint
                      -> -- | @layout@ is a 'Vulkan.Core10.Handles.PipelineLayout' object used to
                         -- program the bindings.
                         PipelineLayout
                      -> -- | @firstSet@ is the set number of the first descriptor set to be bound.
                         ("firstSet" ::: Word32)
                      -> -- | @pDescriptorSets@ is a pointer to an array of handles to
                         -- 'Vulkan.Core10.Handles.DescriptorSet' objects describing the descriptor
                         -- sets to write to.
                         ("descriptorSets" ::: Vector DescriptorSet)
                      -> -- | @pDynamicOffsets@ is a pointer to an array of @uint32_t@ values
                         -- specifying dynamic offsets.
                         ("dynamicOffsets" ::: Vector Word32)
                      -> io ()
cmdBindDescriptorSets commandBuffer pipelineBindPoint layout firstSet descriptorSets dynamicOffsets = liftIO . evalContT $ do
  let vkCmdBindDescriptorSetsPtr = pVkCmdBindDescriptorSets (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBindDescriptorSetsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindDescriptorSets is null" Nothing Nothing
  let vkCmdBindDescriptorSets' = mkVkCmdBindDescriptorSets vkCmdBindDescriptorSetsPtr
  pPDescriptorSets <- ContT $ allocaBytesAligned @DescriptorSet ((Data.Vector.length (descriptorSets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorSets `plusPtr` (8 * (i)) :: Ptr DescriptorSet) (e)) (descriptorSets)
  pPDynamicOffsets <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (dynamicOffsets)) * 4) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDynamicOffsets `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (dynamicOffsets)
  lift $ traceAroundEvent "vkCmdBindDescriptorSets" (vkCmdBindDescriptorSets' (commandBufferHandle (commandBuffer)) (pipelineBindPoint) (layout) (firstSet) ((fromIntegral (Data.Vector.length $ (descriptorSets)) :: Word32)) (pPDescriptorSets) ((fromIntegral (Data.Vector.length $ (dynamicOffsets)) :: Word32)) (pPDynamicOffsets))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindIndexBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> IndexType -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> IndexType -> IO ()

-- | vkCmdBindIndexBuffer - Bind an index buffer to a command buffer
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindIndexBuffer-offset-00431# @offset@ /must/ be less
--     than the size of @buffer@
--
-- -   #VUID-vkCmdBindIndexBuffer-offset-00432# The sum of @offset@ and the
--     address of the range of 'Vulkan.Core10.Handles.DeviceMemory' object
--     that is backing @buffer@, /must/ be a multiple of the type indicated
--     by @indexType@
--
-- -   #VUID-vkCmdBindIndexBuffer-buffer-00433# @buffer@ /must/ have been
--     created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDEX_BUFFER_BIT'
--     flag
--
-- -   #VUID-vkCmdBindIndexBuffer-buffer-00434# If @buffer@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBindIndexBuffer-indexType-02507# @indexType@ /must/ not
--     be 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR'
--
-- -   #VUID-vkCmdBindIndexBuffer-indexType-02765# If @indexType@ is
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8_EXT', the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-indexTypeUint8 indexTypeUint8>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindIndexBuffer-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindIndexBuffer-buffer-parameter# @buffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdBindIndexBuffer-indexType-parameter# @indexType@ /must/
--     be a valid 'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   #VUID-vkCmdBindIndexBuffer-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindIndexBuffer-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBindIndexBuffer-commonparent# Both of @buffer@, and
--     @commandBuffer@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.IndexType.IndexType'
cmdBindIndexBuffer :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer into which the command is
                      -- recorded.
                      CommandBuffer
                   -> -- | @buffer@ is the buffer being bound.
                      Buffer
                   -> -- | @offset@ is the starting offset in bytes within @buffer@ used in index
                      -- buffer address calculations.
                      ("offset" ::: DeviceSize)
                   -> -- | @indexType@ is a 'Vulkan.Core10.Enums.IndexType.IndexType' value
                      -- specifying whether indices are treated as 16 bits or 32 bits.
                      IndexType
                   -> io ()
cmdBindIndexBuffer commandBuffer buffer offset indexType = liftIO $ do
  let vkCmdBindIndexBufferPtr = pVkCmdBindIndexBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBindIndexBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindIndexBuffer is null" Nothing Nothing
  let vkCmdBindIndexBuffer' = mkVkCmdBindIndexBuffer vkCmdBindIndexBufferPtr
  traceAroundEvent "vkCmdBindIndexBuffer" (vkCmdBindIndexBuffer' (commandBufferHandle (commandBuffer)) (buffer) (offset) (indexType))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindVertexBuffers
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()

-- | vkCmdBindVertexBuffers - Bind vertex buffers to a command buffer
--
-- = Description
--
-- The values taken from elements i of @pBuffers@ and @pOffsets@ replace
-- the current state for the vertex input binding @firstBinding@ + i, for i
-- in [0, @bindingCount@). The vertex input binding is updated to start at
-- the offset indicated by @pOffsets@[i] from the start of the buffer
-- @pBuffers@[i]. All vertex input attributes that use each of these
-- bindings will use these updated addresses in their address calculations
-- for subsequent draw commands. If the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
-- feature is enabled, elements of @pBuffers@ /can/ be
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', and /can/ be used by the
-- vertex shader. If a vertex input attribute is bound to a vertex input
-- binding that is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the values
-- taken from memory are considered to be zero, and missing G, B, or A
-- components are
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input-extraction filled with (0,0,1)>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindVertexBuffers-firstBinding-00624# @firstBinding@
--     /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-vkCmdBindVertexBuffers-firstBinding-00625# The sum of
--     @firstBinding@ and @bindingCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-vkCmdBindVertexBuffers-pOffsets-00626# All elements of
--     @pOffsets@ /must/ be less than the size of the corresponding element
--     in @pBuffers@
--
-- -   #VUID-vkCmdBindVertexBuffers-pBuffers-00627# All elements of
--     @pBuffers@ /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_VERTEX_BUFFER_BIT'
--     flag
--
-- -   #VUID-vkCmdBindVertexBuffers-pBuffers-00628# Each element of
--     @pBuffers@ that is non-sparse /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBindVertexBuffers-pBuffers-04001# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, all elements of @pBuffers@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBindVertexBuffers-pBuffers-04002# If an element of
--     @pBuffers@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', then the
--     corresponding element of @pOffsets@ /must/ be zero
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindVertexBuffers-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindVertexBuffers-pBuffers-parameter# @pBuffers@ /must/
--     be a valid pointer to an array of @bindingCount@ valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     'Vulkan.Core10.Handles.Buffer' handles
--
-- -   #VUID-vkCmdBindVertexBuffers-pOffsets-parameter# @pOffsets@ /must/
--     be a valid pointer to an array of @bindingCount@
--     'Vulkan.Core10.FundamentalTypes.DeviceSize' values
--
-- -   #VUID-vkCmdBindVertexBuffers-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindVertexBuffers-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBindVertexBuffers-bindingCount-arraylength#
--     @bindingCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdBindVertexBuffers-commonparent# Both of @commandBuffer@,
--     and the elements of @pBuffers@ that are valid handles of non-ignored
--     parameters /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdBindVertexBuffers :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer into which the command is
                        -- recorded.
                        CommandBuffer
                     -> -- | @firstBinding@ is the index of the first vertex input binding whose
                        -- state is updated by the command.
                        ("firstBinding" ::: Word32)
                     -> -- | @pBuffers@ is a pointer to an array of buffer handles.
                        ("buffers" ::: Vector Buffer)
                     -> -- | @pOffsets@ is a pointer to an array of buffer offsets.
                        ("offsets" ::: Vector DeviceSize)
                     -> io ()
cmdBindVertexBuffers commandBuffer firstBinding buffers offsets = liftIO . evalContT $ do
  let vkCmdBindVertexBuffersPtr = pVkCmdBindVertexBuffers (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBindVertexBuffersPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindVertexBuffers is null" Nothing Nothing
  let vkCmdBindVertexBuffers' = mkVkCmdBindVertexBuffers vkCmdBindVertexBuffersPtr
  let pBuffersLength = Data.Vector.length $ (buffers)
  lift $ unless ((Data.Vector.length $ (offsets)) == pBuffersLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pOffsets and pBuffers must have the same length" Nothing Nothing
  pPBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (buffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (buffers)
  pPOffsets <- ContT $ allocaBytesAligned @DeviceSize ((Data.Vector.length (offsets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (offsets)
  lift $ traceAroundEvent "vkCmdBindVertexBuffers" (vkCmdBindVertexBuffers' (commandBufferHandle (commandBuffer)) (firstBinding) ((fromIntegral pBuffersLength :: Word32)) (pPBuffers) (pPOffsets))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDraw
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

-- | vkCmdDraw - Draw primitives
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
-- -   #VUID-vkCmdDraw-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDraw-None-02691# If a 'Vulkan.Core10.Handles.ImageView'
--     is accessed using atomic operations as a result of this command,
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDraw-None-02692# If a 'Vulkan.Core10.Handles.ImageView'
--     is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDraw-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDraw-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDraw-flags-02696# Any 'Vulkan.Core10.Handles.Image'
--     created with a 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@
--     containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDraw-None-02697# For each set /n/ that is statically used
--     by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command, a descriptor set /must/ have been bound
--     to /n/ at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDraw-None-02698# For each push constant that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDraw-None-02699# Descriptors in each bound descriptor
--     set, specified via 'cmdBindDescriptorSets', /must/ be valid if they
--     are statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to
--     the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDraw-None-02700# A valid pipeline /must/ be bound to the
--     pipeline bind point used by this command
--
-- -   #VUID-vkCmdDraw-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   #VUID-vkCmdDraw-None-02859# There /must/ not have been any calls to
--     dynamic state setting commands for any state not specified as
--     dynamic in the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command, since that pipeline was
--     bound
--
-- -   #VUID-vkCmdDraw-None-02702# If the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a 'Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   #VUID-vkCmdDraw-None-02703# If the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a 'Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   #VUID-vkCmdDraw-None-02704# If the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a 'Vulkan.Core10.Handles.Sampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   #VUID-vkCmdDraw-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDraw-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDraw-commandBuffer-02707# If @commandBuffer@ is an
--     unprotected command buffer, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDraw-None-04115# If a 'Vulkan.Core10.Handles.ImageView'
--     is accessed using @OpImageWrite@ as a result of this command, then
--     the @Type@ of the @Texel@ operand of that instruction /must/ have at
--     least as many components as the image view’s format.
--
-- -   #VUID-vkCmdDraw-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDraw-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDraw-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDraw-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDraw-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDraw-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDraw-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDraw-renderPass-02684# The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDraw-subpass-02685# The subpass index of the current
--     render pass /must/ be equal to the @subpass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDraw-None-02686# Every input attachment used by the
--     current subpass /must/ be bound to the pipeline via a descriptor set
--
-- -   #VUID-vkCmdDraw-None-04584# Image subresources used as attachments
--     in the current render pass /must/ not be accessed in any way other
--     than as an attachment by this command, except for cases involving
--     read-only access to depth\/stencil attachments as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-attachment-nonattachment Render Pass>
--     chapter
--
-- -   #VUID-vkCmdDraw-maxMultiviewInstanceIndex-02688# If the draw is
--     recorded in a render pass instance with multiview enabled, the
--     maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   #VUID-vkCmdDraw-sampleLocationsEnable-02689# If the bound graphics
--     pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE' and the current subpass
--     has a depth\/stencil attachment, then that attachment /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdDraw-viewportCount-03417# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDraw-scissorCount-03418# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDraw-viewportCount-03419# If the bound graphics pipeline
--     state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic states enabled then both
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--
-- -   #VUID-vkCmdDraw-viewportCount-04137# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDraw-viewportCount-04138# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDraw-viewportCount-04139# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDraw-viewportCount-04140# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDraw-VkPipelineVieportCreateInfo-04141# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDraw-VkPipelineVieportCreateInfo-04142# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDraw-primitiveTopology-03420# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDraw-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDraw-commandBuffer-02712# If @commandBuffer@ is a
--     protected command buffer, any resource written to by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be an unprotected resource
--
-- -   #VUID-vkCmdDraw-commandBuffer-02713# If @commandBuffer@ is a
--     protected command buffer, pipeline stages other than the
--     framebuffer-space and compute stages in the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point /must/ not write to any resource
--
-- -   #VUID-vkCmdDraw-commandBuffer-04617# If any of the shader stages of
--     the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command uses the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-RayQueryKHR RayQueryKHR>
--     capability, then @commandBuffer@ /must/ not be a protected command
--     buffer
--
-- -   #VUID-vkCmdDraw-None-04007# All vertex input bindings accessed via
--     vertex input variables declared in the vertex shader entry point’s
--     interface /must/ have either valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' buffers bound
--
-- -   #VUID-vkCmdDraw-None-04008# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, all vertex input bindings accessed via
--     vertex input variables declared in the vertex shader entry point’s
--     interface /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdDraw-None-02721# For a given vertex buffer binding, any
--     attribute data fetched /must/ be entirely contained within the
--     corresponding vertex buffer binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDraw-commandBuffer-parameter# @commandBuffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDraw-commandBuffer-recording# @commandBuffer@ /must/ be
--     in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDraw-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdDraw-renderpass# This command /must/ only be called
--     inside of a render pass instance
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdDraw :: forall io
         . (MonadIO io)
        => -- | @commandBuffer@ is the command buffer into which the command is
           -- recorded.
           CommandBuffer
        -> -- | @vertexCount@ is the number of vertices to draw.
           ("vertexCount" ::: Word32)
        -> -- | @instanceCount@ is the number of instances to draw.
           ("instanceCount" ::: Word32)
        -> -- | @firstVertex@ is the index of the first vertex to draw.
           ("firstVertex" ::: Word32)
        -> -- | @firstInstance@ is the instance ID of the first instance to draw.
           ("firstInstance" ::: Word32)
        -> io ()
cmdDraw commandBuffer vertexCount instanceCount firstVertex firstInstance = liftIO $ do
  let vkCmdDrawPtr = pVkCmdDraw (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDraw is null" Nothing Nothing
  let vkCmdDraw' = mkVkCmdDraw vkCmdDrawPtr
  traceAroundEvent "vkCmdDraw" (vkCmdDraw' (commandBufferHandle (commandBuffer)) (vertexCount) (instanceCount) (firstVertex) (firstInstance))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndexed
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()

-- | vkCmdDrawIndexed - Issue an indexed draw into a command buffer
--
-- = Description
--
-- When the command is executed, primitives are assembled using the current
-- primitive topology and @indexCount@ vertices whose indices are retrieved
-- from the index buffer. The index buffer is treated as an array of
-- tightly packed unsigned integers of size defined by the
-- 'cmdBindIndexBuffer'::@indexType@ parameter with which the buffer was
-- bound.
--
-- The first vertex index is at an offset of @firstIndex@ × @indexSize@ +
-- @offset@ within the bound index buffer, where @offset@ is the offset
-- specified by 'cmdBindIndexBuffer' and @indexSize@ is the byte size of
-- the type specified by @indexType@. Subsequent index values are retrieved
-- from consecutive locations in the index buffer. Indices are first
-- compared to the primitive restart value, then zero extended to 32 bits
-- (if the @indexType@ is
-- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8_EXT' or
-- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16') and have
-- @vertexOffset@ added to them, before being supplied as the @vertexIndex@
-- value.
--
-- The primitives are drawn @instanceCount@ times with @instanceIndex@
-- starting with @firstInstance@ and increasing sequentially for each
-- instance. The assembled primitives execute the bound graphics pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDrawIndexed-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDrawIndexed-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDrawIndexed-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndexed-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawIndexed-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawIndexed-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDrawIndexed-None-02697# For each set /n/ that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a descriptor set /must/
--     have been bound to /n/ at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawIndexed-None-02698# For each push constant that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawIndexed-None-02699# Descriptors in each bound
--     descriptor set, specified via 'cmdBindDescriptorSets', /must/ be
--     valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdDrawIndexed-None-02700# A valid pipeline /must/ be bound
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDrawIndexed-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   #VUID-vkCmdDrawIndexed-None-02859# There /must/ not have been any
--     calls to dynamic state setting commands for any state not specified
--     as dynamic in the 'Vulkan.Core10.Handles.Pipeline' object bound to
--     the pipeline bind point used by this command, since that pipeline
--     was bound
--
-- -   #VUID-vkCmdDrawIndexed-None-02702# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   #VUID-vkCmdDrawIndexed-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDrawIndexed-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDrawIndexed-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndexed-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndexed-commandBuffer-02707# If @commandBuffer@ is an
--     unprotected command buffer, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDrawIndexed-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawIndexed-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawIndexed-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawIndexed-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawIndexed-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawIndexed-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawIndexed-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawIndexed-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawIndexed-renderPass-02684# The current render pass
--     /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawIndexed-subpass-02685# The subpass index of the
--     current render pass /must/ be equal to the @subpass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawIndexed-None-02686# Every input attachment used by
--     the current subpass /must/ be bound to the pipeline via a descriptor
--     set
--
-- -   #VUID-vkCmdDrawIndexed-None-04584# Image subresources used as
--     attachments in the current render pass /must/ not be accessed in any
--     way other than as an attachment by this command, except for cases
--     involving read-only access to depth\/stencil attachments as
--     described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-attachment-nonattachment Render Pass>
--     chapter
--
-- -   #VUID-vkCmdDrawIndexed-maxMultiviewInstanceIndex-02688# If the draw
--     is recorded in a render pass instance with multiview enabled, the
--     maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   #VUID-vkCmdDrawIndexed-sampleLocationsEnable-02689# If the bound
--     graphics pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE' and the current subpass
--     has a depth\/stencil attachment, then that attachment /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdDrawIndexed-viewportCount-03417# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndexed-scissorCount-03418# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndexed-viewportCount-03419# If the bound graphics
--     pipeline state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic states enabled then both
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexed-viewportCount-04137# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexed-viewportCount-04138# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexed-viewportCount-04139# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexed-viewportCount-04140# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexed-VkPipelineVieportCreateInfo-04141# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexed-VkPipelineVieportCreateInfo-04142# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexed-primitiveTopology-03420# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDrawIndexed-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDrawIndexed-commandBuffer-02712# If @commandBuffer@ is a
--     protected command buffer, any resource written to by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be an unprotected resource
--
-- -   #VUID-vkCmdDrawIndexed-commandBuffer-02713# If @commandBuffer@ is a
--     protected command buffer, pipeline stages other than the
--     framebuffer-space and compute stages in the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point /must/ not write to any resource
--
-- -   #VUID-vkCmdDrawIndexed-commandBuffer-04617# If any of the shader
--     stages of the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline
--     bind point used by this command uses the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-RayQueryKHR RayQueryKHR>
--     capability, then @commandBuffer@ /must/ not be a protected command
--     buffer
--
-- -   #VUID-vkCmdDrawIndexed-None-04007# All vertex input bindings
--     accessed via vertex input variables declared in the vertex shader
--     entry point’s interface /must/ have either valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' buffers bound
--
-- -   #VUID-vkCmdDrawIndexed-None-04008# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, all vertex input bindings accessed via
--     vertex input variables declared in the vertex shader entry point’s
--     interface /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdDrawIndexed-None-02721# For a given vertex buffer
--     binding, any attribute data fetched /must/ be entirely contained
--     within the corresponding vertex buffer binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   #VUID-vkCmdDrawIndexed-indexSize-00463# (@indexSize@ × (@firstIndex@
--     + @indexCount@) + @offset@) /must/ be less than or equal to the size
--     of the bound index buffer, with @indexSize@ being based on the type
--     specified by @indexType@, where the index buffer, @indexType@, and
--     @offset@ are specified via 'cmdBindIndexBuffer'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDrawIndexed-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDrawIndexed-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDrawIndexed-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdDrawIndexed-renderpass# This command /must/ only be
--     called inside of a render pass instance
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdDrawIndexed :: forall io
                . (MonadIO io)
               => -- | @commandBuffer@ is the command buffer into which the command is
                  -- recorded.
                  CommandBuffer
               -> -- | @indexCount@ is the number of vertices to draw.
                  ("indexCount" ::: Word32)
               -> -- | @instanceCount@ is the number of instances to draw.
                  ("instanceCount" ::: Word32)
               -> -- | @firstIndex@ is the base index within the index buffer.
                  ("firstIndex" ::: Word32)
               -> -- | @vertexOffset@ is the value added to the vertex index before indexing
                  -- into the vertex buffer.
                  ("vertexOffset" ::: Int32)
               -> -- | @firstInstance@ is the instance ID of the first instance to draw.
                  ("firstInstance" ::: Word32)
               -> io ()
cmdDrawIndexed commandBuffer indexCount instanceCount firstIndex vertexOffset firstInstance = liftIO $ do
  let vkCmdDrawIndexedPtr = pVkCmdDrawIndexed (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawIndexedPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawIndexed is null" Nothing Nothing
  let vkCmdDrawIndexed' = mkVkCmdDrawIndexed vkCmdDrawIndexedPtr
  traceAroundEvent "vkCmdDrawIndexed" (vkCmdDrawIndexed' (commandBufferHandle (commandBuffer)) (indexCount) (instanceCount) (firstIndex) (vertexOffset) (firstInstance))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirect
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- | vkCmdDrawIndirect - Issue an indirect draw into a command buffer
--
-- = Description
--
-- 'cmdDrawIndirect' behaves similarly to 'cmdDraw' except that the
-- parameters are read by the device from a buffer during execution.
-- @drawCount@ draws are executed by the command, with parameters taken
-- from @buffer@ starting at @offset@ and increasing by @stride@ bytes for
-- each successive draw. The parameters of each draw are encoded in an
-- array of 'Vulkan.Core10.OtherTypes.DrawIndirectCommand' structures. If
-- @drawCount@ is less than or equal to one, @stride@ is ignored.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDrawIndirect-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDrawIndirect-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDrawIndirect-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirect-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawIndirect-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawIndirect-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDrawIndirect-None-02697# For each set /n/ that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a descriptor set /must/
--     have been bound to /n/ at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawIndirect-None-02698# For each push constant that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawIndirect-None-02699# Descriptors in each bound
--     descriptor set, specified via 'cmdBindDescriptorSets', /must/ be
--     valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdDrawIndirect-None-02700# A valid pipeline /must/ be bound
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDrawIndirect-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   #VUID-vkCmdDrawIndirect-None-02859# There /must/ not have been any
--     calls to dynamic state setting commands for any state not specified
--     as dynamic in the 'Vulkan.Core10.Handles.Pipeline' object bound to
--     the pipeline bind point used by this command, since that pipeline
--     was bound
--
-- -   #VUID-vkCmdDrawIndirect-None-02702# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   #VUID-vkCmdDrawIndirect-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDrawIndirect-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDrawIndirect-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndirect-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndirect-commandBuffer-02707# If @commandBuffer@ is
--     an unprotected command buffer, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDrawIndirect-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawIndirect-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawIndirect-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawIndirect-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawIndirect-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawIndirect-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawIndirect-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawIndirect-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawIndirect-renderPass-02684# The current render pass
--     /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawIndirect-subpass-02685# The subpass index of the
--     current render pass /must/ be equal to the @subpass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawIndirect-None-02686# Every input attachment used by
--     the current subpass /must/ be bound to the pipeline via a descriptor
--     set
--
-- -   #VUID-vkCmdDrawIndirect-None-04584# Image subresources used as
--     attachments in the current render pass /must/ not be accessed in any
--     way other than as an attachment by this command, except for cases
--     involving read-only access to depth\/stencil attachments as
--     described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-attachment-nonattachment Render Pass>
--     chapter
--
-- -   #VUID-vkCmdDrawIndirect-maxMultiviewInstanceIndex-02688# If the draw
--     is recorded in a render pass instance with multiview enabled, the
--     maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   #VUID-vkCmdDrawIndirect-sampleLocationsEnable-02689# If the bound
--     graphics pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE' and the current subpass
--     has a depth\/stencil attachment, then that attachment /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdDrawIndirect-viewportCount-03417# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndirect-scissorCount-03418# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndirect-viewportCount-03419# If the bound graphics
--     pipeline state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic states enabled then both
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirect-viewportCount-04137# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirect-viewportCount-04138# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirect-viewportCount-04139# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirect-viewportCount-04140# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirect-VkPipelineVieportCreateInfo-04141# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirect-VkPipelineVieportCreateInfo-04142# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndirect-primitiveTopology-03420# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDrawIndirect-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDrawIndirect-None-04007# All vertex input bindings
--     accessed via vertex input variables declared in the vertex shader
--     entry point’s interface /must/ have either valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' buffers bound
--
-- -   #VUID-vkCmdDrawIndirect-None-04008# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, all vertex input bindings accessed via
--     vertex input variables declared in the vertex shader entry point’s
--     interface /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdDrawIndirect-None-02721# For a given vertex buffer
--     binding, any attribute data fetched /must/ be entirely contained
--     within the corresponding vertex buffer binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   #VUID-vkCmdDrawIndirect-buffer-02708# If @buffer@ is non-sparse then
--     it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdDrawIndirect-buffer-02709# @buffer@ /must/ have been
--     created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-vkCmdDrawIndirect-offset-02710# @offset@ /must/ be a multiple
--     of @4@
--
-- -   #VUID-vkCmdDrawIndirect-commandBuffer-02711# @commandBuffer@ /must/
--     not be a protected command buffer
--
-- -   #VUID-vkCmdDrawIndirect-drawCount-02718# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiDrawIndirect multi-draw indirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   #VUID-vkCmdDrawIndirect-drawCount-02719# @drawCount@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   #VUID-vkCmdDrawIndirect-firstInstance-00478# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, all the @firstInstance@ members of the
--     'Vulkan.Core10.OtherTypes.DrawIndirectCommand' structures accessed
--     by this command /must/ be @0@
--
-- -   #VUID-vkCmdDrawIndirect-drawCount-00476# If @drawCount@ is greater
--     than @1@, @stride@ /must/ be a multiple of @4@ and /must/ be greater
--     than or equal to
--     @sizeof@('Vulkan.Core10.OtherTypes.DrawIndirectCommand')
--
-- -   #VUID-vkCmdDrawIndirect-drawCount-00487# If @drawCount@ is equal to
--     @1@, (@offset@ +
--     @sizeof@('Vulkan.Core10.OtherTypes.DrawIndirectCommand')) /must/ be
--     less than or equal to the size of @buffer@
--
-- -   #VUID-vkCmdDrawIndirect-drawCount-00488# If @drawCount@ is greater
--     than @1@, (@stride@ × (@drawCount@ - 1) + @offset@ +
--     @sizeof@('Vulkan.Core10.OtherTypes.DrawIndirectCommand')) /must/ be
--     less than or equal to the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDrawIndirect-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDrawIndirect-buffer-parameter# @buffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdDrawIndirect-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDrawIndirect-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdDrawIndirect-renderpass# This command /must/ only be
--     called inside of a render pass instance
--
-- -   #VUID-vkCmdDrawIndirect-commonparent# Both of @buffer@, and
--     @commandBuffer@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdDrawIndirect :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer into which the command is
                   -- recorded.
                   CommandBuffer
                -> -- | @buffer@ is the buffer containing draw parameters.
                   Buffer
                -> -- | @offset@ is the byte offset into @buffer@ where parameters begin.
                   ("offset" ::: DeviceSize)
                -> -- | @drawCount@ is the number of draws to execute, and /can/ be zero.
                   ("drawCount" ::: Word32)
                -> -- | @stride@ is the byte stride between successive sets of draw parameters.
                   ("stride" ::: Word32)
                -> io ()
cmdDrawIndirect commandBuffer buffer offset drawCount stride = liftIO $ do
  let vkCmdDrawIndirectPtr = pVkCmdDrawIndirect (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawIndirectPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawIndirect is null" Nothing Nothing
  let vkCmdDrawIndirect' = mkVkCmdDrawIndirect vkCmdDrawIndirectPtr
  traceAroundEvent "vkCmdDrawIndirect" (vkCmdDrawIndirect' (commandBufferHandle (commandBuffer)) (buffer) (offset) (drawCount) (stride))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndexedIndirect
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- | vkCmdDrawIndexedIndirect - Perform an indexed indirect draw
--
-- = Description
--
-- 'cmdDrawIndexedIndirect' behaves similarly to 'cmdDrawIndexed' except
-- that the parameters are read by the device from a buffer during
-- execution. @drawCount@ draws are executed by the command, with
-- parameters taken from @buffer@ starting at @offset@ and increasing by
-- @stride@ bytes for each successive draw. The parameters of each draw are
-- encoded in an array of
-- 'Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand' structures. If
-- @drawCount@ is less than or equal to one, @stride@ is ignored.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDrawIndexedIndirect-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02697# For each set /n/ that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a descriptor set /must/
--     have been bound to /n/ at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02698# For each push constant
--     that is statically used by the 'Vulkan.Core10.Handles.Pipeline'
--     bound to the pipeline bind point used by this command, a push
--     constant value /must/ have been set for the same pipeline bind
--     point, with a 'Vulkan.Core10.Handles.PipelineLayout' that is
--     compatible for push constants, with the
--     'Vulkan.Core10.Handles.PipelineLayout' used to create the current
--     'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02699# Descriptors in each bound
--     descriptor set, specified via 'cmdBindDescriptorSets', /must/ be
--     valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02700# A valid pipeline /must/
--     be bound to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDrawIndexedIndirect-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02859# There /must/ not have
--     been any calls to dynamic state setting commands for any state not
--     specified as dynamic in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command, since that
--     pipeline was bound
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02702# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndexedIndirect-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer, any resource
--     accessed by the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-renderPass-02684# The current render
--     pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-subpass-02685# The subpass index of
--     the current render pass /must/ be equal to the @subpass@ member of
--     the 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' structure
--     specified when creating the 'Vulkan.Core10.Handles.Pipeline' bound
--     to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02686# Every input attachment
--     used by the current subpass /must/ be bound to the pipeline via a
--     descriptor set
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-04584# Image subresources used
--     as attachments in the current render pass /must/ not be accessed in
--     any way other than as an attachment by this command, except for
--     cases involving read-only access to depth\/stencil attachments as
--     described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-attachment-nonattachment Render Pass>
--     chapter
--
-- -   #VUID-vkCmdDrawIndexedIndirect-maxMultiviewInstanceIndex-02688# If
--     the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-sampleLocationsEnable-02689# If the
--     bound graphics pipeline was created with
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE' and the current subpass
--     has a depth\/stencil attachment, then that attachment /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
--     bit set
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewportCount-03417# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndexedIndirect-scissorCount-03418# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewportCount-03419# If the bound
--     graphics pipeline state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic states enabled then both
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewportCount-04137# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewportCount-04138# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewportCount-04139# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewportCount-04140# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-VkPipelineVieportCreateInfo-04141# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-VkPipelineVieportCreateInfo-04142# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled and an instance of
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     chained from @VkPipelineVieportCreateInfo@, then the bound graphics
--     pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-primitiveTopology-03420# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDrawIndexedIndirect-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @viewportCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-04007# All vertex input bindings
--     accessed via vertex input variables declared in the vertex shader
--     entry point’s interface /must/ have either valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' buffers bound
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-04008# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, all vertex input bindings accessed via
--     vertex input variables declared in the vertex shader entry point’s
--     interface /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-02721# For a given vertex buffer
--     binding, any attribute data fetched /must/ be entirely contained
--     within the corresponding vertex buffer binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   #VUID-vkCmdDrawIndexedIndirect-buffer-02708# If @buffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdDrawIndexedIndirect-buffer-02709# @buffer@ /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-vkCmdDrawIndexedIndirect-offset-02710# @offset@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-commandBuffer-02711# @commandBuffer@
--     /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdDrawIndexedIndirect-drawCount-02718# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiDrawIndirect multi-draw indirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-drawCount-02719# @drawCount@ /must/
--     be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-drawCount-00528# If @drawCount@ is
--     greater than @1@, @stride@ /must/ be a multiple of @4@ and /must/ be
--     greater than or equal to
--     @sizeof@('Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand')
--
-- -   #VUID-vkCmdDrawIndexedIndirect-firstInstance-00530# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, all the @firstInstance@ members of the
--     'Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand' structures
--     accessed by this command /must/ be @0@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-drawCount-00539# If @drawCount@ is
--     equal to @1@, (@offset@ +
--     @sizeof@('Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand'))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-drawCount-00540# If @drawCount@ is
--     greater than @1@, (@stride@ × (@drawCount@ - 1) + @offset@ +
--     @sizeof@('Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand'))
--     /must/ be less than or equal to the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDrawIndexedIndirect-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDrawIndexedIndirect-buffer-parameter# @buffer@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdDrawIndexedIndirect-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDrawIndexedIndirect-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdDrawIndexedIndirect-renderpass# This command /must/ only
--     be called inside of a render pass instance
--
-- -   #VUID-vkCmdDrawIndexedIndirect-commonparent# Both of @buffer@, and
--     @commandBuffer@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdDrawIndexedIndirect :: forall io
                        . (MonadIO io)
                       => -- | @commandBuffer@ is the command buffer into which the command is
                          -- recorded.
                          CommandBuffer
                       -> -- | @buffer@ is the buffer containing draw parameters.
                          Buffer
                       -> -- | @offset@ is the byte offset into @buffer@ where parameters begin.
                          ("offset" ::: DeviceSize)
                       -> -- | @drawCount@ is the number of draws to execute, and /can/ be zero.
                          ("drawCount" ::: Word32)
                       -> -- | @stride@ is the byte stride between successive sets of draw parameters.
                          ("stride" ::: Word32)
                       -> io ()
cmdDrawIndexedIndirect commandBuffer buffer offset drawCount stride = liftIO $ do
  let vkCmdDrawIndexedIndirectPtr = pVkCmdDrawIndexedIndirect (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawIndexedIndirectPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawIndexedIndirect is null" Nothing Nothing
  let vkCmdDrawIndexedIndirect' = mkVkCmdDrawIndexedIndirect vkCmdDrawIndexedIndirectPtr
  traceAroundEvent "vkCmdDrawIndexedIndirect" (vkCmdDrawIndexedIndirect' (commandBufferHandle (commandBuffer)) (buffer) (offset) (drawCount) (stride))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatch
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> IO ()

-- | vkCmdDispatch - Dispatch compute work items
--
-- = Description
--
-- When the command is executed, a global workgroup consisting of
-- @groupCountX@ × @groupCountY@ × @groupCountZ@ local workgroups is
-- assembled.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDispatch-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatch-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatch-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDispatch-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatch-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatch-flags-02696# Any 'Vulkan.Core10.Handles.Image'
--     created with a 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@
--     containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDispatch-None-02697# For each set /n/ that is statically
--     used by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatch-None-02698# For each push constant that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatch-None-02699# Descriptors in each bound descriptor
--     set, specified via 'cmdBindDescriptorSets', /must/ be valid if they
--     are statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to
--     the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatch-None-02700# A valid pipeline /must/ be bound to
--     the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatch-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   #VUID-vkCmdDispatch-None-02859# There /must/ not have been any calls
--     to dynamic state setting commands for any state not specified as
--     dynamic in the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command, since that pipeline was
--     bound
--
-- -   #VUID-vkCmdDispatch-None-02702# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   #VUID-vkCmdDispatch-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDispatch-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDispatch-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDispatch-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDispatch-commandBuffer-02707# If @commandBuffer@ is an
--     unprotected command buffer, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDispatch-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDispatch-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDispatch-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDispatch-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDispatch-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDispatch-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDispatch-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDispatch-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDispatch-commandBuffer-02712# If @commandBuffer@ is a
--     protected command buffer, any resource written to by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be an unprotected resource
--
-- -   #VUID-vkCmdDispatch-commandBuffer-02713# If @commandBuffer@ is a
--     protected command buffer, pipeline stages other than the
--     framebuffer-space and compute stages in the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point /must/ not write to any resource
--
-- -   #VUID-vkCmdDispatch-commandBuffer-04617# If any of the shader stages
--     of the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command uses the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-RayQueryKHR RayQueryKHR>
--     capability, then @commandBuffer@ /must/ not be a protected command
--     buffer
--
-- -   #VUID-vkCmdDispatch-groupCountX-00386# @groupCountX@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   #VUID-vkCmdDispatch-groupCountY-00387# @groupCountY@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   #VUID-vkCmdDispatch-groupCountZ-00388# @groupCountZ@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDispatch-commandBuffer-parameter# @commandBuffer@ /must/
--     be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDispatch-commandBuffer-recording# @commandBuffer@ /must/
--     be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDispatch-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdDispatch-renderpass# This command /must/ only be called
--     outside of a render pass instance
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               | Compute                                                                                                                             |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdDispatch :: forall io
             . (MonadIO io)
            => -- | @commandBuffer@ is the command buffer into which the command will be
               -- recorded.
               CommandBuffer
            -> -- | @groupCountX@ is the number of local workgroups to dispatch in the X
               -- dimension.
               ("groupCountX" ::: Word32)
            -> -- | @groupCountY@ is the number of local workgroups to dispatch in the Y
               -- dimension.
               ("groupCountY" ::: Word32)
            -> -- | @groupCountZ@ is the number of local workgroups to dispatch in the Z
               -- dimension.
               ("groupCountZ" ::: Word32)
            -> io ()
cmdDispatch commandBuffer groupCountX groupCountY groupCountZ = liftIO $ do
  let vkCmdDispatchPtr = pVkCmdDispatch (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDispatchPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDispatch is null" Nothing Nothing
  let vkCmdDispatch' = mkVkCmdDispatch vkCmdDispatchPtr
  traceAroundEvent "vkCmdDispatch" (vkCmdDispatch' (commandBufferHandle (commandBuffer)) (groupCountX) (groupCountY) (groupCountZ))
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
-- = Description
--
-- 'cmdDispatchIndirect' behaves similarly to 'cmdDispatch' except that the
-- parameters are read by the device from a buffer during execution. The
-- parameters of the dispatch are encoded in a
-- 'Vulkan.Core10.OtherTypes.DispatchIndirectCommand' structure taken from
-- @buffer@ starting at @offset@.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDispatchIndirect-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatchIndirect-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatchIndirect-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchIndirect-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatchIndirect-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatchIndirect-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDispatchIndirect-None-02697# For each set /n/ that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a descriptor set /must/
--     have been bound to /n/ at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchIndirect-None-02698# For each push constant that
--     is statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to
--     the pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchIndirect-None-02699# Descriptors in each bound
--     descriptor set, specified via 'cmdBindDescriptorSets', /must/ be
--     valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdDispatchIndirect-None-02700# A valid pipeline /must/ be
--     bound to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchIndirect-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   #VUID-vkCmdDispatchIndirect-None-02859# There /must/ not have been
--     any calls to dynamic state setting commands for any state not
--     specified as dynamic in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command, since that
--     pipeline was bound
--
-- -   #VUID-vkCmdDispatchIndirect-None-02702# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   #VUID-vkCmdDispatchIndirect-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdDispatchIndirect-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdDispatchIndirect-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchIndirect-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchIndirect-commandBuffer-02707# If @commandBuffer@
--     is an unprotected command buffer, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDispatchIndirect-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDispatchIndirect-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdDispatchIndirect-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDispatchIndirect-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDispatchIndirect-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdDispatchIndirect-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdDispatchIndirect-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDispatchIndirect-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdDispatchIndirect-buffer-02708# If @buffer@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdDispatchIndirect-buffer-02709# @buffer@ /must/ have been
--     created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-vkCmdDispatchIndirect-offset-02710# @offset@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-vkCmdDispatchIndirect-commandBuffer-02711# @commandBuffer@
--     /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdDispatchIndirect-offset-00407# The sum of @offset@ and
--     the size of 'Vulkan.Core10.OtherTypes.DispatchIndirectCommand'
--     /must/ be less than or equal to the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDispatchIndirect-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDispatchIndirect-buffer-parameter# @buffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdDispatchIndirect-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDispatchIndirect-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdDispatchIndirect-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdDispatchIndirect-commonparent# Both of @buffer@, and
--     @commandBuffer@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               | Compute                                                                                                                             |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdDispatchIndirect :: forall io
                     . (MonadIO io)
                    => -- | @commandBuffer@ is the command buffer into which the command will be
                       -- recorded.
                       CommandBuffer
                    -> -- | @buffer@ is the buffer containing dispatch parameters.
                       Buffer
                    -> -- | @offset@ is the byte offset into @buffer@ where parameters begin.
                       ("offset" ::: DeviceSize)
                    -> io ()
cmdDispatchIndirect commandBuffer buffer offset = liftIO $ do
  let vkCmdDispatchIndirectPtr = pVkCmdDispatchIndirect (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDispatchIndirectPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDispatchIndirect is null" Nothing Nothing
  let vkCmdDispatchIndirect' = mkVkCmdDispatchIndirect vkCmdDispatchIndirectPtr
  traceAroundEvent "vkCmdDispatchIndirect" (vkCmdDispatchIndirect' (commandBufferHandle (commandBuffer)) (buffer) (offset))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> Buffer -> Word32 -> Ptr BufferCopy -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> Buffer -> Word32 -> Ptr BufferCopy -> IO ()

-- | vkCmdCopyBuffer - Copy data between buffer regions
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
-- -   #VUID-vkCmdCopyBuffer-commandBuffer-01822# If @commandBuffer@ is an
--     unprotected command buffer, then @srcBuffer@ /must/ not be a
--     protected buffer
--
-- -   #VUID-vkCmdCopyBuffer-commandBuffer-01823# If @commandBuffer@ is an
--     unprotected command buffer, then @dstBuffer@ /must/ not be a
--     protected buffer
--
-- -   #VUID-vkCmdCopyBuffer-commandBuffer-01824# If @commandBuffer@ is a
--     protected command buffer, then @dstBuffer@ /must/ not be an
--     unprotected buffer
--
-- -   #VUID-vkCmdCopyBuffer-srcOffset-00113# The @srcOffset@ member of
--     each element of @pRegions@ /must/ be less than the size of
--     @srcBuffer@
--
-- -   #VUID-vkCmdCopyBuffer-dstOffset-00114# The @dstOffset@ member of
--     each element of @pRegions@ /must/ be less than the size of
--     @dstBuffer@
--
-- -   #VUID-vkCmdCopyBuffer-size-00115# The @size@ member of each element
--     of @pRegions@ /must/ be less than or equal to the size of
--     @srcBuffer@ minus @srcOffset@
--
-- -   #VUID-vkCmdCopyBuffer-size-00116# The @size@ member of each element
--     of @pRegions@ /must/ be less than or equal to the size of
--     @dstBuffer@ minus @dstOffset@
--
-- -   #VUID-vkCmdCopyBuffer-pRegions-00117# The union of the source
--     regions, and the union of the destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-vkCmdCopyBuffer-srcBuffer-00118# @srcBuffer@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-vkCmdCopyBuffer-srcBuffer-00119# If @srcBuffer@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyBuffer-dstBuffer-00120# @dstBuffer@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdCopyBuffer-dstBuffer-00121# If @dstBuffer@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyBuffer-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyBuffer-srcBuffer-parameter# @srcBuffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdCopyBuffer-dstBuffer-parameter# @dstBuffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdCopyBuffer-pRegions-parameter# @pRegions@ /must/ be a
--     valid pointer to an array of @regionCount@ valid 'BufferCopy'
--     structures
--
-- -   #VUID-vkCmdCopyBuffer-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyBuffer-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyBuffer-renderpass# This command /must/ only be called
--     outside of a render pass instance
--
-- -   #VUID-vkCmdCopyBuffer-regionCount-arraylength# @regionCount@ /must/
--     be greater than @0@
--
-- -   #VUID-vkCmdCopyBuffer-commonparent# Each of @commandBuffer@,
--     @dstBuffer@, and @srcBuffer@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'BufferCopy',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdCopyBuffer :: forall io
               . (MonadIO io)
              => -- | @commandBuffer@ is the command buffer into which the command will be
                 -- recorded.
                 CommandBuffer
              -> -- | @srcBuffer@ is the source buffer.
                 ("srcBuffer" ::: Buffer)
              -> -- | @dstBuffer@ is the destination buffer.
                 ("dstBuffer" ::: Buffer)
              -> -- | @pRegions@ is a pointer to an array of 'BufferCopy' structures
                 -- specifying the regions to copy.
                 ("regions" ::: Vector BufferCopy)
              -> io ()
cmdCopyBuffer commandBuffer srcBuffer dstBuffer regions = liftIO . evalContT $ do
  let vkCmdCopyBufferPtr = pVkCmdCopyBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyBuffer is null" Nothing Nothing
  let vkCmdCopyBuffer' = mkVkCmdCopyBuffer vkCmdCopyBufferPtr
  pPRegions <- ContT $ allocaBytesAligned @BufferCopy ((Data.Vector.length (regions)) * 24) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions `plusPtr` (24 * (i)) :: Ptr BufferCopy) (e)) (regions)
  lift $ traceAroundEvent "vkCmdCopyBuffer" (vkCmdCopyBuffer' (commandBufferHandle (commandBuffer)) (srcBuffer) (dstBuffer) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageCopy -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageCopy -> IO ()

-- | vkCmdCopyImage - Copy data between images
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
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT', or
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'. For
-- the purposes of 'cmdCopyImage', each plane of a multi-planar image is
-- treated as having the format listed in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes>
-- for the plane identified by the @aspectMask@ of the corresponding
-- subresource. This applies both to 'Vulkan.Core10.Enums.Format.Format'
-- and to coordinates used in the copy, which correspond to texels in the
-- /plane/ rather than how these texels map to coordinates in the image as
-- a whole.
--
-- Note
--
-- For example, the
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' plane
-- of a 'Vulkan.Core10.Enums.Format.FORMAT_G8_B8R8_2PLANE_420_UNORM' image
-- is compatible with an image of format
-- 'Vulkan.Core10.Enums.Format.FORMAT_R8G8_UNORM' and (less usefully) with
-- the 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
-- plane of an image of format
-- 'Vulkan.Core10.Enums.Format.FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16',
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
-- -   #VUID-vkCmdCopyImage-commandBuffer-01825# If @commandBuffer@ is an
--     unprotected command buffer, then @srcImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdCopyImage-commandBuffer-01826# If @commandBuffer@ is an
--     unprotected command buffer, then @dstImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdCopyImage-commandBuffer-01827# If @commandBuffer@ is a
--     protected command buffer, then @dstImage@ /must/ not be an
--     unprotected image
--
-- -   #VUID-vkCmdCopyImage-pRegions-00124# The union of all source
--     regions, and the union of all destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-vkCmdCopyImage-srcImage-01995# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_SRC_BIT'
--
-- -   #VUID-vkCmdCopyImage-srcImage-00126# @srcImage@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-vkCmdCopyImage-srcImage-01546# If @srcImage@ is non-sparse
--     then the image or /disjoint/ plane to be copied /must/ be bound
--     completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyImage-srcImageLayout-00128# @srcImageLayout@ /must/
--     specify the layout of the image subresources of @srcImage@ specified
--     in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-vkCmdCopyImage-srcImageLayout-01917# @srcImageLayout@ /must/
--     be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   #VUID-vkCmdCopyImage-dstImage-01996# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
--
-- -   #VUID-vkCmdCopyImage-dstImage-00131# @dstImage@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdCopyImage-dstImage-01547# If @dstImage@ is non-sparse
--     then the image or /disjoint/ plane that is the destination of the
--     copy /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyImage-dstImageLayout-00133# @dstImageLayout@ /must/
--     specify the layout of the image subresources of @dstImage@ specified
--     in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-vkCmdCopyImage-dstImageLayout-01395# @dstImageLayout@ /must/
--     be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   #VUID-vkCmdCopyImage-srcImage-01548# If the
--     'Vulkan.Core10.Enums.Format.Format' of each of @srcImage@ and
--     @dstImage@ is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     the 'Vulkan.Core10.Enums.Format.Format' of each of @srcImage@ and
--     @dstImage@ /must/ be compatible, as defined
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-images-format-compatibility above>
--
-- -   #VUID-vkCmdCopyImage-None-01549# In a copy to or from a plane of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image>,
--     the 'Vulkan.Core10.Enums.Format.Format' of the image and plane
--     /must/ be compatible according to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes the description of compatible planes>
--     for the plane being copied
--
-- -   #VUID-vkCmdCopyImage-srcImage-00136# The sample count of @srcImage@
--     and @dstImage@ /must/ match
--
-- -   #VUID-vkCmdCopyImage-srcSubresource-01696# The
--     @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-vkCmdCopyImage-dstSubresource-01697# The
--     @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-vkCmdCopyImage-srcSubresource-01698# The
--     @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @srcImage@ was created
--
-- -   #VUID-vkCmdCopyImage-dstSubresource-01699# The
--     @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-vkCmdCopyImage-srcOffset-01783# The @srcOffset@ and @extent@
--     members of each element of @pRegions@ /must/ respect the image
--     transfer granularity requirements of @commandBuffer@’s command
--     pool’s queue family, as described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   #VUID-vkCmdCopyImage-dstOffset-01784# The @dstOffset@ and @extent@
--     members of each element of @pRegions@ /must/ respect the image
--     transfer granularity requirements of @commandBuffer@’s command
--     pool’s queue family, as described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   #VUID-vkCmdCopyImage-dstImage-02542# @dstImage@ and @srcImage@
--     /must/ not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-vkCmdCopyImage-srcImage-01551# If neither @srcImage@ nor
--     @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
--     then for each element of @pRegions@, @srcSubresource.aspectMask@ and
--     @dstSubresource.aspectMask@ /must/ match
--
-- -   #VUID-vkCmdCopyImage-srcImage-01552# If @srcImage@ has a
--     'Vulkan.Core10.Enums.Format.Format' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion two planes>
--     then for each element of @pRegions@, @srcSubresource.aspectMask@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--
-- -   #VUID-vkCmdCopyImage-srcImage-01553# If @srcImage@ has a
--     'Vulkan.Core10.Enums.Format.Format' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion three planes>
--     then for each element of @pRegions@, @srcSubresource.aspectMask@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-vkCmdCopyImage-dstImage-01554# If @dstImage@ has a
--     'Vulkan.Core10.Enums.Format.Format' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion two planes>
--     then for each element of @pRegions@, @dstSubresource.aspectMask@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT'
--
-- -   #VUID-vkCmdCopyImage-dstImage-01555# If @dstImage@ has a
--     'Vulkan.Core10.Enums.Format.Format' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion three planes>
--     then for each element of @pRegions@, @dstSubresource.aspectMask@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--
-- -   #VUID-vkCmdCopyImage-srcImage-01556# If @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
--     and the @dstImage@ does not have a multi-planar image format, then
--     for each element of @pRegions@, @dstSubresource.aspectMask@ /must/
--     be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-vkCmdCopyImage-dstImage-01557# If @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>
--     and the @srcImage@ does not have a multi-planar image format, then
--     for each element of @pRegions@, @srcSubresource.aspectMask@ /must/
--     be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-vkCmdCopyImage-srcImage-04443# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @srcSubresource.baseArrayLayer@ /must/ be @0@ and and
--     @srcSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-vkCmdCopyImage-dstImage-04444# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @dstSubresource.baseArrayLayer@ /must/ be @0@ and and
--     @dstSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-vkCmdCopyImage-aspectMask-00142# For each element of
--     @pRegions@, @srcSubresource.aspectMask@ /must/ specify aspects
--     present in @srcImage@
--
-- -   #VUID-vkCmdCopyImage-aspectMask-00143# For each element of
--     @pRegions@, @dstSubresource.aspectMask@ /must/ specify aspects
--     present in @dstImage@
--
-- -   #VUID-vkCmdCopyImage-srcOffset-00144# For each element of
--     @pRegions@, @srcOffset.x@ and (@extent.width@ + @srcOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdCopyImage-srcOffset-00145# For each element of
--     @pRegions@, @srcOffset.y@ and (@extent.height@ + @srcOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdCopyImage-srcImage-00146# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @srcOffset.y@ /must/ be @0@ and @extent.height@
--     /must/ be @1@
--
-- -   #VUID-vkCmdCopyImage-srcOffset-00147# For each element of
--     @pRegions@, @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdCopyImage-srcImage-01785# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-vkCmdCopyImage-dstImage-01786# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-vkCmdCopyImage-srcImage-01787# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @srcOffset.z@ /must/ be @0@
--
-- -   #VUID-vkCmdCopyImage-dstImage-01788# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @dstOffset.z@ /must/ be @0@
--
-- -   #VUID-vkCmdCopyImage-srcImage-01790# If @srcImage@ and @dstImage@
--     are both of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then
--     for each element of @pRegions@, @extent.depth@ /must/ be @1@
--
-- -   #VUID-vkCmdCopyImage-srcImage-01791# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each
--     element of @pRegions@, @extent.depth@ /must/ equal
--     @srcSubresource.layerCount@
--
-- -   #VUID-vkCmdCopyImage-dstImage-01792# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', and @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each
--     element of @pRegions@, @extent.depth@ /must/ equal
--     @dstSubresource.layerCount@
--
-- -   #VUID-vkCmdCopyImage-dstOffset-00150# For each element of
--     @pRegions@, @dstOffset.x@ and (@extent.width@ + @dstOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdCopyImage-dstOffset-00151# For each element of
--     @pRegions@, @dstOffset.y@ and (@extent.height@ + @dstOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdCopyImage-dstImage-00152# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @dstOffset.y@ /must/ be @0@ and @extent.height@
--     /must/ be @1@
--
-- -   #VUID-vkCmdCopyImage-dstOffset-00153# For each element of
--     @pRegions@, @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdCopyImage-srcImage-01727# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, all members of @srcOffset@
--     /must/ be a multiple of the corresponding dimensions of the
--     compressed texel block
--
-- -   #VUID-vkCmdCopyImage-srcImage-01728# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, @extent.width@ /must/ be a
--     multiple of the compressed texel block width or (@extent.width@ +
--     @srcOffset.x@) /must/ equal the width of the specified
--     @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdCopyImage-srcImage-01729# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, @extent.height@ /must/ be a
--     multiple of the compressed texel block height or (@extent.height@ +
--     @srcOffset.y@) /must/ equal the height of the specified
--     @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdCopyImage-srcImage-01730# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, @extent.depth@ /must/ be a
--     multiple of the compressed texel block depth or (@extent.depth@ +
--     @srcOffset.z@) /must/ equal the depth of the specified
--     @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdCopyImage-dstImage-01731# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, all members of @dstOffset@
--     /must/ be a multiple of the corresponding dimensions of the
--     compressed texel block
--
-- -   #VUID-vkCmdCopyImage-dstImage-01732# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, @extent.width@ /must/ be a
--     multiple of the compressed texel block width or (@extent.width@ +
--     @dstOffset.x@) /must/ equal the width of the specified
--     @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdCopyImage-dstImage-01733# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, @extent.height@ /must/ be a
--     multiple of the compressed texel block height or (@extent.height@ +
--     @dstOffset.y@) /must/ equal the height of the specified
--     @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdCopyImage-dstImage-01734# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     then for each element of @pRegions@, @extent.depth@ /must/ be a
--     multiple of the compressed texel block depth or (@extent.depth@ +
--     @dstOffset.z@) /must/ equal the depth of the specified
--     @dstSubresource@ of @dstImage@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyImage-commandBuffer-parameter# @commandBuffer@ /must/
--     be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyImage-srcImage-parameter# @srcImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkCmdCopyImage-srcImageLayout-parameter# @srcImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-vkCmdCopyImage-dstImage-parameter# @dstImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkCmdCopyImage-dstImageLayout-parameter# @dstImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-vkCmdCopyImage-pRegions-parameter# @pRegions@ /must/ be a
--     valid pointer to an array of @regionCount@ valid 'ImageCopy'
--     structures
--
-- -   #VUID-vkCmdCopyImage-commandBuffer-recording# @commandBuffer@ /must/
--     be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyImage-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyImage-renderpass# This command /must/ only be called
--     outside of a render pass instance
--
-- -   #VUID-vkCmdCopyImage-regionCount-arraylength# @regionCount@ /must/
--     be greater than @0@
--
-- -   #VUID-vkCmdCopyImage-commonparent# Each of @commandBuffer@,
--     @dstImage@, and @srcImage@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Image',
-- 'ImageCopy', 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
cmdCopyImage :: forall io
              . (MonadIO io)
             => -- | @commandBuffer@ is the command buffer into which the command will be
                -- recorded.
                CommandBuffer
             -> -- | @srcImage@ is the source image.
                ("srcImage" ::: Image)
             -> -- | @srcImageLayout@ is the current layout of the source image subresource.
                ("srcImageLayout" ::: ImageLayout)
             -> -- | @dstImage@ is the destination image.
                ("dstImage" ::: Image)
             -> -- | @dstImageLayout@ is the current layout of the destination image
                -- subresource.
                ("dstImageLayout" ::: ImageLayout)
             -> -- | @pRegions@ is a pointer to an array of 'ImageCopy' structures specifying
                -- the regions to copy.
                ("regions" ::: Vector ImageCopy)
             -> io ()
cmdCopyImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout regions = liftIO . evalContT $ do
  let vkCmdCopyImagePtr = pVkCmdCopyImage (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyImage is null" Nothing Nothing
  let vkCmdCopyImage' = mkVkCmdCopyImage vkCmdCopyImagePtr
  pPRegions <- ContT $ allocaBytesAligned @ImageCopy ((Data.Vector.length (regions)) * 68) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions `plusPtr` (68 * (i)) :: Ptr ImageCopy) (e)) (regions)
  lift $ traceAroundEvent "vkCmdCopyImage" (vkCmdCopyImage' (commandBufferHandle (commandBuffer)) (srcImage) (srcImageLayout) (dstImage) (dstImageLayout) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions))
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
--     -   scaleu = (xsrc1 - xsrc0) \/ (xdst1 - xdst0)
--
--     -   scalev = (ysrc1 - ysrc0) \/ (ydst1 - ydst0)
--
--     -   scalew = (zsrc1 - zsrc0) \/ (zdst1 - zdst0)
--
--     -   uscaled = uoffset × scaleu
--
--     -   vscaled = voffset × scalev
--
--     -   wscaled = woffset × scalew
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
-- with the filter mode equal to that of @filter@, a mipmap mode of
-- 'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_NEAREST' and
-- an address mode of
-- 'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
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
-- When blitting 3D textures, slices in the destination region bounded by
-- @dstOffsets@[0].z and @dstOffsets@[1].z are sampled from slices in the
-- source region bounded by @srcOffsets@[0].z and @srcOffsets@[1].z. If the
-- @filter@ parameter is 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' then
-- the value sampled from the source image is taken by doing linear
-- filtering using the interpolated __z__ coordinate represented by __w__
-- in the previous equations. If the @filter@ parameter is
-- 'Vulkan.Core10.Enums.Filter.FILTER_NEAREST' then the value sampled from
-- the source image is taken from the single nearest slice, with an
-- implementation-dependent arithmetic rounding mode.
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
-- -   #VUID-vkCmdBlitImage-commandBuffer-01834# If @commandBuffer@ is an
--     unprotected command buffer, then @srcImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdBlitImage-commandBuffer-01835# If @commandBuffer@ is an
--     unprotected command buffer, then @dstImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdBlitImage-commandBuffer-01836# If @commandBuffer@ is a
--     protected command buffer, then @dstImage@ /must/ not be an
--     unprotected image
--
-- -   #VUID-vkCmdBlitImage-pRegions-00215# The source region specified by
--     each element of @pRegions@ /must/ be a region that is contained
--     within @srcImage@
--
-- -   #VUID-vkCmdBlitImage-pRegions-00216# The destination region
--     specified by each element of @pRegions@ /must/ be a region that is
--     contained within @dstImage@
--
-- -   #VUID-vkCmdBlitImage-pRegions-00217# The union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory with any texel that /may/ be sampled during the blit
--     operation
--
-- -   #VUID-vkCmdBlitImage-srcImage-01999# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_SRC_BIT'
--
-- -   #VUID-vkCmdBlitImage-srcImage-01561# @srcImage@ /must/ not use a
--     format listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion ???>
--
-- -   #VUID-vkCmdBlitImage-srcImage-00219# @srcImage@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-vkCmdBlitImage-srcImage-00220# If @srcImage@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBlitImage-srcImageLayout-00221# @srcImageLayout@ /must/
--     specify the layout of the image subresources of @srcImage@ specified
--     in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-vkCmdBlitImage-srcImageLayout-01398# @srcImageLayout@ /must/
--     be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-vkCmdBlitImage-dstImage-02000# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_BLIT_DST_BIT'
--
-- -   #VUID-vkCmdBlitImage-dstImage-01562# @dstImage@ /must/ not use a
--     format listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion ???>
--
-- -   #VUID-vkCmdBlitImage-dstImage-00224# @dstImage@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdBlitImage-dstImage-00225# If @dstImage@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBlitImage-dstImageLayout-00226# @dstImageLayout@ /must/
--     specify the layout of the image subresources of @dstImage@ specified
--     in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-vkCmdBlitImage-dstImageLayout-01399# @dstImageLayout@ /must/
--     be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-vkCmdBlitImage-srcImage-00229# If either of @srcImage@ or
--     @dstImage@ was created with a signed integer
--     'Vulkan.Core10.Enums.Format.Format', the other /must/ also have been
--     created with a signed integer 'Vulkan.Core10.Enums.Format.Format'
--
-- -   #VUID-vkCmdBlitImage-srcImage-00230# If either of @srcImage@ or
--     @dstImage@ was created with an unsigned integer
--     'Vulkan.Core10.Enums.Format.Format', the other /must/ also have been
--     created with an unsigned integer 'Vulkan.Core10.Enums.Format.Format'
--
-- -   #VUID-vkCmdBlitImage-srcImage-00231# If either of @srcImage@ or
--     @dstImage@ was created with a depth\/stencil format, the other
--     /must/ have exactly the same format
--
-- -   #VUID-vkCmdBlitImage-srcImage-00232# If @srcImage@ was created with
--     a depth\/stencil format, @filter@ /must/ be
--     'Vulkan.Core10.Enums.Filter.FILTER_NEAREST'
--
-- -   #VUID-vkCmdBlitImage-srcImage-00233# @srcImage@ /must/ have been
--     created with a @samples@ value of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-vkCmdBlitImage-dstImage-00234# @dstImage@ /must/ have been
--     created with a @samples@ value of
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-vkCmdBlitImage-filter-02001# If @filter@ is
--     'Vulkan.Core10.Enums.Filter.FILTER_LINEAR', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdBlitImage-filter-02002# If @filter@ is
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdBlitImage-filter-00237# If @filter@ is
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT', @srcImage@
--     /must/ be of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
--
-- -   #VUID-vkCmdBlitImage-srcSubresource-01705# The
--     @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-vkCmdBlitImage-dstSubresource-01706# The
--     @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-vkCmdBlitImage-srcSubresource-01707# The
--     @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @srcImage@ was created
--
-- -   #VUID-vkCmdBlitImage-dstSubresource-01708# The
--     @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-vkCmdBlitImage-dstImage-02545# @dstImage@ and @srcImage@
--     /must/ not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-vkCmdBlitImage-srcImage-00240# If either @srcImage@ or
--     @dstImage@ is of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D',
--     then for each element of @pRegions@, @srcSubresource.baseArrayLayer@
--     and @dstSubresource.baseArrayLayer@ /must/ each be @0@, and
--     @srcSubresource.layerCount@ and @dstSubresource.layerCount@ /must/
--     each be @1@.
--
-- -   #VUID-vkCmdBlitImage-aspectMask-00241# For each element of
--     @pRegions@, @srcSubresource.aspectMask@ /must/ specify aspects
--     present in @srcImage@
--
-- -   #VUID-vkCmdBlitImage-aspectMask-00242# For each element of
--     @pRegions@, @dstSubresource.aspectMask@ /must/ specify aspects
--     present in @dstImage@
--
-- -   #VUID-vkCmdBlitImage-srcOffset-00243# For each element of
--     @pRegions@, @srcOffset@[0].x and @srcOffset@[1].x /must/ both be
--     greater than or equal to @0@ and less than or equal to the width of
--     the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdBlitImage-srcOffset-00244# For each element of
--     @pRegions@, @srcOffset@[0].y and @srcOffset@[1].y /must/ both be
--     greater than or equal to @0@ and less than or equal to the height of
--     the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdBlitImage-srcImage-00245# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @srcOffset@[0].y /must/ be @0@ and @srcOffset@[1].y
--     /must/ be @1@
--
-- -   #VUID-vkCmdBlitImage-srcOffset-00246# For each element of
--     @pRegions@, @srcOffset@[0].z and @srcOffset@[1].z /must/ both be
--     greater than or equal to @0@ and less than or equal to the depth of
--     the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdBlitImage-srcImage-00247# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @srcOffset@[0].z /must/ be @0@ and @srcOffset@[1].z
--     /must/ be @1@
--
-- -   #VUID-vkCmdBlitImage-dstOffset-00248# For each element of
--     @pRegions@, @dstOffset@[0].x and @dstOffset@[1].x /must/ both be
--     greater than or equal to @0@ and less than or equal to the width of
--     the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdBlitImage-dstOffset-00249# For each element of
--     @pRegions@, @dstOffset@[0].y and @dstOffset@[1].y /must/ both be
--     greater than or equal to @0@ and less than or equal to the height of
--     the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdBlitImage-dstImage-00250# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @dstOffset@[0].y /must/ be @0@ and @dstOffset@[1].y
--     /must/ be @1@
--
-- -   #VUID-vkCmdBlitImage-dstOffset-00251# For each element of
--     @pRegions@, @dstOffset@[0].z and @dstOffset@[1].z /must/ both be
--     greater than or equal to @0@ and less than or equal to the depth of
--     the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdBlitImage-dstImage-00252# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @dstOffset@[0].z /must/ be @0@ and @dstOffset@[1].z
--     /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBlitImage-commandBuffer-parameter# @commandBuffer@ /must/
--     be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBlitImage-srcImage-parameter# @srcImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkCmdBlitImage-srcImageLayout-parameter# @srcImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-vkCmdBlitImage-dstImage-parameter# @dstImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkCmdBlitImage-dstImageLayout-parameter# @dstImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-vkCmdBlitImage-pRegions-parameter# @pRegions@ /must/ be a
--     valid pointer to an array of @regionCount@ valid 'ImageBlit'
--     structures
--
-- -   #VUID-vkCmdBlitImage-filter-parameter# @filter@ /must/ be a valid
--     'Vulkan.Core10.Enums.Filter.Filter' value
--
-- -   #VUID-vkCmdBlitImage-commandBuffer-recording# @commandBuffer@ /must/
--     be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBlitImage-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBlitImage-renderpass# This command /must/ only be called
--     outside of a render pass instance
--
-- -   #VUID-vkCmdBlitImage-regionCount-arraylength# @regionCount@ /must/
--     be greater than @0@
--
-- -   #VUID-vkCmdBlitImage-commonparent# Each of @commandBuffer@,
--     @dstImage@, and @srcImage@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.Filter.Filter', 'Vulkan.Core10.Handles.Image',
-- 'ImageBlit', 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
cmdBlitImage :: forall io
              . (MonadIO io)
             => -- | @commandBuffer@ is the command buffer into which the command will be
                -- recorded.
                CommandBuffer
             -> -- | @srcImage@ is the source image.
                ("srcImage" ::: Image)
             -> -- | @srcImageLayout@ is the layout of the source image subresources for the
                -- blit.
                ("srcImageLayout" ::: ImageLayout)
             -> -- | @dstImage@ is the destination image.
                ("dstImage" ::: Image)
             -> -- | @dstImageLayout@ is the layout of the destination image subresources for
                -- the blit.
                ("dstImageLayout" ::: ImageLayout)
             -> -- | @pRegions@ is a pointer to an array of 'ImageBlit' structures specifying
                -- the regions to blit.
                ("regions" ::: Vector ImageBlit)
             -> -- | @filter@ is a 'Vulkan.Core10.Enums.Filter.Filter' specifying the filter
                -- to apply if the blits require scaling.
                Filter
             -> io ()
cmdBlitImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout regions filter' = liftIO . evalContT $ do
  let vkCmdBlitImagePtr = pVkCmdBlitImage (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBlitImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBlitImage is null" Nothing Nothing
  let vkCmdBlitImage' = mkVkCmdBlitImage vkCmdBlitImagePtr
  pPRegions <- ContT $ allocaBytesAligned @ImageBlit ((Data.Vector.length (regions)) * 80) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions `plusPtr` (80 * (i)) :: Ptr ImageBlit) (e)) (regions)
  lift $ traceAroundEvent "vkCmdBlitImage" (vkCmdBlitImage' (commandBufferHandle (commandBuffer)) (srcImage) (srcImageLayout) (dstImage) (dstImageLayout) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions) (filter'))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBufferToImage
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> Image -> ImageLayout -> Word32 -> Ptr BufferImageCopy -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> Image -> ImageLayout -> Word32 -> Ptr BufferImageCopy -> IO ()

-- | vkCmdCopyBufferToImage - Copy data from a buffer into an image
--
-- = Description
--
-- Each region in @pRegions@ is copied from the specified region of the
-- source buffer to the specified region of the destination image.
--
-- If the format of @dstImage@ is a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
-- regions of each plane to be a target of a copy /must/ be specified
-- separately using the @pRegions@ member of the 'BufferImageCopy'
-- structure. In this case, the @aspectMask@ of @imageSubresource@ /must/
-- be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT', or
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'. For
-- the purposes of 'cmdCopyBufferToImage', each plane of a multi-planar
-- image is treated as having the format listed in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes>
-- for the plane identified by the @aspectMask@ of the corresponding
-- subresource. This applies both to 'Vulkan.Core10.Enums.Format.Format'
-- and to coordinates used in the copy, which correspond to texels in the
-- /plane/ rather than how these texels map to coordinates in the image as
-- a whole.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-01828# If @commandBuffer@
--     is an unprotected command buffer, then @srcBuffer@ /must/ not be a
--     protected buffer
--
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-01829# If @commandBuffer@
--     is an unprotected command buffer, then @dstImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-01830# If @commandBuffer@
--     is a protected command buffer, then @dstImage@ /must/ not be an
--     unprotected image
--
-- -   #VUID-vkCmdCopyBufferToImage-pRegions-00172# The image region
--     specified by each element of @pRegions@ /must/ be a region that is
--     contained within @dstImage@ if the @dstImage@’s
--     'Vulkan.Core10.Enums.Format.Format' is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     and /must/ be a region that is contained within the plane being
--     copied to if the @dstImage@’s 'Vulkan.Core10.Enums.Format.Format' is
--     a multi-planar format
--
-- -   #VUID-vkCmdCopyBufferToImage-pRegions-00171# @srcBuffer@ /must/ be
--     large enough to contain all buffer locations that are accessed
--     according to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   #VUID-vkCmdCopyBufferToImage-pRegions-00173# The union of all source
--     regions, and the union of all destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-vkCmdCopyBufferToImage-srcBuffer-00174# @srcBuffer@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-vkCmdCopyBufferToImage-dstImage-01997# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
--
-- -   #VUID-vkCmdCopyBufferToImage-srcBuffer-00176# If @srcBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyBufferToImage-dstImage-00177# @dstImage@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdCopyBufferToImage-dstImage-00178# If @dstImage@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyBufferToImage-dstImage-00179# @dstImage@ /must/ have
--     a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-vkCmdCopyBufferToImage-dstImageLayout-00180# @dstImageLayout@
--     /must/ specify the layout of the image subresources of @dstImage@
--     specified in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-vkCmdCopyBufferToImage-dstImageLayout-01396# @dstImageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   #VUID-vkCmdCopyBufferToImage-imageSubresource-01701# The
--     @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-vkCmdCopyBufferToImage-imageSubresource-01702# The
--     @imageSubresource.baseArrayLayer@ + @imageSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-vkCmdCopyBufferToImage-imageOffset-01793# The @imageOffset@
--     and @imageExtent@ members of each element of @pRegions@ /must/
--     respect the image transfer granularity requirements of
--     @commandBuffer@’s command pool’s queue family, as described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   #VUID-vkCmdCopyBufferToImage-dstImage-02543# @dstImage@ /must/ not
--     have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-04052# If the queue
--     family used to create the 'Vulkan.Core10.Handles.CommandPool' which
--     @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', the
--     @bufferOffset@ member of any element of @pRegions@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-vkCmdCopyBufferToImage-dstImage-04053# If @dstImage@ has a
--     depth\/stencil format, the @bufferOffset@ member of any element of
--     @pRegions@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-04477# If the queue
--     family used to create the 'Vulkan.Core10.Handles.CommandPool' which
--     @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', for each
--     element of @pRegions@, the @aspectMask@ member of @imageSubresource@
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'.
--
-- -   #VUID-vkCmdCopyBufferToImage-imageOffset-00197# For each element of
--     @pRegions@, @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified @imageSubresource@
--     of @dstImage@ where this refers to the width of the /plane/ of the
--     image involved in the copy in the case of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   #VUID-vkCmdCopyBufferToImage-imageOffset-00198# For each element of
--     @pRegions@, @imageOffset.y@ and (imageExtent.height +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified @imageSubresource@
--     of @dstImage@ where this refers to the height of the /plane/ of the
--     image involved in the copy in the case of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   #VUID-vkCmdCopyBufferToImage-bufferOffset-01558# If @dstImage@ does
--     not have either a depth\/stencil or a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @bufferOffset@ /must/ be a
--     multiple of the format’s texel block size
--
-- -   #VUID-vkCmdCopyBufferToImage-bufferOffset-01559# If @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @bufferOffset@ /must/ be a
--     multiple of the element size of the compatible format for the format
--     and the @aspectMask@ of the @imageSubresource@ as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes ???>
--
-- -   #VUID-vkCmdCopyBufferToImage-srcImage-00199# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @imageOffset.y@ /must/ be @0@ and
--     @imageExtent.height@ /must/ be @1@
--
-- -   #VUID-vkCmdCopyBufferToImage-imageOffset-00200# For each element of
--     @pRegions@, @imageOffset.z@ and (imageExtent.depth +
--     @imageOffset.z@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the depth of the specified @imageSubresource@
--     of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-srcImage-00201# If @dstImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @imageOffset.z@ /must/ be @0@ and @imageExtent.depth@
--     /must/ be @1@
--
-- -   #VUID-vkCmdCopyBufferToImage-bufferRowLength-00203# If @dstImage@ is
--     a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @bufferRowLength@ /must/ be a
--     multiple of the compressed texel block width
--
-- -   #VUID-vkCmdCopyBufferToImage-bufferImageHeight-00204# If @dstImage@
--     is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @bufferImageHeight@ /must/ be a
--     multiple of the compressed texel block height
--
-- -   #VUID-vkCmdCopyBufferToImage-imageOffset-00205# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, all members of @imageOffset@ /must/
--     be a multiple of the corresponding dimensions of the compressed
--     texel block
--
-- -   #VUID-vkCmdCopyBufferToImage-bufferOffset-00206# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @bufferOffset@ /must/ be a multiple
--     of the compressed texel block size in bytes
--
-- -   #VUID-vkCmdCopyBufferToImage-imageExtent-00207# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @imageExtent.width@ /must/ be a
--     multiple of the compressed texel block width or (@imageExtent.width@
--     + @imageOffset.x@) /must/ equal the width of the specified
--     @imageSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-imageExtent-00208# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @imageExtent.height@ /must/ be a
--     multiple of the compressed texel block height or
--     (@imageExtent.height@ + @imageOffset.y@) /must/ equal the height of
--     the specified @imageSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-imageExtent-00209# If @dstImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @imageExtent.depth@ /must/ be a
--     multiple of the compressed texel block depth or (@imageExtent.depth@
--     + @imageOffset.z@) /must/ equal the depth of the specified
--     @imageSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-aspectMask-00211# For each element of
--     @pRegions@, @imageSubresource.aspectMask@ /must/ specify aspects
--     present in @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-aspectMask-01560# If @dstImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @imageSubresource.aspectMask@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--     (with
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--     valid only for image formats with three planes)
--
-- -   #VUID-vkCmdCopyBufferToImage-baseArrayLayer-00213# If @dstImage@ is
--     of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each
--     element of @pRegions@, @imageSubresource.baseArrayLayer@ /must/ be
--     @0@ and @imageSubresource.layerCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyBufferToImage-srcBuffer-parameter# @srcBuffer@ /must/
--     be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdCopyBufferToImage-dstImage-parameter# @dstImage@ /must/
--     be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkCmdCopyBufferToImage-dstImageLayout-parameter#
--     @dstImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-vkCmdCopyBufferToImage-pRegions-parameter# @pRegions@ /must/
--     be a valid pointer to an array of @regionCount@ valid
--     'BufferImageCopy' structures
--
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyBufferToImage-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyBufferToImage-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- -   #VUID-vkCmdCopyBufferToImage-commonparent# Each of @commandBuffer@,
--     @dstImage@, and @srcBuffer@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'BufferImageCopy',
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
cmdCopyBufferToImage :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer into which the command will be
                        -- recorded.
                        CommandBuffer
                     -> -- | @srcBuffer@ is the source buffer.
                        ("srcBuffer" ::: Buffer)
                     -> -- | @dstImage@ is the destination image.
                        ("dstImage" ::: Image)
                     -> -- | @dstImageLayout@ is the layout of the destination image subresources for
                        -- the copy.
                        ("dstImageLayout" ::: ImageLayout)
                     -> -- | @pRegions@ is a pointer to an array of 'BufferImageCopy' structures
                        -- specifying the regions to copy.
                        ("regions" ::: Vector BufferImageCopy)
                     -> io ()
cmdCopyBufferToImage commandBuffer srcBuffer dstImage dstImageLayout regions = liftIO . evalContT $ do
  let vkCmdCopyBufferToImagePtr = pVkCmdCopyBufferToImage (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyBufferToImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyBufferToImage is null" Nothing Nothing
  let vkCmdCopyBufferToImage' = mkVkCmdCopyBufferToImage vkCmdCopyBufferToImagePtr
  pPRegions <- ContT $ allocaBytesAligned @BufferImageCopy ((Data.Vector.length (regions)) * 56) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions `plusPtr` (56 * (i)) :: Ptr BufferImageCopy) (e)) (regions)
  lift $ traceAroundEvent "vkCmdCopyBufferToImage" (vkCmdCopyBufferToImage' (commandBufferHandle (commandBuffer)) (srcBuffer) (dstImage) (dstImageLayout) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImageToBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Buffer -> Word32 -> Ptr BufferImageCopy -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Buffer -> Word32 -> Ptr BufferImageCopy -> IO ()

-- | vkCmdCopyImageToBuffer - Copy image data into a buffer
--
-- = Description
--
-- Each region in @pRegions@ is copied from the specified region of the
-- source image to the specified region of the destination buffer.
--
-- If the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@ is a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
-- regions of each plane to be a source of a copy /must/ be specified
-- separately using the @pRegions@ member of the 'BufferImageCopy'
-- structure. In this case, the @aspectMask@ of @imageSubresource@ /must/
-- be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT', or
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'. For
-- the purposes of 'cmdCopyBufferToImage', each plane of a multi-planar
-- image is treated as having the format listed in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes>
-- for the plane identified by the @aspectMask@ of the corresponding
-- subresource. This applies both to 'Vulkan.Core10.Enums.Format.Format'
-- and to coordinates used in the copy, which correspond to texels in the
-- /plane/ rather than how these texels map to coordinates in the image as
-- a whole.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyImageToBuffer-commandBuffer-01831# If @commandBuffer@
--     is an unprotected command buffer, then @srcImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdCopyImageToBuffer-commandBuffer-01832# If @commandBuffer@
--     is an unprotected command buffer, then @dstBuffer@ /must/ not be a
--     protected buffer
--
-- -   #VUID-vkCmdCopyImageToBuffer-commandBuffer-01833# If @commandBuffer@
--     is a protected command buffer, then @dstBuffer@ /must/ not be an
--     unprotected buffer
--
-- -   #VUID-vkCmdCopyImageToBuffer-pRegions-00182# The image region
--     specified by each element of @pRegions@ /must/ be a region that is
--     contained within @srcImage@ if the @srcImage@’s
--     'Vulkan.Core10.Enums.Format.Format' is not a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     and /must/ be a region that is contained within the plane being
--     copied if the @srcImage@’s 'Vulkan.Core10.Enums.Format.Format' is a
--     multi-planar format
--
-- -   #VUID-vkCmdCopyImageToBuffer-pRegions-00183# @dstBuffer@ /must/ be
--     large enough to contain all buffer locations that are accessed
--     according to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   #VUID-vkCmdCopyImageToBuffer-pRegions-00184# The union of all source
--     regions, and the union of all destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImage-00186# @srcImage@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImage-01998# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_SRC_BIT'
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImage-00187# If @srcImage@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyImageToBuffer-dstBuffer-00191# @dstBuffer@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdCopyImageToBuffer-dstBuffer-00192# If @dstBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImage-00188# @srcImage@ /must/ have
--     a sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImageLayout-00189# @srcImageLayout@
--     /must/ specify the layout of the image subresources of @srcImage@
--     specified in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImageLayout-01397# @srcImageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageSubresource-01703# The
--     @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageSubresource-01704# The
--     @imageSubresource.baseArrayLayer@ + @imageSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @srcImage@ was created
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageOffset-01794# The @imageOffset@
--     and @imageExtent@ members of each element of @pRegions@ /must/
--     respect the image transfer granularity requirements of
--     @commandBuffer@’s command pool’s queue family, as described in
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImage-02544# @srcImage@ /must/ not
--     have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-vkCmdCopyImageToBuffer-commandBuffer-04054# If the queue
--     family used to create the 'Vulkan.Core10.Handles.CommandPool' which
--     @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', the
--     @bufferOffset@ member of any element of @pRegions@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImage-04055# If @srcImage@ has a
--     depth\/stencil format, the @bufferOffset@ member of any element of
--     @pRegions@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageOffset-00197# For each element of
--     @pRegions@ , @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified @imageSubresource@
--     of @srcImage@ where this refers to the width of the /plane/ of the
--     image involved in the copy in the case of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageOffset-00198# For each element of
--     @pRegions@ , @imageOffset.y@ and (imageExtent.height +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified @imageSubresource@
--     of @srcImage@ where this refers to the height of the /plane/ of the
--     image involved in the copy in the case of a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
--
-- -   #VUID-vkCmdCopyImageToBuffer-bufferOffset-01558# If @srcImage@ does
--     not have either a depth\/stencil or a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @bufferOffset@ /must/ be a
--     multiple of the format’s texel block size
--
-- -   #VUID-vkCmdCopyImageToBuffer-bufferOffset-01559# If @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @bufferOffset@ /must/ be a
--     multiple of the element size of the compatible format for the format
--     and the @aspectMask@ of the @imageSubresource@ as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes ???>
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImage-00199# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each
--     element of @pRegions@, @imageOffset.y@ /must/ be @0@ and
--     @imageExtent.height@ /must/ be @1@
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageOffset-00200# For each element of
--     @pRegions@, @imageOffset.z@ and (imageExtent.depth +
--     @imageOffset.z@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the depth of the specified @imageSubresource@
--     of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImage-00201# If @srcImage@ is of
--     type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @imageOffset.z@ /must/ be @0@ and @imageExtent.depth@
--     /must/ be @1@
--
-- -   #VUID-vkCmdCopyImageToBuffer-bufferRowLength-00203# If @srcImage@ is
--     a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @bufferRowLength@ /must/ be a
--     multiple of the compressed texel block width
--
-- -   #VUID-vkCmdCopyImageToBuffer-bufferImageHeight-00204# If @srcImage@
--     is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @bufferImageHeight@ /must/ be a
--     multiple of the compressed texel block height
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageOffset-00205# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, all members of @imageOffset@ /must/
--     be a multiple of the corresponding dimensions of the compressed
--     texel block
--
-- -   #VUID-vkCmdCopyImageToBuffer-bufferOffset-00206# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @bufferOffset@ /must/ be a multiple
--     of the compressed texel block size in bytes
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageExtent-00207# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @imageExtent.width@ /must/ be a
--     multiple of the compressed texel block width or (@imageExtent.width@
--     + @imageOffset.x@) /must/ equal the width of the specified
--     @imageSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageExtent-00208# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @imageExtent.height@ /must/ be a
--     multiple of the compressed texel block height or
--     (@imageExtent.height@ + @imageOffset.y@) /must/ equal the height of
--     the specified @imageSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageExtent-00209# If @srcImage@ is a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#blocked-image blocked image>,
--     for each element of @pRegions@, @imageExtent.depth@ /must/ be a
--     multiple of the compressed texel block depth or (@imageExtent.depth@
--     + @imageOffset.z@) /must/ equal the depth of the specified
--     @imageSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-aspectMask-00211# For each element of
--     @pRegions@, @imageSubresource.aspectMask@ /must/ specify aspects
--     present in @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-aspectMask-01560# If @srcImage@ has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     then for each element of @pRegions@, @imageSubresource.aspectMask@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT',
--     or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--     (with
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'
--     valid only for image formats with three planes)
--
-- -   #VUID-vkCmdCopyImageToBuffer-baseArrayLayer-00213# If @srcImage@ is
--     of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', for each
--     element of @pRegions@, @imageSubresource.baseArrayLayer@ /must/ be
--     @0@ and @imageSubresource.layerCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyImageToBuffer-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImage-parameter# @srcImage@ /must/
--     be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImageLayout-parameter#
--     @srcImageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-vkCmdCopyImageToBuffer-dstBuffer-parameter# @dstBuffer@ /must/
--     be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdCopyImageToBuffer-pRegions-parameter# @pRegions@ /must/
--     be a valid pointer to an array of @regionCount@ valid
--     'BufferImageCopy' structures
--
-- -   #VUID-vkCmdCopyImageToBuffer-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyImageToBuffer-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdCopyImageToBuffer-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyImageToBuffer-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- -   #VUID-vkCmdCopyImageToBuffer-commonparent# Each of @commandBuffer@,
--     @dstBuffer@, and @srcImage@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'BufferImageCopy',
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
cmdCopyImageToBuffer :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer into which the command will be
                        -- recorded.
                        CommandBuffer
                     -> -- | @srcImage@ is the source image.
                        ("srcImage" ::: Image)
                     -> -- | @srcImageLayout@ is the layout of the source image subresources for the
                        -- copy.
                        ("srcImageLayout" ::: ImageLayout)
                     -> -- | @dstBuffer@ is the destination buffer.
                        ("dstBuffer" ::: Buffer)
                     -> -- | @pRegions@ is a pointer to an array of 'BufferImageCopy' structures
                        -- specifying the regions to copy.
                        ("regions" ::: Vector BufferImageCopy)
                     -> io ()
cmdCopyImageToBuffer commandBuffer srcImage srcImageLayout dstBuffer regions = liftIO . evalContT $ do
  let vkCmdCopyImageToBufferPtr = pVkCmdCopyImageToBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyImageToBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyImageToBuffer is null" Nothing Nothing
  let vkCmdCopyImageToBuffer' = mkVkCmdCopyImageToBuffer vkCmdCopyImageToBufferPtr
  pPRegions <- ContT $ allocaBytesAligned @BufferImageCopy ((Data.Vector.length (regions)) * 56) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions `plusPtr` (56 * (i)) :: Ptr BufferImageCopy) (e)) (regions)
  lift $ traceAroundEvent "vkCmdCopyImageToBuffer" (vkCmdCopyImageToBuffer' (commandBufferHandle (commandBuffer)) (srcImage) (srcImageLayout) (dstBuffer) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdUpdateBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> Ptr () -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> Ptr () -> IO ()

-- | vkCmdUpdateBuffer - Update a buffer’s contents from host memory
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
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
-- /must/ be specified in @usage@ of
-- 'Vulkan.Core10.Buffer.BufferCreateInfo' in order for the buffer to be
-- compatible with 'cmdUpdateBuffer'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdUpdateBuffer-dstOffset-00032# @dstOffset@ /must/ be less
--     than the size of @dstBuffer@
--
-- -   #VUID-vkCmdUpdateBuffer-dataSize-00033# @dataSize@ /must/ be less
--     than or equal to the size of @dstBuffer@ minus @dstOffset@
--
-- -   #VUID-vkCmdUpdateBuffer-dstBuffer-00034# @dstBuffer@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdUpdateBuffer-dstBuffer-00035# If @dstBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdUpdateBuffer-dstOffset-00036# @dstOffset@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-vkCmdUpdateBuffer-dataSize-00037# @dataSize@ /must/ be less
--     than or equal to @65536@
--
-- -   #VUID-vkCmdUpdateBuffer-dataSize-00038# @dataSize@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-vkCmdUpdateBuffer-commandBuffer-01813# If @commandBuffer@ is
--     an unprotected command buffer, then @dstBuffer@ /must/ not be a
--     protected buffer
--
-- -   #VUID-vkCmdUpdateBuffer-commandBuffer-01814# If @commandBuffer@ is a
--     protected command buffer, then @dstBuffer@ /must/ not be an
--     unprotected buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdUpdateBuffer-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdUpdateBuffer-dstBuffer-parameter# @dstBuffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdUpdateBuffer-pData-parameter# @pData@ /must/ be a valid
--     pointer to an array of @dataSize@ bytes
--
-- -   #VUID-vkCmdUpdateBuffer-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdUpdateBuffer-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdUpdateBuffer-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdUpdateBuffer-dataSize-arraylength# @dataSize@ /must/ be
--     greater than @0@
--
-- -   #VUID-vkCmdUpdateBuffer-commonparent# Both of @commandBuffer@, and
--     @dstBuffer@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdUpdateBuffer :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer into which the command will be
                   -- recorded.
                   CommandBuffer
                -> -- | @dstBuffer@ is a handle to the buffer to be updated.
                   ("dstBuffer" ::: Buffer)
                -> -- | @dstOffset@ is the byte offset into the buffer to start updating, and
                   -- /must/ be a multiple of 4.
                   ("dstOffset" ::: DeviceSize)
                -> -- | @dataSize@ is the number of bytes to update, and /must/ be a multiple of
                   -- 4.
                   ("dataSize" ::: DeviceSize)
                -> -- | @pData@ is a pointer to the source data for the buffer update, and
                   -- /must/ be at least @dataSize@ bytes in size.
                   ("data" ::: Ptr ())
                -> io ()
cmdUpdateBuffer commandBuffer dstBuffer dstOffset dataSize data' = liftIO $ do
  let vkCmdUpdateBufferPtr = pVkCmdUpdateBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdUpdateBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdUpdateBuffer is null" Nothing Nothing
  let vkCmdUpdateBuffer' = mkVkCmdUpdateBuffer vkCmdUpdateBufferPtr
  traceAroundEvent "vkCmdUpdateBuffer" (vkCmdUpdateBuffer' (commandBufferHandle (commandBuffer)) (dstBuffer) (dstOffset) (dataSize) (data'))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdFillBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> Word32 -> IO ()

-- | vkCmdFillBuffer - Fill a region of a buffer with a fixed value
--
-- = Description
--
-- 'cmdFillBuffer' is treated as “transfer” operation for the purposes of
-- synchronization barriers. The
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
-- /must/ be specified in @usage@ of
-- 'Vulkan.Core10.Buffer.BufferCreateInfo' in order for the buffer to be
-- compatible with 'cmdFillBuffer'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdFillBuffer-dstOffset-00024# @dstOffset@ /must/ be less
--     than the size of @dstBuffer@
--
-- -   #VUID-vkCmdFillBuffer-dstOffset-00025# @dstOffset@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-vkCmdFillBuffer-size-00026# If @size@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be greater
--     than @0@
--
-- -   #VUID-vkCmdFillBuffer-size-00027# If @size@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be less than
--     or equal to the size of @dstBuffer@ minus @dstOffset@
--
-- -   #VUID-vkCmdFillBuffer-size-00028# If @size@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be a multiple
--     of @4@
--
-- -   #VUID-vkCmdFillBuffer-dstBuffer-00029# @dstBuffer@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdFillBuffer-dstBuffer-00031# If @dstBuffer@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdFillBuffer-commandBuffer-01811# If @commandBuffer@ is an
--     unprotected command buffer, then @dstBuffer@ /must/ not be a
--     protected buffer
--
-- -   #VUID-vkCmdFillBuffer-commandBuffer-01812# If @commandBuffer@ is a
--     protected command buffer, then @dstBuffer@ /must/ not be an
--     unprotected buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdFillBuffer-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdFillBuffer-dstBuffer-parameter# @dstBuffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdFillBuffer-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdFillBuffer-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics or compute
--     operations
--
-- -   #VUID-vkCmdFillBuffer-renderpass# This command /must/ only be called
--     outside of a render pass instance
--
-- -   #VUID-vkCmdFillBuffer-commonparent# Both of @commandBuffer@, and
--     @dstBuffer@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Transfer                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdFillBuffer :: forall io
               . (MonadIO io)
              => -- | @commandBuffer@ is the command buffer into which the command will be
                 -- recorded.
                 CommandBuffer
              -> -- | @dstBuffer@ is the buffer to be filled.
                 ("dstBuffer" ::: Buffer)
              -> -- | @dstOffset@ is the byte offset into the buffer at which to start
                 -- filling, and /must/ be a multiple of 4.
                 ("dstOffset" ::: DeviceSize)
              -> -- | @size@ is the number of bytes to fill, and /must/ be either a multiple
                 -- of 4, or 'Vulkan.Core10.APIConstants.WHOLE_SIZE' to fill the range from
                 -- @offset@ to the end of the buffer. If
                 -- 'Vulkan.Core10.APIConstants.WHOLE_SIZE' is used and the remaining size
                 -- of the buffer is not a multiple of 4, then the nearest smaller multiple
                 -- is used.
                 DeviceSize
              -> -- | @data@ is the 4-byte word written repeatedly to the buffer to fill
                 -- @size@ bytes of data. The data word is written to memory according to
                 -- the host endianness.
                 ("data" ::: Word32)
              -> io ()
cmdFillBuffer commandBuffer dstBuffer dstOffset size data' = liftIO $ do
  let vkCmdFillBufferPtr = pVkCmdFillBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdFillBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdFillBuffer is null" Nothing Nothing
  let vkCmdFillBuffer' = mkVkCmdFillBuffer vkCmdFillBufferPtr
  traceAroundEvent "vkCmdFillBuffer" (vkCmdFillBuffer' (commandBufferHandle (commandBuffer)) (dstBuffer) (dstOffset) (size) (data'))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdClearColorImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Ptr ClearColorValue -> Word32 -> Ptr ImageSubresourceRange -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Ptr ClearColorValue -> Word32 -> Ptr ImageSubresourceRange -> IO ()

-- | vkCmdClearColorImage - Clear regions of a color image
--
-- = Description
--
-- Each specified range in @pRanges@ is cleared to the value specified by
-- @pColor@.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdClearColorImage-image-01993# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @image@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
--
-- -   #VUID-vkCmdClearColorImage-image-00002# @image@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdClearColorImage-image-01545# @image@ /must/ not use a
--     format listed in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion>
--
-- -   #VUID-vkCmdClearColorImage-image-00003# If @image@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdClearColorImage-imageLayout-00004# @imageLayout@ /must/
--     specify the layout of the image subresource ranges of @image@
--     specified in @pRanges@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-vkCmdClearColorImage-imageLayout-01394# @imageLayout@ /must/
--     be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--
-- -   #VUID-vkCmdClearColorImage-aspectMask-02498# The
--     'Vulkan.Core10.ImageView.ImageSubresourceRange'::@aspectMask@
--     members of the elements of the @pRanges@ array /must/ each only
--     include
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-vkCmdClearColorImage-baseMipLevel-01470# The
--     'Vulkan.Core10.ImageView.ImageSubresourceRange'::@baseMipLevel@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-vkCmdClearColorImage-pRanges-01692# For each
--     'Vulkan.Core10.ImageView.ImageSubresourceRange' element of
--     @pRanges@, if the @levelCount@ member is not
--     'Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS', then
--     @baseMipLevel@ + @levelCount@ /must/ be less than the @mipLevels@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was
--     created
--
-- -   #VUID-vkCmdClearColorImage-baseArrayLayer-01472# The
--     'Vulkan.Core10.ImageView.ImageSubresourceRange'::@baseArrayLayer@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @arrayLayers@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-vkCmdClearColorImage-pRanges-01693# For each
--     'Vulkan.Core10.ImageView.ImageSubresourceRange' element of
--     @pRanges@, if the @layerCount@ member is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', then
--     @baseArrayLayer@ + @layerCount@ /must/ be less than the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @image@ was created
--
-- -   #VUID-vkCmdClearColorImage-image-00007# @image@ /must/ not have a
--     compressed or depth\/stencil format
--
-- -   #VUID-vkCmdClearColorImage-commandBuffer-01805# If @commandBuffer@
--     is an unprotected command buffer, then @image@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdClearColorImage-commandBuffer-01806# If @commandBuffer@
--     is a protected command buffer, then @image@ /must/ not be an
--     unprotected image
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdClearColorImage-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdClearColorImage-image-parameter# @image@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkCmdClearColorImage-imageLayout-parameter# @imageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-vkCmdClearColorImage-pColor-parameter# @pColor@ /must/ be a
--     valid pointer to a valid 'ClearColorValue' union
--
-- -   #VUID-vkCmdClearColorImage-pRanges-parameter# @pRanges@ /must/ be a
--     valid pointer to an array of @rangeCount@ valid
--     'Vulkan.Core10.ImageView.ImageSubresourceRange' structures
--
-- -   #VUID-vkCmdClearColorImage-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdClearColorImage-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdClearColorImage-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdClearColorImage-rangeCount-arraylength# @rangeCount@
--     /must/ be greater than @0@
--
-- -   #VUID-vkCmdClearColorImage-commonparent# Both of @commandBuffer@,
--     and @image@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'ClearColorValue', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.ImageView.ImageSubresourceRange'
cmdClearColorImage :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer into which the command will be
                      -- recorded.
                      CommandBuffer
                   -> -- | @image@ is the image to be cleared.
                      Image
                   -> -- | @imageLayout@ specifies the current layout of the image subresource
                      -- ranges to be cleared, and /must/ be
                      -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
                      -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' or
                      -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'.
                      ImageLayout
                   -> -- | @pColor@ is a pointer to a 'ClearColorValue' structure containing the
                      -- values that the image subresource ranges will be cleared to (see
                      -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears-values>
                      -- below).
                      ClearColorValue
                   -> -- | @pRanges@ is a pointer to an array of
                      -- 'Vulkan.Core10.ImageView.ImageSubresourceRange' structures describing a
                      -- range of mipmap levels, array layers, and aspects to be cleared, as
                      -- described in
                      -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views Image Views>.
                      ("ranges" ::: Vector ImageSubresourceRange)
                   -> io ()
cmdClearColorImage commandBuffer image imageLayout color ranges = liftIO . evalContT $ do
  let vkCmdClearColorImagePtr = pVkCmdClearColorImage (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdClearColorImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdClearColorImage is null" Nothing Nothing
  let vkCmdClearColorImage' = mkVkCmdClearColorImage vkCmdClearColorImagePtr
  pColor <- ContT $ withCStruct (color)
  pPRanges <- ContT $ allocaBytesAligned @ImageSubresourceRange ((Data.Vector.length (ranges)) * 20) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRanges `plusPtr` (20 * (i)) :: Ptr ImageSubresourceRange) (e)) (ranges)
  lift $ traceAroundEvent "vkCmdClearColorImage" (vkCmdClearColorImage' (commandBufferHandle (commandBuffer)) (image) (imageLayout) pColor ((fromIntegral (Data.Vector.length $ (ranges)) :: Word32)) (pPRanges))
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
-- == Valid Usage
--
-- -   #VUID-vkCmdClearDepthStencilImage-image-01994# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @image@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
--
-- -   #VUID-vkCmdClearDepthStencilImage-pRanges-02658# If the @aspect@
--     member of any element of @pRanges@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     and @image@ was created with
--     <VkImageStencilUsageCreateInfo.html separate stencil usage>,
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     /must/ have been included in the
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     used to create @image@
--
-- -   #VUID-vkCmdClearDepthStencilImage-pRanges-02659# If the @aspect@
--     member of any element of @pRanges@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     and @image@ was not created with
--     <VkImageStencilUsageCreateInfo.html separate stencil usage>,
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     /must/ have been included in the
--     'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ used to create
--     @image@
--
-- -   #VUID-vkCmdClearDepthStencilImage-pRanges-02660# If the @aspect@
--     member of any element of @pRanges@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     /must/ have been included in the
--     'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ used to create
--     @image@
--
-- -   #VUID-vkCmdClearDepthStencilImage-image-00010# If @image@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdClearDepthStencilImage-imageLayout-00011# @imageLayout@
--     /must/ specify the layout of the image subresource ranges of @image@
--     specified in @pRanges@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-vkCmdClearDepthStencilImage-imageLayout-00012# @imageLayout@
--     /must/ be either of
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-vkCmdClearDepthStencilImage-aspectMask-02824# The
--     'Vulkan.Core10.ImageView.ImageSubresourceRange'::@aspectMask@ member
--     of each element of the @pRanges@ array /must/ not include bits other
--     than
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkCmdClearDepthStencilImage-image-02825# If the @image@’s
--     format does not have a stencil component, then the
--     'Vulkan.Core10.ImageView.ImageSubresourceRange'::@aspectMask@ member
--     of each element of the @pRanges@ array /must/ not include the
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--     bit
--
-- -   #VUID-vkCmdClearDepthStencilImage-image-02826# If the @image@’s
--     format does not have a depth component, then the
--     'Vulkan.Core10.ImageView.ImageSubresourceRange'::@aspectMask@ member
--     of each element of the @pRanges@ array /must/ not include the
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' bit
--
-- -   #VUID-vkCmdClearDepthStencilImage-baseMipLevel-01474# The
--     'Vulkan.Core10.ImageView.ImageSubresourceRange'::@baseMipLevel@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-vkCmdClearDepthStencilImage-pRanges-01694# For each
--     'Vulkan.Core10.ImageView.ImageSubresourceRange' element of
--     @pRanges@, if the @levelCount@ member is not
--     'Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS', then
--     @baseMipLevel@ + @levelCount@ /must/ be less than the @mipLevels@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was
--     created
--
-- -   #VUID-vkCmdClearDepthStencilImage-baseArrayLayer-01476# The
--     'Vulkan.Core10.ImageView.ImageSubresourceRange'::@baseArrayLayer@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @arrayLayers@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-vkCmdClearDepthStencilImage-pRanges-01695# For each
--     'Vulkan.Core10.ImageView.ImageSubresourceRange' element of
--     @pRanges@, if the @layerCount@ member is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS', then
--     @baseArrayLayer@ + @layerCount@ /must/ be less than the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @image@ was created
--
-- -   #VUID-vkCmdClearDepthStencilImage-image-00014# @image@ /must/ have a
--     depth\/stencil format
--
-- -   #VUID-vkCmdClearDepthStencilImage-commandBuffer-01807# If
--     @commandBuffer@ is an unprotected command buffer, then @image@
--     /must/ not be a protected image
--
-- -   #VUID-vkCmdClearDepthStencilImage-commandBuffer-01808# If
--     @commandBuffer@ is a protected command buffer, then @image@ /must/
--     not be an unprotected image
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdClearDepthStencilImage-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdClearDepthStencilImage-image-parameter# @image@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkCmdClearDepthStencilImage-imageLayout-parameter#
--     @imageLayout@ /must/ be a valid
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-vkCmdClearDepthStencilImage-pDepthStencil-parameter#
--     @pDepthStencil@ /must/ be a valid pointer to a valid
--     'ClearDepthStencilValue' structure
--
-- -   #VUID-vkCmdClearDepthStencilImage-pRanges-parameter# @pRanges@
--     /must/ be a valid pointer to an array of @rangeCount@ valid
--     'Vulkan.Core10.ImageView.ImageSubresourceRange' structures
--
-- -   #VUID-vkCmdClearDepthStencilImage-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdClearDepthStencilImage-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdClearDepthStencilImage-renderpass# This command /must/
--     only be called outside of a render pass instance
--
-- -   #VUID-vkCmdClearDepthStencilImage-rangeCount-arraylength#
--     @rangeCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdClearDepthStencilImage-commonparent# Both of
--     @commandBuffer@, and @image@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'ClearDepthStencilValue', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.ImageView.ImageSubresourceRange'
cmdClearDepthStencilImage :: forall io
                           . (MonadIO io)
                          => -- | @commandBuffer@ is the command buffer into which the command will be
                             -- recorded.
                             CommandBuffer
                          -> -- | @image@ is the image to be cleared.
                             Image
                          -> -- | @imageLayout@ specifies the current layout of the image subresource
                             -- ranges to be cleared, and /must/ be
                             -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' or
                             -- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'.
                             ImageLayout
                          -> -- | @pDepthStencil@ is a pointer to a 'ClearDepthStencilValue' structure
                             -- containing the values that the depth and stencil image subresource
                             -- ranges will be cleared to (see
                             -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears-values>
                             -- below).
                             ClearDepthStencilValue
                          -> -- | @pRanges@ is a pointer to an array of
                             -- 'Vulkan.Core10.ImageView.ImageSubresourceRange' structures describing a
                             -- range of mipmap levels, array layers, and aspects to be cleared, as
                             -- described in
                             -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views Image Views>.
                             ("ranges" ::: Vector ImageSubresourceRange)
                          -> io ()
cmdClearDepthStencilImage commandBuffer image imageLayout depthStencil ranges = liftIO . evalContT $ do
  let vkCmdClearDepthStencilImagePtr = pVkCmdClearDepthStencilImage (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdClearDepthStencilImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdClearDepthStencilImage is null" Nothing Nothing
  let vkCmdClearDepthStencilImage' = mkVkCmdClearDepthStencilImage vkCmdClearDepthStencilImagePtr
  pDepthStencil <- ContT $ withCStruct (depthStencil)
  pPRanges <- ContT $ allocaBytesAligned @ImageSubresourceRange ((Data.Vector.length (ranges)) * 20) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRanges `plusPtr` (20 * (i)) :: Ptr ImageSubresourceRange) (e)) (ranges)
  lift $ traceAroundEvent "vkCmdClearDepthStencilImage" (vkCmdClearDepthStencilImage' (commandBufferHandle (commandBuffer)) (image) (imageLayout) pDepthStencil ((fromIntegral (Data.Vector.length $ (ranges)) :: Word32)) (pPRanges))
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
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
-- stage. Clears to depth\/stencil attachments are executed as
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth depth writes>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil writes>
-- by the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT'
-- and
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT'
-- stages.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdClearAttachments-aspectMask-02501# If the @aspectMask@
--     member of any element of @pAttachments@ contains
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
--     then the @colorAttachment@ member of that element /must/ either
--     refer to a color attachment which is
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', or /must/ be a valid
--     color attachment
--
-- -   #VUID-vkCmdClearAttachments-aspectMask-02502# If the @aspectMask@
--     member of any element of @pAttachments@ contains
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT',
--     then the current subpass\' depth\/stencil attachment /must/ either
--     be 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', or /must/ have a
--     depth component
--
-- -   #VUID-vkCmdClearAttachments-aspectMask-02503# If the @aspectMask@
--     member of any element of @pAttachments@ contains
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     then the current subpass\' depth\/stencil attachment /must/ either
--     be 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', or /must/ have a
--     stencil component
--
-- -   #VUID-vkCmdClearAttachments-rect-02682# The @rect@ member of each
--     element of @pRects@ /must/ have an @extent.width@ greater than @0@
--
-- -   #VUID-vkCmdClearAttachments-rect-02683# The @rect@ member of each
--     element of @pRects@ /must/ have an @extent.height@ greater than @0@
--
-- -   #VUID-vkCmdClearAttachments-pRects-00016# The rectangular region
--     specified by each element of @pRects@ /must/ be contained within the
--     render area of the current render pass instance
--
-- -   #VUID-vkCmdClearAttachments-pRects-00017# The layers specified by
--     each element of @pRects@ /must/ be contained within every attachment
--     that @pAttachments@ refers to
--
-- -   #VUID-vkCmdClearAttachments-layerCount-01934# The @layerCount@
--     member of each element of @pRects@ /must/ not be @0@
--
-- -   #VUID-vkCmdClearAttachments-commandBuffer-02504# If @commandBuffer@
--     is an unprotected command buffer, then each attachment to be cleared
--     /must/ not be a protected image
--
-- -   #VUID-vkCmdClearAttachments-commandBuffer-02505# If @commandBuffer@
--     is a protected command buffer, then each attachment to be cleared
--     /must/ not be an unprotected image
--
-- -   #VUID-vkCmdClearAttachments-baseArrayLayer-00018# If the render pass
--     instance this is recorded in uses multiview, then @baseArrayLayer@
--     /must/ be zero and @layerCount@ /must/ be one
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdClearAttachments-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdClearAttachments-pAttachments-parameter# @pAttachments@
--     /must/ be a valid pointer to an array of @attachmentCount@ valid
--     'ClearAttachment' structures
--
-- -   #VUID-vkCmdClearAttachments-pRects-parameter# @pRects@ /must/ be a
--     valid pointer to an array of @rectCount@ 'ClearRect' structures
--
-- -   #VUID-vkCmdClearAttachments-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdClearAttachments-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdClearAttachments-renderpass# This command /must/ only be
--     called inside of a render pass instance
--
-- -   #VUID-vkCmdClearAttachments-attachmentCount-arraylength#
--     @attachmentCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdClearAttachments-rectCount-arraylength# @rectCount@
--     /must/ be greater than @0@
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'ClearAttachment', 'ClearRect', 'Vulkan.Core10.Handles.CommandBuffer'
cmdClearAttachments :: forall io
                     . (MonadIO io)
                    => -- | @commandBuffer@ is the command buffer into which the command will be
                       -- recorded.
                       CommandBuffer
                    -> -- | @pAttachments@ is a pointer to an array of 'ClearAttachment' structures
                       -- defining the attachments to clear and the clear values to use. If any
                       -- attachment to be cleared in the current subpass is
                       -- 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED', then the clear has no
                       -- effect on that attachment.
                       ("attachments" ::: Vector ClearAttachment)
                    -> -- | @pRects@ is a pointer to an array of 'ClearRect' structures defining
                       -- regions within each selected attachment to clear.
                       ("rects" ::: Vector ClearRect)
                    -> io ()
cmdClearAttachments commandBuffer attachments rects = liftIO . evalContT $ do
  let vkCmdClearAttachmentsPtr = pVkCmdClearAttachments (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdClearAttachmentsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdClearAttachments is null" Nothing Nothing
  let vkCmdClearAttachments' = mkVkCmdClearAttachments vkCmdClearAttachmentsPtr
  pPAttachments <- ContT $ allocaBytesAligned @ClearAttachment ((Data.Vector.length (attachments)) * 24) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachments `plusPtr` (24 * (i)) :: Ptr ClearAttachment) (e) . ($ ())) (attachments)
  pPRects <- ContT $ allocaBytesAligned @ClearRect ((Data.Vector.length (rects)) * 24) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRects `plusPtr` (24 * (i)) :: Ptr ClearRect) (e)) (rects)
  lift $ traceAroundEvent "vkCmdClearAttachments" (vkCmdClearAttachments' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (attachments)) :: Word32)) (pPAttachments) ((fromIntegral (Data.Vector.length $ (rects)) :: Word32)) (pPRects))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResolveImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageResolve -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageResolve -> IO ()

-- | vkCmdResolveImage - Resolve regions of an image
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
-- @width@, @height@ and @depth@. Each element of @pRegions@ /must/ be a
-- region that is contained within its corresponding image.
--
-- Resolves are done layer by layer starting with @baseArrayLayer@ member
-- of @srcSubresource@ for the source and @dstSubresource@ for the
-- destination. @layerCount@ layers are resolved to the destination image.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdResolveImage-commandBuffer-01837# If @commandBuffer@ is
--     an unprotected command buffer, then @srcImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdResolveImage-commandBuffer-01838# If @commandBuffer@ is
--     an unprotected command buffer, then @dstImage@ /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdResolveImage-commandBuffer-01839# If @commandBuffer@ is a
--     protected command buffer, then @dstImage@ /must/ not be an
--     unprotected image
--
-- -   #VUID-vkCmdResolveImage-pRegions-00255# The union of all source
--     regions, and the union of all destination regions, specified by the
--     elements of @pRegions@, /must/ not overlap in memory
--
-- -   #VUID-vkCmdResolveImage-srcImage-00256# If @srcImage@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdResolveImage-srcImage-00257# @srcImage@ /must/ have a
--     sample count equal to any valid sample count value other than
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-vkCmdResolveImage-dstImage-00258# If @dstImage@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdResolveImage-dstImage-00259# @dstImage@ /must/ have a
--     sample count equal to
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-vkCmdResolveImage-srcImageLayout-00260# @srcImageLayout@
--     /must/ specify the layout of the image subresources of @srcImage@
--     specified in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-vkCmdResolveImage-srcImageLayout-01400# @srcImageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-vkCmdResolveImage-dstImageLayout-00262# @dstImageLayout@
--     /must/ specify the layout of the image subresources of @dstImage@
--     specified in @pRegions@ at the time this command is executed on a
--     'Vulkan.Core10.Handles.Device'
--
-- -   #VUID-vkCmdResolveImage-dstImageLayout-01401# @dstImageLayout@
--     /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHARED_PRESENT_KHR',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- -   #VUID-vkCmdResolveImage-dstImage-02003# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-vkCmdResolveImage-srcImage-01386# @srcImage@ and @dstImage@
--     /must/ have been created with the same image format
--
-- -   #VUID-vkCmdResolveImage-srcSubresource-01709# The
--     @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @srcImage@ was created
--
-- -   #VUID-vkCmdResolveImage-dstSubresource-01710# The
--     @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @dstImage@ was created
--
-- -   #VUID-vkCmdResolveImage-srcSubresource-01711# The
--     @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @srcImage@ was created
--
-- -   #VUID-vkCmdResolveImage-dstSubresource-01712# The
--     @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @dstImage@ was created
--
-- -   #VUID-vkCmdResolveImage-dstImage-02546# @dstImage@ and @srcImage@
--     /must/ not have been created with @flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'
--
-- -   #VUID-vkCmdResolveImage-srcImage-04446# If either @srcImage@ or
--     @dstImage@ are of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @srcSubresource.baseArrayLayer@ /must/ be @0@ and
--     @srcSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-vkCmdResolveImage-srcImage-04447# If either @srcImage@ or
--     @dstImage@ are of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @dstSubresource.baseArrayLayer@ /must/ be @0@ and
--     @dstSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-vkCmdResolveImage-srcOffset-00269# For each element of
--     @pRegions@, @srcOffset.x@ and (@extent.width@ + @srcOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdResolveImage-srcOffset-00270# For each element of
--     @pRegions@, @srcOffset.y@ and (@extent.height@ + @srcOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdResolveImage-srcImage-00271# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @srcOffset.y@ /must/ be @0@ and @extent.height@
--     /must/ be @1@
--
-- -   #VUID-vkCmdResolveImage-srcOffset-00272# For each element of
--     @pRegions@, @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdResolveImage-srcImage-00273# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- -   #VUID-vkCmdResolveImage-dstOffset-00274# For each element of
--     @pRegions@, @dstOffset.x@ and (@extent.width@ + @dstOffset.x@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the width of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdResolveImage-dstOffset-00275# For each element of
--     @pRegions@, @dstOffset.y@ and (@extent.height@ + @dstOffset.y@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the height of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdResolveImage-dstImage-00276# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @dstOffset.y@ /must/ be @0@ and @extent.height@
--     /must/ be @1@
--
-- -   #VUID-vkCmdResolveImage-dstOffset-00277# For each element of
--     @pRegions@, @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdResolveImage-dstImage-00278# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/
--     be @1@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdResolveImage-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdResolveImage-srcImage-parameter# @srcImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkCmdResolveImage-srcImageLayout-parameter# @srcImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-vkCmdResolveImage-dstImage-parameter# @dstImage@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkCmdResolveImage-dstImageLayout-parameter# @dstImageLayout@
--     /must/ be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout'
--     value
--
-- -   #VUID-vkCmdResolveImage-pRegions-parameter# @pRegions@ /must/ be a
--     valid pointer to an array of @regionCount@ valid 'ImageResolve'
--     structures
--
-- -   #VUID-vkCmdResolveImage-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdResolveImage-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdResolveImage-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdResolveImage-regionCount-arraylength# @regionCount@
--     /must/ be greater than @0@
--
-- -   #VUID-vkCmdResolveImage-commonparent# Each of @commandBuffer@,
--     @dstImage@, and @srcImage@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout', 'ImageResolve'
cmdResolveImage :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer into which the command will be
                   -- recorded.
                   CommandBuffer
                -> -- | @srcImage@ is the source image.
                   ("srcImage" ::: Image)
                -> -- | @srcImageLayout@ is the layout of the source image subresources for the
                   -- resolve.
                   ("srcImageLayout" ::: ImageLayout)
                -> -- | @dstImage@ is the destination image.
                   ("dstImage" ::: Image)
                -> -- | @dstImageLayout@ is the layout of the destination image subresources for
                   -- the resolve.
                   ("dstImageLayout" ::: ImageLayout)
                -> -- | @pRegions@ is a pointer to an array of 'ImageResolve' structures
                   -- specifying the regions to resolve.
                   ("regions" ::: Vector ImageResolve)
                -> io ()
cmdResolveImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout regions = liftIO . evalContT $ do
  let vkCmdResolveImagePtr = pVkCmdResolveImage (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdResolveImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdResolveImage is null" Nothing Nothing
  let vkCmdResolveImage' = mkVkCmdResolveImage vkCmdResolveImagePtr
  pPRegions <- ContT $ allocaBytesAligned @ImageResolve ((Data.Vector.length (regions)) * 68) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions `plusPtr` (68 * (i)) :: Ptr ImageResolve) (e)) (regions)
  lift $ traceAroundEvent "vkCmdResolveImage" (vkCmdResolveImage' (commandBufferHandle (commandBuffer)) (srcImage) (srcImageLayout) (dstImage) (dstImageLayout) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetEvent
  :: FunPtr (Ptr CommandBuffer_T -> Event -> PipelineStageFlags -> IO ()) -> Ptr CommandBuffer_T -> Event -> PipelineStageFlags -> IO ()

-- | vkCmdSetEvent - Set an event object to signaled state
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
-- If @event@ is already in the signaled state when 'cmdSetEvent' is
-- executed on the device, then 'cmdSetEvent' has no effect, no event
-- signal operation occurs, and no execution dependency is generated.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetEvent-stageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04097# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-vkCmdSetEvent-stageMask-4098# Any pipeline stage included in
--     @stageMask@ /must/ be supported by the capabilities of the queue
--     family specified by the @queueFamilyIndex@ member of the
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that was
--     used to create the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   #VUID-vkCmdSetEvent-stageMask-01149# @stageMask@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'
--
-- -   #VUID-vkCmdSetEvent-commandBuffer-01152# @commandBuffer@’s current
--     device mask /must/ include exactly one physical device
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetEvent-commandBuffer-parameter# @commandBuffer@ /must/
--     be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetEvent-event-parameter# @event@ /must/ be a valid
--     'Vulkan.Core10.Handles.Event' handle
--
-- -   #VUID-vkCmdSetEvent-stageMask-parameter# @stageMask@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-vkCmdSetEvent-stageMask-requiredbitmask# @stageMask@ /must/
--     not be @0@
--
-- -   #VUID-vkCmdSetEvent-commandBuffer-recording# @commandBuffer@ /must/
--     be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetEvent-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdSetEvent-renderpass# This command /must/ only be called
--     outside of a render pass instance
--
-- -   #VUID-vkCmdSetEvent-commonparent# Both of @commandBuffer@, and
--     @event@ /must/ have been created, allocated, or retrieved from the
--     same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Event',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags'
cmdSetEvent :: forall io
             . (MonadIO io)
            => -- | @commandBuffer@ is the command buffer into which the command is
               -- recorded.
               CommandBuffer
            -> -- | @event@ is the event that will be signaled.
               Event
            -> -- | @stageMask@ specifies the
               -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>
               -- used to determine the first
               -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>.
               ("stageMask" ::: PipelineStageFlags)
            -> io ()
cmdSetEvent commandBuffer event stageMask = liftIO $ do
  let vkCmdSetEventPtr = pVkCmdSetEvent (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetEvent is null" Nothing Nothing
  let vkCmdSetEvent' = mkVkCmdSetEvent vkCmdSetEventPtr
  traceAroundEvent "vkCmdSetEvent" (vkCmdSetEvent' (commandBufferHandle (commandBuffer)) (event) (stageMask))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResetEvent
  :: FunPtr (Ptr CommandBuffer_T -> Event -> PipelineStageFlags -> IO ()) -> Ptr CommandBuffer_T -> Event -> PipelineStageFlags -> IO ()

-- | vkCmdResetEvent - Reset an event object to non-signaled state
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
-- If @event@ is already in the unsignaled state when 'cmdResetEvent' is
-- executed on the device, then 'cmdResetEvent' has no effect, no event
-- unsignal operation occurs, and no execution dependency is generated.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdResetEvent-stageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04097# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-vkCmdResetEvent-stageMask-4098# Any pipeline stage included in
--     @stageMask@ /must/ be supported by the capabilities of the queue
--     family specified by the @queueFamilyIndex@ member of the
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that was
--     used to create the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   #VUID-vkCmdResetEvent-stageMask-01153# @stageMask@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'
--
-- -   #VUID-vkCmdResetEvent-event-01156# When this command executes,
--     @event@ /must/ not be waited on by a 'cmdWaitEvents' command that is
--     currently executing
--
-- -   #VUID-vkCmdResetEvent-commandBuffer-01157# @commandBuffer@’s current
--     device mask /must/ include exactly one physical device
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdResetEvent-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdResetEvent-event-parameter# @event@ /must/ be a valid
--     'Vulkan.Core10.Handles.Event' handle
--
-- -   #VUID-vkCmdResetEvent-stageMask-parameter# @stageMask@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-vkCmdResetEvent-stageMask-requiredbitmask# @stageMask@ /must/
--     not be @0@
--
-- -   #VUID-vkCmdResetEvent-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdResetEvent-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdResetEvent-renderpass# This command /must/ only be called
--     outside of a render pass instance
--
-- -   #VUID-vkCmdResetEvent-commonparent# Both of @commandBuffer@, and
--     @event@ /must/ have been created, allocated, or retrieved from the
--     same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Event',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags'
cmdResetEvent :: forall io
               . (MonadIO io)
              => -- | @commandBuffer@ is the command buffer into which the command is
                 -- recorded.
                 CommandBuffer
              -> -- | @event@ is the event that will be unsignaled.
                 Event
              -> -- | @stageMask@ is a bitmask of
                 -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                 -- specifying the
                 -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>
                 -- used to determine when the @event@ is unsignaled.
                 ("stageMask" ::: PipelineStageFlags)
              -> io ()
cmdResetEvent commandBuffer event stageMask = liftIO $ do
  let vkCmdResetEventPtr = pVkCmdResetEvent (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdResetEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdResetEvent is null" Nothing Nothing
  let vkCmdResetEvent' = mkVkCmdResetEvent vkCmdResetEventPtr
  traceAroundEvent "vkCmdResetEvent" (vkCmdResetEvent' (commandBufferHandle (commandBuffer)) (event) (stageMask))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWaitEventsUnsafe
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()

foreign import ccall
  "dynamic" mkVkCmdWaitEventsSafe
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()

-- | cmdWaitEvents with selectable safeness
cmdWaitEventsSafeOrUnsafe :: forall io
                           . (MonadIO io)
                          => (FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ())
                          -> -- | @commandBuffer@ is the command buffer into which the command is
                             -- recorded.
                             CommandBuffer
                          -> -- | @pEvents@ is a pointer to an array of event object handles to wait on.
                             ("events" ::: Vector Event)
                          -> -- | @srcStageMask@ is a bitmask of
                             -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                             -- specifying the
                             -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>.
                             ("srcStageMask" ::: PipelineStageFlags)
                          -> -- | @dstStageMask@ is a bitmask of
                             -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                             -- specifying the
                             -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages destination stage mask>.
                             ("dstStageMask" ::: PipelineStageFlags)
                          -> -- | @pMemoryBarriers@ is a pointer to an array of
                             -- 'Vulkan.Core10.OtherTypes.MemoryBarrier' structures.
                             ("memoryBarriers" ::: Vector MemoryBarrier)
                          -> -- | @pBufferMemoryBarriers@ is a pointer to an array of
                             -- 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier' structures.
                             ("bufferMemoryBarriers" ::: Vector BufferMemoryBarrier)
                          -> -- | @pImageMemoryBarriers@ is a pointer to an array of
                             -- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier' structures.
                             ("imageMemoryBarriers" ::: Vector (SomeStruct ImageMemoryBarrier))
                          -> io ()
cmdWaitEventsSafeOrUnsafe mkVkCmdWaitEvents commandBuffer events srcStageMask dstStageMask memoryBarriers bufferMemoryBarriers imageMemoryBarriers = liftIO . evalContT $ do
  let vkCmdWaitEventsPtr = pVkCmdWaitEvents (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdWaitEventsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWaitEvents is null" Nothing Nothing
  let vkCmdWaitEvents' = mkVkCmdWaitEvents vkCmdWaitEventsPtr
  pPEvents <- ContT $ allocaBytesAligned @Event ((Data.Vector.length (events)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPEvents `plusPtr` (8 * (i)) :: Ptr Event) (e)) (events)
  pPMemoryBarriers <- ContT $ allocaBytesAligned @MemoryBarrier ((Data.Vector.length (memoryBarriers)) * 24) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMemoryBarriers `plusPtr` (24 * (i)) :: Ptr MemoryBarrier) (e)) (memoryBarriers)
  pPBufferMemoryBarriers <- ContT $ allocaBytesAligned @BufferMemoryBarrier ((Data.Vector.length (bufferMemoryBarriers)) * 56) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBufferMemoryBarriers `plusPtr` (56 * (i)) :: Ptr BufferMemoryBarrier) (e)) (bufferMemoryBarriers)
  pPImageMemoryBarriers <- ContT $ allocaBytesAligned @(ImageMemoryBarrier _) ((Data.Vector.length (imageMemoryBarriers)) * 72) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPImageMemoryBarriers `plusPtr` (72 * (i)) :: Ptr (ImageMemoryBarrier _))) (e) . ($ ())) (imageMemoryBarriers)
  lift $ traceAroundEvent "vkCmdWaitEvents" (vkCmdWaitEvents' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (events)) :: Word32)) (pPEvents) (srcStageMask) (dstStageMask) ((fromIntegral (Data.Vector.length $ (memoryBarriers)) :: Word32)) (pPMemoryBarriers) ((fromIntegral (Data.Vector.length $ (bufferMemoryBarriers)) :: Word32)) (pPBufferMemoryBarriers) ((fromIntegral (Data.Vector.length $ (imageMemoryBarriers)) :: Word32)) (forgetExtensions (pPImageMemoryBarriers)))
  pure $ ()

-- | vkCmdWaitEvents - Wait for one or more events and insert a set of memory
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
-- 'Vulkan.Core10.Event.setEvent' are only included in the first
-- synchronization scope if
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT' is
-- included in @srcStageMask@.
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
-- command and 'cmdSetEvent' or 'cmdWaitEvents' commands submitted after
-- it. Similarly, there is no ordering guarantee between sequential
-- 'cmdSetEvent' commands, leading to a race condition on the event status.
-- Waiting on an event in these cases without additional synchronization
-- can result in a data race. Another execution dependency (e.g. a pipeline
-- barrier or semaphore with
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ALL_COMMANDS_BIT')
-- is needed to prevent such race conditions.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04097# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-4098# Any pipeline stage included
--     in @srcStageMask@ /must/ be supported by the capabilities of the
--     queue family specified by the @queueFamilyIndex@ member of the
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that was
--     used to create the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04097# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-4098# Any pipeline stage included
--     in @dstStageMask@ /must/ be supported by the capabilities of the
--     queue family specified by the @queueFamilyIndex@ member of the
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that was
--     used to create the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   #VUID-vkCmdWaitEvents-srcAccessMask-02815# The @srcAccessMask@
--     member of each element of @pMemoryBarriers@ /must/ only include
--     access flags that are supported by one or more of the pipeline
--     stages in @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-vkCmdWaitEvents-dstAccessMask-02816# The @dstAccessMask@
--     member of each element of @pMemoryBarriers@ /must/ only include
--     access flags that are supported by one or more of the pipeline
--     stages in @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-vkCmdWaitEvents-pBufferMemoryBarriers-02817# For any element
--     of @pBufferMemoryBarriers@, if its @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ members are equal, or if its
--     @srcQueueFamilyIndex@ is the queue family index that was used to
--     create the command pool that @commandBuffer@ was allocated from,
--     then its @srcAccessMask@ member /must/ only contain access flags
--     that are supported by one or more of the pipeline stages in
--     @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-vkCmdWaitEvents-pBufferMemoryBarriers-02818# For any element
--     of @pBufferMemoryBarriers@, if its @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ members are equal, or if its
--     @dstQueueFamilyIndex@ is the queue family index that was used to
--     create the command pool that @commandBuffer@ was allocated from,
--     then its @dstAccessMask@ member /must/ only contain access flags
--     that are supported by one or more of the pipeline stages in
--     @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-vkCmdWaitEvents-pImageMemoryBarriers-02819# For any element of
--     @pImageMemoryBarriers@, if its @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ members are equal, or if its
--     @srcQueueFamilyIndex@ is the queue family index that was used to
--     create the command pool that @commandBuffer@ was allocated from,
--     then its @srcAccessMask@ member /must/ only contain access flags
--     that are supported by one or more of the pipeline stages in
--     @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-vkCmdWaitEvents-pImageMemoryBarriers-02820# For any element of
--     @pImageMemoryBarriers@, if its @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ members are equal, or if its
--     @dstQueueFamilyIndex@ is the queue family index that was used to
--     create the command pool that @commandBuffer@ was allocated from,
--     then its @dstAccessMask@ member /must/ only contain access flags
--     that are supported by one or more of the pipeline stages in
--     @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-01158# @srcStageMask@ /must/ be
--     the bitwise OR of the @stageMask@ parameter used in previous calls
--     to 'cmdSetEvent' with any of the members of @pEvents@ and
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'
--     if any of the members of @pEvents@ was set using
--     'Vulkan.Core10.Event.setEvent'
--
-- -   #VUID-vkCmdWaitEvents-pEvents-01163# If @pEvents@ includes one or
--     more events that will be signaled by 'Vulkan.Core10.Event.setEvent'
--     after @commandBuffer@ has been submitted to a queue, then
--     'cmdWaitEvents' /must/ not be called inside a render pass instance
--
-- -   #VUID-vkCmdWaitEvents-srcQueueFamilyIndex-02803# The
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ members of any
--     element of @pBufferMemoryBarriers@ or @pImageMemoryBarriers@ /must/
--     be equal
--
-- -   #VUID-vkCmdWaitEvents-commandBuffer-01167# @commandBuffer@’s current
--     device mask /must/ include exactly one physical device
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdWaitEvents-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdWaitEvents-pEvents-parameter# @pEvents@ /must/ be a valid
--     pointer to an array of @eventCount@ valid
--     'Vulkan.Core10.Handles.Event' handles
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-parameter# @srcStageMask@ /must/
--     be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-requiredbitmask# @srcStageMask@
--     /must/ not be @0@
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-parameter# @dstStageMask@ /must/
--     be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-requiredbitmask# @dstStageMask@
--     /must/ not be @0@
--
-- -   #VUID-vkCmdWaitEvents-pMemoryBarriers-parameter# If
--     @memoryBarrierCount@ is not @0@, @pMemoryBarriers@ /must/ be a valid
--     pointer to an array of @memoryBarrierCount@ valid
--     'Vulkan.Core10.OtherTypes.MemoryBarrier' structures
--
-- -   #VUID-vkCmdWaitEvents-pBufferMemoryBarriers-parameter# If
--     @bufferMemoryBarrierCount@ is not @0@, @pBufferMemoryBarriers@
--     /must/ be a valid pointer to an array of @bufferMemoryBarrierCount@
--     valid 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier' structures
--
-- -   #VUID-vkCmdWaitEvents-pImageMemoryBarriers-parameter# If
--     @imageMemoryBarrierCount@ is not @0@, @pImageMemoryBarriers@ /must/
--     be a valid pointer to an array of @imageMemoryBarrierCount@ valid
--     'Vulkan.Core10.OtherTypes.ImageMemoryBarrier' structures
--
-- -   #VUID-vkCmdWaitEvents-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdWaitEvents-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdWaitEvents-eventCount-arraylength# @eventCount@ /must/ be
--     greater than @0@
--
-- -   #VUID-vkCmdWaitEvents-commonparent# Both of @commandBuffer@, and the
--     elements of @pEvents@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Event',
-- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
-- 'Vulkan.Core10.OtherTypes.MemoryBarrier',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags'
cmdWaitEvents :: forall io
               . (MonadIO io)
              => -- | @commandBuffer@ is the command buffer into which the command is
                 -- recorded.
                 CommandBuffer
              -> -- | @pEvents@ is a pointer to an array of event object handles to wait on.
                 ("events" ::: Vector Event)
              -> -- | @srcStageMask@ is a bitmask of
                 -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                 -- specifying the
                 -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>.
                 ("srcStageMask" ::: PipelineStageFlags)
              -> -- | @dstStageMask@ is a bitmask of
                 -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                 -- specifying the
                 -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages destination stage mask>.
                 ("dstStageMask" ::: PipelineStageFlags)
              -> -- | @pMemoryBarriers@ is a pointer to an array of
                 -- 'Vulkan.Core10.OtherTypes.MemoryBarrier' structures.
                 ("memoryBarriers" ::: Vector MemoryBarrier)
              -> -- | @pBufferMemoryBarriers@ is a pointer to an array of
                 -- 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier' structures.
                 ("bufferMemoryBarriers" ::: Vector BufferMemoryBarrier)
              -> -- | @pImageMemoryBarriers@ is a pointer to an array of
                 -- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier' structures.
                 ("imageMemoryBarriers" ::: Vector (SomeStruct ImageMemoryBarrier))
              -> io ()
cmdWaitEvents = cmdWaitEventsSafeOrUnsafe mkVkCmdWaitEventsUnsafe

-- | A variant of 'cmdWaitEvents' which makes a *safe* FFI call
cmdWaitEventsSafe :: forall io
                   . (MonadIO io)
                  => -- | @commandBuffer@ is the command buffer into which the command is
                     -- recorded.
                     CommandBuffer
                  -> -- | @pEvents@ is a pointer to an array of event object handles to wait on.
                     ("events" ::: Vector Event)
                  -> -- | @srcStageMask@ is a bitmask of
                     -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                     -- specifying the
                     -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>.
                     ("srcStageMask" ::: PipelineStageFlags)
                  -> -- | @dstStageMask@ is a bitmask of
                     -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                     -- specifying the
                     -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages destination stage mask>.
                     ("dstStageMask" ::: PipelineStageFlags)
                  -> -- | @pMemoryBarriers@ is a pointer to an array of
                     -- 'Vulkan.Core10.OtherTypes.MemoryBarrier' structures.
                     ("memoryBarriers" ::: Vector MemoryBarrier)
                  -> -- | @pBufferMemoryBarriers@ is a pointer to an array of
                     -- 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier' structures.
                     ("bufferMemoryBarriers" ::: Vector BufferMemoryBarrier)
                  -> -- | @pImageMemoryBarriers@ is a pointer to an array of
                     -- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier' structures.
                     ("imageMemoryBarriers" ::: Vector (SomeStruct ImageMemoryBarrier))
                  -> io ()
cmdWaitEventsSafe = cmdWaitEventsSafeOrUnsafe mkVkCmdWaitEventsSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPipelineBarrier
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlags -> PipelineStageFlags -> DependencyFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlags -> PipelineStageFlags -> DependencyFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()

-- | vkCmdPipelineBarrier - Insert a memory dependency
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
-- If @dependencyFlags@ includes
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_BY_REGION_BIT', then
-- any dependency between
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space>
-- pipeline stages is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-local>
-- - otherwise it is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-global>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04097# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-4098# Any pipeline stage
--     included in @srcStageMask@ /must/ be supported by the capabilities
--     of the queue family specified by the @queueFamilyIndex@ member of
--     the 'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that
--     was used to create the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04097# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-4098# Any pipeline stage
--     included in @dstStageMask@ /must/ be supported by the capabilities
--     of the queue family specified by the @queueFamilyIndex@ member of
--     the 'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that
--     was used to create the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   #VUID-vkCmdPipelineBarrier-srcAccessMask-02815# The @srcAccessMask@
--     member of each element of @pMemoryBarriers@ /must/ only include
--     access flags that are supported by one or more of the pipeline
--     stages in @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-vkCmdPipelineBarrier-dstAccessMask-02816# The @dstAccessMask@
--     member of each element of @pMemoryBarriers@ /must/ only include
--     access flags that are supported by one or more of the pipeline
--     stages in @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-vkCmdPipelineBarrier-pBufferMemoryBarriers-02817# For any
--     element of @pBufferMemoryBarriers@, if its @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ members are equal, or if its
--     @srcQueueFamilyIndex@ is the queue family index that was used to
--     create the command pool that @commandBuffer@ was allocated from,
--     then its @srcAccessMask@ member /must/ only contain access flags
--     that are supported by one or more of the pipeline stages in
--     @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-vkCmdPipelineBarrier-pBufferMemoryBarriers-02818# For any
--     element of @pBufferMemoryBarriers@, if its @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ members are equal, or if its
--     @dstQueueFamilyIndex@ is the queue family index that was used to
--     create the command pool that @commandBuffer@ was allocated from,
--     then its @dstAccessMask@ member /must/ only contain access flags
--     that are supported by one or more of the pipeline stages in
--     @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-vkCmdPipelineBarrier-pImageMemoryBarriers-02819# For any
--     element of @pImageMemoryBarriers@, if its @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ members are equal, or if its
--     @srcQueueFamilyIndex@ is the queue family index that was used to
--     create the command pool that @commandBuffer@ was allocated from,
--     then its @srcAccessMask@ member /must/ only contain access flags
--     that are supported by one or more of the pipeline stages in
--     @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-vkCmdPipelineBarrier-pImageMemoryBarriers-02820# For any
--     element of @pImageMemoryBarriers@, if its @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ members are equal, or if its
--     @dstQueueFamilyIndex@ is the queue family index that was used to
--     create the command pool that @commandBuffer@ was allocated from,
--     then its @dstAccessMask@ member /must/ only contain access flags
--     that are supported by one or more of the pipeline stages in
--     @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>
--
-- -   #VUID-vkCmdPipelineBarrier-pDependencies-02285# If
--     'cmdPipelineBarrier' is called within a render pass instance, the
--     render pass /must/ have been created with at least one
--     'Vulkan.Core10.Pass.SubpassDependency' instance in
--     'Vulkan.Core10.Pass.RenderPassCreateInfo'::@pDependencies@ that
--     expresses a dependency from the current subpass to itself, with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scopes>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scopes>
--     that are all supersets of the scopes defined in this command
--
-- -   #VUID-vkCmdPipelineBarrier-bufferMemoryBarrierCount-01178# If
--     'cmdPipelineBarrier' is called within a render pass instance, it
--     /must/ not include any buffer memory barriers
--
-- -   #VUID-vkCmdPipelineBarrier-image-04073# If 'cmdPipelineBarrier' is
--     called within a render pass instance, the @image@ member of any
--     image memory barrier included in this command /must/ be an
--     attachment used in the current subpass both as an input attachment,
--     and as either a color or depth\/stencil attachment
--
-- -   #VUID-vkCmdPipelineBarrier-oldLayout-01181# If 'cmdPipelineBarrier'
--     is called within a render pass instance, the @oldLayout@ and
--     @newLayout@ members of any image memory barrier included in this
--     command /must/ be equal
--
-- -   #VUID-vkCmdPipelineBarrier-srcQueueFamilyIndex-01182# If
--     'cmdPipelineBarrier' is called within a render pass instance, the
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ members of any image
--     memory barrier included in this command /must/ be equal
--
-- -   #VUID-vkCmdPipelineBarrier-dependencyFlags-01186# If
--     'cmdPipelineBarrier' is called outside of a render pass instance,
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT'
--     /must/ not be included in the dependency flags
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPipelineBarrier-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-parameter# @srcStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-requiredbitmask#
--     @srcStageMask@ /must/ not be @0@
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-parameter# @dstStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-requiredbitmask#
--     @dstStageMask@ /must/ not be @0@
--
-- -   #VUID-vkCmdPipelineBarrier-dependencyFlags-parameter#
--     @dependencyFlags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits' values
--
-- -   #VUID-vkCmdPipelineBarrier-pMemoryBarriers-parameter# If
--     @memoryBarrierCount@ is not @0@, @pMemoryBarriers@ /must/ be a valid
--     pointer to an array of @memoryBarrierCount@ valid
--     'Vulkan.Core10.OtherTypes.MemoryBarrier' structures
--
-- -   #VUID-vkCmdPipelineBarrier-pBufferMemoryBarriers-parameter# If
--     @bufferMemoryBarrierCount@ is not @0@, @pBufferMemoryBarriers@
--     /must/ be a valid pointer to an array of @bufferMemoryBarrierCount@
--     valid 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier' structures
--
-- -   #VUID-vkCmdPipelineBarrier-pImageMemoryBarriers-parameter# If
--     @imageMemoryBarrierCount@ is not @0@, @pImageMemoryBarriers@ /must/
--     be a valid pointer to an array of @imageMemoryBarrierCount@ valid
--     'Vulkan.Core10.OtherTypes.ImageMemoryBarrier' structures
--
-- -   #VUID-vkCmdPipelineBarrier-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPipelineBarrier-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Transfer                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags',
-- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
-- 'Vulkan.Core10.OtherTypes.MemoryBarrier',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags'
cmdPipelineBarrier :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer into which the command is
                      -- recorded.
                      CommandBuffer
                   -> -- | @srcStageMask@ is a bitmask of
                      -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                      -- specifying the
                      -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>.
                      ("srcStageMask" ::: PipelineStageFlags)
                   -> -- | @dstStageMask@ is a bitmask of
                      -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                      -- specifying the
                      -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>.
                      ("dstStageMask" ::: PipelineStageFlags)
                   -> -- | @dependencyFlags@ is a bitmask of
                      -- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits' specifying
                      -- how execution and memory dependencies are formed.
                      DependencyFlags
                   -> -- | @pMemoryBarriers@ is a pointer to an array of
                      -- 'Vulkan.Core10.OtherTypes.MemoryBarrier' structures.
                      ("memoryBarriers" ::: Vector MemoryBarrier)
                   -> -- | @pBufferMemoryBarriers@ is a pointer to an array of
                      -- 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier' structures.
                      ("bufferMemoryBarriers" ::: Vector BufferMemoryBarrier)
                   -> -- | @pImageMemoryBarriers@ is a pointer to an array of
                      -- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier' structures.
                      ("imageMemoryBarriers" ::: Vector (SomeStruct ImageMemoryBarrier))
                   -> io ()
cmdPipelineBarrier commandBuffer srcStageMask dstStageMask dependencyFlags memoryBarriers bufferMemoryBarriers imageMemoryBarriers = liftIO . evalContT $ do
  let vkCmdPipelineBarrierPtr = pVkCmdPipelineBarrier (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdPipelineBarrierPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPipelineBarrier is null" Nothing Nothing
  let vkCmdPipelineBarrier' = mkVkCmdPipelineBarrier vkCmdPipelineBarrierPtr
  pPMemoryBarriers <- ContT $ allocaBytesAligned @MemoryBarrier ((Data.Vector.length (memoryBarriers)) * 24) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMemoryBarriers `plusPtr` (24 * (i)) :: Ptr MemoryBarrier) (e)) (memoryBarriers)
  pPBufferMemoryBarriers <- ContT $ allocaBytesAligned @BufferMemoryBarrier ((Data.Vector.length (bufferMemoryBarriers)) * 56) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBufferMemoryBarriers `plusPtr` (56 * (i)) :: Ptr BufferMemoryBarrier) (e)) (bufferMemoryBarriers)
  pPImageMemoryBarriers <- ContT $ allocaBytesAligned @(ImageMemoryBarrier _) ((Data.Vector.length (imageMemoryBarriers)) * 72) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPImageMemoryBarriers `plusPtr` (72 * (i)) :: Ptr (ImageMemoryBarrier _))) (e) . ($ ())) (imageMemoryBarriers)
  lift $ traceAroundEvent "vkCmdPipelineBarrier" (vkCmdPipelineBarrier' (commandBufferHandle (commandBuffer)) (srcStageMask) (dstStageMask) (dependencyFlags) ((fromIntegral (Data.Vector.length $ (memoryBarriers)) :: Word32)) (pPMemoryBarriers) ((fromIntegral (Data.Vector.length $ (bufferMemoryBarriers)) :: Word32)) (pPBufferMemoryBarriers) ((fromIntegral (Data.Vector.length $ (imageMemoryBarriers)) :: Word32)) (forgetExtensions (pPImageMemoryBarriers)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginQuery
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> QueryControlFlags -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> QueryControlFlags -> IO ()

-- | vkCmdBeginQuery - Begin a query
--
-- = Description
--
-- If the @queryType@ of the pool is
-- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION' and @flags@
-- contains
-- 'Vulkan.Core10.Enums.QueryControlFlagBits.QUERY_CONTROL_PRECISE_BIT', an
-- implementation /must/ return a result that matches the actual number of
-- samples passed. This is described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-occlusion Occlusion Queries>.
--
-- Calling 'cmdBeginQuery' is equivalent to calling
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginQueryIndexedEXT'
-- with the @index@ parameter set to zero.
--
-- After beginning a query, that query is considered /active/ within the
-- command buffer it was called in until that same query is ended. Queries
-- active in a primary command buffer when secondary command buffers are
-- executed are considered active for those secondary command buffers.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginQuery-queryPool-01922# @queryPool@ /must/ have been
--     created with a @queryType@ that differs from that of any queries
--     that are
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
--     within @commandBuffer@
--
-- -   #VUID-vkCmdBeginQuery-None-00807# All queries used by the command
--     /must/ be unavailable
--
-- -   #VUID-vkCmdBeginQuery-queryType-02804# The @queryType@ used to
--     create @queryPool@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP'
--
-- -   #VUID-vkCmdBeginQuery-queryType-00800# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-occlusionQueryPrecise precise occlusion queries>
--     feature is not enabled, or the @queryType@ used to create
--     @queryPool@ was not
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION', @flags@ /must/
--     not contain
--     'Vulkan.Core10.Enums.QueryControlFlagBits.QUERY_CONTROL_PRECISE_BIT'
--
-- -   #VUID-vkCmdBeginQuery-query-00802# @query@ /must/ be less than the
--     number of queries in @queryPool@
--
-- -   #VUID-vkCmdBeginQuery-queryType-00803# If the @queryType@ used to
--     create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION', the
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginQuery-queryType-00804# If the @queryType@ used to
--     create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS' and
--     any of the @pipelineStatistics@ indicate graphics operations, the
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginQuery-queryType-00805# If the @queryType@ used to
--     create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS' and
--     any of the @pipelineStatistics@ indicate compute operations, the
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdBeginQuery-commandBuffer-01885# @commandBuffer@ /must/
--     not be a protected command buffer
--
-- -   #VUID-vkCmdBeginQuery-query-00808# If called within a render pass
--     instance, the sum of @query@ and the number of bits set in the
--     current subpass’s view mask /must/ be less than or equal to the
--     number of queries in @queryPool@
--
-- -   #VUID-vkCmdBeginQuery-queryType-02327# If the @queryType@ used to
--     create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginQuery-queryType-02328# If the @queryType@ used to
--     create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT'
--     then
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackQueries@
--     /must/ be supported
--
-- -   #VUID-vkCmdBeginQuery-queryPool-03223# If @queryPool@ was created
--     with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#profiling-lock profiling lock>
--     /must/ have been held before
--     'Vulkan.Core10.CommandBuffer.beginCommandBuffer' was called on
--     @commandBuffer@
--
-- -   #VUID-vkCmdBeginQuery-queryPool-03224# If @queryPool@ was created
--     with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' and
--     one of the counters used to create @queryPool@ was
--     'Vulkan.Extensions.VK_KHR_performance_query.PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR',
--     the query begin /must/ be the first recorded command in
--     @commandBuffer@
--
-- -   #VUID-vkCmdBeginQuery-queryPool-03225# If @queryPool@ was created
--     with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' and
--     one of the counters used to create @queryPool@ was
--     'Vulkan.Extensions.VK_KHR_performance_query.PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR',
--     the begin command /must/ not be recorded within a render pass
--     instance
--
-- -   #VUID-vkCmdBeginQuery-queryPool-03226# If @queryPool@ was created
--     with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' and
--     another query pool with a @queryType@
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' has
--     been used within @commandBuffer@, its parent primary command buffer
--     or secondary command buffer recorded within the same parent primary
--     command buffer as @commandBuffer@, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-performanceCounterMultipleQueryPools performanceCounterMultipleQueryPools>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBeginQuery-None-02863# If @queryPool@ was created with a
--     @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     this command /must/ not be recorded in a command buffer that, either
--     directly or through secondary command buffers, also contains a
--     'cmdResetQueryPool' command affecting the same query
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginQuery-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginQuery-queryPool-parameter# @queryPool@ /must/ be a
--     valid 'Vulkan.Core10.Handles.QueryPool' handle
--
-- -   #VUID-vkCmdBeginQuery-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlagBits'
--     values
--
-- -   #VUID-vkCmdBeginQuery-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginQuery-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdBeginQuery-commonparent# Both of @commandBuffer@, and
--     @queryPool@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlags',
-- 'Vulkan.Core10.Handles.QueryPool'
cmdBeginQuery :: forall io
               . (MonadIO io)
              => -- | @commandBuffer@ is the command buffer into which this command will be
                 -- recorded.
                 CommandBuffer
              -> -- | @queryPool@ is the query pool that will manage the results of the query.
                 QueryPool
              -> -- | @query@ is the query index within the query pool that will contain the
                 -- results.
                 ("query" ::: Word32)
              -> -- | @flags@ is a bitmask of
                 -- 'Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlagBits'
                 -- specifying constraints on the types of queries that /can/ be performed.
                 QueryControlFlags
              -> io ()
cmdBeginQuery commandBuffer queryPool query flags = liftIO $ do
  let vkCmdBeginQueryPtr = pVkCmdBeginQuery (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBeginQueryPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginQuery is null" Nothing Nothing
  let vkCmdBeginQuery' = mkVkCmdBeginQuery vkCmdBeginQueryPtr
  traceAroundEvent "vkCmdBeginQuery" (vkCmdBeginQuery' (commandBufferHandle (commandBuffer)) (queryPool) (query) (flags))
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginQuery' and 'cmdEndQuery'
--
-- Note that 'cmdEndQuery' is *not* called if an exception is thrown by the
-- inner action.
cmdUseQuery :: forall io r . MonadIO io => CommandBuffer -> QueryPool -> Word32 -> QueryControlFlags -> io r -> io r
cmdUseQuery commandBuffer queryPool query flags a =
  (cmdBeginQuery commandBuffer queryPool query flags) *> a <* (cmdEndQuery commandBuffer queryPool query)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndQuery
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> IO ()

-- | vkCmdEndQuery - Ends a query
--
-- = Description
--
-- Calling 'cmdEndQuery' is equivalent to calling
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndQueryIndexedEXT' with
-- the @index@ parameter set to zero.
--
-- As queries operate asynchronously, ending a query does not immediately
-- set the query’s status to available. A query is considered /finished/
-- when the final results of the query are ready to be retrieved by
-- 'Vulkan.Core10.Query.getQueryPoolResults' and 'cmdCopyQueryPoolResults',
-- and this is when the query’s status is set to available.
--
-- Once a query is ended the query /must/ finish in finite time, unless the
-- state of the query is changed using other commands, e.g. by issuing a
-- reset of the query.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndQuery-None-01923# All queries used by the command
--     /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
--
-- -   #VUID-vkCmdEndQuery-query-00810# @query@ /must/ be less than the
--     number of queries in @queryPool@
--
-- -   #VUID-vkCmdEndQuery-commandBuffer-01886# @commandBuffer@ /must/ not
--     be a protected command buffer
--
-- -   #VUID-vkCmdEndQuery-query-00812# If 'cmdEndQuery' is called within a
--     render pass instance, the sum of @query@ and the number of bits set
--     in the current subpass’s view mask /must/ be less than or equal to
--     the number of queries in @queryPool@
--
-- -   #VUID-vkCmdEndQuery-queryPool-03227# If @queryPool@ was created with
--     a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' and
--     one or more of the counters used to create @queryPool@ was
--     'Vulkan.Extensions.VK_KHR_performance_query.PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR',
--     the 'cmdEndQuery' /must/ be the last recorded command in
--     @commandBuffer@
--
-- -   #VUID-vkCmdEndQuery-queryPool-03228# If @queryPool@ was created with
--     a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' and
--     one or more of the counters used to create @queryPool@ was
--     'Vulkan.Extensions.VK_KHR_performance_query.PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR',
--     the 'cmdEndQuery' /must/ not be recorded within a render pass
--     instance
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndQuery-commandBuffer-parameter# @commandBuffer@ /must/
--     be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndQuery-queryPool-parameter# @queryPool@ /must/ be a
--     valid 'Vulkan.Core10.Handles.QueryPool' handle
--
-- -   #VUID-vkCmdEndQuery-commandBuffer-recording# @commandBuffer@ /must/
--     be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndQuery-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdEndQuery-commonparent# Both of @commandBuffer@, and
--     @queryPool@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.QueryPool'
cmdEndQuery :: forall io
             . (MonadIO io)
            => -- | @commandBuffer@ is the command buffer into which this command will be
               -- recorded.
               CommandBuffer
            -> -- | @queryPool@ is the query pool that is managing the results of the query.
               QueryPool
            -> -- | @query@ is the query index within the query pool where the result is
               -- stored.
               ("query" ::: Word32)
            -> io ()
cmdEndQuery commandBuffer queryPool query = liftIO $ do
  let vkCmdEndQueryPtr = pVkCmdEndQuery (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdEndQueryPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndQuery is null" Nothing Nothing
  let vkCmdEndQuery' = mkVkCmdEndQuery vkCmdEndQueryPtr
  traceAroundEvent "vkCmdEndQuery" (vkCmdEndQuery' (commandBufferHandle (commandBuffer)) (queryPool) (query))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResetQueryPool
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> IO ()

-- | vkCmdResetQueryPool - Reset queries in a query pool
--
-- = Description
--
-- When executed on a queue, this command sets the status of query indices
-- [@firstQuery@, @firstQuery@ + @queryCount@ - 1] to unavailable.
--
-- If the @queryType@ used to create @queryPool@ was
-- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR', this
-- command sets the status of query indices [@firstQuery@, @firstQuery@ +
-- @queryCount@ - 1] to unavailable for each pass of @queryPool@, as
-- indicated by a call to
-- 'Vulkan.Extensions.VK_KHR_performance_query.getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'.
--
-- Note
--
-- Because 'cmdResetQueryPool' resets all the passes of the indicated
-- queries, applications must not record a 'cmdResetQueryPool' command for
-- a @queryPool@ created with
-- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR' in a
-- command buffer that needs to be submitted multiple times as indicated by
-- a call to
-- 'Vulkan.Extensions.VK_KHR_performance_query.getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'.
-- Otherwise applications will never be able to complete the recorded
-- queries.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdResetQueryPool-firstQuery-00796# @firstQuery@ /must/ be
--     less than the number of queries in @queryPool@
--
-- -   #VUID-vkCmdResetQueryPool-firstQuery-00797# The sum of @firstQuery@
--     and @queryCount@ /must/ be less than or equal to the number of
--     queries in @queryPool@
--
-- -   #VUID-vkCmdResetQueryPool-None-02841# All queries used by the
--     command /must/ not be active
--
-- -   #VUID-vkCmdResetQueryPool-firstQuery-02862# If @queryPool@ was
--     created with
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     this command /must/ not be recorded in a command buffer that, either
--     directly or through secondary command buffers, also contains begin
--     commands for a query from the set of queries [@firstQuery@,
--     @firstQuery@ + @queryCount@ - 1]
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdResetQueryPool-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdResetQueryPool-queryPool-parameter# @queryPool@ /must/ be
--     a valid 'Vulkan.Core10.Handles.QueryPool' handle
--
-- -   #VUID-vkCmdResetQueryPool-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdResetQueryPool-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdResetQueryPool-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdResetQueryPool-commonparent# Both of @commandBuffer@, and
--     @queryPool@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.QueryPool'
cmdResetQueryPool :: forall io
                   . (MonadIO io)
                  => -- | @commandBuffer@ is the command buffer into which this command will be
                     -- recorded.
                     CommandBuffer
                  -> -- | @queryPool@ is the handle of the query pool managing the queries being
                     -- reset.
                     QueryPool
                  -> -- | @firstQuery@ is the initial query index to reset.
                     ("firstQuery" ::: Word32)
                  -> -- | @queryCount@ is the number of queries to reset.
                     ("queryCount" ::: Word32)
                  -> io ()
cmdResetQueryPool commandBuffer queryPool firstQuery queryCount = liftIO $ do
  let vkCmdResetQueryPoolPtr = pVkCmdResetQueryPool (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdResetQueryPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdResetQueryPool is null" Nothing Nothing
  let vkCmdResetQueryPool' = mkVkCmdResetQueryPool vkCmdResetQueryPoolPtr
  traceAroundEvent "vkCmdResetQueryPool" (vkCmdResetQueryPool' (commandBufferHandle (commandBuffer)) (queryPool) (firstQuery) (queryCount))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteTimestamp
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlagBits -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlagBits -> QueryPool -> Word32 -> IO ()

-- | vkCmdWriteTimestamp - Write a device timestamp into a query object
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
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04074# @pipelineStage@
--     /must/ be a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported valid stage>
--     for the queue family that was used to create the command pool that
--     @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04075# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04076# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04077# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditional rendering>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04078# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragment density map>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04079# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transform feedback>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04080# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04081# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shading rate image>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   #VUID-vkCmdWriteTimestamp-queryPool-01416# @queryPool@ /must/ have
--     been created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP'
--
-- -   #VUID-vkCmdWriteTimestamp-queryPool-00828# The query identified by
--     @queryPool@ and @query@ /must/ be /unavailable/
--
-- -   #VUID-vkCmdWriteTimestamp-timestampValidBits-00829# The command
--     pool’s queue family /must/ support a non-zero @timestampValidBits@
--
-- -   #VUID-vkCmdWriteTimestamp-None-00830# All queries used by the
--     command /must/ be unavailable
--
-- -   #VUID-vkCmdWriteTimestamp-query-00831# If 'cmdWriteTimestamp' is
--     called within a render pass instance, the sum of @query@ and the
--     number of bits set in the current subpass’s view mask /must/ be less
--     than or equal to the number of queries in @queryPool@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdWriteTimestamp-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-parameter# @pipelineStage@
--     /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     value
--
-- -   #VUID-vkCmdWriteTimestamp-queryPool-parameter# @queryPool@ /must/ be
--     a valid 'Vulkan.Core10.Handles.QueryPool' handle
--
-- -   #VUID-vkCmdWriteTimestamp-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdWriteTimestamp-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdWriteTimestamp-commonparent# Both of @commandBuffer@, and
--     @queryPool@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
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
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits',
-- 'Vulkan.Core10.Handles.QueryPool'
cmdWriteTimestamp :: forall io
                   . (MonadIO io)
                  => -- | @commandBuffer@ is the command buffer into which the command will be
                     -- recorded.
                     CommandBuffer
                  -> -- | @pipelineStage@ is one of the
                     -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits',
                     -- specifying a stage of the pipeline.
                     PipelineStageFlagBits
                  -> -- | @queryPool@ is the query pool that will manage the timestamp.
                     QueryPool
                  -> -- | @query@ is the query within the query pool that will contain the
                     -- timestamp.
                     ("query" ::: Word32)
                  -> io ()
cmdWriteTimestamp commandBuffer pipelineStage queryPool query = liftIO $ do
  let vkCmdWriteTimestampPtr = pVkCmdWriteTimestamp (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdWriteTimestampPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteTimestamp is null" Nothing Nothing
  let vkCmdWriteTimestamp' = mkVkCmdWriteTimestamp vkCmdWriteTimestampPtr
  traceAroundEvent "vkCmdWriteTimestamp" (vkCmdWriteTimestamp' (commandBufferHandle (commandBuffer)) (pipelineStage) (queryPool) (query))
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
-- = Description
--
-- 'cmdCopyQueryPoolResults' is guaranteed to see the effect of previous
-- uses of 'cmdResetQueryPool' in the same queue, without any additional
-- synchronization. Thus, the results will always reflect the most recent
-- use of the query.
--
-- @flags@ has the same possible values described above for the @flags@
-- parameter of 'Vulkan.Core10.Query.getQueryPoolResults', but the
-- different style of execution causes some subtle behavioral differences.
-- Because 'cmdCopyQueryPoolResults' executes in order with respect to
-- other query commands, there is less ambiguity about which use of a query
-- is being requested.
--
-- Results for all requested occlusion queries, pipeline statistics
-- queries, transform feedback queries, and timestamp queries are written
-- as 64-bit unsigned integer values if
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT' is set or
-- 32-bit unsigned integer values otherwise. Performance queries store
-- results in a tightly packed array whose type is determined by the @unit@
-- member of the corresponding
-- 'Vulkan.Extensions.VK_KHR_performance_query.PerformanceCounterKHR'.
--
-- If neither of
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT' and
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- are set, results are only written out for queries in the available
-- state.
--
-- If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT' is
-- set, the implementation will wait for each query’s status to be in the
-- available state before retrieving the numerical results for that query.
-- This is guaranteed to reflect the most recent use of the query on the
-- same queue, assuming that the query is not being simultaneously used by
-- other queues. If the query does not become available in a finite amount
-- of time (e.g. due to not issuing a query since the last reset), a
-- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST' error /may/ occur.
--
-- Similarly, if
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is set and
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT' is not
-- set, the availability is guaranteed to reflect the most recent use of
-- the query on the same queue, assuming that the query is not being
-- simultaneously used by other queues. As with
-- 'Vulkan.Core10.Query.getQueryPoolResults', implementations /must/
-- guarantee that if they return a non-zero availability value, then the
-- numerical results are valid.
--
-- If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT' is
-- set, 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT' is
-- not set, and the query’s status is unavailable, an intermediate result
-- value between zero and the final result value is written for that query.
--
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
-- /must/ not be used if the pool’s @queryType@ is
-- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP'.
--
-- 'cmdCopyQueryPoolResults' is considered to be a transfer operation, and
-- its writes to buffer memory /must/ be synchronized using
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFER_BIT'
-- and 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_TRANSFER_WRITE_BIT'
-- before using the results.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyQueryPoolResults-dstOffset-00819# @dstOffset@ /must/
--     be less than the size of @dstBuffer@
--
-- -   #VUID-vkCmdCopyQueryPoolResults-firstQuery-00820# @firstQuery@
--     /must/ be less than the number of queries in @queryPool@
--
-- -   #VUID-vkCmdCopyQueryPoolResults-firstQuery-00821# The sum of
--     @firstQuery@ and @queryCount@ /must/ be less than or equal to the
--     number of queries in @queryPool@
--
-- -   #VUID-vkCmdCopyQueryPoolResults-flags-00822# If
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT' is not
--     set in @flags@ then @dstOffset@ and @stride@ /must/ be multiples of
--     @4@
--
-- -   #VUID-vkCmdCopyQueryPoolResults-flags-00823# If
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT' is set
--     in @flags@ then @dstOffset@ and @stride@ /must/ be multiples of @8@
--
-- -   #VUID-vkCmdCopyQueryPoolResults-dstBuffer-00824# @dstBuffer@ /must/
--     have enough storage, from @dstOffset@, to contain the result of each
--     query, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-memorylayout here>
--
-- -   #VUID-vkCmdCopyQueryPoolResults-dstBuffer-00825# @dstBuffer@ /must/
--     have been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdCopyQueryPoolResults-dstBuffer-00826# If @dstBuffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdCopyQueryPoolResults-queryType-00827# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP', @flags@ /must/
--     not contain
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
--
-- -   #VUID-vkCmdCopyQueryPoolResults-queryType-03232# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     'Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryPropertiesKHR'::@allowCommandBufferQueryCopies@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-vkCmdCopyQueryPoolResults-queryType-03233# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT',
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT'
--     or 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT'
--
-- -   #VUID-vkCmdCopyQueryPoolResults-queryType-03234# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the @queryPool@ /must/ have been submitted once for each pass as
--     retrieved via a call to
--     'Vulkan.Extensions.VK_KHR_performance_query.getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'
--
-- -   #VUID-vkCmdCopyQueryPoolResults-queryType-02734#
--     'cmdCopyQueryPoolResults' /must/ not be called if the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_INTEL'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyQueryPoolResults-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyQueryPoolResults-queryPool-parameter# @queryPool@
--     /must/ be a valid 'Vulkan.Core10.Handles.QueryPool' handle
--
-- -   #VUID-vkCmdCopyQueryPoolResults-dstBuffer-parameter# @dstBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdCopyQueryPoolResults-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlagBits' values
--
-- -   #VUID-vkCmdCopyQueryPoolResults-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyQueryPoolResults-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdCopyQueryPoolResults-renderpass# This command /must/ only
--     be called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyQueryPoolResults-commonparent# Each of
--     @commandBuffer@, @dstBuffer@, and @queryPool@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Transfer                                                                                                                            |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Handles.QueryPool',
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlags'
cmdCopyQueryPoolResults :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which this command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @queryPool@ is the query pool managing the queries containing the
                           -- desired results.
                           QueryPool
                        -> -- | @firstQuery@ is the initial query index.
                           ("firstQuery" ::: Word32)
                        -> -- | @queryCount@ is the number of queries. @firstQuery@ and @queryCount@
                           -- together define a range of queries.
                           ("queryCount" ::: Word32)
                        -> -- | @dstBuffer@ is a 'Vulkan.Core10.Handles.Buffer' object that will receive
                           -- the results of the copy command.
                           ("dstBuffer" ::: Buffer)
                        -> -- | @dstOffset@ is an offset into @dstBuffer@.
                           ("dstOffset" ::: DeviceSize)
                        -> -- | @stride@ is the stride in bytes between results for individual queries
                           -- within @dstBuffer@. The required size of the backing memory for
                           -- @dstBuffer@ is determined as described above for
                           -- 'Vulkan.Core10.Query.getQueryPoolResults'.
                           ("stride" ::: DeviceSize)
                        -> -- | @flags@ is a bitmask of
                           -- 'Vulkan.Core10.Enums.QueryResultFlagBits.QueryResultFlagBits' specifying
                           -- how and when results are returned.
                           QueryResultFlags
                        -> io ()
cmdCopyQueryPoolResults commandBuffer queryPool firstQuery queryCount dstBuffer dstOffset stride flags = liftIO $ do
  let vkCmdCopyQueryPoolResultsPtr = pVkCmdCopyQueryPoolResults (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdCopyQueryPoolResultsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyQueryPoolResults is null" Nothing Nothing
  let vkCmdCopyQueryPoolResults' = mkVkCmdCopyQueryPoolResults vkCmdCopyQueryPoolResultsPtr
  traceAroundEvent "vkCmdCopyQueryPoolResults" (vkCmdCopyQueryPoolResults' (commandBufferHandle (commandBuffer)) (queryPool) (firstQuery) (queryCount) (dstBuffer) (dstOffset) (stride) (flags))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushConstants
  :: FunPtr (Ptr CommandBuffer_T -> PipelineLayout -> ShaderStageFlags -> Word32 -> Word32 -> Ptr () -> IO ()) -> Ptr CommandBuffer_T -> PipelineLayout -> ShaderStageFlags -> Word32 -> Word32 -> Ptr () -> IO ()

-- | vkCmdPushConstants - Update the values of push constants
--
-- = Description
--
-- When a command buffer begins recording, all push constant values are
-- undefined.
--
-- Push constant values /can/ be updated incrementally, causing shader
-- stages in @stageFlags@ to read the new data from @pValues@ for push
-- constants modified by this command, while still reading the previous
-- data for push constants not modified by this command. When a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipeline-bindpoint-commands bound pipeline command>
-- is issued, the bound pipeline’s layout /must/ be compatible with the
-- layouts used to set the values of all push constants in the pipeline
-- layout’s push constant ranges, as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>.
-- Binding a pipeline with a layout that is not compatible with the push
-- constant layout does not disturb the push constant values.
--
-- Note
--
-- As @stageFlags@ needs to include all flags the relevant push constant
-- ranges were created with, any flags that are not supported by the queue
-- family that the 'Vulkan.Core10.Handles.CommandPool' used to allocate
-- @commandBuffer@ was created on are ignored.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPushConstants-offset-01795# For each byte in the range
--     specified by @offset@ and @size@ and for each shader stage in
--     @stageFlags@, there /must/ be a push constant range in @layout@ that
--     includes that byte and that stage
--
-- -   #VUID-vkCmdPushConstants-offset-01796# For each byte in the range
--     specified by @offset@ and @size@ and for each push constant range
--     that overlaps that byte, @stageFlags@ /must/ include all stages in
--     that push constant range’s
--     'Vulkan.Core10.PipelineLayout.PushConstantRange'::@stageFlags@
--
-- -   #VUID-vkCmdPushConstants-offset-00368# @offset@ /must/ be a multiple
--     of @4@
--
-- -   #VUID-vkCmdPushConstants-size-00369# @size@ /must/ be a multiple of
--     @4@
--
-- -   #VUID-vkCmdPushConstants-offset-00370# @offset@ /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPushConstantsSize@
--
-- -   #VUID-vkCmdPushConstants-size-00371# @size@ /must/ be less than or
--     equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPushConstantsSize@
--     minus @offset@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPushConstants-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPushConstants-layout-parameter# @layout@ /must/ be a
--     valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-vkCmdPushConstants-stageFlags-parameter# @stageFlags@ /must/
--     be a valid combination of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' values
--
-- -   #VUID-vkCmdPushConstants-stageFlags-requiredbitmask# @stageFlags@
--     /must/ not be @0@
--
-- -   #VUID-vkCmdPushConstants-pValues-parameter# @pValues@ /must/ be a
--     valid pointer to an array of @size@ bytes
--
-- -   #VUID-vkCmdPushConstants-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPushConstants-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- -   #VUID-vkCmdPushConstants-size-arraylength# @size@ /must/ be greater
--     than @0@
--
-- -   #VUID-vkCmdPushConstants-commonparent# Both of @commandBuffer@, and
--     @layout@ /must/ have been created, allocated, or retrieved from the
--     same 'Vulkan.Core10.Handles.Device'
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags'
cmdPushConstants :: forall io
                  . (MonadIO io)
                 => -- | @commandBuffer@ is the command buffer in which the push constant update
                    -- will be recorded.
                    CommandBuffer
                 -> -- | @layout@ is the pipeline layout used to program the push constant
                    -- updates.
                    PipelineLayout
                 -> -- | @stageFlags@ is a bitmask of
                    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' specifying
                    -- the shader stages that will use the push constants in the updated range.
                    ShaderStageFlags
                 -> -- | @offset@ is the start offset of the push constant range to update, in
                    -- units of bytes.
                    ("offset" ::: Word32)
                 -> -- | @size@ is the size of the push constant range to update, in units of
                    -- bytes.
                    ("size" ::: Word32)
                 -> -- | @pValues@ is a pointer to an array of @size@ bytes containing the new
                    -- push constant values.
                    ("values" ::: Ptr ())
                 -> io ()
cmdPushConstants commandBuffer layout stageFlags offset size values = liftIO $ do
  let vkCmdPushConstantsPtr = pVkCmdPushConstants (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdPushConstantsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushConstants is null" Nothing Nothing
  let vkCmdPushConstants' = mkVkCmdPushConstants vkCmdPushConstantsPtr
  traceAroundEvent "vkCmdPushConstants" (vkCmdPushConstants' (commandBufferHandle (commandBuffer)) (layout) (stageFlags) (offset) (size) (values))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginRenderPass
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct RenderPassBeginInfo) -> SubpassContents -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct RenderPassBeginInfo) -> SubpassContents -> IO ()

-- | vkCmdBeginRenderPass - Begin a new render pass
--
-- = Description
--
-- After beginning a render pass instance, the command buffer is ready to
-- record the commands for the first subpass of that render pass.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginRenderPass-initialLayout-00895# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass-initialLayout-01758# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass-initialLayout-02842# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass-stencilInitialLayout-02843# If any of the
--     @stencilInitialLayout@ or @stencilFinalLayout@ member of the
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout'
--     structures or the @stencilLayout@ member of the
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentReferenceStencilLayout'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass-initialLayout-00897# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT' or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass-initialLayout-00898# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass-initialLayout-00899# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--
-- -   #VUID-vkCmdBeginRenderPass-initialLayout-00900# If any of the
--     @initialLayout@ members of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures specified when
--     creating the render pass specified in the @renderPass@ member of
--     @pRenderPassBegin@ is not
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED', then each
--     such @initialLayout@ /must/ be equal to the current layout of the
--     corresponding attachment image subresource of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@
--
-- -   #VUID-vkCmdBeginRenderPass-srcStageMask-00901# The @srcStageMask@
--     and @dstStageMask@ members of any element of the @pDependencies@
--     member of 'Vulkan.Core10.Pass.RenderPassCreateInfo' used to create
--     @renderPass@ /must/ be supported by the capabilities of the queue
--     family identified by the @queueFamilyIndex@ member of the
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' used to create the
--     command pool which @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdBeginRenderPass-framebuffer-02532# For any attachment in
--     @framebuffer@ that is used by @renderPass@ and is bound to memory
--     locations that are also bound to another attachment used by
--     @renderPass@, and if at least one of those uses causes either
--     attachment to be written to, both attachments /must/ have had the
--     'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT'
--     set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginRenderPass-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginRenderPass-pRenderPassBegin-parameter#
--     @pRenderPassBegin@ /must/ be a valid pointer to a valid
--     'RenderPassBeginInfo' structure
--
-- -   #VUID-vkCmdBeginRenderPass-contents-parameter# @contents@ /must/ be
--     a valid 'Vulkan.Core10.Enums.SubpassContents.SubpassContents' value
--
-- -   #VUID-vkCmdBeginRenderPass-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginRenderPass-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginRenderPass-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdBeginRenderPass-bufferlevel# @commandBuffer@ /must/ be a
--     primary 'Vulkan.Core10.Handles.CommandBuffer'
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Graphics                                                                                                              | Graphics                                                                                                                            |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'RenderPassBeginInfo',
-- 'Vulkan.Core10.Enums.SubpassContents.SubpassContents'
cmdBeginRenderPass :: forall a io
                    . (Extendss RenderPassBeginInfo a, PokeChain a, MonadIO io)
                   => -- | @commandBuffer@ is the command buffer in which to record the command.
                      CommandBuffer
                   -> -- | @pRenderPassBegin@ is a pointer to a 'RenderPassBeginInfo' structure
                      -- specifying the render pass to begin an instance of, and the framebuffer
                      -- the instance uses.
                      (RenderPassBeginInfo a)
                   -> -- | @contents@ is a 'Vulkan.Core10.Enums.SubpassContents.SubpassContents'
                      -- value specifying how the commands in the first subpass will be provided.
                      SubpassContents
                   -> io ()
cmdBeginRenderPass commandBuffer renderPassBegin contents = liftIO . evalContT $ do
  let vkCmdBeginRenderPassPtr = pVkCmdBeginRenderPass (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBeginRenderPassPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginRenderPass is null" Nothing Nothing
  let vkCmdBeginRenderPass' = mkVkCmdBeginRenderPass vkCmdBeginRenderPassPtr
  pRenderPassBegin <- ContT $ withCStruct (renderPassBegin)
  lift $ traceAroundEvent "vkCmdBeginRenderPass" (vkCmdBeginRenderPass' (commandBufferHandle (commandBuffer)) (forgetExtensions pRenderPassBegin) (contents))
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginRenderPass' and 'cmdEndRenderPass'
--
-- Note that 'cmdEndRenderPass' is *not* called if an exception is thrown
-- by the inner action.
cmdUseRenderPass :: forall a io r . (Extendss RenderPassBeginInfo a, PokeChain a, MonadIO io) => CommandBuffer -> RenderPassBeginInfo a -> SubpassContents -> io r -> io r
cmdUseRenderPass commandBuffer pRenderPassBegin contents a =
  (cmdBeginRenderPass commandBuffer pRenderPassBegin contents) *> a <* (cmdEndRenderPass commandBuffer)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdNextSubpass
  :: FunPtr (Ptr CommandBuffer_T -> SubpassContents -> IO ()) -> Ptr CommandBuffer_T -> SubpassContents -> IO ()

-- | vkCmdNextSubpass - Transition to the next subpass of a render pass
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
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
-- pipeline stage and their writes are synchronized with
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
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
-- -   #VUID-vkCmdNextSubpass-None-00909# The current subpass index /must/
--     be less than the number of subpasses in the render pass minus one
--
-- -   #VUID-vkCmdNextSubpass-None-02349# This command /must/ not be
--     recorded when transform feedback is active
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdNextSubpass-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdNextSubpass-contents-parameter# @contents@ /must/ be a
--     valid 'Vulkan.Core10.Enums.SubpassContents.SubpassContents' value
--
-- -   #VUID-vkCmdNextSubpass-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdNextSubpass-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdNextSubpass-renderpass# This command /must/ only be
--     called inside of a render pass instance
--
-- -   #VUID-vkCmdNextSubpass-bufferlevel# @commandBuffer@ /must/ be a
--     primary 'Vulkan.Core10.Handles.CommandBuffer'
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.SubpassContents.SubpassContents'
cmdNextSubpass :: forall io
                . (MonadIO io)
               => -- | @commandBuffer@ is the command buffer in which to record the command.
                  CommandBuffer
               -> -- | @contents@ specifies how the commands in the next subpass will be
                  -- provided, in the same fashion as the corresponding parameter of
                  -- 'cmdBeginRenderPass'.
                  SubpassContents
               -> io ()
cmdNextSubpass commandBuffer contents = liftIO $ do
  let vkCmdNextSubpassPtr = pVkCmdNextSubpass (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdNextSubpassPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdNextSubpass is null" Nothing Nothing
  let vkCmdNextSubpass' = mkVkCmdNextSubpass vkCmdNextSubpassPtr
  traceAroundEvent "vkCmdNextSubpass" (vkCmdNextSubpass' (commandBufferHandle (commandBuffer)) (contents))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndRenderPass
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- | vkCmdEndRenderPass - End the current render pass
--
-- = Description
--
-- Ending a render pass instance performs any multisample resolve
-- operations on the final subpass.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndRenderPass-None-00910# The current subpass index
--     /must/ be equal to the number of subpasses in the render pass minus
--     one
--
-- -   #VUID-vkCmdEndRenderPass-None-02351# This command /must/ not be
--     recorded when transform feedback is active
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndRenderPass-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndRenderPass-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndRenderPass-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdEndRenderPass-renderpass# This command /must/ only be
--     called inside of a render pass instance
--
-- -   #VUID-vkCmdEndRenderPass-bufferlevel# @commandBuffer@ /must/ be a
--     primary 'Vulkan.Core10.Handles.CommandBuffer'
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Graphics                                                                                                              | Graphics                                                                                                                            |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdEndRenderPass :: forall io
                  . (MonadIO io)
                 => -- | @commandBuffer@ is the command buffer in which to end the current render
                    -- pass instance.
                    CommandBuffer
                 -> io ()
cmdEndRenderPass commandBuffer = liftIO $ do
  let vkCmdEndRenderPassPtr = pVkCmdEndRenderPass (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdEndRenderPassPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndRenderPass is null" Nothing Nothing
  let vkCmdEndRenderPass' = mkVkCmdEndRenderPass vkCmdEndRenderPassPtr
  traceAroundEvent "vkCmdEndRenderPass" (vkCmdEndRenderPass' (commandBufferHandle (commandBuffer)))
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
-- = Description
--
-- If any element of @pCommandBuffers@ was not recorded with the
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
-- flag, and it was recorded into any other primary command buffer which is
-- currently in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle executable or recording state>,
-- that primary command buffer becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00088# Each element of
--     @pCommandBuffers@ /must/ have been allocated with a @level@ of
--     'Vulkan.Core10.Enums.CommandBufferLevel.COMMAND_BUFFER_LEVEL_SECONDARY'
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00089# Each element of
--     @pCommandBuffers@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00091# If any element of
--     @pCommandBuffers@ was not recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--     flag, it /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00092# If any element of
--     @pCommandBuffers@ was not recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--     flag, it /must/ not have already been recorded to @commandBuffer@
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00093# If any element of
--     @pCommandBuffers@ was not recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--     flag, it /must/ not appear more than once in @pCommandBuffers@
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00094# Each element of
--     @pCommandBuffers@ /must/ have been allocated from a
--     'Vulkan.Core10.Handles.CommandPool' that was created for the same
--     queue family as the 'Vulkan.Core10.Handles.CommandPool' from which
--     @commandBuffer@ was allocated
--
-- -   #VUID-vkCmdExecuteCommands-contents-00095# If 'cmdExecuteCommands'
--     is being called within a render pass instance, that render pass
--     instance /must/ have been begun with the @contents@ parameter of
--     'cmdBeginRenderPass' set to
--     'Vulkan.Core10.Enums.SubpassContents.SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS'
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00096# If
--     'cmdExecuteCommands' is being called within a render pass instance,
--     each element of @pCommandBuffers@ /must/ have been recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00097# If
--     'cmdExecuteCommands' is being called within a render pass instance,
--     each element of @pCommandBuffers@ /must/ have been recorded with
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@subpass@
--     set to the index of the subpass which the given command buffer will
--     be executed in
--
-- -   #VUID-vkCmdExecuteCommands-pInheritanceInfo-00098# If
--     'cmdExecuteCommands' is being called within a render pass instance,
--     the render passes specified in the
--     @pBeginInfo->pInheritanceInfo->renderPass@ members of the
--     'Vulkan.Core10.CommandBuffer.beginCommandBuffer' commands used to
--     begin recording each element of @pCommandBuffers@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the current render pass
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00099# If
--     'cmdExecuteCommands' is being called within a render pass instance,
--     and any element of @pCommandBuffers@ was recorded with
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@framebuffer@
--     not equal to 'Vulkan.Core10.APIConstants.NULL_HANDLE', that
--     'Vulkan.Core10.Handles.Framebuffer' /must/ match the
--     'Vulkan.Core10.Handles.Framebuffer' used in the current render pass
--     instance
--
-- -   #VUID-vkCmdExecuteCommands-pNext-02865# If 'cmdExecuteCommands' is
--     being called within a render pass instance that included
--     'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'
--     in the @pNext@ chain of 'RenderPassBeginInfo', then each element of
--     @pCommandBuffers@ /must/ have been recorded with
--     'Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM'
--     in the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'
--
-- -   #VUID-vkCmdExecuteCommands-pNext-02866# If 'cmdExecuteCommands' is
--     being called within a render pass instance that included
--     'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'
--     in the @pNext@ chain of 'RenderPassBeginInfo', then each element of
--     @pCommandBuffers@ /must/ have been recorded with
--     'Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM'::@transform@
--     identical to
--     'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'::@transform@
--
-- -   #VUID-vkCmdExecuteCommands-pNext-02867# If 'cmdExecuteCommands' is
--     being called within a render pass instance that included
--     'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'
--     in the @pNext@ chain of 'RenderPassBeginInfo', then each element of
--     @pCommandBuffers@ /must/ have been recorded with
--     'Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM'::@renderArea@
--     identical to 'RenderPassBeginInfo'::@renderArea@
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00100# If
--     'cmdExecuteCommands' is not being called within a render pass
--     instance, each element of @pCommandBuffers@ /must/ not have been
--     recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-00101# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-inheritedQueries inherited queries>
--     feature is not enabled, @commandBuffer@ /must/ not have any queries
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-00102# If @commandBuffer@
--     has a 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION' query
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>,
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@occlusionQueryEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-00103# If @commandBuffer@
--     has a 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION' query
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>,
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@queryFlags@
--     having all bits set that are set for the query
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-00104# If @commandBuffer@
--     has a 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS'
--     query
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>,
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@pipelineStatistics@
--     having all bits set that are set in the
--     'Vulkan.Core10.Handles.QueryPool' the query uses
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00105# Each element of
--     @pCommandBuffers@ /must/ not begin any query types that are
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-active active>
--     in @commandBuffer@
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-01820# If @commandBuffer@
--     is a protected command buffer, then each element of
--     @pCommandBuffers@ /must/ be a protected command buffer
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-01821# If @commandBuffer@
--     is an unprotected command buffer, then each element of
--     @pCommandBuffers@ /must/ be an unprotected command buffer
--
-- -   #VUID-vkCmdExecuteCommands-None-02286# This command /must/ not be
--     recorded when transform feedback is active
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-parameter#
--     @pCommandBuffers@ /must/ be a valid pointer to an array of
--     @commandBufferCount@ valid 'Vulkan.Core10.Handles.CommandBuffer'
--     handles
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, or compute
--     operations
--
-- -   #VUID-vkCmdExecuteCommands-bufferlevel# @commandBuffer@ /must/ be a
--     primary 'Vulkan.Core10.Handles.CommandBuffer'
--
-- -   #VUID-vkCmdExecuteCommands-commandBufferCount-arraylength#
--     @commandBufferCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdExecuteCommands-commonparent# Both of @commandBuffer@,
--     and the elements of @pCommandBuffers@ /must/ have been created,
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Transfer                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Graphics                                                                                                              |                                                                                                                                     |
-- |                                                                                                                            |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdExecuteCommands :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is a handle to a primary command buffer that the
                      -- secondary command buffers are executed in.
                      CommandBuffer
                   -> -- | @pCommandBuffers@ is a pointer to an array of @commandBufferCount@
                      -- secondary command buffer handles, which are recorded to execute in the
                      -- primary command buffer in the order they are listed in the array.
                      ("commandBuffers" ::: Vector CommandBuffer)
                   -> io ()
cmdExecuteCommands commandBuffer commandBuffers = liftIO . evalContT $ do
  let vkCmdExecuteCommandsPtr = pVkCmdExecuteCommands (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdExecuteCommandsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdExecuteCommands is null" Nothing Nothing
  let vkCmdExecuteCommands' = mkVkCmdExecuteCommands vkCmdExecuteCommandsPtr
  pPCommandBuffers <- ContT $ allocaBytesAligned @(Ptr CommandBuffer_T) ((Data.Vector.length (commandBuffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBuffers `plusPtr` (8 * (i)) :: Ptr (Ptr CommandBuffer_T)) (commandBufferHandle (e))) (commandBuffers)
  lift $ traceAroundEvent "vkCmdExecuteCommands" (vkCmdExecuteCommands' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (commandBuffers)) :: Word32)) (pPCommandBuffers))
  pure $ ()


-- | VkClearRect - Structure specifying a clear rectangle
--
-- = Description
--
-- The layers [@baseArrayLayer@, @baseArrayLayer@ + @layerCount@) counting
-- from the base layer of the attachment image view are cleared.
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Rect2D', 'cmdClearAttachments'
data ClearRect = ClearRect
  { -- | @rect@ is the two-dimensional region to be cleared.
    rect :: Rect2D
  , -- | @baseArrayLayer@ is the first layer to be cleared.
    baseArrayLayer :: Word32
  , -- | @layerCount@ is the number of layers to clear.
    layerCount :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ClearRect)
#endif
deriving instance Show ClearRect

instance ToCStruct ClearRect where
  withCStruct x f = allocaBytesAligned 24 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ClearRect{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Rect2D)) (rect)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (baseArrayLayer)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (layerCount)
    f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Rect2D)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct ClearRect where
  peekCStruct p = do
    rect <- peekCStruct @Rect2D ((p `plusPtr` 0 :: Ptr Rect2D))
    baseArrayLayer <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    layerCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ ClearRect
             rect baseArrayLayer layerCount

instance Storable ClearRect where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ClearRect where
  zero = ClearRect
           zero
           zero
           zero


-- | VkImageSubresourceLayers - Structure specifying an image subresource
-- layers
--
-- == Valid Usage
--
-- -   #VUID-VkImageSubresourceLayers-aspectMask-00167# If @aspectMask@
--     contains
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT', it
--     /must/ not contain either of
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkImageSubresourceLayers-aspectMask-00168# @aspectMask@ /must/
--     not contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_METADATA_BIT'
--
-- -   #VUID-VkImageSubresourceLayers-aspectMask-02247# @aspectMask@ /must/
--     not include @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ for any index
--     @i@
--
-- -   #VUID-VkImageSubresourceLayers-layerCount-01700# @layerCount@ /must/
--     be greater than 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageSubresourceLayers-aspectMask-parameter# @aspectMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' values
--
-- -   #VUID-VkImageSubresourceLayers-aspectMask-requiredbitmask#
--     @aspectMask@ /must/ not be @0@
--
-- = See Also
--
-- 'BufferImageCopy',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.BufferImageCopy2KHR',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags', 'ImageBlit',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.ImageBlit2KHR', 'ImageCopy',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.ImageCopy2KHR', 'ImageResolve',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.ImageResolve2KHR'
data ImageSubresourceLayers = ImageSubresourceLayers
  { -- | @aspectMask@ is a combination of
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits', selecting
    -- the color, depth and\/or stencil aspects to be copied.
    aspectMask :: ImageAspectFlags
  , -- | @mipLevel@ is the mipmap level to copy from.
    mipLevel :: Word32
  , -- | @baseArrayLayer@ and @layerCount@ are the starting layer and number of
    -- layers to copy.
    baseArrayLayer :: Word32
  , -- No documentation found for Nested "VkImageSubresourceLayers" "layerCount"
    layerCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageSubresourceLayers)
#endif
deriving instance Show ImageSubresourceLayers

instance ToCStruct ImageSubresourceLayers where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSubresourceLayers{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (aspectMask)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (mipLevel)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (baseArrayLayer)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (layerCount)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct ImageSubresourceLayers where
  peekCStruct p = do
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 0 :: Ptr ImageAspectFlags))
    mipLevel <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    baseArrayLayer <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    layerCount <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ ImageSubresourceLayers
             aspectMask mipLevel baseArrayLayer layerCount

instance Storable ImageSubresourceLayers where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSubresourceLayers where
  zero = ImageSubresourceLayers
           zero
           zero
           zero
           zero


-- | VkBufferCopy - Structure specifying a buffer copy operation
--
-- == Valid Usage
--
-- -   #VUID-VkBufferCopy-size-01988# The @size@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize', 'cmdCopyBuffer'
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
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferCopy)
#endif
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
-- For 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D' images, copies are
-- performed slice by slice starting with the @z@ member of the @srcOffset@
-- or @dstOffset@, and copying @depth@ slices. For images with multiple
-- layers, copies are performed layer by layer starting with the
-- @baseArrayLayer@ member of the @srcSubresource@ or @dstSubresource@ and
-- copying @layerCount@ layers. Image data /can/ be copied between images
-- with different image types. If one image is
-- 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D' and the other image is
-- 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D' with multiple layers, then
-- each slice is copied to or from a different layer.
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
-- -   #VUID-VkImageCopy-extent-00140# The number of slices of the @extent@
--     (for 3D) or layers of the @srcSubresource@ (for non-3D) /must/ match
--     the number of slices of the @extent@ (for 3D) or layers of the
--     @dstSubresource@ (for non-3D)
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageCopy-srcSubresource-parameter# @srcSubresource@ /must/
--     be a valid 'ImageSubresourceLayers' structure
--
-- -   #VUID-VkImageCopy-dstSubresource-parameter# @dstSubresource@ /must/
--     be a valid 'ImageSubresourceLayers' structure
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Extent3D', 'ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D', 'cmdCopyImage'
data ImageCopy = ImageCopy
  { -- | @srcSubresource@ and @dstSubresource@ are 'ImageSubresourceLayers'
    -- structures specifying the image subresources of the images used for the
    -- source and destination image data, respectively.
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
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCopy)
#endif
deriving instance Show ImageCopy

instance ToCStruct ImageCopy where
  withCStruct x f = allocaBytesAligned 68 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCopy{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (srcSubresource)
    poke ((p `plusPtr` 16 :: Ptr Offset3D)) (srcOffset)
    poke ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers)) (dstSubresource)
    poke ((p `plusPtr` 44 :: Ptr Offset3D)) (dstOffset)
    poke ((p `plusPtr` 56 :: Ptr Extent3D)) (extent)
    f
  cStructSize = 68
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct ImageCopy where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers))
    srcOffset <- peekCStruct @Offset3D ((p `plusPtr` 16 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers))
    dstOffset <- peekCStruct @Offset3D ((p `plusPtr` 44 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 56 :: Ptr Extent3D))
    pure $ ImageCopy
             srcSubresource srcOffset dstSubresource dstOffset extent

instance Storable ImageCopy where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

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
-- -   #VUID-VkImageBlit-aspectMask-00238# The @aspectMask@ member of
--     @srcSubresource@ and @dstSubresource@ /must/ match
--
-- -   #VUID-VkImageBlit-layerCount-00239# The @layerCount@ member of
--     @srcSubresource@ and @dstSubresource@ /must/ match
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageBlit-srcSubresource-parameter# @srcSubresource@ /must/
--     be a valid 'ImageSubresourceLayers' structure
--
-- -   #VUID-VkImageBlit-dstSubresource-parameter# @dstSubresource@ /must/
--     be a valid 'ImageSubresourceLayers' structure
--
-- = See Also
--
-- 'ImageSubresourceLayers', 'Vulkan.Core10.FundamentalTypes.Offset3D',
-- 'cmdBlitImage'
data ImageBlit = ImageBlit
  { -- | @srcSubresource@ is the subresource to blit from.
    srcSubresource :: ImageSubresourceLayers
  , -- | @srcOffsets@ is a pointer to an array of two
    -- 'Vulkan.Core10.FundamentalTypes.Offset3D' structures specifying the
    -- bounds of the source region within @srcSubresource@.
    srcOffsets :: (Offset3D, Offset3D)
  , -- | @dstSubresource@ is the subresource to blit into.
    dstSubresource :: ImageSubresourceLayers
  , -- | @dstOffsets@ is a pointer to an array of two
    -- 'Vulkan.Core10.FundamentalTypes.Offset3D' structures specifying the
    -- bounds of the destination region within @dstSubresource@.
    dstOffsets :: (Offset3D, Offset3D)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageBlit)
#endif
deriving instance Show ImageBlit

instance ToCStruct ImageBlit where
  withCStruct x f = allocaBytesAligned 80 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageBlit{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (srcSubresource)
    let pSrcOffsets' = lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray 2 Offset3D)))
    case (srcOffsets) of
      (e0, e1) -> do
        poke (pSrcOffsets' :: Ptr Offset3D) (e0)
        poke (pSrcOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    poke ((p `plusPtr` 40 :: Ptr ImageSubresourceLayers)) (dstSubresource)
    let pDstOffsets' = lowerArrayPtr ((p `plusPtr` 56 :: Ptr (FixedArray 2 Offset3D)))
    case (dstOffsets) of
      (e0, e1) -> do
        poke (pDstOffsets' :: Ptr Offset3D) (e0)
        poke (pDstOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    f
  cStructSize = 80
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (zero)
    let pSrcOffsets' = lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray 2 Offset3D)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pSrcOffsets' :: Ptr Offset3D) (e0)
        poke (pSrcOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    poke ((p `plusPtr` 40 :: Ptr ImageSubresourceLayers)) (zero)
    let pDstOffsets' = lowerArrayPtr ((p `plusPtr` 56 :: Ptr (FixedArray 2 Offset3D)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pDstOffsets' :: Ptr Offset3D) (e0)
        poke (pDstOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    f

instance FromCStruct ImageBlit where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers))
    let psrcOffsets = lowerArrayPtr @Offset3D ((p `plusPtr` 16 :: Ptr (FixedArray 2 Offset3D)))
    srcOffsets0 <- peekCStruct @Offset3D ((psrcOffsets `advancePtrBytes` 0 :: Ptr Offset3D))
    srcOffsets1 <- peekCStruct @Offset3D ((psrcOffsets `advancePtrBytes` 12 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 40 :: Ptr ImageSubresourceLayers))
    let pdstOffsets = lowerArrayPtr @Offset3D ((p `plusPtr` 56 :: Ptr (FixedArray 2 Offset3D)))
    dstOffsets0 <- peekCStruct @Offset3D ((pdstOffsets `advancePtrBytes` 0 :: Ptr Offset3D))
    dstOffsets1 <- peekCStruct @Offset3D ((pdstOffsets `advancePtrBytes` 12 :: Ptr Offset3D))
    pure $ ImageBlit
             srcSubresource ((srcOffsets0, srcOffsets1)) dstSubresource ((dstOffsets0, dstOffsets1))

instance Storable ImageBlit where
  sizeOf ~_ = 80
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

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
--     'Vulkan.Core10.Enums.Format.FORMAT_S8_UINT' value per texel.
--
-- -   data copied to or from the depth aspect of a
--     'Vulkan.Core10.Enums.Format.FORMAT_D16_UNORM' or
--     'Vulkan.Core10.Enums.Format.FORMAT_D16_UNORM_S8_UINT' format is
--     tightly packed with one
--     'Vulkan.Core10.Enums.Format.FORMAT_D16_UNORM' value per texel.
--
-- -   data copied to or from the depth aspect of a
--     'Vulkan.Core10.Enums.Format.FORMAT_D32_SFLOAT' or
--     'Vulkan.Core10.Enums.Format.FORMAT_D32_SFLOAT_S8_UINT' format is
--     tightly packed with one
--     'Vulkan.Core10.Enums.Format.FORMAT_D32_SFLOAT' value per texel.
--
-- -   data copied to or from the depth aspect of a
--     'Vulkan.Core10.Enums.Format.FORMAT_X8_D24_UNORM_PACK32' or
--     'Vulkan.Core10.Enums.Format.FORMAT_D24_UNORM_S8_UINT' format is
--     packed with one 32-bit word per texel with the D24 value in the LSBs
--     of the word, and undefined values in the eight MSBs.
--
-- Note
--
-- To copy both the depth and stencil aspects of a depth\/stencil format,
-- two entries in @pRegions@ /can/ be used, where one specifies the depth
-- aspect in @imageSubresource@, and the other specifies the stencil
-- aspect.
--
-- Because depth or stencil aspect buffer to image copies /may/ require
-- format conversions on some implementations, they are not supported on
-- queues that do not support graphics.
--
-- When copying to a depth aspect, and the
-- @VK_EXT_depth_range_unrestricted@ extension is not enabled, the data in
-- buffer memory /must/ be in the range [0,1], or the resulting values are
-- undefined.
--
-- Copies are done layer by layer starting with image layer
-- @baseArrayLayer@ member of @imageSubresource@. @layerCount@ layers are
-- copied from the source image or to the destination image.
--
-- For purpose of valid usage statements here and in related copy commands,
-- a /blocked image/ is defined as:
--
-- -   a image with a /single-plane/, “@_422@” format, which is treated as
--     a format with a 2 × 1 compressed texel block, or
--
-- -   a compressed image.
--
-- == Valid Usage
--
-- -   #VUID-VkBufferImageCopy-bufferRowLength-00195# @bufferRowLength@
--     /must/ be @0@, or greater than or equal to the @width@ member of
--     @imageExtent@
--
-- -   #VUID-VkBufferImageCopy-bufferImageHeight-00196# @bufferImageHeight@
--     /must/ be @0@, or greater than or equal to the @height@ member of
--     @imageExtent@
--
-- -   #VUID-VkBufferImageCopy-aspectMask-00212# The @aspectMask@ member of
--     @imageSubresource@ /must/ only have a single bit set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferImageCopy-imageSubresource-parameter#
--     @imageSubresource@ /must/ be a valid 'ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.FundamentalTypes.Extent3D', 'ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D', 'cmdCopyBufferToImage',
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
  , -- | @imageSubresource@ is a 'ImageSubresourceLayers' used to specify the
    -- specific image subresources of the image used for the source or
    -- destination image data.
    imageSubresource :: ImageSubresourceLayers
  , -- | @imageOffset@ selects the initial @x@, @y@, @z@ offsets in texels of the
    -- sub-region of the source or destination image data.
    imageOffset :: Offset3D
  , -- | @imageExtent@ is the size in texels of the image to copy in @width@,
    -- @height@ and @depth@.
    imageExtent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferImageCopy)
#endif
deriving instance Show BufferImageCopy

instance ToCStruct BufferImageCopy where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferImageCopy{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (bufferOffset)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (bufferRowLength)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (bufferImageHeight)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (imageSubresource)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (imageOffset)
    poke ((p `plusPtr` 44 :: Ptr Extent3D)) (imageExtent)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Extent3D)) (zero)
    f

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

instance Storable BufferImageCopy where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

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
-- -   #VUID-VkImageResolve-aspectMask-00266# The @aspectMask@ member of
--     @srcSubresource@ and @dstSubresource@ /must/ only contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkImageResolve-layerCount-00267# The @layerCount@ member of
--     @srcSubresource@ and @dstSubresource@ /must/ match
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageResolve-srcSubresource-parameter# @srcSubresource@
--     /must/ be a valid 'ImageSubresourceLayers' structure
--
-- -   #VUID-VkImageResolve-dstSubresource-parameter# @dstSubresource@
--     /must/ be a valid 'ImageSubresourceLayers' structure
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Extent3D', 'ImageSubresourceLayers',
-- 'Vulkan.Core10.FundamentalTypes.Offset3D', 'cmdResolveImage'
data ImageResolve = ImageResolve
  { -- | @srcSubresource@ and @dstSubresource@ are 'ImageSubresourceLayers'
    -- structures specifying the image subresources of the images used for the
    -- source and destination image data, respectively. Resolve of
    -- depth\/stencil images is not supported.
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
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageResolve)
#endif
deriving instance Show ImageResolve

instance ToCStruct ImageResolve where
  withCStruct x f = allocaBytesAligned 68 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageResolve{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (srcSubresource)
    poke ((p `plusPtr` 16 :: Ptr Offset3D)) (srcOffset)
    poke ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers)) (dstSubresource)
    poke ((p `plusPtr` 44 :: Ptr Offset3D)) (dstOffset)
    poke ((p `plusPtr` 56 :: Ptr Extent3D)) (extent)
    f
  cStructSize = 68
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct ImageResolve where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers))
    srcOffset <- peekCStruct @Offset3D ((p `plusPtr` 16 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers))
    dstOffset <- peekCStruct @Offset3D ((p `plusPtr` 44 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 56 :: Ptr Extent3D))
    pure $ ImageResolve
             srcSubresource srcOffset dstSubresource dstOffset extent

instance Storable ImageResolve where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

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
-- all layers of @framebuffer@. The application /must/ ensure (using
-- scissor if necessary) that all rendering is contained within the render
-- area. The render area, after any transform specified by
-- 'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'::@transform@
-- is applied, /must/ be contained within the framebuffer dimensions.
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-renderpass-transform render pass transform>
-- is enabled, then @renderArea@ /must/ equal the framebuffer
-- pre-transformed dimensions. After @renderArea@ has been transformed by
-- 'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'::@transform@,
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
-- -   #VUID-VkRenderPassBeginInfo-clearValueCount-00902# @clearValueCount@
--     /must/ be greater than the largest attachment index in @renderPass@
--     that specifies a @loadOp@ (or @stencilLoadOp@, if the attachment has
--     a depth\/stencil format) of
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'
--
-- -   #VUID-VkRenderPassBeginInfo-renderPass-00904# @renderPass@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Vulkan.Core10.Pass.FramebufferCreateInfo' structure specified when
--     creating @framebuffer@
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02850# If the @pNext@ chain does
--     not contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.x@ /must/ be greater than or equal to 0
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02851# If the @pNext@ chain does
--     not contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.y@ /must/ be greater than or equal to 0
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02852# If the @pNext@ chain does
--     not contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.x@ + @renderArea.offset.width@ /must/ be less
--     than or equal to 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@width@
--     the @framebuffer@ was created with
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02853# If the @pNext@ chain does
--     not contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.y@ + @renderArea.offset.height@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.Pass.FramebufferCreateInfo'::@height@ the
--     @framebuffer@ was created with
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02854# If the @pNext@ chain
--     contains
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     the @offset.x@ member of each element of @pDeviceRenderAreas@ /must/
--     be greater than or equal to 0
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02855# If the @pNext@ chain
--     contains
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     the @offset.y@ member of each element of @pDeviceRenderAreas@ /must/
--     be greater than or equal to 0
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02856# If the @pNext@ chain
--     contains
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     @offset.x@ + @offset.width@ of each element of @pDeviceRenderAreas@
--     /must/ be less than or equal to
--     'Vulkan.Core10.Pass.FramebufferCreateInfo'::@width@ the
--     @framebuffer@ was created with
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02857# If the @pNext@ chain
--     contains
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     @offset.y@ + @offset.height@ of each element of @pDeviceRenderAreas@
--     /must/ be less than or equal to
--     'Vulkan.Core10.Pass.FramebufferCreateInfo'::@height@ the
--     @framebuffer@ was created with
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-03207# If @framebuffer@ was
--     created with a 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@flags@
--     value that did not include
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     and the @pNext@ chain includes a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure, its @attachmentCount@ /must/ be zero
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-03208# If @framebuffer@ was
--     created with a 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@flags@
--     value that included
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     the @attachmentCount@ of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be equal to the value
--     of
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@attachmentImageInfoCount@
--     used to create @framebuffer@
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-02780# If @framebuffer@ was
--     created with a 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@flags@
--     value that included
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ have been created on
--     the same 'Vulkan.Core10.Handles.Device' as @framebuffer@ and
--     @renderPass@
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-03209# If @framebuffer@ was
--     created with a 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@flags@
--     value that included
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Vulkan.Core10.Handles.ImageView' of an image created with a value
--     of 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ equal to the
--     @flags@ member of the corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachmentImageInfos@
--     used to create @framebuffer@
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-04627# If @framebuffer@ was
--     created with a 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@flags@
--     value that included
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Vulkan.Core10.Handles.ImageView' with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-inherited-usage an inherited usage>
--     equal to the @usage@ member of the corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachmentImageInfos@
--     used to create @framebuffer@
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-03211# If @framebuffer@ was
--     created with a 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@flags@
--     value that included
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Vulkan.Core10.Handles.ImageView' with a width equal to the @width@
--     member of the corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachmentImageInfos@
--     used to create @framebuffer@
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-03212# If @framebuffer@ was
--     created with a 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@flags@
--     value that included
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Vulkan.Core10.Handles.ImageView' with a height equal to the
--     @height@ member of the corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachmentImageInfos@
--     used to create @framebuffer@
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-03213# If @framebuffer@ was
--     created with a 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@flags@
--     value that included
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Vulkan.Core10.Handles.ImageView' of an image created with a value
--     of
--     'Vulkan.Core10.ImageView.ImageViewCreateInfo'::@subresourceRange.layerCount@
--     equal to the @layerCount@ member of the corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachmentImageInfos@
--     used to create @framebuffer@
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-03214# If @framebuffer@ was
--     created with a 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@flags@
--     value that included
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Vulkan.Core10.Handles.ImageView' of an image created with a value
--     of
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@viewFormatCount@
--     equal to the @viewFormatCount@ member of the corresponding element
--     of
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachmentImageInfos@
--     used to create @framebuffer@
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-03215# If @framebuffer@ was
--     created with a 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@flags@
--     value that included
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Vulkan.Core10.Handles.ImageView' of an image created with a set of
--     elements in
--     'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo'::@pViewFormats@
--     equal to the set of elements in the @pViewFormats@ member of the
--     corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo'::@pAttachmentImageInfos@
--     used to create @framebuffer@
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-03216# If @framebuffer@ was
--     created with a 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@flags@
--     value that included
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Vulkan.Core10.Handles.ImageView' of an image created with a value
--     of 'Vulkan.Core10.ImageView.ImageViewCreateInfo'::@format@ equal to
--     the corresponding value of
--     'Vulkan.Core10.Pass.AttachmentDescription'::@format@ in @renderPass@
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-03217# If @framebuffer@ was
--     created with a 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@flags@
--     value that included
--     'Vulkan.Core10.Enums.FramebufferCreateFlagBits.FRAMEBUFFER_CREATE_IMAGELESS_BIT',
--     each element of the @pAttachments@ member of a
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo'
--     structure included in the @pNext@ chain /must/ be a
--     'Vulkan.Core10.Handles.ImageView' of an image created with a value
--     of 'Vulkan.Core10.Image.ImageCreateInfo'::@samples@ equal to the
--     corresponding value of
--     'Vulkan.Core10.Pass.AttachmentDescription'::@samples@ in
--     @renderPass@
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02869# If the @pNext@ chain
--     includes
--     'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM',
--     @renderArea.offset@ /must/ equal (0,0)
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02870# If the @pNext@ chain
--     includes
--     'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM',
--     @renderArea.extent@ transformed by
--     'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'::@transform@
--     /must/ equal the @framebuffer@ dimensions
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderPassBeginInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO'
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo',
--     'Vulkan.Extensions.VK_EXT_sample_locations.RenderPassSampleLocationsBeginInfoEXT',
--     or
--     'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'
--
-- -   #VUID-VkRenderPassBeginInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkRenderPassBeginInfo-renderPass-parameter# @renderPass@
--     /must/ be a valid 'Vulkan.Core10.Handles.RenderPass' handle
--
-- -   #VUID-VkRenderPassBeginInfo-framebuffer-parameter# @framebuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.Framebuffer' handle
--
-- -   #VUID-VkRenderPassBeginInfo-pClearValues-parameter# If
--     @clearValueCount@ is not @0@, @pClearValues@ /must/ be a valid
--     pointer to an array of @clearValueCount@ 'ClearValue' unions
--
-- -   #VUID-VkRenderPassBeginInfo-commonparent# Both of @framebuffer@, and
--     @renderPass@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'ClearValue', 'Vulkan.Core10.Handles.Framebuffer',
-- 'Vulkan.Core10.FundamentalTypes.Rect2D',
-- 'Vulkan.Core10.Handles.RenderPass',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdBeginRenderPass',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.cmdBeginRenderPass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.cmdBeginRenderPass2KHR'
data RenderPassBeginInfo (es :: [Type]) = RenderPassBeginInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @renderPass@ is the render pass to begin an instance of.
    renderPass :: RenderPass
  , -- | @framebuffer@ is the framebuffer containing the attachments that are
    -- used with the render pass.
    framebuffer :: Framebuffer
  , -- | @renderArea@ is the render area that is affected by the render pass
    -- instance, and is described in more detail below.
    renderArea :: Rect2D
  , -- | @pClearValues@ is a pointer to an array of @clearValueCount@
    -- 'ClearValue' structures that contains clear values for each attachment,
    -- if the attachment uses a @loadOp@ value of
    -- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR' or if
    -- the attachment has a depth\/stencil format and uses a @stencilLoadOp@
    -- value of
    -- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'. The
    -- array is indexed by attachment number. Only elements corresponding to
    -- cleared attachments are used. Other elements of @pClearValues@ are
    -- ignored.
    clearValues :: Vector ClearValue
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassBeginInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RenderPassBeginInfo es)

instance Extensible RenderPassBeginInfo where
  extensibleTypeName = "RenderPassBeginInfo"
  setNext x next = x{next = next}
  getNext RenderPassBeginInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RenderPassBeginInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @RenderPassTransformBeginInfoQCOM = Just f
    | Just Refl <- eqT @e @RenderPassAttachmentBeginInfo = Just f
    | Just Refl <- eqT @e @RenderPassSampleLocationsBeginInfoEXT = Just f
    | Just Refl <- eqT @e @DeviceGroupRenderPassBeginInfo = Just f
    | otherwise = Nothing

instance (Extendss RenderPassBeginInfo es, PokeChain es) => ToCStruct (RenderPassBeginInfo es) where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassBeginInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderPass)) (renderPass)
    lift $ poke ((p `plusPtr` 24 :: Ptr Framebuffer)) (framebuffer)
    lift $ poke ((p `plusPtr` 32 :: Ptr Rect2D)) (renderArea)
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
    lift $ poke ((p `plusPtr` 32 :: Ptr Rect2D)) (zero)
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


-- | VkClearDepthStencilValue - Structure specifying a clear depth stencil
-- value
--
-- == Valid Usage
--
-- -   #VUID-VkClearDepthStencilValue-depth-00022# Unless the
--     @VK_EXT_depth_range_unrestricted@ extension is enabled @depth@
--     /must/ be between @0.0@ and @1.0@, inclusive
--
-- = See Also
--
-- 'ClearValue', 'cmdClearDepthStencilImage'
data ClearDepthStencilValue = ClearDepthStencilValue
  { -- | @depth@ is the clear value for the depth aspect of the depth\/stencil
    -- attachment. It is a floating-point value which is automatically
    -- converted to the attachment’s format.
    depth :: Float
  , -- | @stencil@ is the clear value for the stencil aspect of the
    -- depth\/stencil attachment. It is a 32-bit integer value which is
    -- converted to the attachment’s format by taking the appropriate number of
    -- LSBs.
    stencil :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ClearDepthStencilValue)
#endif
deriving instance Show ClearDepthStencilValue

instance ToCStruct ClearDepthStencilValue where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ClearDepthStencilValue{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (depth))
    poke ((p `plusPtr` 4 :: Ptr Word32)) (stencil)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct ClearDepthStencilValue where
  peekCStruct p = do
    depth <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    stencil <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ ClearDepthStencilValue
             (coerce @CFloat @Float depth) stencil

instance Storable ClearDepthStencilValue where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ClearDepthStencilValue where
  zero = ClearDepthStencilValue
           zero
           zero


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
-- 'Vulkan.Core10.Pass.AttachmentDescription' to
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR', as
-- described for 'Vulkan.Core10.Pass.createRenderPass'.
--
-- == Valid Usage
--
-- -   #VUID-VkClearAttachment-aspectMask-00019# If @aspectMask@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT', it
--     /must/ not include
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkClearAttachment-aspectMask-00020# @aspectMask@ /must/ not
--     include
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_METADATA_BIT'
--
-- -   #VUID-VkClearAttachment-aspectMask-02246# @aspectMask@ /must/ not
--     include @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ for any index @i@
--
-- -   #VUID-VkClearAttachment-clearValue-00021# @clearValue@ /must/ be a
--     valid 'ClearValue' union
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkClearAttachment-aspectMask-parameter# @aspectMask@ /must/ be
--     a valid combination of
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits' values
--
-- -   #VUID-VkClearAttachment-aspectMask-requiredbitmask# @aspectMask@
--     /must/ not be @0@
--
-- = See Also
--
-- 'ClearValue',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- 'cmdClearAttachments'
data ClearAttachment = ClearAttachment
  { -- | @aspectMask@ is a mask selecting the color, depth and\/or stencil
    -- aspects of the attachment to be cleared.
    aspectMask :: ImageAspectFlags
  , -- | @colorAttachment@ is only meaningful if
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT' is set
    -- in @aspectMask@, in which case it is an index to the @pColorAttachments@
    -- array in the 'Vulkan.Core10.Pass.SubpassDescription' structure of the
    -- current subpass which selects the color attachment to clear.
    colorAttachment :: Word32
  , -- | @clearValue@ is the color or depth\/stencil value to clear the
    -- attachment to, as described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears-values Clear Values>
    -- below.
    clearValue :: ClearValue
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ClearAttachment)
#endif
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


data ClearColorValue
  = Float32 Float Float Float Float
  | Int32 Int32 Int32 Int32 Int32
  | Uint32 Word32 Word32 Word32 Word32
  deriving (Show)

instance ToCStruct ClearColorValue where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr ClearColorValue -> ClearColorValue -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    Float32 v0 v1 v2 v3 -> lift $ do
      let pFloat32 = lowerArrayPtr (castPtr @_ @(FixedArray 4 CFloat) p)
      case ((v0, v1, v2, v3)) of
        (e0, e1, e2, e3) -> do
          poke (pFloat32 :: Ptr CFloat) (CFloat (e0))
          poke (pFloat32 `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
          poke (pFloat32 `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
          poke (pFloat32 `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    Int32 v0 v1 v2 v3 -> lift $ do
      let pInt32 = lowerArrayPtr (castPtr @_ @(FixedArray 4 Int32) p)
      case ((v0, v1, v2, v3)) of
        (e0, e1, e2, e3) -> do
          poke (pInt32 :: Ptr Int32) (e0)
          poke (pInt32 `plusPtr` 4 :: Ptr Int32) (e1)
          poke (pInt32 `plusPtr` 8 :: Ptr Int32) (e2)
          poke (pInt32 `plusPtr` 12 :: Ptr Int32) (e3)
    Uint32 v0 v1 v2 v3 -> lift $ do
      let pUint32 = lowerArrayPtr (castPtr @_ @(FixedArray 4 Word32) p)
      case ((v0, v1, v2, v3)) of
        (e0, e1, e2, e3) -> do
          poke (pUint32 :: Ptr Word32) (e0)
          poke (pUint32 `plusPtr` 4 :: Ptr Word32) (e1)
          poke (pUint32 `plusPtr` 8 :: Ptr Word32) (e2)
          poke (pUint32 `plusPtr` 12 :: Ptr Word32) (e3)
  pokeZeroCStruct :: Ptr ClearColorValue -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 16
  cStructAlignment = 4

instance Zero ClearColorValue where
  zero = Float32 zero zero zero zero


data ClearValue
  = Color ClearColorValue
  | DepthStencil ClearDepthStencilValue
  deriving (Show)

instance ToCStruct ClearValue where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr ClearValue -> ClearValue -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    Color v -> ContT $ pokeCStruct (castPtr @_ @ClearColorValue p) (v) . ($ ())
    DepthStencil v -> lift $ poke (castPtr @_ @ClearDepthStencilValue p) (v)
  pokeZeroCStruct :: Ptr ClearValue -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 16
  cStructAlignment = 4

instance Zero ClearValue where
  zero = Color zero

