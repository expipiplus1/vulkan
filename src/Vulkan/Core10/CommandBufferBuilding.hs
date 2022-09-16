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
import Foreign.Marshal.Alloc (allocaBytes)
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
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#dispatch dispatching commands>.
--
-- -   The pipeline bound to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--     controls the behavior of all
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#drawing drawing commands>.
--
-- -   The pipeline bound to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_RAY_TRACING_KHR'
--     controls the behavior of
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysKHR' and
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysIndirectKHR'.
--
-- -   The pipeline bound to
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI'
--     controls the behavior of
--     'Vulkan.Extensions.VK_HUAWEI_subpass_shading.cmdSubpassShadingHUAWEI'.
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-variableMultisampleRate variableMultisampleRate>
--     feature is not supported, @pipeline@ is a graphics pipeline, the
--     current subpass
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-noattachments uses no attachments>,
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
--     @pipeline@ /must/ be a ray tracing pipeline
--
-- -   #VUID-vkCmdBindPipeline-pipelineBindPoint-06721# If
--     @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_RAY_TRACING_KHR',
--     @commandBuffer@ /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdBindPipeline-pipeline-03382# @pipeline@ /must/ not have
--     been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--     set
--
-- -   #VUID-vkCmdBindPipeline-commandBuffer-04808# If @commandBuffer@ is a
--     secondary command buffer with
--     'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV'::@viewportScissor2D@
--     enabled and @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS',
--     then the @pipeline@ /must/ have been created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     or 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT', and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     or 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR' enabled
--
-- -   #VUID-vkCmdBindPipeline-commandBuffer-04809# If @commandBuffer@ is a
--     secondary command buffer with
--     'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV'::@viewportScissor2D@
--     enabled and @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--     and @pipeline@ was created with
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'
--     structure and its @discardRectangleCount@ member is not @0@, then
--     the pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT'
--     enabled
--
-- -   #VUID-vkCmdBindPipeline-pipelineBindPoint-04881# If
--     @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS'
--     and the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-provokingVertexModePerPipeline provokingVertexModePerPipeline>
--     limit is 'Vulkan.Core10.FundamentalTypes.FALSE', then pipeline’s
--     'Vulkan.Extensions.VK_EXT_provoking_vertex.PipelineRasterizationProvokingVertexStateCreateInfoEXT'::@provokingVertexMode@
--     /must/ be the same as that of any other pipelines previously bound
--     to this bind point within the current render pass instance,
--     including any pipeline already bound when beginning the render pass
--     instance
--
-- -   #VUID-vkCmdBindPipeline-pipelineBindPoint-04949# If
--     @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI',
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdBindPipeline-pipelineBindPoint-04950# If
--     @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_SUBPASS_SHADING_HUAWEI',
--     @pipeline@ /must/ be a subpass shading pipeline
--
-- -   #VUID-vkCmdBindPipeline-pipeline-06195# If @pipeline@ is a graphics
--     pipeline, this command has been called inside a render pass instance
--     started with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     and commands using the previously bound graphics pipeline have been
--     recorded within the render pass instance, then the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--     specified by this pipeline /must/ match that set in the previous
--     pipeline
--
-- -   #VUID-vkCmdBindPipeline-pipeline-06196# If @pipeline@ is a graphics
--     pipeline, this command has been called inside a render pass instance
--     started with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     and commands using the previously bound graphics pipeline have been
--     recorded within the render pass instance, then the elements of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     specified by this pipeline /must/ match that set in the previous
--     pipeline
--
-- -   #VUID-vkCmdBindPipeline-pipeline-06197# If @pipeline@ is a graphics
--     pipeline, this command has been called inside a render pass instance
--     started with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     and commands using the previously bound graphics pipeline have been
--     recorded within the render pass instance, then the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     specified by this pipeline /must/ match that set in the previous
--     pipeline
--
-- -   #VUID-vkCmdBindPipeline-pipeline-06194# If @pipeline@ is a graphics
--     pipeline, this command has been called inside a render pass instance
--     started with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     and commands using the previously bound graphics pipeline have been
--     recorded within the render pass instance, then the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     specified by this pipeline /must/ match that set in the previous
--     pipeline
--
-- -   #VUID-vkCmdBindPipeline-pipeline-06856# If @pipeline@ is a graphics
--     pipeline, this command has been called inside a render pass instance
--     started with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     and the @pNext@ chain of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'
--     includes a
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'
--     structure with @multisampledRenderToSingleSampledEnable@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the value of
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pMultisampleState@::@rasterizationSamples@
--     /must/ be equal to
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'::@rasterizationSamples@.
--
-- -   #VUID-vkCmdBindPipeline-pipelineBindPoint-06653# If
--     @pipelineBindPoint@ is
--     'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS',
--     @pipeline@ /must/ have been created without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
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
-- -   #VUID-vkCmdBindPipeline-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdBindPipelinePtr = pVkCmdBindPipeline (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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

-- | vkCmdSetViewport - Set the viewport dynamically for a command buffer
--
-- = Description
--
-- This command sets the viewport transformation parameters state for
-- subsequent drawing commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@pViewports@
-- values used to create the currently active pipeline.
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiViewport multiViewport>
--     feature is not enabled, @firstViewport@ /must/ be @0@
--
-- -   #VUID-vkCmdSetViewport-viewportCount-01225# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiViewport multiViewport>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- -   #VUID-vkCmdSetViewport-commandBuffer-04821# @commandBuffer@ /must/
--     not have
--     'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV'::@viewportScissor2D@
--     enabled
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
-- -   #VUID-vkCmdSetViewport-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdSetViewportPtr = pVkCmdSetViewport (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetViewportPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetViewport is null" Nothing Nothing
  let vkCmdSetViewport' = mkVkCmdSetViewport vkCmdSetViewportPtr
  pPViewports <- ContT $ allocaBytes @Viewport ((Data.Vector.length (viewports)) * 24)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPViewports `plusPtr` (24 * (i)) :: Ptr Viewport) (e)) (viewports)
  lift $ traceAroundEvent "vkCmdSetViewport" (vkCmdSetViewport' (commandBufferHandle (commandBuffer)) (firstViewport) ((fromIntegral (Data.Vector.length $ (viewports)) :: Word32)) (pPViewports))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetScissor
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()

-- | vkCmdSetScissor - Set scissor rectangles dynamically for a command
-- buffer
--
-- = Description
--
-- The scissor rectangles taken from element i of @pScissors@ replace the
-- current state for the scissor index @firstScissor@ + i, for i in [0,
-- @scissorCount@).
--
-- This command sets the scissor rectangles for subsequent drawing commands
-- when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@pScissors@
-- values used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetScissor-firstScissor-00592# The sum of @firstScissor@
--     and @scissorCount@ /must/ be between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   #VUID-vkCmdSetScissor-firstScissor-00593# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiViewport multiViewport>
--     feature is not enabled, @firstScissor@ /must/ be @0@
--
-- -   #VUID-vkCmdSetScissor-scissorCount-00594# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiViewport multiViewport>
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
-- -   #VUID-vkCmdSetScissor-viewportScissor2D-04789# If this command is
--     recorded in a secondary command buffer with
--     'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV'::@viewportScissor2D@
--     enabled, then this function /must/ not be called
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
-- -   #VUID-vkCmdSetScissor-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdSetScissorPtr = pVkCmdSetScissor (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetScissorPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetScissor is null" Nothing Nothing
  let vkCmdSetScissor' = mkVkCmdSetScissor vkCmdSetScissorPtr
  pPScissors <- ContT $ allocaBytes @Rect2D ((Data.Vector.length (scissors)) * 16)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (scissors)
  lift $ traceAroundEvent "vkCmdSetScissor" (vkCmdSetScissor' (commandBufferHandle (commandBuffer)) (firstScissor) ((fromIntegral (Data.Vector.length $ (scissors)) :: Word32)) (pPScissors))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetLineWidth
  :: FunPtr (Ptr CommandBuffer_T -> CFloat -> IO ()) -> Ptr CommandBuffer_T -> CFloat -> IO ()

-- | vkCmdSetLineWidth - Set line width dynamically for a command buffer
--
-- = Description
--
-- This command sets the line width for subsequent drawing commands when
-- the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_WIDTH' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@lineWidth@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetLineWidth-lineWidth-00788# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-wideLines wideLines>
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
-- -   #VUID-vkCmdSetLineWidth-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdSetLineWidthPtr = pVkCmdSetLineWidth (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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

-- | vkCmdSetDepthBias - Set depth bias factors and clamp dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the depth bias parameters for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the corresponding
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@depthBiasConstantFactor@,
-- @depthBiasClamp@, and @depthBiasSlopeFactor@ values used to create the
-- currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDepthBias-depthBiasClamp-00790# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-depthBiasClamp depthBiasClamp>
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
-- -   #VUID-vkCmdSetDepthBias-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdSetDepthBiasPtr = pVkCmdSetDepthBias (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- = Description
--
-- This command sets blend constants for subsequent drawing commands when
-- the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_BLEND_CONSTANTS' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'::@blendConstants@
-- values used to create the currently active pipeline.
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
-- -   #VUID-vkCmdSetBlendConstants-videocoding# This command /must/ only
--     be called outside of a video coding scope
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetBlendConstants :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer into which the command will be
                        -- recorded.
                        CommandBuffer
                     -> -- | @blendConstants@ is a pointer to an array of four values specifying the
                        -- Rc, Gc, Bc, and Ac components of the blend constant color used in
                        -- blending, depending on the
                        -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#framebuffer-blendfactors blend factor>.
                        ("blendConstants" ::: (Float, Float, Float, Float))
                     -> io ()
cmdSetBlendConstants commandBuffer blendConstants = liftIO . evalContT $ do
  let vkCmdSetBlendConstantsPtr = pVkCmdSetBlendConstants (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetBlendConstantsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetBlendConstants is null" Nothing Nothing
  let vkCmdSetBlendConstants' = mkVkCmdSetBlendConstants vkCmdSetBlendConstantsPtr
  pBlendConstants <- ContT $ allocaBytes @(FixedArray 4 CFloat) 16
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

-- | vkCmdSetDepthBounds - Set depth bounds range dynamically for a command
-- buffer
--
-- = Description
--
-- This command sets the depth bounds range for subsequent drawing commands
-- when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@minDepthBounds@
-- and
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@maxDepthBounds@
-- values used to create the currently active pipeline.
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
-- -   #VUID-vkCmdSetDepthBounds-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdSetDepthBoundsPtr = pVkCmdSetDepthBounds (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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

-- | vkCmdSetStencilCompareMask - Set stencil compare mask dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the stencil compare mask for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_COMPARE_MASK'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.StencilOpState'::@compareMask@ value used to
-- create the currently active pipeline, for both front and back faces.
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
-- -   #VUID-vkCmdSetStencilCompareMask-videocoding# This command /must/
--     only be called outside of a video coding scope
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdSetStencilCompareMaskPtr = pVkCmdSetStencilCompareMask (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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

-- | vkCmdSetStencilWriteMask - Set stencil write mask dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the stencil write mask for subsequent drawing commands
-- when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_WRITE_MASK' set
-- in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@writeMask@
-- value used to create the currently active pipeline, for both front and
-- back faces.
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
-- -   #VUID-vkCmdSetStencilWriteMask-videocoding# This command /must/ only
--     be called outside of a video coding scope
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdSetStencilWriteMaskPtr = pVkCmdSetStencilWriteMask (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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

-- | vkCmdSetStencilReference - Set stencil reference value dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the stencil reference value for subsequent drawing
-- commands when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_REFERENCE' set
-- in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'::@reference@
-- value used to create the currently active pipeline, for both front and
-- back faces.
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
-- -   #VUID-vkCmdSetStencilReference-videocoding# This command /must/ only
--     be called outside of a video coding scope
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdSetStencilReferencePtr = pVkCmdSetStencilReference (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- 'cmdBindDescriptorSets' binds descriptor sets
-- @pDescriptorSets@[0..@descriptorSetCount@-1] to set numbers
-- [@firstSet@..@firstSet@+@descriptorSetCount@-1] for subsequent
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-bindpoint-commands bound pipeline commands>
-- set by @pipelineBindPoint@. Any bindings that were previously applied
-- via these sets are no longer valid.
--
-- Once bound, a descriptor set affects rendering of subsequent commands
-- that interact with the given pipeline type in the command buffer until
-- either a different set is bound to the same set number, or the set is
-- disturbed as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>.
--
-- A compatible descriptor set /must/ be bound for all set numbers that any
-- shaders in a pipeline access, at the time that a drawing or dispatching
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-bindpoint-commands bound pipeline commands>
-- with that pipeline type, as defined in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>
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
--     sum of the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#dynamic-effective-offset effective offset>
--     and the range of the binding /must/ be less than or equal to the
--     size of the buffer
--
-- -   #VUID-vkCmdBindDescriptorSets-pDescriptorSets-06715# For each
--     dynamic uniform or storage buffer binding in @pDescriptorSets@, if
--     the range was set with 'Vulkan.Core10.APIConstants.WHOLE_SIZE' then
--     @pDynamicOffsets@ which corresponds to the descriptor binding /must/
--     be 0
--
-- -   #VUID-vkCmdBindDescriptorSets-pDescriptorSets-04616# Each element of
--     @pDescriptorSets@ /must/ not have been allocated from a
--     'Vulkan.Core10.Handles.DescriptorPool' with the
--     'Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits.DESCRIPTOR_POOL_CREATE_HOST_ONLY_BIT_EXT'
--     flag set
--
-- -   #VUID-vkCmdBindDescriptorSets-graphicsPipelineLibrary-06754# If
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-graphicsPipelineLibrary graphicsPipelineLibrary>
--     is not enabled, each element of @pDescriptorSets@ /must/ be a valid
--     'Vulkan.Core10.Handles.DescriptorSet'
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
--     @descriptorSetCount@ valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     'Vulkan.Core10.Handles.DescriptorSet' handles
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
-- -   #VUID-vkCmdBindDescriptorSets-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- -   #VUID-vkCmdBindDescriptorSets-descriptorSetCount-arraylength#
--     @descriptorSetCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdBindDescriptorSets-commonparent# Each of @commandBuffer@,
--     @layout@, and the elements of @pDescriptorSets@ that are valid
--     handles of non-ignored parameters /must/ have been created,
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
                         -- sets to bind to.
                         ("descriptorSets" ::: Vector DescriptorSet)
                      -> -- | @pDynamicOffsets@ is a pointer to an array of @uint32_t@ values
                         -- specifying dynamic offsets.
                         ("dynamicOffsets" ::: Vector Word32)
                      -> io ()
cmdBindDescriptorSets commandBuffer pipelineBindPoint layout firstSet descriptorSets dynamicOffsets = liftIO . evalContT $ do
  let vkCmdBindDescriptorSetsPtr = pVkCmdBindDescriptorSets (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBindDescriptorSetsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindDescriptorSets is null" Nothing Nothing
  let vkCmdBindDescriptorSets' = mkVkCmdBindDescriptorSets vkCmdBindDescriptorSetsPtr
  pPDescriptorSets <- ContT $ allocaBytes @DescriptorSet ((Data.Vector.length (descriptorSets)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorSets `plusPtr` (8 * (i)) :: Ptr DescriptorSet) (e)) (descriptorSets)
  pPDynamicOffsets <- ContT $ allocaBytes @Word32 ((Data.Vector.length (dynamicOffsets)) * 4)
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-indexTypeUint8 indexTypeUint8>
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
-- -   #VUID-vkCmdBindIndexBuffer-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
                      -- specifying the size of the indices.
                      IndexType
                   -> io ()
cmdBindIndexBuffer commandBuffer buffer offset indexType = liftIO $ do
  let vkCmdBindIndexBufferPtr = pVkCmdBindIndexBuffer (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- for subsequent drawing commands. If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
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
-- -   #VUID-vkCmdBindVertexBuffers-videocoding# This command /must/ only
--     be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdBindVertexBuffersPtr = pVkCmdBindVertexBuffers (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBindVertexBuffersPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindVertexBuffers is null" Nothing Nothing
  let vkCmdBindVertexBuffers' = mkVkCmdBindVertexBuffers vkCmdBindVertexBuffersPtr
  let pBuffersLength = Data.Vector.length $ (buffers)
  lift $ unless ((Data.Vector.length $ (offsets)) == pBuffersLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pOffsets and pBuffers must have the same length" Nothing Nothing
  pPBuffers <- ContT $ allocaBytes @Buffer ((Data.Vector.length (buffers)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (buffers)
  pPOffsets <- ContT $ allocaBytes @DeviceSize ((Data.Vector.length (offsets)) * 8)
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
-- -   #VUID-vkCmdDraw-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDraw-None-06479# If a 'Vulkan.Core10.Handles.ImageView'
--     is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdDraw-None-02691# If a 'Vulkan.Core10.Handles.ImageView'
--     is accessed using atomic operations as a result of this command,
--     then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDraw-None-02692# If a 'Vulkan.Core10.Handles.ImageView'
--     is sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a
--     result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDraw-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDraw-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
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
-- -   #VUID-vkCmdDraw-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDraw-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDraw-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDraw-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
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
-- -   #VUID-vkCmdDraw-maintenance4-06425# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
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
--     /must/ have been set or inherited (if the
--     @VK_NV_inherited_viewport_scissor@ extension is enabled) for
--     @commandBuffer@, and done so after any previously bound pipeline
--     with the corresponding state not specified as dynamic
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
-- -   #VUID-vkCmdDraw-uniformBuffers-06935# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a uniform buffer, and that stage
--     was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDraw-storageBuffers-06936# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a storage buffer, and that stage
--     was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDraw-commandBuffer-02707# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDraw-None-06550# If the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a 'Vulkan.Core10.Handles.Sampler' or
--     'Vulkan.Core10.Handles.ImageView' object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ only be used with @OpImageSample*@ or
--     @OpImageSparseSample*@ instructions
--
-- -   #VUID-vkCmdDraw-ConstOffset-06551# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ not use the @ConstOffset@ and @Offset@ operands
--
-- -   #VUID-vkCmdDraw-None-04115# If a 'Vulkan.Core10.Handles.ImageView'
--     is accessed using @OpImageWrite@ as a result of this command, then
--     the @Type@ of the @Texel@ operand of that instruction /must/ have at
--     least as many components as the image view’s format
--
-- -   #VUID-vkCmdDraw-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdDraw-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDraw-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDraw-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDraw-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDraw-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDraw-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDraw-OpImageWeightedSampleQCOM-06971# If
--     @OpImageWeightedSampleQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDraw-OpImageWeightedSampleQCOM-06972# If
--     @OpImageWeightedSampleQCOM@ uses a 'Vulkan.Core10.Handles.ImageView'
--     as a sample weight image as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDraw-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdDraw-OpImageBlockMatchSSDQCOM-06974# If
--     @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDraw-OpImageBlockMatchSADQCOM-06975# If
--     @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDraw-OpImageBlockMatchSADQCOM-06976# If
--     @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>.
--
-- -   #VUID-vkCmdDraw-OpImageWeightedSampleQCOM-06977# If
--     @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdDraw-OpImageWeightedSampleQCOM-06978# If any command
--     other than @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ not have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdDraw-None-07288# Any shader invocation executed by this
--     command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
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
-- -   #VUID-vkCmdDraw-None-06537# Memory backing image subresources used
--     as attachments in the current render pass /must/ not be written in
--     any way other than as an attachment by this command
--
-- -   #VUID-vkCmdDraw-None-06538# If any recorded command in the current
--     subpass will write to an image subresource as an attachment, this
--     command /must/ not read from the memory backing that image
--     subresource in any other way than as an attachment
--
-- -   #VUID-vkCmdDraw-None-06539# If any recorded command in the current
--     subpass will read from an image subresource used as an attachment in
--     any way other than as an attachment, this command /must/ not write
--     to that image subresource as an attachment
--
-- -   #VUID-vkCmdDraw-None-06886# If the current render pass instance uses
--     a depth\/stencil attachment with a read-only layout for the depth
--     aspect,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth-write depth writes>
--     /must/ be disabled
--
-- -   #VUID-vkCmdDraw-None-06887# If the current render pass instance uses
--     a depth\/stencil attachment with a read-only layout for the stencil
--     aspect and stencil test is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil all stencil ops>
--     /must/ be 'Vulkan.Core10.Enums.StencilOp.STENCIL_OP_KEEP'
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
-- -   #VUID-vkCmdDraw-None-06666# If the bound graphics pipeline state was
--     created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDraw-viewportCount-03417# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDraw-scissorCount-03418# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @scissorCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDraw-viewportCount-03419# If the bound graphics pipeline
--     state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic states enabled then both
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     and
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--
-- -   #VUID-vkCmdDraw-viewportCount-04137# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDraw-viewportCount-04138# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDraw-viewportCount-04139# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDraw-viewportCount-04140# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDraw-VkPipelineVieportCreateInfo-04141# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo', then the
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDraw-VkPipelineVieportCreateInfo-04142# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo', then the
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDraw-None-04876# If the bound graphics pipeline state was
--     created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDraw-None-04877# If the bound graphics pipeline state was
--     created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDraw-logicOp-04878# If the bound graphics pipeline state
--     was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetLogicOpEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command and the @logicOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.LogicOp.LogicOp' value
--
-- -   #VUID-vkCmdDraw-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDraw-blendEnable-04727# If rasterization is not disabled
--     in the bound graphics pipeline, then for each color attachment in
--     the subpass, if the corresponding image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     do not contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT',
--     then the @blendEnable@ member of the corresponding element of the
--     @pAttachments@ member of @pColorBlendState@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdDraw-multisampledRenderToSingleSampled-07284# If
--     rasterization is not disabled in the bound graphics pipeline, and
--     none of the @VK_AMD_mixed_attachment_samples@ extension, the
--     @VK_NV_framebuffer_mixed_samples@ extension, or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature are enabled, then
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     /must/ be the same as the current subpass color and\/or
--     depth\/stencil attachments
--
-- -   #VUID-vkCmdDraw-imageView-06172# If the current render pass instance
--     was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDraw-imageView-06173# If the current render pass instance
--     was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDraw-imageView-06174# If the current render pass instance
--     was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDraw-imageView-06175# If the current render pass instance
--     was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDraw-imageView-06176# If the current render pass instance
--     was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDraw-imageView-06177# If the current render pass instance
--     was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDraw-viewMask-06178# If the current render pass instance
--     was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound graphics pipeline /must/ have been created with
--     a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@viewMask@
--
-- -   #VUID-vkCmdDraw-colorAttachmentCount-06179# If the current render
--     pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound graphics pipeline /must/ have been created with
--     a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--
-- -   #VUID-vkCmdDraw-colorAttachmentCount-06180# If the current render
--     pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a 'Vulkan.Core10.Enums.Format.Format' equal to the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDraw-attachmentCount-06667# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @attachmentCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ be greater than or equal to the
--     'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'::@attachmentCount@
--     of the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDraw-attachmentCount-06815# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @attachmentCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ be less than or equal to the @maxColorAttachments@ member of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'
--
-- -   #VUID-vkCmdDraw-pDepthAttachment-06181# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the 'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDraw-pStencilAttachment-06182# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the 'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDraw-imageView-06183# If the current render pass instance
--     was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the currently
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdDraw-imageView-06184# If the current render pass instance
--     was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the currently
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- -   #VUID-vkCmdDraw-colorAttachmentCount-06185# If the currently bound
--     pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a sample count equal to the corresponding element of the
--     @pColorAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDraw-pDepthAttachment-06186# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDraw-pStencilAttachment-06187# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDraw-multisampledRenderToSingleSampled-07285# If the
--     currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a sample count equal to the value of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDraw-multisampledRenderToSingleSampled-07286# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDraw-multisampledRenderToSingleSampled-07287# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDraw-renderPass-06198# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline /must/ have been created with a
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@renderPass@
--     equal to 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdDraw-primitivesGeneratedQueryWithRasterizerDiscard-06708#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithRasterizerDiscard primitivesGeneratedQueryWithRasterizerDiscard>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-discard rasterization discard>
--     /must/ not be enabled.
--
-- -   #VUID-vkCmdDraw-primitivesGeneratedQueryWithNonZeroStreams-06709# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithNonZeroStreams primitivesGeneratedQueryWithNonZeroStreams>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active, the bound graphics pipeline /must/ not have been
--     created with a non-zero value in
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@.
--
-- -   #VUID-vkCmdDraw-stage-07073# If the currently bound pipeline was
--     created with the
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of an element of
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ set
--     to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     then
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-mesh-shader Mesh Shader Queries>
--     must not be active
--
-- -   #VUID-vkCmdDraw-commandBuffer-02712# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource written to by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be an unprotected resource
--
-- -   #VUID-vkCmdDraw-commandBuffer-02713# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, pipeline stages other than the framebuffer-space
--     and compute stages in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command /must/ not
--     write to any resource
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
-- -   #VUID-vkCmdDraw-primitiveTopology-03420# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDraw-None-04912# If the bound graphics pipeline was
--     created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--     dynamic states enabled, then
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command
--
-- -   #VUID-vkCmdDraw-pStrides-04913# If the bound graphics pipeline was
--     created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @pStrides@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--     /must/ not be @NULL@
--
-- -   #VUID-vkCmdDraw-None-04914# If the bound graphics pipeline state was
--     created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command
--
-- -   #VUID-vkCmdDraw-None-04875# If the bound graphics pipeline state was
--     created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPatchControlPointsEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDraw-None-04879# If the bound graphics pipeline state was
--     created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDraw-stage-06481# The bound graphics pipeline /must/ not
--     have been created with the
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of an element of
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ set
--     to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
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
-- -   #VUID-vkCmdDraw-videocoding# This command /must/ only be called
--     outside of a video coding scope
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdDrawPtr = pVkCmdDraw (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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

-- | vkCmdDrawIndexed - Draw primitives with indexed vertices
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
-- -   #VUID-vkCmdDrawIndexed-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDrawIndexed-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
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
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndexed-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawIndexed-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
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
-- -   #VUID-vkCmdDrawIndexed-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndexed-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndexed-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndexed-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
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
-- -   #VUID-vkCmdDrawIndexed-maintenance4-06425# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
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
--     /must/ have been set or inherited (if the
--     @VK_NV_inherited_viewport_scissor@ extension is enabled) for
--     @commandBuffer@, and done so after any previously bound pipeline
--     with the corresponding state not specified as dynamic
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
-- -   #VUID-vkCmdDrawIndexed-uniformBuffers-06935# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a uniform buffer, and that stage
--     was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndexed-storageBuffers-06936# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a storage buffer, and that stage
--     was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndexed-commandBuffer-02707# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDrawIndexed-None-06550# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ only be used with @OpImageSample*@ or
--     @OpImageSparseSample*@ instructions
--
-- -   #VUID-vkCmdDrawIndexed-ConstOffset-06551# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ not use the @ConstOffset@ and @Offset@ operands
--
-- -   #VUID-vkCmdDrawIndexed-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format
--
-- -   #VUID-vkCmdDrawIndexed-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdDrawIndexed-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDrawIndexed-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDrawIndexed-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDrawIndexed-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDrawIndexed-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDrawIndexed-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDrawIndexed-OpImageWeightedSampleQCOM-06971# If
--     @OpImageWeightedSampleQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndexed-OpImageWeightedSampleQCOM-06972# If
--     @OpImageWeightedSampleQCOM@ uses a 'Vulkan.Core10.Handles.ImageView'
--     as a sample weight image as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndexed-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndexed-OpImageBlockMatchSSDQCOM-06974# If
--     @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndexed-OpImageBlockMatchSADQCOM-06975# If
--     @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndexed-OpImageBlockMatchSADQCOM-06976# If
--     @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>.
--
-- -   #VUID-vkCmdDrawIndexed-OpImageWeightedSampleQCOM-06977# If
--     @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdDrawIndexed-OpImageWeightedSampleQCOM-06978# If any
--     command other than @OpImageWeightedSampleQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdDrawIndexed-None-07288# Any shader invocation executed by
--     this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
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
-- -   #VUID-vkCmdDrawIndexed-None-06537# Memory backing image subresources
--     used as attachments in the current render pass /must/ not be written
--     in any way other than as an attachment by this command
--
-- -   #VUID-vkCmdDrawIndexed-None-06538# If any recorded command in the
--     current subpass will write to an image subresource as an attachment,
--     this command /must/ not read from the memory backing that image
--     subresource in any other way than as an attachment
--
-- -   #VUID-vkCmdDrawIndexed-None-06539# If any recorded command in the
--     current subpass will read from an image subresource used as an
--     attachment in any way other than as an attachment, this command
--     /must/ not write to that image subresource as an attachment
--
-- -   #VUID-vkCmdDrawIndexed-None-06886# If the current render pass
--     instance uses a depth\/stencil attachment with a read-only layout
--     for the depth aspect,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth-write depth writes>
--     /must/ be disabled
--
-- -   #VUID-vkCmdDrawIndexed-None-06887# If the current render pass
--     instance uses a depth\/stencil attachment with a read-only layout
--     for the stencil aspect and stencil test is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil all stencil ops>
--     /must/ be 'Vulkan.Core10.Enums.StencilOp.STENCIL_OP_KEEP'
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
-- -   #VUID-vkCmdDrawIndexed-None-06666# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndexed-viewportCount-03417# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndexed-scissorCount-03418# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @scissorCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndexed-viewportCount-03419# If the bound graphics
--     pipeline state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic states enabled then both
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     and
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--
-- -   #VUID-vkCmdDrawIndexed-viewportCount-04137# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndexed-viewportCount-04138# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndexed-viewportCount-04139# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndexed-viewportCount-04140# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndexed-VkPipelineVieportCreateInfo-04141# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo', then the
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndexed-VkPipelineVieportCreateInfo-04142# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo', then the
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndexed-None-04876# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndexed-None-04877# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndexed-logicOp-04878# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetLogicOpEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command and the @logicOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.LogicOp.LogicOp' value
--
-- -   #VUID-vkCmdDrawIndexed-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDrawIndexed-blendEnable-04727# If rasterization is not
--     disabled in the bound graphics pipeline, then for each color
--     attachment in the subpass, if the corresponding image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     do not contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT',
--     then the @blendEnable@ member of the corresponding element of the
--     @pAttachments@ member of @pColorBlendState@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdDrawIndexed-multisampledRenderToSingleSampled-07284# If
--     rasterization is not disabled in the bound graphics pipeline, and
--     none of the @VK_AMD_mixed_attachment_samples@ extension, the
--     @VK_NV_framebuffer_mixed_samples@ extension, or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature are enabled, then
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     /must/ be the same as the current subpass color and\/or
--     depth\/stencil attachments
--
-- -   #VUID-vkCmdDrawIndexed-imageView-06172# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDrawIndexed-imageView-06173# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDrawIndexed-imageView-06174# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDrawIndexed-imageView-06175# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDrawIndexed-imageView-06176# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDrawIndexed-imageView-06177# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDrawIndexed-viewMask-06178# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound graphics pipeline /must/ have been created with
--     a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@viewMask@
--
-- -   #VUID-vkCmdDrawIndexed-colorAttachmentCount-06179# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound graphics pipeline /must/ have been created with
--     a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--
-- -   #VUID-vkCmdDrawIndexed-colorAttachmentCount-06180# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a 'Vulkan.Core10.Enums.Format.Format' equal to the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndexed-attachmentCount-06667# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @attachmentCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ be greater than or equal to the
--     'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'::@attachmentCount@
--     of the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndexed-attachmentCount-06815# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @attachmentCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ be less than or equal to the @maxColorAttachments@ member of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'
--
-- -   #VUID-vkCmdDrawIndexed-pDepthAttachment-06181# If the current render
--     pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the 'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndexed-pStencilAttachment-06182# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the 'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndexed-imageView-06183# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the currently
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdDrawIndexed-imageView-06184# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the currently
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndexed-colorAttachmentCount-06185# If the currently
--     bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a sample count equal to the corresponding element of the
--     @pColorAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndexed-pDepthAttachment-06186# If the current render
--     pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndexed-pStencilAttachment-06187# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndexed-multisampledRenderToSingleSampled-07285# If
--     the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a sample count equal to the value of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndexed-multisampledRenderToSingleSampled-07286# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndexed-multisampledRenderToSingleSampled-07287# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndexed-renderPass-06198# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline /must/ have been created with a
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@renderPass@
--     equal to 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdDrawIndexed-primitivesGeneratedQueryWithRasterizerDiscard-06708#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithRasterizerDiscard primitivesGeneratedQueryWithRasterizerDiscard>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-discard rasterization discard>
--     /must/ not be enabled.
--
-- -   #VUID-vkCmdDrawIndexed-primitivesGeneratedQueryWithNonZeroStreams-06709#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithNonZeroStreams primitivesGeneratedQueryWithNonZeroStreams>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active, the bound graphics pipeline /must/ not have been
--     created with a non-zero value in
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@.
--
-- -   #VUID-vkCmdDrawIndexed-stage-07073# If the currently bound pipeline
--     was created with the
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of an element of
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ set
--     to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     then
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-mesh-shader Mesh Shader Queries>
--     must not be active
--
-- -   #VUID-vkCmdDrawIndexed-commandBuffer-02712# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource written to by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be an unprotected resource
--
-- -   #VUID-vkCmdDrawIndexed-commandBuffer-02713# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, pipeline stages other than the framebuffer-space
--     and compute stages in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command /must/ not
--     write to any resource
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
-- -   #VUID-vkCmdDrawIndexed-primitiveTopology-03420# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDrawIndexed-None-04912# If the bound graphics pipeline
--     was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--     dynamic states enabled, then
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command
--
-- -   #VUID-vkCmdDrawIndexed-pStrides-04913# If the bound graphics
--     pipeline was created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @pStrides@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--     /must/ not be @NULL@
--
-- -   #VUID-vkCmdDrawIndexed-None-04914# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command
--
-- -   #VUID-vkCmdDrawIndexed-None-04875# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPatchControlPointsEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndexed-None-04879# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndexed-stage-06481# The bound graphics pipeline
--     /must/ not have been created with the
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of an element of
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ set
--     to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndexed-None-07312# An index buffer /must/ be bound
--
-- -   #VUID-vkCmdDrawIndexed-firstIndex-04932# (@indexSize@ ×
--     (@firstIndex@ + @indexCount@) + @offset@) /must/ be less than or
--     equal to the size of the bound index buffer, with @indexSize@ being
--     based on the type specified by @indexType@, where the index buffer,
--     @indexType@, and @offset@ are specified via 'cmdBindIndexBuffer'
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
-- -   #VUID-vkCmdDrawIndexed-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdDrawIndexedPtr = pVkCmdDrawIndexed (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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

-- | vkCmdDrawIndirect - Draw primitives with indirect parameters
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
-- -   #VUID-vkCmdDrawIndirect-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDrawIndirect-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
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
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirect-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawIndirect-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
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
-- -   #VUID-vkCmdDrawIndirect-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndirect-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndirect-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndirect-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
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
-- -   #VUID-vkCmdDrawIndirect-maintenance4-06425# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
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
--     /must/ have been set or inherited (if the
--     @VK_NV_inherited_viewport_scissor@ extension is enabled) for
--     @commandBuffer@, and done so after any previously bound pipeline
--     with the corresponding state not specified as dynamic
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
-- -   #VUID-vkCmdDrawIndirect-uniformBuffers-06935# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a uniform buffer, and that stage
--     was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndirect-storageBuffers-06936# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a storage buffer, and that stage
--     was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndirect-commandBuffer-02707# If @commandBuffer@ is
--     an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDrawIndirect-None-06550# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ only be used with @OpImageSample*@ or
--     @OpImageSparseSample*@ instructions
--
-- -   #VUID-vkCmdDrawIndirect-ConstOffset-06551# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ not use the @ConstOffset@ and @Offset@ operands
--
-- -   #VUID-vkCmdDrawIndirect-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format
--
-- -   #VUID-vkCmdDrawIndirect-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdDrawIndirect-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDrawIndirect-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDrawIndirect-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDrawIndirect-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDrawIndirect-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDrawIndirect-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDrawIndirect-OpImageWeightedSampleQCOM-06971# If
--     @OpImageWeightedSampleQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirect-OpImageWeightedSampleQCOM-06972# If
--     @OpImageWeightedSampleQCOM@ uses a 'Vulkan.Core10.Handles.ImageView'
--     as a sample weight image as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirect-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirect-OpImageBlockMatchSSDQCOM-06974# If
--     @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirect-OpImageBlockMatchSADQCOM-06975# If
--     @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndirect-OpImageBlockMatchSADQCOM-06976# If
--     @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>.
--
-- -   #VUID-vkCmdDrawIndirect-OpImageWeightedSampleQCOM-06977# If
--     @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdDrawIndirect-OpImageWeightedSampleQCOM-06978# If any
--     command other than @OpImageWeightedSampleQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdDrawIndirect-None-07288# Any shader invocation executed
--     by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
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
-- -   #VUID-vkCmdDrawIndirect-None-06537# Memory backing image
--     subresources used as attachments in the current render pass /must/
--     not be written in any way other than as an attachment by this
--     command
--
-- -   #VUID-vkCmdDrawIndirect-None-06538# If any recorded command in the
--     current subpass will write to an image subresource as an attachment,
--     this command /must/ not read from the memory backing that image
--     subresource in any other way than as an attachment
--
-- -   #VUID-vkCmdDrawIndirect-None-06539# If any recorded command in the
--     current subpass will read from an image subresource used as an
--     attachment in any way other than as an attachment, this command
--     /must/ not write to that image subresource as an attachment
--
-- -   #VUID-vkCmdDrawIndirect-None-06886# If the current render pass
--     instance uses a depth\/stencil attachment with a read-only layout
--     for the depth aspect,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth-write depth writes>
--     /must/ be disabled
--
-- -   #VUID-vkCmdDrawIndirect-None-06887# If the current render pass
--     instance uses a depth\/stencil attachment with a read-only layout
--     for the stencil aspect and stencil test is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil all stencil ops>
--     /must/ be 'Vulkan.Core10.Enums.StencilOp.STENCIL_OP_KEEP'
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
-- -   #VUID-vkCmdDrawIndirect-None-06666# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirect-viewportCount-03417# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndirect-scissorCount-03418# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @scissorCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndirect-viewportCount-03419# If the bound graphics
--     pipeline state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic states enabled then both
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     and
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--
-- -   #VUID-vkCmdDrawIndirect-viewportCount-04137# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirect-viewportCount-04138# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirect-viewportCount-04139# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirect-viewportCount-04140# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirect-VkPipelineVieportCreateInfo-04141# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo', then the
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirect-VkPipelineVieportCreateInfo-04142# If the
--     bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo', then the
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndirect-None-04876# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirect-None-04877# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirect-logicOp-04878# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetLogicOpEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command and the @logicOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.LogicOp.LogicOp' value
--
-- -   #VUID-vkCmdDrawIndirect-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDrawIndirect-blendEnable-04727# If rasterization is not
--     disabled in the bound graphics pipeline, then for each color
--     attachment in the subpass, if the corresponding image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     do not contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT',
--     then the @blendEnable@ member of the corresponding element of the
--     @pAttachments@ member of @pColorBlendState@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdDrawIndirect-multisampledRenderToSingleSampled-07284# If
--     rasterization is not disabled in the bound graphics pipeline, and
--     none of the @VK_AMD_mixed_attachment_samples@ extension, the
--     @VK_NV_framebuffer_mixed_samples@ extension, or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature are enabled, then
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     /must/ be the same as the current subpass color and\/or
--     depth\/stencil attachments
--
-- -   #VUID-vkCmdDrawIndirect-imageView-06172# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDrawIndirect-imageView-06173# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDrawIndirect-imageView-06174# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDrawIndirect-imageView-06175# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDrawIndirect-imageView-06176# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDrawIndirect-imageView-06177# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDrawIndirect-viewMask-06178# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound graphics pipeline /must/ have been created with
--     a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@viewMask@
--
-- -   #VUID-vkCmdDrawIndirect-colorAttachmentCount-06179# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound graphics pipeline /must/ have been created with
--     a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--
-- -   #VUID-vkCmdDrawIndirect-colorAttachmentCount-06180# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a 'Vulkan.Core10.Enums.Format.Format' equal to the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndirect-attachmentCount-06667# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @attachmentCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ be greater than or equal to the
--     'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'::@attachmentCount@
--     of the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndirect-attachmentCount-06815# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @attachmentCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ be less than or equal to the @maxColorAttachments@ member of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'
--
-- -   #VUID-vkCmdDrawIndirect-pDepthAttachment-06181# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the 'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndirect-pStencilAttachment-06182# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the 'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndirect-imageView-06183# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the currently
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdDrawIndirect-imageView-06184# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the currently
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndirect-colorAttachmentCount-06185# If the currently
--     bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a sample count equal to the corresponding element of the
--     @pColorAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndirect-pDepthAttachment-06186# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndirect-pStencilAttachment-06187# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndirect-multisampledRenderToSingleSampled-07285# If
--     the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a sample count equal to the value of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndirect-multisampledRenderToSingleSampled-07286# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndirect-multisampledRenderToSingleSampled-07287# If
--     the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndirect-renderPass-06198# If the current render pass
--     instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline /must/ have been created with a
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@renderPass@
--     equal to 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdDrawIndirect-primitivesGeneratedQueryWithRasterizerDiscard-06708#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithRasterizerDiscard primitivesGeneratedQueryWithRasterizerDiscard>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-discard rasterization discard>
--     /must/ not be enabled.
--
-- -   #VUID-vkCmdDrawIndirect-primitivesGeneratedQueryWithNonZeroStreams-06709#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithNonZeroStreams primitivesGeneratedQueryWithNonZeroStreams>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active, the bound graphics pipeline /must/ not have been
--     created with a non-zero value in
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@.
--
-- -   #VUID-vkCmdDrawIndirect-stage-07073# If the currently bound pipeline
--     was created with the
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of an element of
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ set
--     to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     then
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-mesh-shader Mesh Shader Queries>
--     must not be active
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
-- -   #VUID-vkCmdDrawIndirect-primitiveTopology-03420# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDrawIndirect-None-04912# If the bound graphics pipeline
--     was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--     dynamic states enabled, then
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command
--
-- -   #VUID-vkCmdDrawIndirect-pStrides-04913# If the bound graphics
--     pipeline was created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @pStrides@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--     /must/ not be @NULL@
--
-- -   #VUID-vkCmdDrawIndirect-None-04914# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command
--
-- -   #VUID-vkCmdDrawIndirect-None-04875# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPatchControlPointsEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirect-None-04879# If the bound graphics pipeline
--     state was created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndirect-stage-06481# The bound graphics pipeline
--     /must/ not have been created with the
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of an element of
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ set
--     to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiDrawIndirect multiDrawIndirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   #VUID-vkCmdDrawIndirect-drawCount-02719# @drawCount@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxDrawIndirectCount@
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
-- -   #VUID-vkCmdDrawIndirect-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdDrawIndirectPtr = pVkCmdDrawIndirect (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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

-- | vkCmdDrawIndexedIndirect - Draw primitives with indirect parameters and
-- indexed vertices
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
-- -   #VUID-vkCmdDrawIndexedIndirect-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
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
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
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
-- -   #VUID-vkCmdDrawIndexedIndirect-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
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
-- -   #VUID-vkCmdDrawIndexedIndirect-maintenance4-06425# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
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
--     /must/ have been set or inherited (if the
--     @VK_NV_inherited_viewport_scissor@ extension is enabled) for
--     @commandBuffer@, and done so after any previously bound pipeline
--     with the corresponding state not specified as dynamic
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
-- -   #VUID-vkCmdDrawIndexedIndirect-uniformBuffers-06935# If any stage of
--     the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a uniform buffer, and that
--     stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndexedIndirect-storageBuffers-06936# If any stage of
--     the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a storage buffer, and that
--     stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDrawIndexedIndirect-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-06550# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ only be used with @OpImageSample*@ or
--     @OpImageSparseSample*@ instructions
--
-- -   #VUID-vkCmdDrawIndexedIndirect-ConstOffset-06551# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ not use the @ConstOffset@ and @Offset@ operands
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdDrawIndexedIndirect-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDrawIndexedIndirect-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDrawIndexedIndirect-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDrawIndexedIndirect-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDrawIndexedIndirect-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDrawIndexedIndirect-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpImageWeightedSampleQCOM-06971# If
--     @OpImageWeightedSampleQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpImageWeightedSampleQCOM-06972# If
--     @OpImageWeightedSampleQCOM@ uses a 'Vulkan.Core10.Handles.ImageView'
--     as a sample weight image as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpImageBlockMatchSSDQCOM-06974# If
--     @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpImageBlockMatchSADQCOM-06975# If
--     @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpImageBlockMatchSADQCOM-06976# If
--     @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpImageWeightedSampleQCOM-06977# If
--     @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-OpImageWeightedSampleQCOM-06978# If
--     any command other than @OpImageWeightedSampleQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-07288# Any shader invocation
--     executed by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
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
-- -   #VUID-vkCmdDrawIndexedIndirect-None-06537# Memory backing image
--     subresources used as attachments in the current render pass /must/
--     not be written in any way other than as an attachment by this
--     command
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-06538# If any recorded command
--     in the current subpass will write to an image subresource as an
--     attachment, this command /must/ not read from the memory backing
--     that image subresource in any other way than as an attachment
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-06539# If any recorded command
--     in the current subpass will read from an image subresource used as
--     an attachment in any way other than as an attachment, this command
--     /must/ not write to that image subresource as an attachment
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-06886# If the current render
--     pass instance uses a depth\/stencil attachment with a read-only
--     layout for the depth aspect,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth-write depth writes>
--     /must/ be disabled
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-06887# If the current render
--     pass instance uses a depth\/stencil attachment with a read-only
--     layout for the stencil aspect and stencil test is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil all stencil ops>
--     /must/ be 'Vulkan.Core10.Enums.StencilOp.STENCIL_OP_KEEP'
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
-- -   #VUID-vkCmdDrawIndexedIndirect-None-06666# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewportCount-03417# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@scissorCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndexedIndirect-scissorCount-03418# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @scissorCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ match the
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'::@viewportCount@
--     of the pipeline
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewportCount-03419# If the bound
--     graphics pipeline state was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic states enabled then both
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     and
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ match the @scissorCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetScissorWithCount'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewportCount-04137# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewportCount-04138# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewportCount-04139# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic state enabled, then the bound graphics pipeline /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewportCount-04140# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV'
--     dynamic states enabled then the @viewportCount@ parameter in the
--     last call to
--     'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV'
--     /must/ be greater than or equal to the @viewportCount@ parameter in
--     the last call to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-VkPipelineVieportCreateInfo-04141# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo', then the
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-VkPipelineVieportCreateInfo-04142# If
--     the bound graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled and a
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     structure chained from
--     'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo', then the
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'::@exclusiveScissorCount@
--     greater or equal to the @viewportCount@ parameter in the last call
--     to
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-04876# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetRasterizerDiscardEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-04877# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS_ENABLE'
--     dynamic state enabled then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state2.cmdSetDepthBiasEnable'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndexedIndirect-logicOp-04878# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetLogicOpEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command and the @logicOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.LogicOp.LogicOp' value
--
-- -   #VUID-vkCmdDrawIndexedIndirect-primitiveFragmentShadingRateWithMultipleViewports-04552#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, the bound graphics pipeline was created with
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     dynamic state enabled, and any of the shader stages of the bound
--     graphics pipeline write to the @PrimitiveShadingRateKHR@ built-in,
--     then
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @viewportCount@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
--     /must/ be @1@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-blendEnable-04727# If rasterization
--     is not disabled in the bound graphics pipeline, then for each color
--     attachment in the subpass, if the corresponding image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     do not contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT',
--     then the @blendEnable@ member of the corresponding element of the
--     @pAttachments@ member of @pColorBlendState@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-multisampledRenderToSingleSampled-07284#
--     If rasterization is not disabled in the bound graphics pipeline, and
--     none of the @VK_AMD_mixed_attachment_samples@ extension, the
--     @VK_NV_framebuffer_mixed_samples@ extension, or the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature are enabled, then
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     /must/ be the same as the current subpass color and\/or
--     depth\/stencil attachments
--
-- -   #VUID-vkCmdDrawIndexedIndirect-imageView-06172# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDrawIndexedIndirect-imageView-06173# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDrawIndexedIndirect-imageView-06174# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDrawIndexedIndirect-imageView-06175# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDrawIndexedIndirect-imageView-06176# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pDepthAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pDepthAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the depth attachment
--
-- -   #VUID-vkCmdDrawIndexedIndirect-imageView-06177# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @imageView@ member of @pStencilAttachment@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the @layout@ member of
--     @pStencilAttachment@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL',
--     this command /must/ not write any values to the stencil attachment
--
-- -   #VUID-vkCmdDrawIndexedIndirect-viewMask-06178# If the current render
--     pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound graphics pipeline /must/ have been created with
--     a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@viewMask@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-colorAttachmentCount-06179# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound graphics pipeline /must/ have been created with
--     a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-colorAttachmentCount-06180# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a 'Vulkan.Core10.Enums.Format.Format' equal to the
--     corresponding element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndexedIndirect-attachmentCount-06667# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @attachmentCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ be greater than or equal to the
--     'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'::@attachmentCount@
--     of the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndexedIndirect-attachmentCount-06815# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @attachmentCount@ parameter of
--     'Vulkan.Extensions.VK_EXT_color_write_enable.cmdSetColorWriteEnableEXT'
--     /must/ be less than or equal to the @maxColorAttachments@ member of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-pDepthAttachment-06181# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the 'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-pStencilAttachment-06182# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the 'Vulkan.Core10.Enums.Format.Format' used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-imageView-06183# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentShadingRateAttachmentInfoKHR'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the currently
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-imageView-06184# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.RenderingFragmentDensityMapAttachmentInfoEXT'::@imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the currently
--     bound graphics pipeline /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-colorAttachmentCount-06185# If the
--     currently bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a sample count equal to the corresponding element of the
--     @pColorAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndexedIndirect-pDepthAttachment-06186# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-pStencilAttachment-06187# If the
--     current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created with a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-multisampledRenderToSingleSampled-07285#
--     If the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and the current render pass instance was
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     with a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter greater than @0@, then each element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     array with a @imageView@ not equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ have been created
--     with a sample count equal to the value of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     used to create the currently bound graphics pipeline
--
-- -   #VUID-vkCmdDrawIndexedIndirect-multisampledRenderToSingleSampled-07286#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-multisampledRenderToSingleSampled-07287#
--     If the current render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline was created without a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature is not enabled, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
--     used to create the currently bound graphics pipeline /must/ be equal
--     to the sample count used to create
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-renderPass-06198# If the current
--     render pass instance was begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the currently bound pipeline /must/ have been created with a
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@renderPass@
--     equal to 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdDrawIndexedIndirect-primitivesGeneratedQueryWithRasterizerDiscard-06708#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithRasterizerDiscard primitivesGeneratedQueryWithRasterizerDiscard>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-discard rasterization discard>
--     /must/ not be enabled.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-primitivesGeneratedQueryWithNonZeroStreams-06709#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitivesGeneratedQueryWithNonZeroStreams primitivesGeneratedQueryWithNonZeroStreams>
--     feature is not enabled and the
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     query is active, the bound graphics pipeline /must/ not have been
--     created with a non-zero value in
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@.
--
-- -   #VUID-vkCmdDrawIndexedIndirect-stage-07073# If the currently bound
--     pipeline was created with the
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of an element of
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ set
--     to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     then
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-mesh-shader Mesh Shader Queries>
--     must not be active
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
-- -   #VUID-vkCmdDrawIndexedIndirect-primitiveTopology-03420# If the bound
--     graphics pipeline state was created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command, and the @primitiveTopology@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT'
--     /must/ be of the same
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-primitive-topology-class topology class>
--     as the pipeline
--     'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'::@topology@
--     state
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-04912# If the bound graphics
--     pipeline was created with both the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     and
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--     dynamic states enabled, then
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command
--
-- -   #VUID-vkCmdDrawIndexedIndirect-pStrides-04913# If the bound graphics
--     pipeline was created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT'
--     dynamic state enabled, but not the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command, and the @pStrides@ parameter of
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT'
--     /must/ not be @NULL@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-04914# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled, then
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     /must/ have been called in the current command buffer prior to this
--     draw command
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-04875# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPatchControlPointsEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-04879# If the bound graphics
--     pipeline state was created with the
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE_EXT'
--     dynamic state enabled then
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state2.cmdSetPrimitiveRestartEnableEXT'
--     /must/ have been called in the current command buffer prior to this
--     drawing command
--
-- -   #VUID-vkCmdDrawIndexedIndirect-stage-06481# The bound graphics
--     pipeline /must/ not have been created with the
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'::@stage@
--     member of an element of
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pStages@ set
--     to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiDrawIndirect multiDrawIndirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-drawCount-02719# @drawCount@ /must/
--     be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   #VUID-vkCmdDrawIndexedIndirect-None-07312# An index buffer /must/ be
--     bound
--
-- -   #VUID-vkCmdDrawIndexedIndirect-drawCount-00528# If @drawCount@ is
--     greater than @1@, @stride@ /must/ be a multiple of @4@ and /must/ be
--     greater than or equal to
--     @sizeof@('Vulkan.Core10.OtherTypes.DrawIndexedIndirectCommand')
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
-- -   #VUID-vkCmdDrawIndexedIndirect-videocoding# This command /must/ only
--     be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdDrawIndexedIndirectPtr = pVkCmdDrawIndexedIndirect (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- -   #VUID-vkCmdDispatch-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatch-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
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
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDispatch-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatch-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
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
-- -   #VUID-vkCmdDispatch-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatch-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatch-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatch-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
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
-- -   #VUID-vkCmdDispatch-maintenance4-06425# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
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
--     /must/ have been set or inherited (if the
--     @VK_NV_inherited_viewport_scissor@ extension is enabled) for
--     @commandBuffer@, and done so after any previously bound pipeline
--     with the corresponding state not specified as dynamic
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
-- -   #VUID-vkCmdDispatch-uniformBuffers-06935# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a uniform buffer, and that stage
--     was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatch-storageBuffers-06936# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a storage buffer, and that stage
--     was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatch-commandBuffer-02707# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDispatch-None-06550# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ only be used with @OpImageSample*@ or
--     @OpImageSparseSample*@ instructions
--
-- -   #VUID-vkCmdDispatch-ConstOffset-06551# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ not use the @ConstOffset@ and @Offset@ operands
--
-- -   #VUID-vkCmdDispatch-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format
--
-- -   #VUID-vkCmdDispatch-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdDispatch-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatch-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatch-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatch-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatch-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatch-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatch-OpImageWeightedSampleQCOM-06971# If
--     @OpImageWeightedSampleQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatch-OpImageWeightedSampleQCOM-06972# If
--     @OpImageWeightedSampleQCOM@ uses a 'Vulkan.Core10.Handles.ImageView'
--     as a sample weight image as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatch-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatch-OpImageBlockMatchSSDQCOM-06974# If
--     @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatch-OpImageBlockMatchSADQCOM-06975# If
--     @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatch-OpImageBlockMatchSADQCOM-06976# If
--     @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>.
--
-- -   #VUID-vkCmdDispatch-OpImageWeightedSampleQCOM-06977# If
--     @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdDispatch-OpImageWeightedSampleQCOM-06978# If any command
--     other than @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ not have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdDispatch-None-07288# Any shader invocation executed by
--     this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
--
-- -   #VUID-vkCmdDispatch-commandBuffer-02712# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource written to by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be an unprotected resource
--
-- -   #VUID-vkCmdDispatch-commandBuffer-02713# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, pipeline stages other than the framebuffer-space
--     and compute stages in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command /must/ not
--     write to any resource
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
-- -   #VUID-vkCmdDispatch-videocoding# This command /must/ only be called
--     outside of a video coding scope
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
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Compute                                                                                                               | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdDispatchPtr = pVkCmdDispatch (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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

-- | vkCmdDispatchIndirect - Dispatch compute work items with indirect
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
-- -   #VUID-vkCmdDispatchIndirect-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatchIndirect-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
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
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchIndirect-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatchIndirect-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
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
-- -   #VUID-vkCmdDispatchIndirect-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchIndirect-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchIndirect-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchIndirect-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
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
-- -   #VUID-vkCmdDispatchIndirect-maintenance4-06425# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a push constant value
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
--     /must/ have been set or inherited (if the
--     @VK_NV_inherited_viewport_scissor@ extension is enabled) for
--     @commandBuffer@, and done so after any previously bound pipeline
--     with the corresponding state not specified as dynamic
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
-- -   #VUID-vkCmdDispatchIndirect-uniformBuffers-06935# If any stage of
--     the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a uniform buffer, and that
--     stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchIndirect-storageBuffers-06936# If any stage of
--     the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a storage buffer, and that
--     stage was created without enabling either
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_robustness.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchIndirect-commandBuffer-02707# If @commandBuffer@
--     is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdDispatchIndirect-None-06550# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ only be used with @OpImageSample*@ or
--     @OpImageSparseSample*@ instructions
--
-- -   #VUID-vkCmdDispatchIndirect-ConstOffset-06551# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' or 'Vulkan.Core10.Handles.ImageView'
--     object that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>,
--     that object /must/ not use the @ConstOffset@ and @Offset@ operands
--
-- -   #VUID-vkCmdDispatchIndirect-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format
--
-- -   #VUID-vkCmdDispatchIndirect-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdDispatchIndirect-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatchIndirect-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatchIndirect-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatchIndirect-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatchIndirect-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatchIndirect-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatchIndirect-OpImageWeightedSampleQCOM-06971# If
--     @OpImageWeightedSampleQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchIndirect-OpImageWeightedSampleQCOM-06972# If
--     @OpImageWeightedSampleQCOM@ uses a 'Vulkan.Core10.Handles.ImageView'
--     as a sample weight image as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchIndirect-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchIndirect-OpImageBlockMatchSSDQCOM-06974# If
--     @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchIndirect-OpImageBlockMatchSADQCOM-06975# If
--     @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchIndirect-OpImageBlockMatchSADQCOM-06976# If
--     @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>.
--
-- -   #VUID-vkCmdDispatchIndirect-OpImageWeightedSampleQCOM-06977# If
--     @OpImageWeightedSampleQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdDispatchIndirect-OpImageWeightedSampleQCOM-06978# If any
--     command other than @OpImageWeightedSampleQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'.
--
-- -   #VUID-vkCmdDispatchIndirect-None-07288# Any shader invocation
--     executed by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
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
-- -   #VUID-vkCmdDispatchIndirect-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Compute                                                                                                               | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdDispatchIndirectPtr = pVkCmdDispatchIndirect (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @srcBuffer@ /must/ not be a protected buffer
--
-- -   #VUID-vkCmdCopyBuffer-commandBuffer-01823# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstBuffer@ /must/ not be a protected buffer
--
-- -   #VUID-vkCmdCopyBuffer-commandBuffer-01824# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstBuffer@ /must/ not be an unprotected buffer
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
-- -   #VUID-vkCmdCopyBuffer-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdCopyBufferPtr = pVkCmdCopyBuffer (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyBuffer is null" Nothing Nothing
  let vkCmdCopyBuffer' = mkVkCmdCopyBuffer vkCmdCopyBufferPtr
  pPRegions <- ContT $ allocaBytes @BufferCopy ((Data.Vector.length (regions)) * 24)
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
-- If either @srcImage@ or @dstImage@ has a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
-- regions of each plane to be copied /must/ be specified separately using
-- the @srcSubresource@ and @dstSubresource@ members of the 'ImageCopy'
-- structure. In this case, the @aspectMask@ of the @srcSubresource@ or
-- @dstSubresource@ that refers to the multi-planar image /must/ be
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT', or
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'. For
-- the purposes of 'cmdCopyImage', each plane of a multi-planar image is
-- treated as having the format listed in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-compatible-planes>
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
-- If the format of the destination image has a different
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-compatibility-classes block extent>
-- than the source image (e.g. one is a compressed format), the offset and
-- extent for each of the regions specified is
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-size-compatibility scaled according to the block extents of each format>
-- to match in size.
--
-- 'cmdCopyImage' /can/ be used to copy image data between multisample
-- images, but both images /must/ have the same number of samples.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyImage-commandBuffer-01825# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @srcImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdCopyImage-commandBuffer-01826# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdCopyImage-commandBuffer-01827# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be an unprotected image
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
--     @dstImage@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-size-compatibility size-compatible>
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
--     of @pRegions@, @srcSubresource.baseArrayLayer@ /must/ be @0@ and
--     @srcSubresource.layerCount@ /must/ be @1@
--
-- -   #VUID-vkCmdCopyImage-dstImage-04444# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @dstSubresource.baseArrayLayer@ /must/ be @0@ and
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
-- -   #VUID-vkCmdCopyImage-srcOffset-00147# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@)
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
-- -   #VUID-vkCmdCopyImage-dstOffset-00153# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_3D', then for each element
--     of @pRegions@, @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@)
--     /must/ both be greater than or equal to @0@ and less than or equal
--     to the depth of the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdCopyImage-pRegions-07278# For each element of @pRegions@,
--     @srcOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImage-pRegions-07279# For each element of @pRegions@,
--     @srcOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImage-pRegions-07280# For each element of @pRegions@,
--     @srcOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImage-pRegions-07281# For each element of @pRegions@,
--     @dstOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyImage-pRegions-07282# For each element of @pRegions@,
--     @dstOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyImage-pRegions-07283# For each element of @pRegions@,
--     @dstOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyImage-srcImage-01728# For each element of @pRegions@,
--     if the sum of @srcOffset.x@ and @extent.width@ does not equal the
--     width of the the subresource specified by @srcSubresource@,
--     @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImage-srcImage-01729# For each element of @pRegions@,
--     if the sum of @srcOffset.y@ and @extent.height@ does not equal the
--     height of the the subresource specified by @srcSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImage-srcImage-01730# For each element of @pRegions@,
--     if the sum of @srcOffset.z@ and @extent.depth@ does not equal the
--     depth of the the subresource specified by @srcSubresource@,
--     @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImage-dstImage-01732# For each element of @pRegions@,
--     if the sum of @dstOffset.x@ and @extent.width@ does not equal the
--     width of the the subresource specified by @dstSubresource@,
--     @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyImage-dstImage-01733# For each element of @pRegions@,
--     if the sum of @dstOffset.y@ and @extent.height@ does not equal the
--     height of the the subresource specified by @dstSubresource@,
--     @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyImage-dstImage-01734# For each element of @pRegions@,
--     if the sum of @dstOffset.z@ and @extent.depth@ does not equal the
--     depth of the the subresource specified by @dstSubresource@,
--     @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyImage-aspect-06662# If the @aspect@ member of any
--     element of @pRegions@ includes any flag other than
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--     or @srcImage@ was not created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     /must/ have been included in the
--     'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ used to create
--     @srcImage@
--
-- -   #VUID-vkCmdCopyImage-aspect-06663# If the @aspect@ member of any
--     element of @pRegions@ includes any flag other than
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--     or @dstImage@ was not created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     /must/ have been included in the
--     'Vulkan.Core10.Image.ImageCreateInfo'::@usage@ used to create
--     @dstImage@
--
-- -   #VUID-vkCmdCopyImage-aspect-06664# If the @aspect@ member of any
--     element of @pRegions@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     and @srcImage@ was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     /must/ have been included in the
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     used to create @srcImage@
--
-- -   #VUID-vkCmdCopyImage-aspect-06665# If the @aspect@ member of any
--     element of @pRegions@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     and @dstImage@ was created with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageStencilUsageCreateInfo separate stencil usage>,
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     /must/ have been included in the
--     'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo'::@stencilUsage@
--     used to create @dstImage@
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
-- -   #VUID-vkCmdCopyImage-videocoding# This command /must/ only be called
--     outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdCopyImagePtr = pVkCmdCopyImage (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyImage is null" Nothing Nothing
  let vkCmdCopyImage' = mkVkCmdCopyImage vkCmdCopyImagePtr
  pPRegions <- ContT $ allocaBytes @ImageCopy ((Data.Vector.length (regions)) * 68)
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-unnormalized-to-integer unnormalized to integer conversion>:
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures Image Operations chapter>,
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
-- implementation-dependent, the exact results of a blitting operation are
-- also implementation-dependent.
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
-- -   Format conversions on unorm, snorm, scaled and packed float formats
--     of the copied aspect of the image are performed by first converting
--     the pixels to float values.
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
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @srcImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdBlitImage-commandBuffer-01835# If @commandBuffer@ is an
--     unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdBlitImage-commandBuffer-01836# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be an unprotected image
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
-- -   #VUID-vkCmdBlitImage-srcImage-06421# @srcImage@ /must/ not use a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion format that requires a sampler Y′CBCR conversion>
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
-- -   #VUID-vkCmdBlitImage-dstImage-06422# @dstImage@ /must/ not use a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion format that requires a sampler Y′CBCR conversion>
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
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT', then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdBlitImage-filter-00237# If @filter@ is
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT', @srcImage@ /must/ be
--     of type 'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D'
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
--     each be @1@
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
--     @pRegions@, @srcOffsets@[0].x and @srcOffsets@[1].x /must/ both be
--     greater than or equal to @0@ and less than or equal to the width of
--     the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdBlitImage-srcOffset-00244# For each element of
--     @pRegions@, @srcOffsets@[0].y and @srcOffsets@[1].y /must/ both be
--     greater than or equal to @0@ and less than or equal to the height of
--     the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdBlitImage-srcImage-00245# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @srcOffsets@[0].y /must/ be @0@ and @srcOffsets@[1].y
--     /must/ be @1@
--
-- -   #VUID-vkCmdBlitImage-srcOffset-00246# For each element of
--     @pRegions@, @srcOffsets@[0].z and @srcOffsets@[1].z /must/ both be
--     greater than or equal to @0@ and less than or equal to the depth of
--     the specified @srcSubresource@ of @srcImage@
--
-- -   #VUID-vkCmdBlitImage-srcImage-00247# If @srcImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @srcOffsets@[0].z /must/ be @0@ and @srcOffsets@[1].z
--     /must/ be @1@
--
-- -   #VUID-vkCmdBlitImage-dstOffset-00248# For each element of
--     @pRegions@, @dstOffsets@[0].x and @dstOffsets@[1].x /must/ both be
--     greater than or equal to @0@ and less than or equal to the width of
--     the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdBlitImage-dstOffset-00249# For each element of
--     @pRegions@, @dstOffsets@[0].y and @dstOffsets@[1].y /must/ both be
--     greater than or equal to @0@ and less than or equal to the height of
--     the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdBlitImage-dstImage-00250# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D', then for each element
--     of @pRegions@, @dstOffsets@[0].y /must/ be @0@ and @dstOffsets@[1].y
--     /must/ be @1@
--
-- -   #VUID-vkCmdBlitImage-dstOffset-00251# For each element of
--     @pRegions@, @dstOffsets@[0].z and @dstOffsets@[1].z /must/ both be
--     greater than or equal to @0@ and less than or equal to the depth of
--     the specified @dstSubresource@ of @dstImage@
--
-- -   #VUID-vkCmdBlitImage-dstImage-00252# If @dstImage@ is of type
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageType.IMAGE_TYPE_2D', then for each element
--     of @pRegions@, @dstOffsets@[0].z /must/ be @0@ and @dstOffsets@[1].z
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
-- -   #VUID-vkCmdBlitImage-videocoding# This command /must/ only be called
--     outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdBlitImagePtr = pVkCmdBlitImage (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBlitImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBlitImage is null" Nothing Nothing
  let vkCmdBlitImage' = mkVkCmdBlitImage vkCmdBlitImagePtr
  pPRegions <- ContT $ allocaBytes @ImageBlit ((Data.Vector.length (regions)) * 80)
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
-- If @dstImage@ has a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
-- regions of each plane to be a target of a copy /must/ be specified
-- separately using the @pRegions@ member of the 'BufferImageCopy'
-- structure. In this case, the @aspectMask@ of @imageSubresource@ /must/
-- be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT', or
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'. For
-- the purposes of 'cmdCopyBufferToImage', each plane of a multi-planar
-- image is treated as having the format listed in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-compatible-planes>
-- for the plane identified by the @aspectMask@ of the corresponding
-- subresource. This applies both to 'Vulkan.Core10.Enums.Format.Format'
-- and to coordinates used in the copy, which correspond to texels in the
-- /plane/ rather than how these texels map to coordinates in the image as
-- a whole.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-01828# If @commandBuffer@
--     is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @srcBuffer@ /must/ not be a protected buffer
--
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-01829# If @commandBuffer@
--     is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-01830# If @commandBuffer@
--     is a protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be an unprotected image
--
-- -   #VUID-vkCmdCopyBufferToImage-pRegions-06217# The image region
--     specified by each element of @pRegions@ /must/ be contained within
--     the specified @imageSubresource@ of @dstImage@
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
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-04477# If the queue
--     family used to create the 'Vulkan.Core10.Handles.CommandPool' which
--     @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT', for each
--     element of @pRegions@, the @aspectMask@ member of @imageSubresource@
--     /must/ not be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkCmdCopyBufferToImage-pRegions-06218# For each element of
--     @pRegions@, @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified @imageSubresource@
--     of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-pRegions-06219# For each element of
--     @pRegions@, @imageOffset.y@ and (@imageExtent.height@ +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified @imageSubresource@
--     of @dstImage@
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
--     @pRegions@, @imageOffset.z@ and (@imageExtent.depth@ +
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
-- -   #VUID-vkCmdCopyBufferToImage-bufferRowLength-00203# For each element
--     of @pRegions@, @bufferRowLength@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-bufferImageHeight-00204# For each
--     element of @pRegions@, @bufferImageHeight@ /must/ be a multiple of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-pRegions-07273# For each element of
--     @pRegions@, @bufferOffset@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block size>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-pRegions-07274# For each element of
--     @pRegions@, @imageOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-pRegions-07275# For each element of
--     @pRegions@, @imageOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-pRegions-07276# For each element of
--     @pRegions@, @imageOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-imageExtent-00207# For each element of
--     @pRegions@, if the sum of @imageOffset.x@ and @extent.width@ does
--     not equal the width of the the subresource specified by
--     @srcSubresource@, @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-imageExtent-00208# For each element of
--     @pRegions@, if the sum of @imageOffset.y@ and @extent.height@ does
--     not equal the height of the the subresource specified by
--     @srcSubresource@, @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
--
-- -   #VUID-vkCmdCopyBufferToImage-imageExtent-00209# For each element of
--     @pRegions@, if the sum of @imageOffset.z@ and @extent.depth@ does
--     not equal the depth of the the subresource specified by
--     @srcSubresource@, @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @dstImage@
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
-- -   #VUID-vkCmdCopyBufferToImage-pRegions-07277# For each element of
--     @pRegions@, @bufferRowLength@ divided by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     and then multiplied by the texel block size of @dstImage@ /must/ be
--     less than or equal to 231-1
--
-- -   #VUID-vkCmdCopyBufferToImage-commandBuffer-04052# If the queue
--     family used to create the 'Vulkan.Core10.Handles.CommandPool' which
--     @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', the
--     @bufferOffset@ member of any element of @pRegions@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-vkCmdCopyBufferToImage-srcImage-04053# If @dstImage@ has a
--     depth\/stencil format, the @bufferOffset@ member of any element of
--     @pRegions@ /must/ be a multiple of @4@
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
-- -   #VUID-vkCmdCopyBufferToImage-videocoding# This command /must/ only
--     be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdCopyBufferToImagePtr = pVkCmdCopyBufferToImage (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyBufferToImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyBufferToImage is null" Nothing Nothing
  let vkCmdCopyBufferToImage' = mkVkCmdCopyBufferToImage vkCmdCopyBufferToImagePtr
  pPRegions <- ContT $ allocaBytes @BufferImageCopy ((Data.Vector.length (regions)) * 56)
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
-- If @srcImage@ has a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
-- regions of each plane to be a source of a copy /must/ be specified
-- separately using the @pRegions@ member of the 'BufferImageCopy'
-- structure. In this case, the @aspectMask@ of @imageSubresource@ /must/
-- be 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT', or
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'. For
-- the purposes of 'cmdCopyBufferToImage', each plane of a multi-planar
-- image is treated as having the format listed in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-compatible-planes>
-- for the plane identified by the @aspectMask@ of the corresponding
-- subresource. This applies both to 'Vulkan.Core10.Enums.Format.Format'
-- and to coordinates used in the copy, which correspond to texels in the
-- /plane/ rather than how these texels map to coordinates in the image as
-- a whole.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyImageToBuffer-commandBuffer-01831# If @commandBuffer@
--     is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @srcImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdCopyImageToBuffer-commandBuffer-01832# If @commandBuffer@
--     is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstBuffer@ /must/ not be a protected buffer
--
-- -   #VUID-vkCmdCopyImageToBuffer-commandBuffer-01833# If @commandBuffer@
--     is a protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstBuffer@ /must/ not be an unprotected buffer
--
-- -   #VUID-vkCmdCopyImageToBuffer-pRegions-06220# The image region
--     specified by each element of @pRegions@ /must/ be contained within
--     the specified @imageSubresource@ of @srcImage@
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
-- -   #VUID-vkCmdCopyImageToBuffer-pRegions-06221# For each element of
--     @pRegions@, @imageOffset.x@ and (@imageExtent.width@ +
--     @imageOffset.x@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the width of the specified @imageSubresource@
--     of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-pRegions-06222# For each element of
--     @pRegions@, @imageOffset.y@ and (imageExtent.height +
--     @imageOffset.y@) /must/ both be greater than or equal to @0@ and
--     less than or equal to the height of the specified @imageSubresource@
--     of @srcImage@
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
--     @pRegions@, @imageOffset.z@ and (@imageExtent.depth@ +
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
-- -   #VUID-vkCmdCopyImageToBuffer-bufferRowLength-00203# For each element
--     of @pRegions@, @bufferRowLength@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-bufferImageHeight-00204# For each
--     element of @pRegions@, @bufferImageHeight@ /must/ be a multiple of
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-pRegions-07273# For each element of
--     @pRegions@, @bufferOffset@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block size>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-pRegions-07274# For each element of
--     @pRegions@, @imageOffset.x@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-pRegions-07275# For each element of
--     @pRegions@, @imageOffset.y@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-pRegions-07276# For each element of
--     @pRegions@, @imageOffset.z@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageExtent-00207# For each element of
--     @pRegions@, if the sum of @imageOffset.x@ and @extent.width@ does
--     not equal the width of the the subresource specified by
--     @srcSubresource@, @extent.width@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageExtent-00208# For each element of
--     @pRegions@, if the sum of @imageOffset.y@ and @extent.height@ does
--     not equal the height of the the subresource specified by
--     @srcSubresource@, @extent.height@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent height>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
--
-- -   #VUID-vkCmdCopyImageToBuffer-imageExtent-00209# For each element of
--     @pRegions@, if the sum of @imageOffset.z@ and @extent.depth@ does
--     not equal the depth of the the subresource specified by
--     @srcSubresource@, @extent.depth@ /must/ be a multiple of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent depth>
--     of the 'Vulkan.Core10.Enums.Format.Format' of @srcImage@
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
-- -   #VUID-vkCmdCopyImageToBuffer-pRegions-07277# For each element of
--     @pRegions@, @bufferRowLength@ divided by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility-classes texel block extent width>
--     and then multiplied by the texel block size of @srcImage@ /must/ be
--     less than or equal to 231-1
--
-- -   #VUID-vkCmdCopyImageToBuffer-commandBuffer-04052# If the queue
--     family used to create the 'Vulkan.Core10.Handles.CommandPool' which
--     @commandBuffer@ was allocated from does not support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', the
--     @bufferOffset@ member of any element of @pRegions@ /must/ be a
--     multiple of @4@
--
-- -   #VUID-vkCmdCopyImageToBuffer-srcImage-04053# If @srcImage@ has a
--     depth\/stencil format, the @bufferOffset@ member of any element of
--     @pRegions@ /must/ be a multiple of @4@
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
-- -   #VUID-vkCmdCopyImageToBuffer-videocoding# This command /must/ only
--     be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdCopyImageToBufferPtr = pVkCmdCopyImageToBuffer (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdCopyImageToBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyImageToBuffer is null" Nothing Nothing
  let vkCmdCopyImageToBuffer' = mkVkCmdCopyImageToBuffer vkCmdCopyImageToBufferPtr
  pPRegions <- ContT $ allocaBytes @BufferImageCopy ((Data.Vector.length (regions)) * 56)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#copies-buffers copies>.
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#copies-buffers buffer to buffer copies>
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
-- is treated as a “transfer” operation for the purposes of synchronization
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
--     an unprotected command buffer and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstBuffer@ /must/ not be a protected buffer
--
-- -   #VUID-vkCmdUpdateBuffer-commandBuffer-01814# If @commandBuffer@ is a
--     protected command buffer and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstBuffer@ /must/ not be an unprotected buffer
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
-- -   #VUID-vkCmdUpdateBuffer-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdUpdateBufferPtr = pVkCmdUpdateBuffer (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- 'cmdFillBuffer' is treated as a “transfer” operation for the purposes of
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
--     unprotected command buffer and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstBuffer@ /must/ not be a protected buffer
--
-- -   #VUID-vkCmdFillBuffer-commandBuffer-01812# If @commandBuffer@ is a
--     protected command buffer and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstBuffer@ /must/ not be an unprotected buffer
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
-- -   #VUID-vkCmdFillBuffer-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdFillBufferPtr = pVkCmdFillBuffer (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @image@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
--
-- -   #VUID-vkCmdClearColorImage-image-00002# @image@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdClearColorImage-image-01545# @image@ /must/ not use any
--     of the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion formats that require a sampler Y′CBCR conversion>
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
-- -   #VUID-vkCmdClearColorImage-pColor-04961# @pColor@ /must/ be a valid
--     pointer to a 'ClearColorValue' union
--
-- -   #VUID-vkCmdClearColorImage-commandBuffer-01805# If @commandBuffer@
--     is an unprotected command buffer and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @image@ /must/ not be a protected image
--
-- -   #VUID-vkCmdClearColorImage-commandBuffer-01806# If @commandBuffer@
--     is a protected command buffer and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, /must/ not be an unprotected image
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
-- -   #VUID-vkCmdClearColorImage-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
                      -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#clears-values>
                      -- below).
                      ClearColorValue
                   -> -- | @pRanges@ is a pointer to an array of
                      -- 'Vulkan.Core10.ImageView.ImageSubresourceRange' structures describing a
                      -- range of mipmap levels, array layers, and aspects to be cleared, as
                      -- described in
                      -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-views Image Views>.
                      ("ranges" ::: Vector ImageSubresourceRange)
                   -> io ()
cmdClearColorImage commandBuffer image imageLayout color ranges = liftIO . evalContT $ do
  let vkCmdClearColorImagePtr = pVkCmdClearColorImage (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdClearColorImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdClearColorImage is null" Nothing Nothing
  let vkCmdClearColorImage' = mkVkCmdClearColorImage vkCmdClearColorImagePtr
  pColor <- ContT $ withCStruct (color)
  pPRanges <- ContT $ allocaBytes @ImageSubresourceRange ((Data.Vector.length (ranges)) * 20)
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-format-features format features>
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
--     @commandBuffer@ is an unprotected command buffer and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @image@ /must/ not be a protected image
--
-- -   #VUID-vkCmdClearDepthStencilImage-commandBuffer-01808# If
--     @commandBuffer@ is a protected command buffer and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @image@ /must/ not be an unprotected image
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
-- -   #VUID-vkCmdClearDepthStencilImage-videocoding# This command /must/
--     only be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
                             -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#clears-values>
                             -- below).
                             ClearDepthStencilValue
                          -> -- | @pRanges@ is a pointer to an array of
                             -- 'Vulkan.Core10.ImageView.ImageSubresourceRange' structures describing a
                             -- range of mipmap levels, array layers, and aspects to be cleared, as
                             -- described in
                             -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-views Image Views>.
                             ("ranges" ::: Vector ImageSubresourceRange)
                          -> io ()
cmdClearDepthStencilImage commandBuffer image imageLayout depthStencil ranges = liftIO . evalContT $ do
  let vkCmdClearDepthStencilImagePtr = pVkCmdClearDepthStencilImage (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdClearDepthStencilImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdClearDepthStencilImage is null" Nothing Nothing
  let vkCmdClearDepthStencilImage' = mkVkCmdClearDepthStencilImage vkCmdClearDepthStencilImagePtr
  pDepthStencil <- ContT $ withCStruct (depthStencil)
  pPRanges <- ContT $ allocaBytes @ImageSubresourceRange ((Data.Vector.length (ranges)) * 20)
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
-- If the render pass has a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-fragmentdensitymapattachment fragment density map attachment>,
-- clears follow the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragmentdensitymapops operations of fragment density maps>
-- as if each clear region was a primitive which generates fragments. The
-- clear color is applied to all pixels inside each fragment’s area
-- regardless if the pixels lie outside of the clear region. Clears /may/
-- have a different set of supported fragment areas than draws.
--
-- Unlike other
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#clears clear commands>,
-- 'cmdClearAttachments' is not a transfer command. It performs its
-- operations in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-order rasterization order>.
-- For color attachments, the operations are executed as color attachment
-- writes, by the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
-- stage. For depth\/stencil attachments, the operations are executed as
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-depth depth writes>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-stencil stencil writes>
-- by the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT'
-- and
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT'
-- stages.
--
-- 'cmdClearAttachments' is not affected by the bound pipeline state.
--
-- Note
--
-- It is generally preferable to clear attachments by using the
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR' load
-- operation at the start of rendering, as it is more efficient on some
-- implementations.
--
-- If any attachment’s @aspectMask@ to be cleared is not backed by an image
-- view, the clear has no effect on that aspect.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdClearAttachments-pAttachments-07270# For each element of
--     @pAttachments@, the corresponding attachment in the current render
--     pass instance /must/ either not be backed by an image view, or
--     contain each of the aspects specified in @aspectMask@
--
-- -   #VUID-vkCmdClearAttachments-aspectMask-07271# If the @aspectMask@
--     member of any element of @pAttachments@ contains
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT',
--     the @colorAttachment@ /must/ be a valid color attachment index in
--     the current render pass instance
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
-- -   #VUID-vkCmdClearAttachments-pRects-06937# The layers specified by
--     each element of @pRects@ /must/ be contained within every attachment
--     that @pAttachments@ refers to, i.e. for each element of @pRects@,
--     'ClearRect'::@baseArrayLayer@
--     'ClearRect'::@layerCount@ /must/ be less than or equal to the number
--     of layers rendered to in the current render pass instance
--
-- -   #VUID-vkCmdClearAttachments-layerCount-01934# The @layerCount@
--     member of each element of @pRects@ /must/ not be @0@
--
-- -   #VUID-vkCmdClearAttachments-commandBuffer-02504# If @commandBuffer@
--     is an unprotected command buffer and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, each attachment to be cleared /must/ not be a
--     protected image
--
-- -   #VUID-vkCmdClearAttachments-commandBuffer-02505# If @commandBuffer@
--     is a protected command buffer and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, each attachment to be cleared /must/ not be an
--     unprotected image
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
-- -   #VUID-vkCmdClearAttachments-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'ClearAttachment', 'ClearRect', 'Vulkan.Core10.Handles.CommandBuffer'
cmdClearAttachments :: forall io
                     . (MonadIO io)
                    => -- | @commandBuffer@ is the command buffer into which the command will be
                       -- recorded.
                       CommandBuffer
                    -> -- | @pAttachments@ is a pointer to an array of 'ClearAttachment' structures
                       -- defining the attachments to clear and the clear values to use.
                       ("attachments" ::: Vector ClearAttachment)
                    -> -- | @pRects@ is a pointer to an array of 'ClearRect' structures defining
                       -- regions within each selected attachment to clear.
                       ("rects" ::: Vector ClearRect)
                    -> io ()
cmdClearAttachments commandBuffer attachments rects = liftIO . evalContT $ do
  let vkCmdClearAttachmentsPtr = pVkCmdClearAttachments (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdClearAttachmentsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdClearAttachments is null" Nothing Nothing
  let vkCmdClearAttachments' = mkVkCmdClearAttachments vkCmdClearAttachmentsPtr
  pPAttachments <- ContT $ allocaBytes @ClearAttachment ((Data.Vector.length (attachments)) * 24)
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachments `plusPtr` (24 * (i)) :: Ptr ClearAttachment) (e) . ($ ())) (attachments)
  pPRects <- ContT $ allocaBytes @ClearRect ((Data.Vector.length (rects)) * 24)
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
--     an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @srcImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdResolveImage-commandBuffer-01838# If @commandBuffer@ is
--     an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be a protected image
--
-- -   #VUID-vkCmdResolveImage-commandBuffer-01839# If @commandBuffer@ is a
--     protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, @dstImage@ /must/ not be an unprotected image
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
-- -   #VUID-vkCmdResolveImage-linearColorAttachment-06519# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-linearColorAttachment linearColorAttachment>
--     feature is enabled and the image is created with
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR', the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV'
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
-- -   #VUID-vkCmdResolveImage-srcImage-06762# @srcImage@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   #VUID-vkCmdResolveImage-srcImage-06763# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_SRC_BIT'
--
-- -   #VUID-vkCmdResolveImage-dstImage-06764# @dstImage@ /must/ have been
--     created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   #VUID-vkCmdResolveImage-dstImage-06765# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_TRANSFER_DST_BIT'
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
-- -   #VUID-vkCmdResolveImage-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdResolveImagePtr = pVkCmdResolveImage (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdResolveImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdResolveImage is null" Nothing Nothing
  let vkCmdResolveImage' = mkVkCmdResolveImage vkCmdResolveImagePtr
  pPRegions <- ContT $ allocaBytes @ImageResolve ((Data.Vector.length (regions)) * 68)
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
-- 'cmdSetEvent' behaves identically to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdSetEvent2',
-- except that it does not define an access scope, and /must/ only be used
-- with 'cmdWaitEvents', not
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWaitEvents2'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetEvent-stageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdSetEvent-stageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdSetEvent-shadingRateImage-07318# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdSetEvent-stageMask-03937# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @stageMask@ /must/ not be @0@
--
-- -   #VUID-vkCmdSetEvent-stageMask-06457# Any pipeline stage included in
--     @stageMask@ /must/ be supported by the capabilities of the queue
--     family specified by the @queueFamilyIndex@ member of the
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that was
--     used to create the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
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
-- -   #VUID-vkCmdSetEvent-commandBuffer-recording# @commandBuffer@ /must/
--     be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetEvent-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, compute, decode, or encode
--     operations
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Both                                                                                                                        | Graphics                                                                                                              | Synchronization                                                                                                                        |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Decode                                                                                                                |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Encode                                                                                                                |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
               -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>
               -- used to determine the first
               -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>.
               ("stageMask" ::: PipelineStageFlags)
            -> io ()
cmdSetEvent commandBuffer event stageMask = liftIO $ do
  let vkCmdSetEventPtr = pVkCmdSetEvent (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- 'cmdResetEvent' behaves identically to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdResetEvent2'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdResetEvent-stageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent-stageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent-shadingRateImage-07318# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdResetEvent-stageMask-03937# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @stageMask@ /must/ not be @0@
--
-- -   #VUID-vkCmdResetEvent-stageMask-06458# Any pipeline stage included
--     in @stageMask@ /must/ be supported by the capabilities of the queue
--     family specified by the @queueFamilyIndex@ member of the
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that was
--     used to create the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   #VUID-vkCmdResetEvent-stageMask-01153# @stageMask@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'
--
-- -   #VUID-vkCmdResetEvent-event-03834# There /must/ be an execution
--     dependency between 'cmdResetEvent' and the execution of any
--     'cmdWaitEvents' that includes @event@ in its @pEvents@ parameter
--
-- -   #VUID-vkCmdResetEvent-event-03835# There /must/ be an execution
--     dependency between 'cmdResetEvent' and the execution of any
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWaitEvents2'
--     that includes @event@ in its @pEvents@ parameter
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
-- -   #VUID-vkCmdResetEvent-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdResetEvent-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, compute, decode, or encode
--     operations
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Both                                                                                                                        | Graphics                                                                                                              | Synchronization                                                                                                                        |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Decode                                                                                                                |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Encode                                                                                                                |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
                 -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>
                 -- used to determine when the @event@ is unsignaled.
                 ("stageMask" ::: PipelineStageFlags)
              -> io ()
cmdResetEvent commandBuffer event stageMask = liftIO $ do
  let vkCmdResetEventPtr = pVkCmdResetEvent (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
                             -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>.
                             ("srcStageMask" ::: PipelineStageFlags)
                          -> -- | @dstStageMask@ is a bitmask of
                             -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                             -- specifying the
                             -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages destination stage mask>.
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
  let vkCmdWaitEventsPtr = pVkCmdWaitEvents (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdWaitEventsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWaitEvents is null" Nothing Nothing
  let vkCmdWaitEvents' = mkVkCmdWaitEvents vkCmdWaitEventsPtr
  pPEvents <- ContT $ allocaBytes @Event ((Data.Vector.length (events)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPEvents `plusPtr` (8 * (i)) :: Ptr Event) (e)) (events)
  pPMemoryBarriers <- ContT $ allocaBytes @MemoryBarrier ((Data.Vector.length (memoryBarriers)) * 24)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMemoryBarriers `plusPtr` (24 * (i)) :: Ptr MemoryBarrier) (e)) (memoryBarriers)
  pPBufferMemoryBarriers <- ContT $ allocaBytes @BufferMemoryBarrier ((Data.Vector.length (bufferMemoryBarriers)) * 56)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBufferMemoryBarriers `plusPtr` (56 * (i)) :: Ptr BufferMemoryBarrier) (e)) (bufferMemoryBarriers)
  pPImageMemoryBarriers <- ContT $ allocaBytes @(ImageMemoryBarrier _) ((Data.Vector.length (imageMemoryBarriers)) * 72)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPImageMemoryBarriers `plusPtr` (72 * (i)) :: Ptr (ImageMemoryBarrier _))) (e) . ($ ())) (imageMemoryBarriers)
  lift $ traceAroundEvent "vkCmdWaitEvents" (vkCmdWaitEvents' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (events)) :: Word32)) (pPEvents) (srcStageMask) (dstStageMask) ((fromIntegral (Data.Vector.length $ (memoryBarriers)) :: Word32)) (pPMemoryBarriers) ((fromIntegral (Data.Vector.length $ (bufferMemoryBarriers)) :: Word32)) (pPBufferMemoryBarriers) ((fromIntegral (Data.Vector.length $ (imageMemoryBarriers)) :: Word32)) (forgetExtensions (pPImageMemoryBarriers)))
  pure $ ()

-- | vkCmdWaitEvents - Wait for one or more events and insert a set of memory
--
-- = Description
--
-- 'cmdWaitEvents' is largely similar to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWaitEvents2',
-- but /can/ only wait on signal operations defined by 'cmdSetEvent'. As
-- 'cmdSetEvent' does not define any access scopes, 'cmdWaitEvents' defines
-- the first access scope for each event signal operation in addition to
-- its own access scopes.
--
-- Note
--
-- Since 'cmdSetEvent' does not have any dependency information beyond a
-- stage mask, implementations do not have the same opportunity to perform
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-available-and-visible availability and visibility operations>
-- or
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>
-- in advance as they do with
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdSetEvent2' and
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWaitEvents2'.
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- are included in the first synchronization scope, if the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically latest>
-- pipeline stage in their @stageMask@ parameter is
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earlier>
-- than or equal to the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically latest>
-- pipeline stage in @srcStageMask@. Event signal operations performed by
-- 'Vulkan.Core10.Event.setEvent' are only included in the first
-- synchronization scope if
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT' is
-- included in @srcStageMask@.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur later in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- The second synchronization scope is limited to operations on the
-- pipeline stages determined by the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to accesses in the pipeline stages determined by the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@. Within that, the first access scope only
-- includes the first access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the first access scope
-- includes no accesses.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to accesses in the pipeline stages determined by the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@. Within that, the second access scope only
-- includes the second access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the second access scope
-- includes no accesses.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-shadingRateImage-07318# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-03937# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @srcStageMask@ /must/ not be @0@
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWaitEvents-shadingRateImage-07318# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-03937# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @dstStageMask@ /must/ not be @0@
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
-- -   #VUID-vkCmdWaitEvents-srcStageMask-06459# Any pipeline stage
--     included in @srcStageMask@ /must/ be supported by the capabilities
--     of the queue family specified by the @queueFamilyIndex@ member of
--     the 'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that
--     was used to create the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   #VUID-vkCmdWaitEvents-dstStageMask-06460# Any pipeline stage
--     included in @dstStageMask@ /must/ be supported by the capabilities
--     of the queue family specified by the @queueFamilyIndex@ member of
--     the 'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that
--     was used to create the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-01158# @srcStageMask@ /must/ be
--     the bitwise OR of the @stageMask@ parameter used in previous calls
--     to 'cmdSetEvent' with any of the elements of @pEvents@ and
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'
--     if any of the elements of @pEvents@ was set using
--     'Vulkan.Core10.Event.setEvent'
--
-- -   #VUID-vkCmdWaitEvents-srcStageMask-07308# If 'cmdWaitEvents' is
--     being called inside a render pass instance, @srcStageMask@ /must/
--     not include
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'
--
-- -   #VUID-vkCmdWaitEvents-srcQueueFamilyIndex-02803# The
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ members of any
--     element of @pBufferMemoryBarriers@ or @pImageMemoryBarriers@ /must/
--     be equal
--
-- -   #VUID-vkCmdWaitEvents-commandBuffer-01167# @commandBuffer@’s current
--     device mask /must/ include exactly one physical device
--
-- -   #VUID-vkCmdWaitEvents-pEvents-03847# Elements of @pEvents@ /must/
--     not have been signaled by
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdSetEvent2'
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
-- -   #VUID-vkCmdWaitEvents-dstStageMask-parameter# @dstStageMask@ /must/
--     be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
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
--     allocated from /must/ support graphics, compute, decode, or encode
--     operations
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Both                                                                                                                        | Graphics                                                                                                              | Synchronization                                                                                                                        |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Decode                                                                                                                |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Encode                                                                                                                |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
                 -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>.
                 ("srcStageMask" ::: PipelineStageFlags)
              -> -- | @dstStageMask@ is a bitmask of
                 -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                 -- specifying the
                 -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages destination stage mask>.
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
                     -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>.
                     ("srcStageMask" ::: PipelineStageFlags)
                  -> -- | @dstStageMask@ is a bitmask of
                     -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                     -- specifying the
                     -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages destination stage mask>.
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
-- 'cmdPipelineBarrier' operates almost identically to
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdPipelineBarrier2',
-- except that the scopes and barriers are defined as direct parameters
-- rather than being defined by an
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.DependencyInfo'.
--
-- When 'cmdPipelineBarrier' is submitted to a queue, it defines a memory
-- dependency between commands that were submitted before it, and those
-- submitted after it.
--
-- If 'cmdPipelineBarrier' was recorded outside a render pass instance, the
-- first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- If 'cmdPipelineBarrier' was recorded inside a render pass instance, the
-- first synchronization scope includes only commands that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- within the same subpass. In either case, the first synchronization scope
-- is limited to operations on the pipeline stages determined by the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@.
--
-- If 'cmdPipelineBarrier' was recorded outside a render pass instance, the
-- second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur later in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- If 'cmdPipelineBarrier' was recorded inside a render pass instance, the
-- second synchronization scope includes only commands that occur later in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- within the same subpass. In either case, the second synchronization
-- scope is limited to operations on the pipeline stages determined by the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to accesses in the pipeline stages determined by the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@. Within that, the first access scope only
-- includes the first access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the first access scope
-- includes no accesses.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to accesses in the pipeline stages determined by the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@. Within that, the second access scope only
-- includes the second access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the second access scope
-- includes no accesses.
--
-- If @dependencyFlags@ includes
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_BY_REGION_BIT', then
-- any dependency between
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space>
-- pipeline stages is
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-local>
-- - otherwise it is
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-global>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-shadingRateImage-07318# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-03937# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @srcStageMask@ /must/ not be @0@
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04091# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04092# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04093# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04094# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04095# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-04096# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdPipelineBarrier-shadingRateImage-07318# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-03937# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @dstStageMask@ /must/ not be @0@
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
-- -   #VUID-vkCmdPipelineBarrier-None-06191# If 'cmdPipelineBarrier' is
--     called within a render pass instance, the render pass /must/ not
--     have been started with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--
-- -   #VUID-vkCmdPipelineBarrier-srcStageMask-06461# Any pipeline stage
--     included in @srcStageMask@ /must/ be supported by the capabilities
--     of the queue family specified by the @queueFamilyIndex@ member of
--     the 'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that
--     was used to create the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-06462# Any pipeline stage
--     included in @dstStageMask@ /must/ be supported by the capabilities
--     of the queue family specified by the @queueFamilyIndex@ member of
--     the 'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' structure that
--     was used to create the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
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
-- -   #VUID-vkCmdPipelineBarrier-dstStageMask-parameter# @dstStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
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
--     allocated from /must/ support transfer, graphics, compute, decode,
--     or encode operations
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Both                                                                                                                        | Transfer                                                                                                              | Synchronization                                                                                                                        |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Decode                                                                                                                |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Encode                                                                                                                |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
                      -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stages>.
                      ("srcStageMask" ::: PipelineStageFlags)
                   -> -- | @dstStageMask@ is a bitmask of
                      -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
                      -- specifying the
                      -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stages>.
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
  let vkCmdPipelineBarrierPtr = pVkCmdPipelineBarrier (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPipelineBarrierPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPipelineBarrier is null" Nothing Nothing
  let vkCmdPipelineBarrier' = mkVkCmdPipelineBarrier vkCmdPipelineBarrierPtr
  pPMemoryBarriers <- ContT $ allocaBytes @MemoryBarrier ((Data.Vector.length (memoryBarriers)) * 24)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMemoryBarriers `plusPtr` (24 * (i)) :: Ptr MemoryBarrier) (e)) (memoryBarriers)
  pPBufferMemoryBarriers <- ContT $ allocaBytes @BufferMemoryBarrier ((Data.Vector.length (bufferMemoryBarriers)) * 56)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBufferMemoryBarriers `plusPtr` (56 * (i)) :: Ptr BufferMemoryBarrier) (e)) (bufferMemoryBarriers)
  pPImageMemoryBarriers <- ContT $ allocaBytes @(ImageMemoryBarrier _) ((Data.Vector.length (imageMemoryBarriers)) * 72)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-occlusion Occlusion Queries>.
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
-- This command defines an execution dependency between other query
-- commands that reference the same query.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands which reference the queries in @queryPool@
-- indicated by @query@ that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands which reference the queries in @queryPool@
-- indicated by @query@ that occur later in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- The operation of this command happens after the first scope and happens
-- before the second scope.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginQuery-None-00807# All queries used by the command
--     /must/ be unavailable
--
-- -   #VUID-vkCmdBeginQuery-queryType-02804# The @queryType@ used to
--     create @queryPool@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP'
--
-- -   #VUID-vkCmdBeginQuery-queryType-04728# The @queryType@ used to
--     create @queryPool@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR'
--
-- -   #VUID-vkCmdBeginQuery-queryType-06741# The @queryType@ used to
--     create @queryPool@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR'
--
-- -   #VUID-vkCmdBeginQuery-queryType-04729# The @queryType@ used to
--     create @queryPool@ /must/ not be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV'
--
-- -   #VUID-vkCmdBeginQuery-queryType-00800# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-occlusionQueryPrecise occlusionQueryPrecise>
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
-- -   #VUID-vkCmdBeginQuery-queryType-04862# If the @queryType@ used to
--     create @queryPool@ was
--     @VK_QUERY_TYPE_VIDEO_ENCODE_BITSTREAM_BUFFER_RANGE_KHR@ the
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#video-encode-operations video encode operations>
--
-- -   #VUID-vkCmdBeginQuery-queryPool-01922# @queryPool@ /must/ have been
--     created with a @queryType@ that differs from that of any queries
--     that are
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-active active>
--     within @commandBuffer@
--
-- -   #VUID-vkCmdBeginQuery-queryType-07070# If the @queryType@ used to
--     create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MESH_PRIMITIVES_GENERATED_EXT'
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
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
-- -   #VUID-vkCmdBeginQuery-queryType-06687# If the @queryType@ used to
--     create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBeginQuery-queryType-06688# If the @queryType@ used to
--     create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PRIMITIVES_GENERATED_EXT'
--     then
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-primitivesGeneratedQuery primitivesGeneratedQuery>
--     /must/ be enabled
--
-- -   #VUID-vkCmdBeginQuery-queryPool-07289# If @queryPool@ was created
--     with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     then the
--     'Vulkan.Extensions.VK_KHR_performance_query.QueryPoolPerformanceCreateInfoKHR'::@queueFamilyIndex@
--     @queryPool@ was created with /must/ equal the queue family index of
--     the 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from
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
--     allocated from /must/ support graphics, compute, decode, or encode
--     operations
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Both                                                                                                                        | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Decode                                                                                                                |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Encode                                                                                                                |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.QueryControlFlagBits.QueryControlFlags',
-- 'Vulkan.Core10.Handles.QueryPool',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginQueryIndexedEXT',
-- 'cmdEndQuery',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndQueryIndexedEXT'
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
  let vkCmdBeginQueryPtr = pVkCmdBeginQuery (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- The command completes the query in @queryPool@ identified by @query@,
-- and marks it as available.
--
-- This command defines an execution dependency between other query
-- commands that reference the same query.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands which reference the queries in @queryPool@
-- indicated by @query@ that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the operation of this command.
--
-- Calling 'cmdEndQuery' is equivalent to calling
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndQueryIndexedEXT' with
-- the @index@ parameter set to zero.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndQuery-None-01923# All queries used by the command
--     /must/ be
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-active active>
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
-- -   [[VUID-{refpage}-None-07007]] If called within a subpass of a render
--     pass instance, the corresponding 'cmdBeginQuery'* command /must/
--     have been called previously within the same subpass
--
-- -   [[VUID-{refpage}-None-07008]] If called outside of a render pass
--     instance, the corresponding 'cmdBeginQuery'* command /must/ have
--     been called outside of a render pass instance
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
--     allocated from /must/ support graphics, compute, decode, or encode
--     operations
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Both                                                                                                                        | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Decode                                                                                                                |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Encode                                                                                                                |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Handles.QueryPool', 'cmdBeginQuery',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginQueryIndexedEXT',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndQueryIndexedEXT'
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
  let vkCmdEndQueryPtr = pVkCmdEndQuery (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- This command defines an execution dependency between other query
-- commands that reference the same query.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands which reference the queries in @queryPool@
-- indicated by @firstQuery@ and @queryCount@ that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands which reference the queries in @queryPool@
-- indicated by @firstQuery@ and @queryCount@ that occur later in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- The operation of this command happens after the first scope and happens
-- before the second scope.
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
--     allocated from /must/ support graphics, compute, decode, or encode
--     operations
--
-- -   #VUID-vkCmdResetQueryPool-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdResetQueryPool-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Decode                                                                                                                |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Encode                                                                                                                |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdResetQueryPoolPtr = pVkCmdResetQueryPool (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- When 'cmdWriteTimestamp' is submitted to a queue, it defines an
-- execution dependency on commands that were submitted before it, and
-- writes a timestamp to a query pool.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- The synchronization scope is limited to operations on the pipeline stage
-- specified by @pipelineStage@.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the timestamp write operation.
--
-- When the timestamp value is written, the availability status of the
-- query is set to available.
--
-- Note
--
-- If an implementation is unable to detect completion and latch the timer
-- immediately after @stage@ has completed, it /may/ instead do so at any
-- logically later stage.
--
-- Comparisons between timestamps are not meaningful if the timestamps are
-- written by commands submitted to different queues.
--
-- Note
--
-- An example of such a comparison is subtracting an older timestamp from a
-- newer one to determine the execution time of a sequence of commands.
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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04076# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04077# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04078# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04079# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-04080# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp-pipelineStage-07077# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp-shadingRateImage-07314# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdWriteTimestamp-synchronization2-06489# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @pipelineStage@ /must/ not be
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_NONE'
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
-- -   #VUID-vkCmdWriteTimestamp-query-04904# @query@ /must/ be less than
--     the number of queries in @queryPool@
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
--     allocated from /must/ support transfer, graphics, compute, decode,
--     or encode operations
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Both                                                                                                                        | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Decode                                                                                                                |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Encode                                                                                                                |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits',
-- 'Vulkan.Core10.Handles.QueryPool'
cmdWriteTimestamp :: forall io
                   . (MonadIO io)
                  => -- | @commandBuffer@ is the command buffer into which the command will be
                     -- recorded.
                     CommandBuffer
                  -> -- | @pipelineStage@ is a
                     -- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits' value,
                     -- specifying a stage of the pipeline.
                     PipelineStageFlagBits
                  -> -- | @queryPool@ is the query pool that will manage the timestamp.
                     QueryPool
                  -> -- | @query@ is the query within the query pool that will contain the
                     -- timestamp.
                     ("query" ::: Word32)
                  -> io ()
cmdWriteTimestamp commandBuffer pipelineStage queryPool query = liftIO $ do
  let vkCmdWriteTimestampPtr = pVkCmdWriteTimestamp (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- Any results written for a query are written according to
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-memorylayout a layout dependent on the query type>.
--
-- Results for any query in @queryPool@ identified by @firstQuery@ and
-- @queryCount@ that is available are copied to @dstBuffer@.
--
-- If
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is set, results for all queries in @queryPool@ identified by
-- @firstQuery@ and @queryCount@ are copied to @dstBuffer@, along with an
-- extra availability value written directly after the results of each
-- query and interpreted as an unsigned integer. A value of zero indicates
-- that the results are not yet available, otherwise the query is complete
-- and results are available.
--
-- If @VK_QUERY_RESULT_WITH_STATUS_BIT_KHR@ is set, results for all queries
-- in @queryPool@ identified by @firstQuery@ and @queryCount@ are copied to
-- @dstBuffer@, along with an extra status value written directly after the
-- results of each query and interpreted as a signed integer. A value of
-- zero indicates that the results are not yet available. Positive values
-- indicate that the operations within the query completed successfully,
-- and the query results are valid. Negative values indicate that the
-- operations within the query completed unsuccessfully.
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueryResultStatusKHR VkQueryResultStatusKHR>
-- defines specific meaning for values returned here, though
-- implementations are free to return other values.
--
-- Results for any available query written by this command are final and
-- represent the final result of the query. If
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_PARTIAL_BIT' is
-- set, then for any query that is unavailable, an intermediate result
-- between zero and the final result value is written for that query.
-- Otherwise, any result written by this command is undefined.
--
-- If 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_64_BIT' is set,
-- results and availability or status values for all queries are written as
-- an array of 64-bit values. If the @queryPool@ was created with
-- 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
-- results for each query are written as an array of the type indicated by
-- 'Vulkan.Extensions.VK_KHR_performance_query.PerformanceCounterKHR'::@storage@
-- for the counter being queried. Otherwise, results and availability or
-- status values are written as an array of 32-bit values. If an unsigned
-- integer query’s value overflows the result type, the value /may/ either
-- wrap or saturate. If a signed integer query’s value overflows the result
-- type, the value is undefined. If a floating point query’s value is not
-- representable as the result type, the value is undefined.
--
-- This command defines an execution dependency between other query
-- commands that reference the same query.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands which reference the queries in @queryPool@
-- indicated by @query@ that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- If @flags@ does not include
-- 'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WAIT_BIT',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndQueryIndexedEXT',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWriteTimestamp2',
-- 'cmdEndQuery', and 'cmdWriteTimestamp' are excluded from this scope.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands which reference the queries in @queryPool@
-- indicated by @query@ that occur later in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- The operation of this command happens after the first scope and happens
-- before the second scope.
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-memorylayout here>
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
-- -   #VUID-vkCmdCopyQueryPoolResults-queryType-06901# If the @queryType@
--     used to create @queryPool@ was
--     @VK_QUERY_TYPE_RESULT_STATUS_ONLY_KHR@, @flags@ /must/ include
--     @VK_QUERY_RESULT_WITH_STATUS_BIT_KHR@
--
-- -   #VUID-vkCmdCopyQueryPoolResults-flags-06902# If @flags@ includes
--     @VK_QUERY_RESULT_WITH_STATUS_BIT_KHR@, it /must/ not include
--     'Vulkan.Core10.Enums.QueryResultFlagBits.QUERY_RESULT_WITH_AVAILABILITY_BIT'
--
-- -   #VUID-vkCmdCopyQueryPoolResults-queryType-06903# If the @queryType@
--     used to create @queryPool@ was
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     @flags@ /must/ not contain @VK_QUERY_RESULT_WITH_STATUS_BIT_KHR@
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
-- -   #VUID-vkCmdCopyQueryPoolResults-videocoding# This command /must/
--     only be called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdCopyQueryPoolResultsPtr = pVkCmdCopyQueryPoolResults (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- undefined. Reads of undefined push constant values by the executing
-- shader return undefined values.
--
-- Push constant values /can/ be updated incrementally, causing shader
-- stages in @stageFlags@ to read the new data from @pValues@ for push
-- constants modified by this command, while still reading the previous
-- data for push constants not modified by this command. When a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-bindpoint-commands bound pipeline command>
-- is issued, the bound pipeline’s layout /must/ be compatible with the
-- layouts used to set the values of all push constants in the pipeline
-- layout’s push constant ranges, as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>.
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
-- -   #VUID-vkCmdPushConstants-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdPushConstantsPtr = pVkCmdPushConstants (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- -   #VUID-vkCmdBeginRenderPass-initialLayout-00900# If the
--     @initialLayout@ member of any of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures specified when
--     creating the render pass specified in the @renderPass@ member of
--     @pRenderPassBegin@ is not
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED', then each
--     such @initialLayout@ /must/ be equal to the current layout of the
--     corresponding attachment image subresource of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@
--
-- -   #VUID-vkCmdBeginRenderPass-srcStageMask-06451# The @srcStageMask@
--     members of any element of the @pDependencies@ member of
--     'Vulkan.Core10.Pass.RenderPassCreateInfo' used to create
--     @renderPass@ /must/ be supported by the capabilities of the queue
--     family identified by the @queueFamilyIndex@ member of the
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo' used to create the
--     command pool which @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdBeginRenderPass-dstStageMask-06452# The @dstStageMask@
--     members of any element of the @pDependencies@ member of
--     'Vulkan.Core10.Pass.RenderPassCreateInfo' used to create
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
-- -   #VUID-vkCmdBeginRenderPass-initialLayout-07000# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including either the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     and either the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     or 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
--     usage bits.
--
-- -   #VUID-vkCmdBeginRenderPass-initialLayout-07001# If any of the
--     @initialLayout@ or @finalLayout@ member of the
--     'Vulkan.Core10.Pass.AttachmentDescription' structures or the
--     @layout@ member of the 'Vulkan.Core10.Pass.AttachmentReference'
--     structures specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--     usage bit.
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
-- -   #VUID-vkCmdBeginRenderPass-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             |                                                                                                                       | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             |                                                                                                                       | Synchronization                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdBeginRenderPassPtr = pVkCmdBeginRenderPass (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- -   #VUID-vkCmdNextSubpass-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             |                                                                                                                       | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             |                                                                                                                       | Synchronization                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdNextSubpassPtr = pVkCmdNextSubpass (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- -   #VUID-vkCmdEndRenderPass-None-06170# The current render pass
--     instance /must/ not have been begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--
-- -   #VUID-vkCmdEndRenderPass-None-07004# If 'cmdBeginQuery'* was called
--     within a subpass of the render pass, the corresponding
--     'cmdEndQuery'* /must/ have been called subsequently within the same
--     subpass.
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
-- -   #VUID-vkCmdEndRenderPass-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | Graphics                                                                                                              | Action                                                                                                                                 |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             |                                                                                                                       | State                                                                                                                                  |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             |                                                                                                                       | Synchronization                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdEndRenderPass :: forall io
                  . (MonadIO io)
                 => -- | @commandBuffer@ is the command buffer in which to end the current render
                    -- pass instance.
                    CommandBuffer
                 -> io ()
cmdEndRenderPass commandBuffer = liftIO $ do
  let vkCmdEndRenderPassPtr = pVkCmdEndRenderPass (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle executable or recording state>,
-- that primary command buffer becomes
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00088# Each element of
--     @pCommandBuffers@ /must/ have been allocated with a @level@ of
--     'Vulkan.Core10.Enums.CommandBufferLevel.COMMAND_BUFFER_LEVEL_SECONDARY'
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00089# Each element of
--     @pCommandBuffers@ /must/ be in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00091# If any element of
--     @pCommandBuffers@ was not recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--     flag, it /must/ not be in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
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
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00096# If
--     'cmdExecuteCommands' is being called within a render pass instance,
--     each element of @pCommandBuffers@ /must/ have been recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
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
-- -   #VUID-vkCmdExecuteCommands-contents-06018# If 'cmdExecuteCommands'
--     is being called within a render pass instance begun with
--     'cmdBeginRenderPass', its @contents@ parameter /must/ have been set
--     to
--     'Vulkan.Core10.Enums.SubpassContents.SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS'
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-06019# If
--     'cmdExecuteCommands' is being called within a render pass instance
--     begun with 'cmdBeginRenderPass', each element of @pCommandBuffers@
--     /must/ have been recorded with
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@subpass@
--     set to the index of the subpass which the given command buffer will
--     be executed in
--
-- -   #VUID-vkCmdExecuteCommands-pBeginInfo-06020# If 'cmdExecuteCommands'
--     is being called within a render pass instance begun with
--     'cmdBeginRenderPass', the render passes specified in the
--     @pBeginInfo->pInheritanceInfo->renderPass@ members of the
--     'Vulkan.Core10.CommandBuffer.beginCommandBuffer' commands used to
--     begin recording each element of @pCommandBuffers@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the current render pass
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-inheritedQueries inheritedQueries>
--     feature is not enabled, @commandBuffer@ /must/ not have any queries
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-active active>
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-00102# If @commandBuffer@
--     has a 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION' query
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-active active>,
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@occlusionQueryEnable@
--     set to 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-00103# If @commandBuffer@
--     has a 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_OCCLUSION' query
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-active active>,
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@queryFlags@
--     having all bits set that are set for the query
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-00104# If @commandBuffer@
--     has a 'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PIPELINE_STATISTICS'
--     query
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-active active>,
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@pipelineStatistics@
--     having all bits set that are set in the
--     'Vulkan.Core10.Handles.QueryPool' the query uses
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-00105# Each element of
--     @pCommandBuffers@ /must/ not begin any query types that are
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#queries-operation-active active>
--     in @commandBuffer@
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-01820# If @commandBuffer@
--     is a protected command buffer and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, each element of @pCommandBuffers@ /must/ be a
--     protected command buffer
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-01821# If @commandBuffer@
--     is an unprotected command buffer and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, each element of @pCommandBuffers@ /must/ be an
--     unprotected command buffer
--
-- -   #VUID-vkCmdExecuteCommands-None-02286# This command /must/ not be
--     recorded when transform feedback is active
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-06533# If
--     'cmdExecuteCommands' is being called within a render pass instance
--     and any recorded command in @commandBuffer@ in the current subpass
--     will write to an image subresource as an attachment, commands
--     recorded in elements of @pCommandBuffers@ /must/ not read from the
--     memory backing that image subresource in any other way
--
-- -   #VUID-vkCmdExecuteCommands-commandBuffer-06534# If
--     'cmdExecuteCommands' is being called within a render pass instance
--     and any recorded command in @commandBuffer@ in the current subpass
--     will read from an image subresource used as an attachment in any way
--     other than as an attachment, commands recorded in elements of
--     @pCommandBuffers@ /must/ not write to that image subresource as an
--     attachment
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-06535# If
--     'cmdExecuteCommands' is being called within a render pass instance
--     and any recorded command in a given element of @pCommandBuffers@
--     will write to an image subresource as an attachment, commands
--     recorded in elements of @pCommandBuffers@ at a higher index /must/
--     not read from the memory backing that image subresource in any other
--     way
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-06536# If
--     'cmdExecuteCommands' is being called within a render pass instance
--     and any recorded command in a given element of @pCommandBuffers@
--     will read from an image subresource used as an attachment in any way
--     other than as an attachment, commands recorded in elements of
--     @pCommandBuffers@ at a higher index /must/ not write to that image
--     subresource as an attachment
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-06021# If
--     @pCommandBuffers@ contains any
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-suspension suspended render pass instances>,
--     there /must/ be no action or synchronization commands between that
--     render pass instance and any render pass instance that resumes it
--
-- -   #VUID-vkCmdExecuteCommands-pCommandBuffers-06022# If
--     @pCommandBuffers@ contains any
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-suspension suspended render pass instances>,
--     there /must/ be no render pass instances between that render pass
--     instance and any render pass instance that resumes it
--
-- -   #VUID-vkCmdExecuteCommands-variableSampleLocations-06023# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-variableSampleLocations variableSampleLocations>
--     limit is not supported, and any element of @pCommandBuffers@
--     contains any
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-suspension suspended render pass instances>,
--     where a graphics pipeline has been bound, any pipelines bound in the
--     render pass instance that resumes it, or any subsequent render pass
--     instances that resume from that one and so on, /must/ use the same
--     sample locations
--
-- -   #VUID-vkCmdExecuteCommands-flags-06024# If 'cmdExecuteCommands' is
--     being called within a render pass instance begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     its
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@flags@
--     parameter /must/ have included
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT'
--
-- -   #VUID-vkCmdExecuteCommands-pBeginInfo-06025# If 'cmdExecuteCommands'
--     is being called within a render pass instance begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the render passes specified in the
--     @pBeginInfo->pInheritanceInfo->renderPass@ members of the
--     'Vulkan.Core10.CommandBuffer.beginCommandBuffer' commands used to
--     begin recording each element of @pCommandBuffers@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdExecuteCommands-flags-06026# If 'cmdExecuteCommands' is
--     being called within a render pass instance begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @flags@ member of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@pInheritanceInfo@
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     equal to the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@flags@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     excluding
--     'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CONTENTS_SECONDARY_COMMAND_BUFFERS_BIT'
--
-- -   #VUID-vkCmdExecuteCommands-colorAttachmentCount-06027# If
--     'cmdExecuteCommands' is being called within a render pass instance
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @colorAttachmentCount@ member of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@pInheritanceInfo@
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     equal to the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@colorAttachmentCount@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--
-- -   #VUID-vkCmdExecuteCommands-imageView-06028# If 'cmdExecuteCommands'
--     is being called within a render pass instance begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     if the @imageView@ member of an element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the corresponding
--     element of the @pColorAttachmentFormats@ member of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@pInheritanceInfo@
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     equal to the format used to create that image view
--
-- -   #VUID-vkCmdExecuteCommands-pDepthAttachment-06029# If
--     'cmdExecuteCommands' is being called within a render pass instance
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     if the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthAttachmentFormat@ member of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@pInheritanceInfo@
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     equal to the format used to create that image view
--
-- -   #VUID-vkCmdExecuteCommands-pStencilAttachment-06030# If
--     'cmdExecuteCommands' is being called within a render pass instance
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     if the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @stencilAttachmentFormat@ member of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@pInheritanceInfo@
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     equal to the format used to create that image view
--
-- -   #VUID-vkCmdExecuteCommands-pDepthAttachment-06774# If
--     'cmdExecuteCommands' is being called within a render pass instance
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     was 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthAttachmentFormat@ member of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@pInheritanceInfo@
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteCommands-pStencilAttachment-06775# If
--     'cmdExecuteCommands' is being called within a render pass instance
--     begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     was 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @stencilAttachmentFormat@ member of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@pInheritanceInfo@
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-vkCmdExecuteCommands-viewMask-06031# If 'cmdExecuteCommands'
--     is being called within a render pass instance begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     the @viewMask@ member of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@pInheritanceInfo@
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     equal to the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@viewMask@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--
-- -   #VUID-vkCmdExecuteCommands-pNext-06032# If 'cmdExecuteCommands' is
--     being called within a render pass instance begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' includes
--     a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, if the @imageView@ member of an element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the corresponding
--     element of the @pColorAttachmentSamples@ member of the
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@pInheritanceInfo@
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     equal to the sample count used to create that image view
--
-- -   #VUID-vkCmdExecuteCommands-pNext-06033# If 'cmdExecuteCommands' is
--     being called within a render pass instance begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' includes
--     a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, if the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of the
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@pInheritanceInfo@
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     equal to the sample count used to create that image view
--
-- -   #VUID-vkCmdExecuteCommands-pNext-06034# If 'cmdExecuteCommands' is
--     being called within a render pass instance begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' includes
--     a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, if the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of the
--     @depthStencilAttachmentSamples@ member of the
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure included in the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@pInheritanceInfo@
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     equal to the sample count used to create that image view
--
-- -   #VUID-vkCmdExecuteCommands-pNext-06035# If 'cmdExecuteCommands' is
--     being called within a render pass instance begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' does not
--     include a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, if the @imageView@ member of an element of the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pColorAttachments@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'::@rasterizationSamples@
--     /must/ be equal to the sample count used to create that image view
--
-- -   #VUID-vkCmdExecuteCommands-pNext-06036# If 'cmdExecuteCommands' is
--     being called within a render pass instance begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' does not
--     include a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, if the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pDepthAttachment->imageView@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'::@rasterizationSamples@
--     /must/ be equal to the sample count used to create that image view
--
-- -   #VUID-vkCmdExecuteCommands-pNext-06037# If 'cmdExecuteCommands' is
--     being called within a render pass instance begun with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     and the @pNext@ chain of
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' does not
--     include a
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_KHR_dynamic_rendering.AttachmentSampleCountInfoNV'
--     structure, if the
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@pStencilAttachment->imageView@
--     parameter to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'::@rasterizationSamples@
--     /must/ be equal to the sample count used to create that image view
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
-- -   #VUID-vkCmdExecuteCommands-videocoding# This command /must/ only be
--     called outside of a video coding scope
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Transfer                                                                                                              | Indirection                                                                                                                            |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  let vkCmdExecuteCommandsPtr = pVkCmdExecuteCommands (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdExecuteCommandsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdExecuteCommands is null" Nothing Nothing
  let vkCmdExecuteCommands' = mkVkCmdExecuteCommands vkCmdExecuteCommandsPtr
  pPCommandBuffers <- ContT $ allocaBytes @(Ptr CommandBuffer_T) ((Data.Vector.length (commandBuffers)) * 8)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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
--     /i/
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'BufferImageCopy',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.BufferImageCopy2',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags', 'ImageBlit',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageBlit2',
-- 'ImageCopy',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageCopy2',
-- 'ImageResolve',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ImageResolve2'
data ImageSubresourceLayers = ImageSubresourceLayers
  { -- | @aspectMask@ is a combination of
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits', selecting
    -- the color, depth and\/or stencil aspects to be copied.
    aspectMask :: ImageAspectFlags
  , -- | @mipLevel@ is the mipmap level to copy
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
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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
-- each slice is copied to or from a different layer; @depth@ slices in the
-- 3D image correspond to @layerCount@ layers in the 2D image, with an
-- effective @depth@ of @1@ used for the 2D image.
--
-- == Valid Usage
--
-- -   #VUID-VkImageCopy-extent-00140# The number of slices of the @extent@
--     (for 3D) or layers of the @srcSubresource@ (for non-3D) /must/ match
--     the number of slices of the @extent@ (for 3D) or layers of the
--     @dstSubresource@ (for non-3D)
--
-- -   #VUID-VkImageCopy-extent-06668# @extent.width@ /must/ not be 0
--
-- -   #VUID-VkImageCopy-extent-06669# @extent.height@ /must/ not be 0
--
-- -   #VUID-VkImageCopy-extent-06670# @extent.depth@ /must/ not be 0
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  withCStruct x f = allocaBytes 68 $ \p -> pokeCStruct p x (f p)
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
-- for the specified source and destination regions.
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  withCStruct x f = allocaBytes 80 $ \p -> pokeCStruct p x (f p)
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
-- -   #VUID-VkBufferImageCopy-imageExtent-06659# @imageExtent.width@
--     /must/ not be 0
--
-- -   #VUID-VkBufferImageCopy-imageExtent-06660# @imageExtent.height@
--     /must/ not be 0
--
-- -   #VUID-VkBufferImageCopy-imageExtent-06661# @imageExtent.depth@
--     /must/ not be 0
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferImageCopy-imageSubresource-parameter#
--     @imageSubresource@ /must/ be a valid 'ImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  withCStruct x f = allocaBytes 68 $ \p -> pokeCStruct p x (f p)
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


-- | VkRenderPassBeginInfo - Structure specifying render pass begin
-- information
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#vertexpostproc-renderpass-transform render pass transform>
-- is enabled, then @renderArea@ /must/ equal the framebuffer
-- pre-transformed dimensions. After @renderArea@ has been transformed by
-- 'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM'::@transform@,
-- the resulting render area /must/ be equal to the framebuffer dimensions.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-subpassShading subpassShading>
-- feature is enabled, then @renderArea@ /must/ equal the framebuffer
-- dimensions.
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
--     specifying a @loadOp@ (or @stencilLoadOp@, if the attachment has a
--     depth\/stencil format) of
--     'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR'
--
-- -   #VUID-VkRenderPassBeginInfo-clearValueCount-04962# If
--     @clearValueCount@ is not @0@, @pClearValues@ /must/ be a valid
--     pointer to an array of @clearValueCount@ 'ClearValue' unions
--
-- -   #VUID-VkRenderPassBeginInfo-renderPass-00904# @renderPass@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-compatibility compatible>
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
--     @renderArea.offset.x@ + @renderArea.extent.width@ /must/ be less
--     than or equal to 'Vulkan.Core10.Pass.FramebufferCreateInfo'::@width@
--     the @framebuffer@ was created with
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02853# If the @pNext@ chain does
--     not contain
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo'
--     or its @deviceRenderAreaCount@ member is equal to 0,
--     @renderArea.offset.y@ + @renderArea.extent.height@ /must/ be less
--     than or equal to
--     'Vulkan.Core10.Pass.FramebufferCreateInfo'::@height@ the
--     @framebuffer@ was created with
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02856# If the @pNext@ chain
--     contains
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     @offset.x@ + @extent.width@ of each element of @pDeviceRenderAreas@
--     /must/ be less than or equal to
--     'Vulkan.Core10.Pass.FramebufferCreateInfo'::@width@ the
--     @framebuffer@ was created with
--
-- -   #VUID-VkRenderPassBeginInfo-pNext-02857# If the @pNext@ chain
--     contains
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
--     @offset.y@ + @extent.height@ of each element of @pDeviceRenderAreas@
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-inherited-usage an inherited usage>
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
-- -   #VUID-VkRenderPassBeginInfo-commonparent# Both of @framebuffer@, and
--     @renderPass@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
    -- 'ClearValue' structures containing clear values for each attachment, if
    -- the attachment uses a @loadOp@ value of
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
  setNext RenderPassBeginInfo{..} next' = RenderPassBeginInfo{next = next', ..}
  getNext RenderPassBeginInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RenderPassBeginInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @RenderPassTransformBeginInfoQCOM = Just f
    | Just Refl <- eqT @e @RenderPassAttachmentBeginInfo = Just f
    | Just Refl <- eqT @e @RenderPassSampleLocationsBeginInfoEXT = Just f
    | Just Refl <- eqT @e @DeviceGroupRenderPassBeginInfo = Just f
    | otherwise = Nothing

instance (Extendss RenderPassBeginInfo es, PokeChain es) => ToCStruct (RenderPassBeginInfo es) where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassBeginInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderPass)) (renderPass)
    lift $ poke ((p `plusPtr` 24 :: Ptr Framebuffer)) (framebuffer)
    lift $ poke ((p `plusPtr` 32 :: Ptr Rect2D)) (renderArea)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (clearValues)) :: Word32))
    pPClearValues' <- ContT $ allocaBytes @ClearValue ((Data.Vector.length (clearValues)) * 16)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  withCStruct x f = allocaBytes 8 $ \p -> pokeCStruct p x (f p)
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
--     include @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ for any index /i/
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'ClearValue',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlags',
-- 'cmdClearAttachments'
data ClearAttachment = ClearAttachment
  { -- | @aspectMask@ is a mask selecting the color, depth and\/or stencil
    -- aspects of the attachment to be cleared.
    aspectMask :: ImageAspectFlags
  , -- | @colorAttachment@ is only meaningful if
    -- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT' is set
    -- in @aspectMask@, in which case it is an index into the currently bound
    -- color attachments.
    colorAttachment :: Word32
  , -- | @clearValue@ is the color or depth\/stencil value to clear the
    -- attachment to, as described in
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#clears-values Clear Values>
    -- below.
    clearValue :: ClearValue
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ClearAttachment)
#endif
deriving instance Show ClearAttachment

instance ToCStruct ClearAttachment where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
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
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
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

