{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_synchronization2"
module Vulkan.Core13.Promoted_From_VK_KHR_synchronization2  ( cmdSetEvent2
                                                            , cmdResetEvent2
                                                            , cmdWaitEvents2
                                                            , cmdWaitEvents2Safe
                                                            , cmdPipelineBarrier2
                                                            , queueSubmit2
                                                            , cmdWriteTimestamp2
                                                            , MemoryBarrier2(..)
                                                            , ImageMemoryBarrier2(..)
                                                            , BufferMemoryBarrier2(..)
                                                            , DependencyInfo(..)
                                                            , SemaphoreSubmitInfo(..)
                                                            , CommandBufferSubmitInfo(..)
                                                            , SubmitInfo2(..)
                                                            , PhysicalDeviceSynchronization2Features(..)
                                                            , ImageLayout(..)
                                                            , StructureType(..)
                                                            , AccessFlagBits(..)
                                                            , AccessFlags
                                                            , PipelineStageFlagBits(..)
                                                            , PipelineStageFlags
                                                            , AccessFlagBits2(..)
                                                            , AccessFlags2
                                                            , PipelineStageFlagBits2(..)
                                                            , PipelineStageFlags2
                                                            , SubmitFlagBits(..)
                                                            , SubmitFlags
                                                            , EventCreateFlagBits(..)
                                                            , EventCreateFlags
                                                            ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlags2)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Dynamic (DeviceCmds(pVkCmdPipelineBarrier2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdResetEvent2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetEvent2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWaitEvents2))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteTimestamp2))
import Vulkan.Dynamic (DeviceCmds(pVkQueueSubmit2))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Event)
import Vulkan.Core10.Handles (Event(..))
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_external_memory_acquire_unmodified (ExternalMemoryAcquireUnmodifiedEXT)
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core10.Handles (Fence(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_frame_boundary (FrameBoundaryEXT)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.ImageView (ImageSubresourceRange)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_low_latency2 (LatencySubmissionPresentIdNV)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PerformanceQuerySubmitInfoKHR)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2(..))
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue(Queue))
import Vulkan.Core10.Handles (Queue_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_ARM_render_pass_striped (RenderPassStripeSubmitInfoARM)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (SampleLocationsInfoEXT)
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core13.Enums.SubmitFlagBits (SubmitFlags)
import Vulkan.Exception (VulkanException(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_win32_keyed_mutex (Win32KeyedMutexAcquireReleaseInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_win32_keyed_mutex (Win32KeyedMutexAcquireReleaseInfoNV)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEPENDENCY_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_BARRIER_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBMIT_INFO_2))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(..))
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlagBits2(..))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlags2)
import Vulkan.Core10.Enums.EventCreateFlagBits (EventCreateFlagBits(..))
import Vulkan.Core10.Enums.EventCreateFlagBits (EventCreateFlags)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(..))
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2(..))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core13.Enums.SubmitFlagBits (SubmitFlagBits(..))
import Vulkan.Core13.Enums.SubmitFlagBits (SubmitFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetEvent2
  :: FunPtr (Ptr CommandBuffer_T -> Event -> Ptr DependencyInfo -> IO ()) -> Ptr CommandBuffer_T -> Event -> Ptr DependencyInfo -> IO ()

-- | vkCmdSetEvent2 - Set an event object to signaled state
--
-- = Description
--
-- When 'cmdSetEvent2' is submitted to a queue, it defines the first half
-- of memory dependencies defined by @pDependencyInfo@, as well as an event
-- signal operation which sets the event to the signaled state. A memory
-- dependency is defined between the event signal operation and commands
-- that occur earlier in submission order.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- are defined by the union of all the memory dependencies defined by
-- @pDependencyInfo@, and are applied to all operations that occur earlier
-- in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers Queue family ownership transfers>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>
-- defined by @pDependencyInfo@ are also included in the first scopes.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the event signal operation, and any
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfers>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>
-- defined by @pDependencyInfo@.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- includes only
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfers>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>.
--
-- Future 'cmdWaitEvents2' commands rely on all values of each element in
-- @pDependencyInfo@ matching exactly with those used to signal the
-- corresponding event. 'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents'
-- /must/ not be used to wait on the result of a signal operation defined
-- by 'cmdSetEvent2'.
--
-- Note
--
-- The extra information provided by 'cmdSetEvent2' compared to
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent' allows implementations
-- to more efficiently schedule the operations required to satisfy the
-- requested dependencies. With
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent', the full dependency
-- information is not known until
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents' is recorded, forcing
-- implementations to insert the required operations at that point and not
-- before.
--
-- If @event@ is already in the signaled state when 'cmdSetEvent2' is
-- executed on the device, then 'cmdSetEvent2' has no effect, no event
-- signal operation occurs, and no dependency is generated.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetEvent2-synchronization2-03824# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetEvent2-dependencyFlags-03825# The @dependencyFlags@
--     member of @pDependencyInfo@ /must/ be @0@
--
-- -   #VUID-vkCmdSetEvent2-srcStageMask-09391# The @srcStageMask@ member
--     of any element of the @pMemoryBarriers@, @pBufferMemoryBarriers@, or
--     @pImageMemoryBarriers@ members of @pDependencyInfo@ /must/ not
--     include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-vkCmdSetEvent2-dstStageMask-09392# The @dstStageMask@ member
--     of any element of the @pMemoryBarriers@, @pBufferMemoryBarriers@, or
--     @pImageMemoryBarriers@ members of @pDependencyInfo@ /must/ not
--     include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-vkCmdSetEvent2-commandBuffer-03826# The current device mask of
--     @commandBuffer@ /must/ include exactly one physical device
--
-- -   #VUID-vkCmdSetEvent2-srcStageMask-03827# The @srcStageMask@ member
--     of any element of the @pMemoryBarriers@, @pBufferMemoryBarriers@, or
--     @pImageMemoryBarriers@ members of @pDependencyInfo@ /must/ only
--     include pipeline stages valid for the queue family that was used to
--     create the command pool that @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdSetEvent2-dstStageMask-03828# The @dstStageMask@ member
--     of any element of the @pMemoryBarriers@, @pBufferMemoryBarriers@, or
--     @pImageMemoryBarriers@ members of @pDependencyInfo@ /must/ only
--     include pipeline stages valid for the queue family that was used to
--     create the command pool that @commandBuffer@ was allocated from
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetEvent2-commandBuffer-parameter# @commandBuffer@ /must/
--     be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetEvent2-event-parameter# @event@ /must/ be a valid
--     'Vulkan.Core10.Handles.Event' handle
--
-- -   #VUID-vkCmdSetEvent2-pDependencyInfo-parameter# @pDependencyInfo@
--     /must/ be a valid pointer to a valid 'DependencyInfo' structure
--
-- -   #VUID-vkCmdSetEvent2-commandBuffer-recording# @commandBuffer@ /must/
--     be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetEvent2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, compute, decode, or encode
--     operations
--
-- -   #VUID-vkCmdSetEvent2-renderpass# This command /must/ only be called
--     outside of a render pass instance
--
-- -   #VUID-vkCmdSetEvent2-commonparent# Both of @commandBuffer@, and
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DependencyInfo',
-- 'Vulkan.Core10.Handles.Event'
cmdSetEvent2 :: forall io
              . (MonadIO io)
             => -- | @commandBuffer@ is the command buffer into which the command is
                -- recorded.
                CommandBuffer
             -> -- | @event@ is the event that will be signaled.
                Event
             -> -- | @pDependencyInfo@ is a pointer to a 'DependencyInfo' structure defining
                -- the first scopes of this operation.
                DependencyInfo
             -> io ()
cmdSetEvent2 commandBuffer event dependencyInfo = liftIO . evalContT $ do
  let vkCmdSetEvent2Ptr = pVkCmdSetEvent2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetEvent2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetEvent2 is null" Nothing Nothing
  let vkCmdSetEvent2' = mkVkCmdSetEvent2 vkCmdSetEvent2Ptr
  pDependencyInfo <- ContT $ withCStruct (dependencyInfo)
  lift $ traceAroundEvent "vkCmdSetEvent2" (vkCmdSetEvent2'
                                              (commandBufferHandle (commandBuffer))
                                              (event)
                                              pDependencyInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResetEvent2
  :: FunPtr (Ptr CommandBuffer_T -> Event -> PipelineStageFlags2 -> IO ()) -> Ptr CommandBuffer_T -> Event -> PipelineStageFlags2 -> IO ()

-- | vkCmdResetEvent2 - Reset an event object to non-signaled state
--
-- = Description
--
-- When 'cmdResetEvent2' is submitted to a queue, it defines an execution
-- dependency on commands that were submitted before it, and defines an
-- event unsignal operation which resets the event to the unsignaled state.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- The synchronization scope is limited to operations by @stageMask@ or
-- stages that are
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earlier>
-- than @stageMask@.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the event unsignal operation.
--
-- If @event@ is already in the unsignaled state when 'cmdResetEvent2' is
-- executed on the device, then this command has no effect, no event
-- unsignal operation occurs, and no execution dependency is generated.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdResetEvent2-stageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdResetEvent2-stageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdResetEvent2-stageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent2-stageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent2-stageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent2-stageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent2-stageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdResetEvent2-stageMask-07316# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdResetEvent2-stageMask-04957# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subpassShading subpassShading>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--
-- -   #VUID-vkCmdResetEvent2-stageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocationMask>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-vkCmdResetEvent2-stageMask-07946# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline feature>
--     are enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-vkCmdResetEvent2-synchronization2-03829# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdResetEvent2-stageMask-03830# @stageMask@ /must/ not
--     include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-vkCmdResetEvent2-event-03831# There /must/ be an execution
--     dependency between 'cmdResetEvent2' and the execution of any
--     'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents' that includes
--     @event@ in its @pEvents@ parameter
--
-- -   #VUID-vkCmdResetEvent2-event-03832# There /must/ be an execution
--     dependency between 'cmdResetEvent2' and the execution of any
--     'cmdWaitEvents2' that includes @event@ in its @pEvents@ parameter
--
-- -   #VUID-vkCmdResetEvent2-commandBuffer-03833# @commandBuffer@’s
--     current device mask /must/ include exactly one physical device
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdResetEvent2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdResetEvent2-event-parameter# @event@ /must/ be a valid
--     'Vulkan.Core10.Handles.Event' handle
--
-- -   #VUID-vkCmdResetEvent2-stageMask-parameter# @stageMask@ /must/ be a
--     valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
--
-- -   #VUID-vkCmdResetEvent2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdResetEvent2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, compute, decode, or encode
--     operations
--
-- -   #VUID-vkCmdResetEvent2-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdResetEvent2-commonparent# Both of @commandBuffer@, and
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'Vulkan.Core10.Handles.Event',
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2'
cmdResetEvent2 :: forall io
                . (MonadIO io)
               => -- | @commandBuffer@ is the command buffer into which the command is
                  -- recorded.
                  CommandBuffer
               -> -- | @event@ is the event that will be unsignaled.
                  Event
               -> -- | @stageMask@ is a
                  -- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2' mask of
                  -- pipeline stages used to determine the first
                  -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>.
                  ("stageMask" ::: PipelineStageFlags2)
               -> io ()
cmdResetEvent2 commandBuffer event stageMask = liftIO $ do
  let vkCmdResetEvent2Ptr = pVkCmdResetEvent2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdResetEvent2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdResetEvent2 is null" Nothing Nothing
  let vkCmdResetEvent2' = mkVkCmdResetEvent2 vkCmdResetEvent2Ptr
  traceAroundEvent "vkCmdResetEvent2" (vkCmdResetEvent2'
                                         (commandBufferHandle (commandBuffer))
                                         (event)
                                         (stageMask))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWaitEvents2Unsafe
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> Ptr DependencyInfo -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> Ptr DependencyInfo -> IO ()

foreign import ccall
  "dynamic" mkVkCmdWaitEvents2Safe
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> Ptr DependencyInfo -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> Ptr DependencyInfo -> IO ()

-- | cmdWaitEvents2 with selectable safeness
cmdWaitEvents2SafeOrUnsafe :: forall io
                            . (MonadIO io)
                           => (FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> Ptr DependencyInfo -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> Ptr DependencyInfo -> IO ())
                           -> -- | @commandBuffer@ is the command buffer into which the command is
                              -- recorded.
                              CommandBuffer
                           -> -- | @pEvents@ is a pointer to an array of @eventCount@ events to wait on.
                              ("events" ::: Vector Event)
                           -> -- | @pDependencyInfos@ is a pointer to an array of @eventCount@
                              -- 'DependencyInfo' structures, defining the second
                              -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>.
                              ("dependencyInfos" ::: Vector DependencyInfo)
                           -> io ()
cmdWaitEvents2SafeOrUnsafe mkVkCmdWaitEvents2 commandBuffer
                                                events
                                                dependencyInfos = liftIO . evalContT $ do
  let vkCmdWaitEvents2Ptr = pVkCmdWaitEvents2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdWaitEvents2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWaitEvents2 is null" Nothing Nothing
  let vkCmdWaitEvents2' = mkVkCmdWaitEvents2 vkCmdWaitEvents2Ptr
  let pEventsLength = Data.Vector.length $ (events)
  lift $ unless ((Data.Vector.length $ (dependencyInfos)) == pEventsLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pDependencyInfos and pEvents must have the same length" Nothing Nothing
  pPEvents <- ContT $ allocaBytes @Event ((Data.Vector.length (events)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPEvents `plusPtr` (8 * (i)) :: Ptr Event) (e)) (events)
  pPDependencyInfos <- ContT $ allocaBytes @DependencyInfo ((Data.Vector.length (dependencyInfos)) * 64)
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPDependencyInfos `plusPtr` (64 * (i)) :: Ptr DependencyInfo) (e) . ($ ())) (dependencyInfos)
  lift $ traceAroundEvent "vkCmdWaitEvents2" (vkCmdWaitEvents2'
                                                (commandBufferHandle (commandBuffer))
                                                ((fromIntegral pEventsLength :: Word32))
                                                (pPEvents)
                                                (pPDependencyInfos))
  pure $ ()

-- | vkCmdWaitEvents2 - Wait for one or more events
--
-- = Description
--
-- When 'cmdWaitEvents2' is submitted to a queue, it inserts memory
-- dependencies according to the elements of @pDependencyInfos@ and each
-- corresponding element of @pEvents@. 'cmdWaitEvents2' /must/ not be used
-- to wait on event signal operations occurring on other queues, or signal
-- operations executed by
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent'.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- of each memory dependency defined by any element i of @pDependencyInfos@
-- are applied to operations that occurred earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- than the last event signal operation on element i of @pEvents@.
--
-- Signal operations for an event at index i are only included if:
--
-- -   The event was signaled by a 'cmdSetEvent2' command that occurred
--     earlier in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>
--     with a @dependencyInfo@ parameter exactly equal to the element of
--     @pDependencyInfos@ at index i ; or
--
-- -   The event was created without
--     'Vulkan.Core10.Enums.EventCreateFlagBits.EVENT_CREATE_DEVICE_ONLY_BIT',
--     and the first
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
--     defined by the element of @pDependencyInfos@ at index i only
--     includes host operations
--     ('Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT').
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- of each memory dependency defined by any element i of @pDependencyInfos@
-- are applied to operations that occurred later in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- than 'cmdWaitEvents2'.
--
-- Note
--
-- 'cmdWaitEvents2' is used with 'cmdSetEvent2' to define a memory
-- dependency between two sets of action commands, roughly in the same way
-- as pipeline barriers, but split into two commands such that work between
-- the two /may/ execute unhindered.
--
-- Note
--
-- Applications should be careful to avoid race conditions when using
-- events. There is no direct ordering guarantee between 'cmdSetEvent2' and
-- 'cmdResetEvent2', 'Vulkan.Core10.CommandBufferBuilding.cmdResetEvent',
-- or 'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent'. Another execution
-- dependency (e.g. a pipeline barrier or semaphore with
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT')
-- is needed to prevent such a race condition.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdWaitEvents2-synchronization2-03836# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdWaitEvents2-pEvents-03837# Members of @pEvents@ /must/
--     not have been signaled by
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent'
--
-- -   #VUID-vkCmdWaitEvents2-pEvents-03838# For any element i of
--     @pEvents@, if that event is signaled by 'cmdSetEvent2', that
--     command’s @dependencyInfo@ parameter /must/ be exactly equal to the
--     ith element of @pDependencyInfos@
--
-- -   #VUID-vkCmdWaitEvents2-pEvents-03839# For any element i of
--     @pEvents@, if that event is signaled by
--     'Vulkan.Core10.Event.setEvent', barriers in the ith element of
--     @pDependencyInfos@ /must/ include only host operations in their
--     first
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
--
-- -   #VUID-vkCmdWaitEvents2-pEvents-03840# For any element i of
--     @pEvents@, if barriers in the ith element of @pDependencyInfos@
--     include only host operations, the ith element of @pEvents@ /must/ be
--     signaled before 'cmdWaitEvents2' is executed
--
-- -   #VUID-vkCmdWaitEvents2-pEvents-03841# For any element i of
--     @pEvents@, if barriers in the ith element of @pDependencyInfos@ do
--     not include host operations, the ith element of @pEvents@ /must/ be
--     signaled by a corresponding 'cmdSetEvent2' that occurred earlier in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>
--
-- -   #VUID-vkCmdWaitEvents2-srcStageMask-03842# The @srcStageMask@ member
--     of any element of the @pMemoryBarriers@, @pBufferMemoryBarriers@, or
--     @pImageMemoryBarriers@ members of @pDependencyInfos@ /must/ either
--     include only pipeline stages valid for the queue family that was
--     used to create the command pool that @commandBuffer@ was allocated
--     from
--
-- -   #VUID-vkCmdWaitEvents2-dstStageMask-03843# The @dstStageMask@ member
--     of any element of the @pMemoryBarriers@, @pBufferMemoryBarriers@, or
--     @pImageMemoryBarriers@ members of @pDependencyInfos@ /must/ only
--     include pipeline stages valid for the queue family that was used to
--     create the command pool that @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdWaitEvents2-dependencyFlags-03844# If 'cmdWaitEvents2' is
--     being called inside a render pass instance, the @srcStageMask@
--     member of any element of the @pMemoryBarriers@,
--     @pBufferMemoryBarriers@, or @pImageMemoryBarriers@ members of
--     @pDependencyInfos@ /must/ not include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-vkCmdWaitEvents2-commandBuffer-03846# @commandBuffer@’s
--     current device mask /must/ include exactly one physical device
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdWaitEvents2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdWaitEvents2-pEvents-parameter# @pEvents@ /must/ be a
--     valid pointer to an array of @eventCount@ valid
--     'Vulkan.Core10.Handles.Event' handles
--
-- -   #VUID-vkCmdWaitEvents2-pDependencyInfos-parameter#
--     @pDependencyInfos@ /must/ be a valid pointer to an array of
--     @eventCount@ valid 'DependencyInfo' structures
--
-- -   #VUID-vkCmdWaitEvents2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdWaitEvents2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, compute, decode, or encode
--     operations
--
-- -   #VUID-vkCmdWaitEvents2-eventCount-arraylength# @eventCount@ /must/
--     be greater than @0@
--
-- -   #VUID-vkCmdWaitEvents2-commonparent# Both of @commandBuffer@, and
--     the elements of @pEvents@ /must/ have been created, allocated, or
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DependencyInfo',
-- 'Vulkan.Core10.Handles.Event'
cmdWaitEvents2 :: forall io
                . (MonadIO io)
               => -- | @commandBuffer@ is the command buffer into which the command is
                  -- recorded.
                  CommandBuffer
               -> -- | @pEvents@ is a pointer to an array of @eventCount@ events to wait on.
                  ("events" ::: Vector Event)
               -> -- | @pDependencyInfos@ is a pointer to an array of @eventCount@
                  -- 'DependencyInfo' structures, defining the second
                  -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>.
                  ("dependencyInfos" ::: Vector DependencyInfo)
               -> io ()
cmdWaitEvents2 = cmdWaitEvents2SafeOrUnsafe mkVkCmdWaitEvents2Unsafe

-- | A variant of 'cmdWaitEvents2' which makes a *safe* FFI call
cmdWaitEvents2Safe :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer into which the command is
                      -- recorded.
                      CommandBuffer
                   -> -- | @pEvents@ is a pointer to an array of @eventCount@ events to wait on.
                      ("events" ::: Vector Event)
                   -> -- | @pDependencyInfos@ is a pointer to an array of @eventCount@
                      -- 'DependencyInfo' structures, defining the second
                      -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>.
                      ("dependencyInfos" ::: Vector DependencyInfo)
                   -> io ()
cmdWaitEvents2Safe = cmdWaitEvents2SafeOrUnsafe mkVkCmdWaitEvents2Safe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPipelineBarrier2
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DependencyInfo -> IO ()) -> Ptr CommandBuffer_T -> Ptr DependencyInfo -> IO ()

-- | vkCmdPipelineBarrier2 - Insert a memory dependency
--
-- = Description
--
-- When 'cmdPipelineBarrier2' is submitted to a queue, it defines memory
-- dependencies between commands that were submitted to the same queue
-- before it, and those submitted to the same queue after it.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- of each memory dependency defined by @pDependencyInfo@ are applied to
-- operations that occurred earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- of each memory dependency defined by @pDependencyInfo@ are applied to
-- operations that occurred later in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
--
-- If 'cmdPipelineBarrier2' is recorded within a render pass instance, the
-- synchronization scopes are limited to operations within the same subpass
-- , or /must/ follow the restrictions for
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-barriers-explicit-renderpass-tileimage Tile Image Access Synchronization>
-- if the render pass instance was started with
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
-- .
--
-- == Valid Usage
--
-- -   #VUID-vkCmdPipelineBarrier2-None-07889# If 'cmdPipelineBarrier2' is
--     called within a render pass instance using a
--     'Vulkan.Core10.Handles.RenderPass' object, the render pass /must/
--     have been created with at least one subpass dependency that
--     expresses a dependency from the current subpass to itself, does not
--     include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_BY_REGION_BIT' if
--     this command does not, does not include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT'
--     if this command does not, and has
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scopes>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scopes>
--     that are all supersets of the scopes defined in this command
--
-- -   #VUID-vkCmdPipelineBarrier2-bufferMemoryBarrierCount-01178# If
--     'cmdPipelineBarrier2' is called within a render pass instance using
--     a 'Vulkan.Core10.Handles.RenderPass' object, it /must/ not include
--     any buffer memory barriers
--
-- -   #VUID-vkCmdPipelineBarrier2-image-04073# If 'cmdPipelineBarrier2' is
--     called within a render pass instance using a
--     'Vulkan.Core10.Handles.RenderPass' object, the @image@ member of any
--     image memory barrier included in this command /must/ be an
--     attachment used in the current subpass both as an input attachment,
--     and as either a color, color resolve, or depth\/stencil attachment
--
-- -   #VUID-vkCmdPipelineBarrier2-image-09373# If 'cmdPipelineBarrier2' is
--     called within a render pass instance using a
--     'Vulkan.Core10.Handles.RenderPass' object, and the @image@ member of
--     any image memory barrier is a color resolve attachment, the
--     corresponding color attachment /must/ be
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED'
--
-- -   #VUID-vkCmdPipelineBarrier2-image-09374# If 'cmdPipelineBarrier2' is
--     called within a render pass instance using a
--     'Vulkan.Core10.Handles.RenderPass' object, and the @image@ member of
--     any image memory barrier is a color resolve attachment, it /must/
--     have been created with a non-zero
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     value
--
-- -   #VUID-vkCmdPipelineBarrier2-oldLayout-01181# If
--     'cmdPipelineBarrier2' is called within a render pass instance, the
--     @oldLayout@ and @newLayout@ members of any image memory barrier
--     included in this command /must/ be equal
--
-- -   #VUID-vkCmdPipelineBarrier2-srcQueueFamilyIndex-01182# If
--     'cmdPipelineBarrier2' is called within a render pass instance, the
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ members of any
--     memory barrier included in this command /must/ be equal
--
-- -   #VUID-vkCmdPipelineBarrier2-None-07890# If 'cmdPipelineBarrier2' is
--     called within a render pass instance, and the source stage masks of
--     any memory barriers include
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stages>,
--     destination stage masks of all memory barriers /must/ only include
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stages>
--
-- -   #VUID-vkCmdPipelineBarrier2-dependencyFlags-07891# If
--     'cmdPipelineBarrier2' is called within a render pass instance, and
--     and the source stage masks of any memory barriers include
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space stages>,
--     then @dependencyFlags@ /must/ include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_BY_REGION_BIT'
--
-- -   #VUID-vkCmdPipelineBarrier2-None-07892# If 'cmdPipelineBarrier2' is
--     called within a render pass instance, the source and destination
--     stage masks of any memory barriers /must/ only include graphics
--     pipeline stages
--
-- -   #VUID-vkCmdPipelineBarrier2-dependencyFlags-01186# If
--     'cmdPipelineBarrier2' is called outside of a render pass instance,
--     the dependency flags /must/ not include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT'
--
-- -   #VUID-vkCmdPipelineBarrier2-None-07893# If 'cmdPipelineBarrier2' is
--     called inside a render pass instance, and there is more than one
--     view in the current subpass, dependency flags /must/ include
--     'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_VIEW_LOCAL_BIT'
--
-- -   #VUID-vkCmdPipelineBarrier2-shaderTileImageColorReadAccess-08718# If
--     'cmdPipelineBarrier2' is called within a render pass instance and
--     none of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderTileImageColorReadAccess shaderTileImageColorReadAccess>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderTileImageDepthReadAccess shaderTileImageDepthReadAccess>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderTileImageStencilReadAccess shaderTileImageStencilReadAccess>
--     features are enabled, the render pass /must/ not have been started
--     with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering'
--
-- -   #VUID-vkCmdPipelineBarrier2-None-08719# If 'cmdPipelineBarrier2' is
--     called within a render pass instance started with
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.cmdBeginRendering',
--     it /must/ adhere to the restrictions in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-barriers-explicit-renderpass-tileimage Explicit Render Pass Tile Image Access Synchronization>
--
-- -   #VUID-vkCmdPipelineBarrier2-synchronization2-03848# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdPipelineBarrier2-srcStageMask-03849# The @srcStageMask@
--     member of any element of the @pMemoryBarriers@,
--     @pBufferMemoryBarriers@, or @pImageMemoryBarriers@ members of
--     @pDependencyInfo@ /must/ only include pipeline stages valid for the
--     queue family that was used to create the command pool that
--     @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdPipelineBarrier2-dstStageMask-03850# The @dstStageMask@
--     member of any element of the @pMemoryBarriers@,
--     @pBufferMemoryBarriers@, or @pImageMemoryBarriers@ members of
--     @pDependencyInfo@ /must/ only include pipeline stages valid for the
--     queue family that was used to create the command pool that
--     @commandBuffer@ was allocated from
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdPipelineBarrier2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdPipelineBarrier2-pDependencyInfo-parameter#
--     @pDependencyInfo@ /must/ be a valid pointer to a valid
--     'DependencyInfo' structure
--
-- -   #VUID-vkCmdPipelineBarrier2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdPipelineBarrier2-commandBuffer-cmdpool# The
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DependencyInfo'
cmdPipelineBarrier2 :: forall io
                     . (MonadIO io)
                    => -- | @commandBuffer@ is the command buffer into which the command is
                       -- recorded.
                       CommandBuffer
                    -> -- | @pDependencyInfo@ is a pointer to a 'DependencyInfo' structure defining
                       -- the scopes of this operation.
                       DependencyInfo
                    -> io ()
cmdPipelineBarrier2 commandBuffer dependencyInfo = liftIO . evalContT $ do
  let vkCmdPipelineBarrier2Ptr = pVkCmdPipelineBarrier2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdPipelineBarrier2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPipelineBarrier2 is null" Nothing Nothing
  let vkCmdPipelineBarrier2' = mkVkCmdPipelineBarrier2 vkCmdPipelineBarrier2Ptr
  pDependencyInfo <- ContT $ withCStruct (dependencyInfo)
  lift $ traceAroundEvent "vkCmdPipelineBarrier2" (vkCmdPipelineBarrier2'
                                                     (commandBufferHandle (commandBuffer))
                                                     pDependencyInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueSubmit2
  :: FunPtr (Ptr Queue_T -> Word32 -> Ptr (SomeStruct SubmitInfo2) -> Fence -> IO Result) -> Ptr Queue_T -> Word32 -> Ptr (SomeStruct SubmitInfo2) -> Fence -> IO Result

-- | vkQueueSubmit2 - Submits command buffers to a queue
--
-- = Description
--
-- 'queueSubmit2' is a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-submission queue submission command>,
-- with each batch defined by an element of @pSubmits@.
--
-- Semaphore operations submitted with 'queueSubmit2' have additional
-- ordering constraints compared to other submission commands, with
-- dependencies involving previous and subsequent queue operations.
-- Information about these additional constraints can be found in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores semaphore>
-- section of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization the synchronization chapter>.
--
-- If any command buffer submitted to this queue is in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>,
-- it is moved to the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
-- Once execution of all submissions of a command buffer complete, it moves
-- from the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>,
-- back to the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>.
-- If a command buffer was recorded with the
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT'
-- flag, it instead moves back to the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle invalid state>.
--
-- If 'queueSubmit2' fails, it /may/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY' or
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'. If it does, the
-- implementation /must/ ensure that the state and contents of any
-- resources or synchronization primitives referenced by the submitted
-- command buffers and any semaphores referenced by @pSubmits@ is
-- unaffected by the call or its failure. If 'queueSubmit2' fails in such a
-- way that the implementation is unable to make that guarantee, the
-- implementation /must/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'. See
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>.
--
-- == Valid Usage
--
-- -   #VUID-vkQueueSubmit2-fence-04894# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ be
--     unsignaled
--
-- -   #VUID-vkQueueSubmit2-fence-04895# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ not be
--     associated with any other queue command that has not yet completed
--     execution on that queue
--
-- -   #VUID-vkQueueSubmit2-synchronization2-03866# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkQueueSubmit2-commandBuffer-03867# If a command recorded into
--     the @commandBuffer@ member of any element of the
--     @pCommandBufferInfos@ member of any element of @pSubmits@ referenced
--     an 'Vulkan.Core10.Handles.Event', that event /must/ not be
--     referenced by a command that has been submitted to another queue and
--     is still in the /pending state/
--
-- -   #VUID-vkQueueSubmit2-semaphore-03868# The @semaphore@ member of any
--     binary semaphore element of the @pSignalSemaphoreInfos@ member of
--     any element of @pSubmits@ /must/ be unsignaled when the semaphore
--     signal operation it defines is executed on the device
--
-- -   #VUID-vkQueueSubmit2-stageMask-03869# The @stageMask@ member of any
--     element of the @pSignalSemaphoreInfos@ member of any element of
--     @pSubmits@ /must/ only include pipeline stages that are supported by
--     the queue family which @queue@ belongs to
--
-- -   #VUID-vkQueueSubmit2-stageMask-03870# The @stageMask@ member of any
--     element of the @pWaitSemaphoreInfos@ member of any element of
--     @pSubmits@ /must/ only include pipeline stages that are supported by
--     the queue family which @queue@ belongs to
--
-- -   #VUID-vkQueueSubmit2-semaphore-03871# When a semaphore wait
--     operation for a binary semaphore is executed, as defined by the
--     @semaphore@ member of any element of the @pWaitSemaphoreInfos@
--     member of any element of @pSubmits@, there /must/ be no other queues
--     waiting on the same semaphore
--
-- -   #VUID-vkQueueSubmit2-semaphore-03873# The @semaphore@ member of any
--     element of the @pWaitSemaphoreInfos@ member of any element of
--     @pSubmits@ that was created with a
--     'Vulkan.Extensions.VK_KHR_timeline_semaphore.SemaphoreTypeKHR' of
--     'Vulkan.Extensions.VK_KHR_timeline_semaphore.SEMAPHORE_TYPE_BINARY_KHR'
--     /must/ reference a semaphore signal operation that has been
--     submitted for execution and any
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operations>
--     on which it depends /must/ have also been submitted for execution
--
-- -   #VUID-vkQueueSubmit2-commandBuffer-03874# The @commandBuffer@ member
--     of any element of the @pCommandBufferInfos@ member of any element of
--     @pSubmits@ /must/ be in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>
--
-- -   #VUID-vkQueueSubmit2-commandBuffer-03875# If a command recorded into
--     the @commandBuffer@ member of any element of the
--     @pCommandBufferInfos@ member of any element of @pSubmits@ was not
--     recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT',
--     it /must/ not be in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   #VUID-vkQueueSubmit2-commandBuffer-03876# Any
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-secondary secondary command buffers recorded>
--     into the @commandBuffer@ member of any element of the
--     @pCommandBufferInfos@ member of any element of @pSubmits@ /must/ be
--     in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>
--
-- -   #VUID-vkQueueSubmit2-commandBuffer-03877# If any
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-secondary secondary command buffers recorded>
--     into the @commandBuffer@ member of any element of the
--     @pCommandBufferInfos@ member of any element of @pSubmits@ was not
--     recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT',
--     it /must/ not be in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   #VUID-vkQueueSubmit2-commandBuffer-03878# The @commandBuffer@ member
--     of any element of the @pCommandBufferInfos@ member of any element of
--     @pSubmits@ /must/ have been allocated from a
--     'Vulkan.Core10.Handles.CommandPool' that was created for the same
--     queue family @queue@ belongs to
--
-- -   #VUID-vkQueueSubmit2-commandBuffer-03879# If a command recorded into
--     the @commandBuffer@ member of any element of the
--     @pCommandBufferInfos@ member of any element of @pSubmits@ includes a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-acquire Queue Family Transfer Acquire Operation>,
--     there /must/ exist a previously submitted
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-release Queue Family Transfer Release Operation>
--     on a queue in the queue family identified by the acquire operation,
--     with parameters matching the acquire operation as defined in the
--     definition of such
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-acquire acquire operations>,
--     and which happens before the acquire operation
--
-- -   #VUID-vkQueueSubmit2-commandBuffer-03880# If a command recorded into
--     the @commandBuffer@ member of any element of the
--     @pCommandBufferInfos@ member of any element of @pSubmits@ was a
--     'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery' whose
--     @queryPool@ was created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#profiling-lock profiling lock>
--     /must/ have been held continuously on the
--     'Vulkan.Core10.Handles.Device' that @queue@ was retrieved from,
--     throughout recording of those command buffers
--
-- -   #VUID-vkQueueSubmit2-queue-06447# If @queue@ was not created with
--     'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_PROTECTED_BIT',
--     the @flags@ member of any element of @pSubmits@ /must/ not include
--     'Vulkan.Core13.Enums.SubmitFlagBits.SUBMIT_PROTECTED_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkQueueSubmit2-queue-parameter# @queue@ /must/ be a valid
--     'Vulkan.Core10.Handles.Queue' handle
--
-- -   #VUID-vkQueueSubmit2-pSubmits-parameter# If @submitCount@ is not
--     @0@, @pSubmits@ /must/ be a valid pointer to an array of
--     @submitCount@ valid 'SubmitInfo2' structures
--
-- -   #VUID-vkQueueSubmit2-fence-parameter# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ be a valid
--     'Vulkan.Core10.Handles.Fence' handle
--
-- -   #VUID-vkQueueSubmit2-commonparent# Both of @fence@, and @queue@ that
--     are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @queue@ /must/ be externally synchronized
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | -                                                                                                                           | Any                                                                                                                   | -                                                                                                                                      |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.Fence', 'Vulkan.Core10.Handles.Queue',
-- 'SubmitInfo2'
queueSubmit2 :: forall io
              . (MonadIO io)
             => -- | @queue@ is the queue that the command buffers will be submitted to.
                Queue
             -> -- | @pSubmits@ is a pointer to an array of 'SubmitInfo2' structures, each
                -- specifying a command buffer submission batch.
                ("submits" ::: Vector (SomeStruct SubmitInfo2))
             -> -- | @fence@ is an /optional/ handle to a fence to be signaled once all
                -- submitted command buffers have completed execution. If @fence@ is not
                -- 'Vulkan.Core10.APIConstants.NULL_HANDLE', it defines a
                -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-fences-signaling fence signal operation>.
                Fence
             -> io ()
queueSubmit2 queue submits fence = liftIO . evalContT $ do
  let vkQueueSubmit2Ptr = pVkQueueSubmit2 (case queue of Queue{deviceCmds} -> deviceCmds)
  lift $ unless (vkQueueSubmit2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueSubmit2 is null" Nothing Nothing
  let vkQueueSubmit2' = mkVkQueueSubmit2 vkQueueSubmit2Ptr
  pPSubmits <- ContT $ allocaBytes @(SubmitInfo2 _) ((Data.Vector.length (submits)) * 64)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPSubmits `plusPtr` (64 * (i)) :: Ptr (SubmitInfo2 _))) (e) . ($ ())) (submits)
  r <- lift $ traceAroundEvent "vkQueueSubmit2" (vkQueueSubmit2'
                                                   (queueHandle (queue))
                                                   ((fromIntegral (Data.Vector.length $ (submits)) :: Word32))
                                                   (forgetExtensions (pPSubmits))
                                                   (fence))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteTimestamp2
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlags2 -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlags2 -> QueryPool -> Word32 -> IO ()

-- | vkCmdWriteTimestamp2 - Write a device timestamp into a query object
--
-- = Description
--
-- When 'cmdWriteTimestamp2' is submitted to a queue, it defines an
-- execution dependency on commands that were submitted before it, and
-- writes a timestamp to a query pool.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- The synchronization scope is limited to operations on the pipeline stage
-- specified by @stage@.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the timestamp write operation.
--
-- Note
--
-- Implementations may write the timestamp at any stage that is
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically later>
-- than @stage@.
--
-- Any timestamp write that
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-execution happens-after>
-- another timestamp write in the same submission /must/ not have a lower
-- value unless its value overflows the maximum supported integer bit width
-- of the query. If @VK_KHR_calibrated_timestamps@ or
-- @VK_EXT_calibrated_timestamps@ is enabled, this extends to timestamp
-- writes across all submissions on the same logical device: any timestamp
-- write that
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-execution happens-after>
-- another /must/ not have a lower value unless its value overflows the
-- maximum supported integer bit width of the query. Timestamps written by
-- this command /must/ be in the
-- 'Vulkan.Extensions.VK_KHR_calibrated_timestamps.TIME_DOMAIN_DEVICE_KHR'
-- <VkTimeDomainKHR.html time domain>. If an overflow occurs, the timestamp
-- value /must/ wrap back to zero.
--
-- Note
--
-- Comparisons between timestamps should be done between timestamps where
-- they are guaranteed to not decrease. For example, subtracting an older
-- timestamp from a newer one to determine the execution time of a sequence
-- of commands is only a reliable measurement if the two timestamp writes
-- were performed in the same submission, or if the writes were performed
-- on the same logical device and @VK_KHR_calibrated_timestamps@ or
-- @VK_EXT_calibrated_timestamps@ is enabled.
--
-- If 'cmdWriteTimestamp2' is called while executing a render pass instance
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
-- -   #VUID-vkCmdWriteTimestamp2-stage-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-07316# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-04957# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subpassShading subpassShading>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocationMask>
--     feature is not enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-07946# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline feature>
--     are enabled, @stage@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-vkCmdWriteTimestamp2-synchronization2-03858# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-03859# @stage@ /must/ only include
--     a single pipeline stage
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-03860# @stage@ /must/ only include
--     stages valid for the queue family that was used to create the
--     command pool that @commandBuffer@ was allocated from
--
-- -   #VUID-vkCmdWriteTimestamp2-queryPool-03861# @queryPool@ /must/ have
--     been created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIMESTAMP'
--
-- -   #VUID-vkCmdWriteTimestamp2-timestampValidBits-03863# The command
--     pool’s queue family /must/ support a non-zero @timestampValidBits@
--
-- -   #VUID-vkCmdWriteTimestamp2-query-04903# @query@ /must/ be less than
--     the number of queries in @queryPool@
--
-- -   #VUID-vkCmdWriteTimestamp2-None-03864# All queries used by the
--     command /must/ be /unavailable/
--
-- -   #VUID-vkCmdWriteTimestamp2-query-03865# If 'cmdWriteTimestamp2' is
--     called within a render pass instance, the sum of @query@ and the
--     number of bits set in the current subpass’s view mask /must/ be less
--     than or equal to the number of queries in @queryPool@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdWriteTimestamp2-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdWriteTimestamp2-stage-parameter# @stage@ /must/ be a
--     valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
--
-- -   #VUID-vkCmdWriteTimestamp2-queryPool-parameter# @queryPool@ /must/
--     be a valid 'Vulkan.Core10.Handles.QueryPool' handle
--
-- -   #VUID-vkCmdWriteTimestamp2-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdWriteTimestamp2-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support transfer, graphics, compute, decode,
--     or encode operations
--
-- -   #VUID-vkCmdWriteTimestamp2-commonparent# Both of @commandBuffer@,
--     and @queryPool@ /must/ have been created, allocated, or retrieved
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Both                                                                                                                        | Transfer                                                                                                              | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | Graphics                                                                                                              |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Compute                                                                                                               |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Decode                                                                                                                |                                                                                                                                        |
-- |                                                                                                                            |                                                                                                                        |                                                                                                                             | Encode                                                                                                                |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2',
-- 'Vulkan.Core10.Handles.QueryPool'
cmdWriteTimestamp2 :: forall io
                    . (MonadIO io)
                   => -- | @commandBuffer@ is the command buffer into which the command will be
                      -- recorded.
                      CommandBuffer
                   -> -- | @stage@ specifies a stage of the pipeline.
                      PipelineStageFlags2
                   -> -- | @queryPool@ is the query pool that will manage the timestamp.
                      QueryPool
                   -> -- | @query@ is the query within the query pool that will contain the
                      -- timestamp.
                      ("query" ::: Word32)
                   -> io ()
cmdWriteTimestamp2 commandBuffer stage queryPool query = liftIO $ do
  let vkCmdWriteTimestamp2Ptr = pVkCmdWriteTimestamp2 (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdWriteTimestamp2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteTimestamp2 is null" Nothing Nothing
  let vkCmdWriteTimestamp2' = mkVkCmdWriteTimestamp2 vkCmdWriteTimestamp2Ptr
  traceAroundEvent "vkCmdWriteTimestamp2" (vkCmdWriteTimestamp2'
                                             (commandBufferHandle (commandBuffer))
                                             (stage)
                                             (queryPool)
                                             (query))
  pure $ ()


-- | VkMemoryBarrier2 - Structure specifying a global memory barrier
--
-- = Description
--
-- This structure defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-memory memory dependency>
-- affecting all device memory.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @srcStageMask@ and @srcAccessMask@.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @dstStageMask@ and @dstAccessMask@.
--
-- == Valid Usage
--
-- -   #VUID-VkMemoryBarrier2-srcStageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcStageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcStageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2-srcStageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2-srcStageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2-srcStageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2-srcStageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2-srcStageMask-07316# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2-srcStageMask-04957# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subpassShading subpassShading>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--
-- -   #VUID-VkMemoryBarrier2-srcStageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocationMask>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkMemoryBarrier2-srcStageMask-07946# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline feature>
--     are enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03900# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INDIRECT_COMMAND_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03901# If @srcAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INDEX_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INDEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03902# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03903# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INPUT_ATTACHMENT_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03904# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_UNIFORM_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03905# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_SAMPLED_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03906# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03907# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-07454# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03909# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03910# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03911# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03912# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03913# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03914# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFER_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_TRANSFER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03915# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFER_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CLEAR_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_TRANSFER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03916# If @srcAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_HOST_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03917# If @srcAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_HOST_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03918# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03919# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03920# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-04747# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03922# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03923# If @srcAccessMask@
--     includes
--     'Vulkan.Extensions.VK_KHR_synchronization2.ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Extensions.VK_KHR_synchronization2.PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-04994# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03924# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03925# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03926# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03927# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-03928# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-06256# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayQuery rayQuery>
--     feature is not enabled and @srcAccessMask@ includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @srcStageMask@ /must/ not include any of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages except
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-07272# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-04858# If @srcAccessMask@
--     includes @VK_ACCESS_2_VIDEO_DECODE_READ_BIT_KHR@, @srcStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR@
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-04859# If @srcAccessMask@
--     includes @VK_ACCESS_2_VIDEO_DECODE_WRITE_BIT_KHR@, @srcStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR@
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-04860# If @srcAccessMask@
--     includes @VK_ACCESS_2_VIDEO_ENCODE_READ_BIT_KHR@, @srcStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR@
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-04861# If @srcAccessMask@
--     includes @VK_ACCESS_2_VIDEO_ENCODE_WRITE_BIT_KHR@, @srcStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR@
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-07455# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-07456# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_WRITE_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-07457# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_WRITE_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-07458# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-08118# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DESCRIPTOR_BUFFER_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-dstStageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstStageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstStageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2-dstStageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2-dstStageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2-dstStageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2-dstStageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2-dstStageMask-07316# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2-dstStageMask-04957# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subpassShading subpassShading>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--
-- -   #VUID-VkMemoryBarrier2-dstStageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocationMask>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkMemoryBarrier2-dstStageMask-07946# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline feature>
--     are enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03900# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INDIRECT_COMMAND_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03901# If @dstAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INDEX_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INDEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03902# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03903# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INPUT_ATTACHMENT_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03904# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_UNIFORM_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03905# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_SAMPLED_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03906# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03907# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-07454# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03909# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03910# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03911# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03912# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03913# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03914# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFER_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_TRANSFER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03915# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFER_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CLEAR_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_TRANSFER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03916# If @dstAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_HOST_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03917# If @dstAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_HOST_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03918# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03919# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03920# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-04747# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03922# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03923# If @dstAccessMask@
--     includes
--     'Vulkan.Extensions.VK_KHR_synchronization2.ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Extensions.VK_KHR_synchronization2.PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-04994# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03924# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03925# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03926# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03927# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-03928# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-06256# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayQuery rayQuery>
--     feature is not enabled and @dstAccessMask@ includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @dstStageMask@ /must/ not include any of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages except
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-07272# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-04858# If @dstAccessMask@
--     includes @VK_ACCESS_2_VIDEO_DECODE_READ_BIT_KHR@, @dstStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR@
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-04859# If @dstAccessMask@
--     includes @VK_ACCESS_2_VIDEO_DECODE_WRITE_BIT_KHR@, @dstStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR@
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-04860# If @dstAccessMask@
--     includes @VK_ACCESS_2_VIDEO_ENCODE_READ_BIT_KHR@, @dstStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR@
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-04861# If @dstAccessMask@
--     includes @VK_ACCESS_2_VIDEO_ENCODE_WRITE_BIT_KHR@, @dstStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR@
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-07455# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-07456# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_WRITE_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-07457# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_WRITE_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-07458# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-08118# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DESCRIPTOR_BUFFER_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryBarrier2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_BARRIER_2'
--
-- -   #VUID-VkMemoryBarrier2-srcStageMask-parameter# @srcStageMask@ /must/
--     be a valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
--
-- -   #VUID-VkMemoryBarrier2-srcAccessMask-parameter# @srcAccessMask@
--     /must/ be a valid combination of
--     'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2' values
--
-- -   #VUID-VkMemoryBarrier2-dstStageMask-parameter# @dstStageMask@ /must/
--     be a valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
--
-- -   #VUID-VkMemoryBarrier2-dstAccessMask-parameter# @dstAccessMask@
--     /must/ be a valid combination of
--     'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2' values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2', 'DependencyInfo',
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryBarrier2 = MemoryBarrier2
  { -- | @srcStageMask@ is a
    -- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2' mask of
    -- pipeline stages to be included in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes first synchronization scope>.
    srcStageMask :: PipelineStageFlags2
  , -- | @srcAccessMask@ is a 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2'
    -- mask of access flags to be included in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes first access scope>.
    srcAccessMask :: AccessFlags2
  , -- | @dstStageMask@ is a
    -- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2' mask of
    -- pipeline stages to be included in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes second synchronization scope>.
    dstStageMask :: PipelineStageFlags2
  , -- | @dstAccessMask@ is a 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2'
    -- mask of access flags to be included in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes second access scope>.
    dstAccessMask :: AccessFlags2
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryBarrier2)
#endif
deriving instance Show MemoryBarrier2

instance ToCStruct MemoryBarrier2 where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryBarrier2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_BARRIER_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2)) (srcStageMask)
    poke ((p `plusPtr` 24 :: Ptr AccessFlags2)) (srcAccessMask)
    poke ((p `plusPtr` 32 :: Ptr PipelineStageFlags2)) (dstStageMask)
    poke ((p `plusPtr` 40 :: Ptr AccessFlags2)) (dstAccessMask)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_BARRIER_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct MemoryBarrier2 where
  peekCStruct p = do
    srcStageMask <- peek @PipelineStageFlags2 ((p `plusPtr` 16 :: Ptr PipelineStageFlags2))
    srcAccessMask <- peek @AccessFlags2 ((p `plusPtr` 24 :: Ptr AccessFlags2))
    dstStageMask <- peek @PipelineStageFlags2 ((p `plusPtr` 32 :: Ptr PipelineStageFlags2))
    dstAccessMask <- peek @AccessFlags2 ((p `plusPtr` 40 :: Ptr AccessFlags2))
    pure $ MemoryBarrier2
             srcStageMask srcAccessMask dstStageMask dstAccessMask

instance Storable MemoryBarrier2 where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryBarrier2 where
  zero = MemoryBarrier2
           zero
           zero
           zero
           zero


-- | VkImageMemoryBarrier2 - Structure specifying an image memory barrier
--
-- = Description
--
-- This structure defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-memory memory dependency>
-- limited to an image subresource range, and /can/ define a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>
-- for that subresource range.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @srcStageMask@ and @srcAccessMask@.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @dstStageMask@ and @dstAccessMask@.
--
-- Both
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scopes>
-- are limited to only memory accesses to @image@ in the subresource range
-- defined by @subresourceRange@.
--
-- If @image@ was created with
-- 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
-- @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, this memory
-- barrier defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>.
-- When executed on a queue in the family identified by
-- @srcQueueFamilyIndex@, this barrier defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- for the specified image subresource range, and the second
-- synchronization and access scopes do not synchronize operations on that
-- queue. When executed on a queue in the family identified by
-- @dstQueueFamilyIndex@, this barrier defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- for the specified image subresource range, and the first synchronization
-- and access scopes do not synchronize operations on that queue.
--
-- A
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>
-- is also defined if the values are not equal, and either is one of the
-- special queue family values reserved for external memory ownership
-- transfers, as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers>.
-- A
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- is defined when @dstQueueFamilyIndex@ is one of those values, and a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- is defined when @srcQueueFamilyIndex@ is one of those values.
--
-- If @oldLayout@ is not equal to @newLayout@, then the memory barrier
-- defines an
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>
-- for the specified image subresource range. If this memory barrier
-- defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>,
-- the layout transition is only executed once between the queues.
--
-- Note
--
-- When the old and new layout are equal, the layout values are ignored -
-- data is preserved no matter what values are specified, or what layout
-- the image is currently in.
--
-- If @image@ has a multi-planar format and the image is /disjoint/, then
-- including
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT' in the
-- @aspectMask@ member of @subresourceRange@ is equivalent to including
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT',
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT', and
-- (for three-plane formats only)
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT'.
--
-- == Valid Usage
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-07316# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-04957# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subpassShading subpassShading>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocationMask>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-07946# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline feature>
--     are enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03900# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INDIRECT_COMMAND_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03901# If @srcAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INDEX_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INDEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03902# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03903# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INPUT_ATTACHMENT_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03904# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_UNIFORM_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03905# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_SAMPLED_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03906# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03907# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-07454# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03909# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03910# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03911# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03912# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03913# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03914# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFER_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_TRANSFER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03915# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFER_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CLEAR_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_TRANSFER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03916# If @srcAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_HOST_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03917# If @srcAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_HOST_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03918# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03919# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03920# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-04747# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03922# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03923# If @srcAccessMask@
--     includes
--     'Vulkan.Extensions.VK_KHR_synchronization2.ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Extensions.VK_KHR_synchronization2.PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-04994# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03924# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03925# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03926# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03927# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-03928# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-06256# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayQuery rayQuery>
--     feature is not enabled and @srcAccessMask@ includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @srcStageMask@ /must/ not include any of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages except
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-07272# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-04858# If @srcAccessMask@
--     includes @VK_ACCESS_2_VIDEO_DECODE_READ_BIT_KHR@, @srcStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-04859# If @srcAccessMask@
--     includes @VK_ACCESS_2_VIDEO_DECODE_WRITE_BIT_KHR@, @srcStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-04860# If @srcAccessMask@
--     includes @VK_ACCESS_2_VIDEO_ENCODE_READ_BIT_KHR@, @srcStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-04861# If @srcAccessMask@
--     includes @VK_ACCESS_2_VIDEO_ENCODE_WRITE_BIT_KHR@, @srcStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-07455# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-07456# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_WRITE_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-07457# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_WRITE_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-07458# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-08118# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DESCRIPTOR_BUFFER_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-dstStageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstStageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstStageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstStageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstStageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstStageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstStageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstStageMask-07316# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2-dstStageMask-04957# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subpassShading subpassShading>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--
-- -   #VUID-VkImageMemoryBarrier2-dstStageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocationMask>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkImageMemoryBarrier2-dstStageMask-07946# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline feature>
--     are enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03900# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INDIRECT_COMMAND_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03901# If @dstAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INDEX_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INDEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03902# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03903# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INPUT_ATTACHMENT_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03904# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_UNIFORM_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03905# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_SAMPLED_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03906# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03907# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-07454# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03909# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03910# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03911# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03912# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03913# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03914# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFER_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_TRANSFER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03915# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFER_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CLEAR_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_TRANSFER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03916# If @dstAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_HOST_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03917# If @dstAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_HOST_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03918# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03919# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03920# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-04747# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03922# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03923# If @dstAccessMask@
--     includes
--     'Vulkan.Extensions.VK_KHR_synchronization2.ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Extensions.VK_KHR_synchronization2.PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-04994# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03924# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03925# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03926# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03927# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-03928# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-06256# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayQuery rayQuery>
--     feature is not enabled and @dstAccessMask@ includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @dstStageMask@ /must/ not include any of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages except
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-07272# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-04858# If @dstAccessMask@
--     includes @VK_ACCESS_2_VIDEO_DECODE_READ_BIT_KHR@, @dstStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-04859# If @dstAccessMask@
--     includes @VK_ACCESS_2_VIDEO_DECODE_WRITE_BIT_KHR@, @dstStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-04860# If @dstAccessMask@
--     includes @VK_ACCESS_2_VIDEO_ENCODE_READ_BIT_KHR@, @dstStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-04861# If @dstAccessMask@
--     includes @VK_ACCESS_2_VIDEO_ENCODE_WRITE_BIT_KHR@, @dstStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-07455# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-07456# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_WRITE_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-07457# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_WRITE_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-07458# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-08118# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DESCRIPTOR_BUFFER_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkImageMemoryBarrier2-oldLayout-01208# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-oldLayout-01209# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-oldLayout-01210# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-oldLayout-01211# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT' or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-oldLayout-01212# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_SRC_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-oldLayout-01213# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSFER_DST_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-oldLayout-01197# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     @oldLayout@ /must/ be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or the
--     current layout of the image subresources affected by the barrier
--
-- -   #VUID-VkImageMemoryBarrier2-newLayout-01198# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     @newLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   #VUID-VkImageMemoryBarrier2-oldLayout-01658# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-oldLayout-01659# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-04065# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with at least one of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-04066# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     set
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-04067# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with at least one of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-04068# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     set
--
-- -   #VUID-VkImageMemoryBarrier2-synchronization2-07793# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @oldLayout@ /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2-synchronization2-07794# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-synchronization2 synchronization2>
--     feature is not enabled, @newLayout@ /must/ not be
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_synchronization2.IMAGE_LAYOUT_READ_ONLY_OPTIMAL_KHR'
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-03938# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_OPTIMAL',
--     @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-03939# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_READ_ONLY_OPTIMAL',
--     @image@ /must/ have been created with at least one of
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT',
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT', or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-oldLayout-02088# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--     then @image@ /must/ have been created with
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--     set
--
-- -   #VUID-VkImageMemoryBarrier2-image-09117# If @image@ was created with
--     a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not equal,
--     @srcQueueFamilyIndex@ /must/ be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL',
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT', or a valid
--     queue family
--
-- -   #VUID-VkImageMemoryBarrier2-image-09118# If @image@ was created with
--     a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not equal,
--     @dstQueueFamilyIndex@ /must/ be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL',
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT', or a valid
--     queue family
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-04070# If
--     @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, at
--     least one of @srcQueueFamilyIndex@ or @dstQueueFamilyIndex@ /must/
--     not be 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL' or
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-None-09119# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory VK_KHR_external_memory>
--     extension is not enabled, and the value of
--     'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@apiVersion@
--     used to create the 'Vulkan.Core10.Handles.Instance' is not greater
--     than or equal to Version 1.1, @srcQueueFamilyIndex@ /must/ not be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL'
--
-- -   #VUID-VkImageMemoryBarrier2-None-09120# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory VK_KHR_external_memory>
--     extension is not enabled, and the value of
--     'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@apiVersion@
--     used to create the 'Vulkan.Core10.Handles.Instance' is not greater
--     than or equal to Version 1.1, @dstQueueFamilyIndex@ /must/ not be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL'
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-09121# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_queue_family_foreign VK_EXT_queue_family_foreign>
--     extension is not enabled @srcQueueFamilyIndex@ /must/ not be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-dstQueueFamilyIndex-09122# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_queue_family_foreign VK_EXT_queue_family_foreign>
--     extension is not enabled @dstQueueFamilyIndex@ /must/ not be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-07120# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_VIDEO_DECODE_SRC_KHR@ then @image@ /must/ have been
--     created with @VK_IMAGE_USAGE_VIDEO_DECODE_SRC_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-07121# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_VIDEO_DECODE_DST_KHR@ then @image@ /must/ have been
--     created with @VK_IMAGE_USAGE_VIDEO_DECODE_DST_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-07122# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_VIDEO_DECODE_DPB_KHR@ then @image@ /must/ have been
--     created with @VK_IMAGE_USAGE_VIDEO_DECODE_DPB_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-07123# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_VIDEO_ENCODE_SRC_KHR@ then @image@ /must/ have been
--     created with @VK_IMAGE_USAGE_VIDEO_ENCODE_SRC_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-07124# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_VIDEO_ENCODE_DST_KHR@ then @image@ /must/ have been
--     created with @VK_IMAGE_USAGE_VIDEO_ENCODE_DST_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-07125# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_VIDEO_ENCODE_DPB_KHR@ then @image@ /must/ have been
--     created with @VK_IMAGE_USAGE_VIDEO_ENCODE_DPB_BIT_KHR@
--
-- -   #VUID-VkImageMemoryBarrier2-srcQueueFamilyIndex-07006# If
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     and @oldLayout@ or @newLayout@ is
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     then @image@ /must/ have been created with either the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     usage bits, and the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     or 'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_SAMPLED_BIT'
--     usage bits, and the
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--     usage bit
--
-- -   #VUID-VkImageMemoryBarrier2-attachmentFeedbackLoopLayout-07313# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFeedbackLoopLayout attachmentFeedbackLoopLayout>
--     feature is not enabled, @newLayout@ /must/ not be
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--
-- -   #VUID-VkImageMemoryBarrier2-subresourceRange-01486#
--     @subresourceRange.baseMipLevel@ /must/ be less than the @mipLevels@
--     specified in 'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was
--     created
--
-- -   #VUID-VkImageMemoryBarrier2-subresourceRange-01724# If
--     @subresourceRange.levelCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_MIP_LEVELS',
--     @subresourceRange.baseMipLevel@ + @subresourceRange.levelCount@
--     /must/ be less than or equal to the @mipLevels@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkImageMemoryBarrier2-subresourceRange-01488#
--     @subresourceRange.baseArrayLayer@ /must/ be less than the
--     @arrayLayers@ specified in 'Vulkan.Core10.Image.ImageCreateInfo'
--     when @image@ was created
--
-- -   #VUID-VkImageMemoryBarrier2-subresourceRange-01725# If
--     @subresourceRange.layerCount@ is not
--     'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS',
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the @arrayLayers@ specified in
--     'Vulkan.Core10.Image.ImageCreateInfo' when @image@ was created
--
-- -   #VUID-VkImageMemoryBarrier2-image-01932# If @image@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkImageMemoryBarrier2-image-09241# If @image@ has a color
--     format that is single-plane, then the @aspectMask@ member of
--     @subresourceRange@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-image-09242# If @image@ has a color
--     format and is not /disjoint/, then the @aspectMask@ member of
--     @subresourceRange@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-image-01672# If @image@ has a
--     multi-planar format and the image is /disjoint/, then the
--     @aspectMask@ member of @subresourceRange@ /must/ include at least
--     one
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-image-03320# If @image@ has a
--     depth\/stencil format with both depth and stencil and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is not enabled, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include both
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-image-03319# If @image@ has a
--     depth\/stencil format with both depth and stencil and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-separateDepthStencilLayouts separateDepthStencilLayouts>
--     feature is enabled, then the @aspectMask@ member of
--     @subresourceRange@ /must/ include either or both
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' and
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkImageMemoryBarrier2-aspectMask-08702# If the @aspectMask@
--     member of @subresourceRange@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT',
--     @oldLayout@ and @newLayout@ /must/ not be one of
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkImageMemoryBarrier2-aspectMask-08703# If the @aspectMask@
--     member of @subresourceRange@ includes
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT',
--     @oldLayout@ and @newLayout@ /must/ not be one of
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL'
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-03854# If either
--     @srcStageMask@ or @dstStageMask@ includes
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT',
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/ be equal
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-03855# If @srcStageMask@
--     includes
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT',
--     and @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ define a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
--     or @oldLayout@ and @newLayout@ define an
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>,
--     @oldLayout@ /must/ be one of
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED',
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED', or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkImageMemoryBarrier2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2'
--
-- -   #VUID-VkImageMemoryBarrier2-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_external_memory_acquire_unmodified.ExternalMemoryAcquireUnmodifiedEXT'
--     or
--     'Vulkan.Extensions.VK_EXT_sample_locations.SampleLocationsInfoEXT'
--
-- -   #VUID-VkImageMemoryBarrier2-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkImageMemoryBarrier2-srcStageMask-parameter# @srcStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
--
-- -   #VUID-VkImageMemoryBarrier2-srcAccessMask-parameter# @srcAccessMask@
--     /must/ be a valid combination of
--     'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2' values
--
-- -   #VUID-VkImageMemoryBarrier2-dstStageMask-parameter# @dstStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
--
-- -   #VUID-VkImageMemoryBarrier2-dstAccessMask-parameter# @dstAccessMask@
--     /must/ be a valid combination of
--     'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2' values
--
-- -   #VUID-VkImageMemoryBarrier2-oldLayout-parameter# @oldLayout@ /must/
--     be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkImageMemoryBarrier2-newLayout-parameter# @newLayout@ /must/
--     be a valid 'Vulkan.Core10.Enums.ImageLayout.ImageLayout' value
--
-- -   #VUID-VkImageMemoryBarrier2-image-parameter# @image@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkImageMemoryBarrier2-subresourceRange-parameter#
--     @subresourceRange@ /must/ be a valid
--     'Vulkan.Core10.ImageView.ImageSubresourceRange' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2', 'DependencyInfo',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.ImageLayout.ImageLayout',
-- 'Vulkan.Core10.ImageView.ImageSubresourceRange',
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data ImageMemoryBarrier2 (es :: [Type]) = ImageMemoryBarrier2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @srcStageMask@ is a
    -- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2' mask of
    -- pipeline stages to be included in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes first synchronization scope>.
    srcStageMask :: PipelineStageFlags2
  , -- | @srcAccessMask@ is a 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2'
    -- mask of access flags to be included in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes first access scope>.
    srcAccessMask :: AccessFlags2
  , -- | @dstStageMask@ is a
    -- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2' mask of
    -- pipeline stages to be included in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes second synchronization scope>.
    dstStageMask :: PipelineStageFlags2
  , -- | @dstAccessMask@ is a 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2'
    -- mask of access flags to be included in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes second access scope>.
    dstAccessMask :: AccessFlags2
  , -- | @oldLayout@ is the old layout in an
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>.
    oldLayout :: ImageLayout
  , -- | @newLayout@ is the new layout in an
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>.
    newLayout :: ImageLayout
  , -- | @srcQueueFamilyIndex@ is the source queue family for a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    srcQueueFamilyIndex :: Word32
  , -- | @dstQueueFamilyIndex@ is the destination queue family for a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    dstQueueFamilyIndex :: Word32
  , -- | @image@ is a handle to the image affected by this barrier.
    image :: Image
  , -- | @subresourceRange@ describes the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-image-views image subresource range>
    -- within @image@ that is affected by this barrier.
    subresourceRange :: ImageSubresourceRange
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageMemoryBarrier2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ImageMemoryBarrier2 es)

instance Extensible ImageMemoryBarrier2 where
  extensibleTypeName = "ImageMemoryBarrier2"
  setNext ImageMemoryBarrier2{..} next' = ImageMemoryBarrier2{next = next', ..}
  getNext ImageMemoryBarrier2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ImageMemoryBarrier2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ExternalMemoryAcquireUnmodifiedEXT = Just f
    | Just Refl <- eqT @e @SampleLocationsInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss ImageMemoryBarrier2 es
         , PokeChain es ) => ToCStruct (ImageMemoryBarrier2 es) where
  withCStruct x f = allocaBytes 96 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageMemoryBarrier2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2)) (srcStageMask)
    lift $ poke ((p `plusPtr` 24 :: Ptr AccessFlags2)) (srcAccessMask)
    lift $ poke ((p `plusPtr` 32 :: Ptr PipelineStageFlags2)) (dstStageMask)
    lift $ poke ((p `plusPtr` 40 :: Ptr AccessFlags2)) (dstAccessMask)
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageLayout)) (oldLayout)
    lift $ poke ((p `plusPtr` 52 :: Ptr ImageLayout)) (newLayout)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (srcQueueFamilyIndex)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (dstQueueFamilyIndex)
    lift $ poke ((p `plusPtr` 64 :: Ptr Image)) (image)
    lift $ poke ((p `plusPtr` 72 :: Ptr ImageSubresourceRange)) (subresourceRange)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 48 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 52 :: Ptr ImageLayout)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 72 :: Ptr ImageSubresourceRange)) (zero)
    lift $ f

instance ( Extendss ImageMemoryBarrier2 es
         , PeekChain es ) => FromCStruct (ImageMemoryBarrier2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    srcStageMask <- peek @PipelineStageFlags2 ((p `plusPtr` 16 :: Ptr PipelineStageFlags2))
    srcAccessMask <- peek @AccessFlags2 ((p `plusPtr` 24 :: Ptr AccessFlags2))
    dstStageMask <- peek @PipelineStageFlags2 ((p `plusPtr` 32 :: Ptr PipelineStageFlags2))
    dstAccessMask <- peek @AccessFlags2 ((p `plusPtr` 40 :: Ptr AccessFlags2))
    oldLayout <- peek @ImageLayout ((p `plusPtr` 48 :: Ptr ImageLayout))
    newLayout <- peek @ImageLayout ((p `plusPtr` 52 :: Ptr ImageLayout))
    srcQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    dstQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    image <- peek @Image ((p `plusPtr` 64 :: Ptr Image))
    subresourceRange <- peekCStruct @ImageSubresourceRange ((p `plusPtr` 72 :: Ptr ImageSubresourceRange))
    pure $ ImageMemoryBarrier2
             next
             srcStageMask
             srcAccessMask
             dstStageMask
             dstAccessMask
             oldLayout
             newLayout
             srcQueueFamilyIndex
             dstQueueFamilyIndex
             image
             subresourceRange

instance es ~ '[] => Zero (ImageMemoryBarrier2 es) where
  zero = ImageMemoryBarrier2
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkBufferMemoryBarrier2 - Structure specifying a buffer memory barrier
--
-- = Description
--
-- This structure defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-memory memory dependency>
-- limited to a range of a buffer, and /can/ define a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>
-- for that range.
--
-- The first
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @srcStageMask@ and @srcAccessMask@.
--
-- The second
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- described by this structure include only operations and memory accesses
-- specified by @dstStageMask@ and @dstAccessMask@.
--
-- Both
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scopes>
-- are limited to only memory accesses to @buffer@ in the range defined by
-- @offset@ and @size@.
--
-- If @buffer@ was created with
-- 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
-- @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, this memory
-- barrier defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>.
-- When executed on a queue in the family identified by
-- @srcQueueFamilyIndex@, this barrier defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- for the specified buffer range, and the second synchronization and
-- access scopes do not synchronize operations on that queue. When executed
-- on a queue in the family identified by @dstQueueFamilyIndex@, this
-- barrier defines a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- for the specified buffer range, and the first synchronization and access
-- scopes do not synchronize operations on that queue.
--
-- A
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operation>
-- is also defined if the values are not equal, and either is one of the
-- special queue family values reserved for external memory ownership
-- transfers, as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers>.
-- A
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- is defined when @dstQueueFamilyIndex@ is one of those values, and a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- is defined when @srcQueueFamilyIndex@ is one of those values.
--
-- == Valid Usage
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-07316# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-04957# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subpassShading subpassShading>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocationMask>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-07946# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline feature>
--     are enabled, @srcStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03900# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INDIRECT_COMMAND_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03901# If @srcAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INDEX_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INDEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03902# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03903# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INPUT_ATTACHMENT_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03904# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_UNIFORM_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03905# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_SAMPLED_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03906# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03907# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-07454# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03909# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03910# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03911# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03912# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03913# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03914# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFER_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_TRANSFER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03915# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFER_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CLEAR_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_TRANSFER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03916# If @srcAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_HOST_READ_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03917# If @srcAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_HOST_WRITE_BIT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03918# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03919# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03920# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-04747# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03922# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03923# If @srcAccessMask@
--     includes
--     'Vulkan.Extensions.VK_KHR_synchronization2.ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Extensions.VK_KHR_synchronization2.PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-04994# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03924# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03925# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03926# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03927# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-03928# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-06256# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayQuery rayQuery>
--     feature is not enabled and @srcAccessMask@ includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @srcStageMask@ /must/ not include any of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages except
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-07272# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-04858# If @srcAccessMask@
--     includes @VK_ACCESS_2_VIDEO_DECODE_READ_BIT_KHR@, @srcStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR@
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-04859# If @srcAccessMask@
--     includes @VK_ACCESS_2_VIDEO_DECODE_WRITE_BIT_KHR@, @srcStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR@
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-04860# If @srcAccessMask@
--     includes @VK_ACCESS_2_VIDEO_ENCODE_READ_BIT_KHR@, @srcStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR@
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-04861# If @srcAccessMask@
--     includes @VK_ACCESS_2_VIDEO_ENCODE_WRITE_BIT_KHR@, @srcStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR@
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-07455# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_READ_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-07456# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_WRITE_BIT_NV',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-07457# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_WRITE_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-07458# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-08118# If @srcAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DESCRIPTOR_BUFFER_READ_BIT_EXT',
--     @srcStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-dstStageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstStageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstStageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstStageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstStageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstStageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstStageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstStageMask-07316# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstStageMask-04957# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subpassShading subpassShading>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstStageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocationMask>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstStageMask-07946# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline feature>
--     are enabled, @dstStageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03900# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INDIRECT_COMMAND_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03901# If @dstAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INDEX_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INDEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03902# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_INPUT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03903# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INPUT_ATTACHMENT_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03904# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_UNIFORM_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03905# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_SAMPLED_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03906# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03907# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_STORAGE_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-07454# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03909# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03910# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03911# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03912# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03913# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03914# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFER_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_TRANSFER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03915# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFER_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CLEAR_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_TRANSFER_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03916# If @dstAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_HOST_READ_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03917# If @dstAccessMask@
--     includes 'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_HOST_WRITE_BIT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03918# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03919# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03920# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-04747# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03922# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03923# If @dstAccessMask@
--     includes
--     'Vulkan.Extensions.VK_KHR_synchronization2.ACCESS_2_SHADING_RATE_IMAGE_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Extensions.VK_KHR_synchronization2.PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-04994# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03924# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03925# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03926# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03927# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of the @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-03928# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-06256# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayQuery rayQuery>
--     feature is not enabled and @dstAccessMask@ includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR',
--     @dstStageMask@ /must/ not include any of the
--     @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages except
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-07272# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-04858# If @dstAccessMask@
--     includes @VK_ACCESS_2_VIDEO_DECODE_READ_BIT_KHR@, @dstStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR@
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-04859# If @dstAccessMask@
--     includes @VK_ACCESS_2_VIDEO_DECODE_WRITE_BIT_KHR@, @dstStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR@
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-04860# If @dstAccessMask@
--     includes @VK_ACCESS_2_VIDEO_ENCODE_READ_BIT_KHR@, @dstStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR@
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-04861# If @dstAccessMask@
--     includes @VK_ACCESS_2_VIDEO_ENCODE_WRITE_BIT_KHR@, @dstStageMask@
--     /must/ include @VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR@
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-07455# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_READ_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-07456# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_OPTICAL_FLOW_WRITE_BIT_NV',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-07457# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_WRITE_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-07458# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_MICROMAP_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-08118# If @dstAccessMask@
--     includes
--     'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DESCRIPTOR_BUFFER_READ_BIT_EXT',
--     @dstStageMask@ /must/ include
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_GRAPHICS_BIT',
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT',
--     or one of @VK_PIPELINE_STAGE_*_SHADER_BIT@ stages
--
-- -   #VUID-VkBufferMemoryBarrier2-offset-01187# @offset@ /must/ be less
--     than the size of @buffer@
--
-- -   #VUID-VkBufferMemoryBarrier2-size-01188# If @size@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be greater
--     than @0@
--
-- -   #VUID-VkBufferMemoryBarrier2-size-01189# If @size@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be less than
--     or equal to than the size of @buffer@ minus @offset@
--
-- -   #VUID-VkBufferMemoryBarrier2-buffer-01931# If @buffer@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkBufferMemoryBarrier2-buffer-09095# If @buffer@ was created
--     with a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not equal,
--     @srcQueueFamilyIndex@ /must/ be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL',
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT', or a valid
--     queue family
--
-- -   #VUID-VkBufferMemoryBarrier2-buffer-09096# If @buffer@ was created
--     with a sharing mode of
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE', and
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not equal,
--     @dstQueueFamilyIndex@ /must/ be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL',
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT', or a valid
--     queue family
--
-- -   #VUID-VkBufferMemoryBarrier2-srcQueueFamilyIndex-04087# If
--     @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, at
--     least one of @srcQueueFamilyIndex@ or @dstQueueFamilyIndex@ /must/
--     not be 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL' or
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-None-09097# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory VK_KHR_external_memory>
--     extension is not enabled, and the value of
--     'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@apiVersion@
--     used to create the 'Vulkan.Core10.Handles.Instance' is not greater
--     than or equal to Version 1.1, @srcQueueFamilyIndex@ /must/ not be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL'
--
-- -   #VUID-VkBufferMemoryBarrier2-None-09098# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_external_memory VK_KHR_external_memory>
--     extension is not enabled, and the value of
--     'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@apiVersion@
--     used to create the 'Vulkan.Core10.Handles.Instance' is not greater
--     than or equal to Version 1.1, @dstQueueFamilyIndex@ /must/ not be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcQueueFamilyIndex-09099# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_queue_family_foreign VK_EXT_queue_family_foreign>
--     extension is not enabled @srcQueueFamilyIndex@ /must/ not be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-dstQueueFamilyIndex-09100# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_queue_family_foreign VK_EXT_queue_family_foreign>
--     extension is not enabled @dstQueueFamilyIndex@ /must/ not be
--     'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-03851# If either
--     @srcStageMask@ or @dstStageMask@ includes
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT',
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/ be equal
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferMemoryBarrier2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2'
--
-- -   #VUID-VkBufferMemoryBarrier2-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_external_memory_acquire_unmodified.ExternalMemoryAcquireUnmodifiedEXT'
--
-- -   #VUID-VkBufferMemoryBarrier2-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkBufferMemoryBarrier2-srcStageMask-parameter# @srcStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
--
-- -   #VUID-VkBufferMemoryBarrier2-srcAccessMask-parameter#
--     @srcAccessMask@ /must/ be a valid combination of
--     'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2' values
--
-- -   #VUID-VkBufferMemoryBarrier2-dstStageMask-parameter# @dstStageMask@
--     /must/ be a valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
--
-- -   #VUID-VkBufferMemoryBarrier2-dstAccessMask-parameter#
--     @dstAccessMask@ /must/ be a valid combination of
--     'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2' values
--
-- -   #VUID-VkBufferMemoryBarrier2-buffer-parameter# @buffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2',
-- 'Vulkan.Core10.Handles.Buffer', 'DependencyInfo',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferMemoryBarrier2 (es :: [Type]) = BufferMemoryBarrier2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @srcStageMask@ is a
    -- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2' mask of
    -- pipeline stages to be included in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes first synchronization scope>.
    srcStageMask :: PipelineStageFlags2
  , -- | @srcAccessMask@ is a 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2'
    -- mask of access flags to be included in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes first access scope>.
    srcAccessMask :: AccessFlags2
  , -- | @dstStageMask@ is a
    -- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2' mask of
    -- pipeline stages to be included in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-scopes second synchronization scope>.
    dstStageMask :: PipelineStageFlags2
  , -- | @dstAccessMask@ is a 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2'
    -- mask of access flags to be included in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-access-scopes second access scope>.
    dstAccessMask :: AccessFlags2
  , -- | @srcQueueFamilyIndex@ is the source queue family for a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    srcQueueFamilyIndex :: Word32
  , -- | @dstQueueFamilyIndex@ is the destination queue family for a
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>.
    dstQueueFamilyIndex :: Word32
  , -- | @buffer@ is a handle to the buffer whose backing memory is affected by
    -- the barrier.
    buffer :: Buffer
  , -- | @offset@ is an offset in bytes into the backing memory for @buffer@;
    -- this is relative to the base offset as bound to the buffer (see
    -- 'Vulkan.Core10.MemoryManagement.bindBufferMemory').
    offset :: DeviceSize
  , -- | @size@ is a size in bytes of the affected area of backing memory for
    -- @buffer@, or 'Vulkan.Core10.APIConstants.WHOLE_SIZE' to use the range
    -- from @offset@ to the end of the buffer.
    size :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferMemoryBarrier2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BufferMemoryBarrier2 es)

instance Extensible BufferMemoryBarrier2 where
  extensibleTypeName = "BufferMemoryBarrier2"
  setNext BufferMemoryBarrier2{..} next' = BufferMemoryBarrier2{next = next', ..}
  getNext BufferMemoryBarrier2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BufferMemoryBarrier2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ExternalMemoryAcquireUnmodifiedEXT = Just f
    | otherwise = Nothing

instance ( Extendss BufferMemoryBarrier2 es
         , PokeChain es ) => ToCStruct (BufferMemoryBarrier2 es) where
  withCStruct x f = allocaBytes 80 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferMemoryBarrier2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineStageFlags2)) (srcStageMask)
    lift $ poke ((p `plusPtr` 24 :: Ptr AccessFlags2)) (srcAccessMask)
    lift $ poke ((p `plusPtr` 32 :: Ptr PipelineStageFlags2)) (dstStageMask)
    lift $ poke ((p `plusPtr` 40 :: Ptr AccessFlags2)) (dstAccessMask)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (srcQueueFamilyIndex)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (dstQueueFamilyIndex)
    lift $ poke ((p `plusPtr` 56 :: Ptr Buffer)) (buffer)
    lift $ poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (offset)
    lift $ poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (size)
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Buffer)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (zero)
    lift $ f

instance ( Extendss BufferMemoryBarrier2 es
         , PeekChain es ) => FromCStruct (BufferMemoryBarrier2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    srcStageMask <- peek @PipelineStageFlags2 ((p `plusPtr` 16 :: Ptr PipelineStageFlags2))
    srcAccessMask <- peek @AccessFlags2 ((p `plusPtr` 24 :: Ptr AccessFlags2))
    dstStageMask <- peek @PipelineStageFlags2 ((p `plusPtr` 32 :: Ptr PipelineStageFlags2))
    dstAccessMask <- peek @AccessFlags2 ((p `plusPtr` 40 :: Ptr AccessFlags2))
    srcQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    dstQueueFamilyIndex <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    buffer <- peek @Buffer ((p `plusPtr` 56 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 64 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 72 :: Ptr DeviceSize))
    pure $ BufferMemoryBarrier2
             next
             srcStageMask
             srcAccessMask
             dstStageMask
             dstAccessMask
             srcQueueFamilyIndex
             dstQueueFamilyIndex
             buffer
             offset
             size

instance es ~ '[] => Zero (BufferMemoryBarrier2 es) where
  zero = BufferMemoryBarrier2
           ()
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkDependencyInfo - Structure specifying dependency information for a
-- synchronization command
--
-- = Description
--
-- This structure defines a set of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-memory memory dependencies>,
-- as well as
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-queue-transfers queue family transfer operations>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>.
--
-- Each member of @pMemoryBarriers@, @pBufferMemoryBarriers@, and
-- @pImageMemoryBarriers@ defines a separate
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-dependencies-memory memory dependency>.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDependencyInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEPENDENCY_INFO'
--
-- -   #VUID-VkDependencyInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkDependencyInfo-dependencyFlags-parameter# @dependencyFlags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits' values
--
-- -   #VUID-VkDependencyInfo-pMemoryBarriers-parameter# If
--     @memoryBarrierCount@ is not @0@, @pMemoryBarriers@ /must/ be a valid
--     pointer to an array of @memoryBarrierCount@ valid 'MemoryBarrier2'
--     structures
--
-- -   #VUID-VkDependencyInfo-pBufferMemoryBarriers-parameter# If
--     @bufferMemoryBarrierCount@ is not @0@, @pBufferMemoryBarriers@
--     /must/ be a valid pointer to an array of @bufferMemoryBarrierCount@
--     valid 'BufferMemoryBarrier2' structures
--
-- -   #VUID-VkDependencyInfo-pImageMemoryBarriers-parameter# If
--     @imageMemoryBarrierCount@ is not @0@, @pImageMemoryBarriers@ /must/
--     be a valid pointer to an array of @imageMemoryBarrierCount@ valid
--     'ImageMemoryBarrier2' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'BufferMemoryBarrier2',
-- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlags',
-- 'ImageMemoryBarrier2', 'MemoryBarrier2',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdPipelineBarrier2',
-- 'Vulkan.Extensions.VK_KHR_synchronization2.cmdPipelineBarrier2KHR',
-- 'cmdSetEvent2',
-- 'Vulkan.Extensions.VK_KHR_synchronization2.cmdSetEvent2KHR',
-- 'cmdWaitEvents2',
-- 'Vulkan.Extensions.VK_KHR_synchronization2.cmdWaitEvents2KHR'
data DependencyInfo = DependencyInfo
  { -- | @dependencyFlags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits' specifying
    -- how execution and memory dependencies are formed.
    dependencyFlags :: DependencyFlags
  , -- | @pMemoryBarriers@ is a pointer to an array of 'MemoryBarrier2'
    -- structures defining memory dependencies between any memory accesses.
    memoryBarriers :: Vector MemoryBarrier2
  , -- | @pBufferMemoryBarriers@ is a pointer to an array of
    -- 'BufferMemoryBarrier2' structures defining memory dependencies between
    -- buffer ranges.
    bufferMemoryBarriers :: Vector (SomeStruct BufferMemoryBarrier2)
  , -- | @pImageMemoryBarriers@ is a pointer to an array of 'ImageMemoryBarrier2'
    -- structures defining memory dependencies between image subresources.
    imageMemoryBarriers :: Vector (SomeStruct ImageMemoryBarrier2)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DependencyInfo)
#endif
deriving instance Show DependencyInfo

instance ToCStruct DependencyInfo where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DependencyInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEPENDENCY_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DependencyFlags)) (dependencyFlags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (memoryBarriers)) :: Word32))
    pPMemoryBarriers' <- ContT $ allocaBytes @MemoryBarrier2 ((Data.Vector.length (memoryBarriers)) * 48)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPMemoryBarriers' `plusPtr` (48 * (i)) :: Ptr MemoryBarrier2) (e)) (memoryBarriers)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr MemoryBarrier2))) (pPMemoryBarriers')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (bufferMemoryBarriers)) :: Word32))
    pPBufferMemoryBarriers' <- ContT $ allocaBytes @(BufferMemoryBarrier2 _) ((Data.Vector.length (bufferMemoryBarriers)) * 80)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPBufferMemoryBarriers' `plusPtr` (80 * (i)) :: Ptr (BufferMemoryBarrier2 _))) (e) . ($ ())) (bufferMemoryBarriers)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (BufferMemoryBarrier2 _)))) (pPBufferMemoryBarriers')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (imageMemoryBarriers)) :: Word32))
    pPImageMemoryBarriers' <- ContT $ allocaBytes @(ImageMemoryBarrier2 _) ((Data.Vector.length (imageMemoryBarriers)) * 96)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPImageMemoryBarriers' `plusPtr` (96 * (i)) :: Ptr (ImageMemoryBarrier2 _))) (e) . ($ ())) (imageMemoryBarriers)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (ImageMemoryBarrier2 _)))) (pPImageMemoryBarriers')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEPENDENCY_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DependencyInfo where
  peekCStruct p = do
    dependencyFlags <- peek @DependencyFlags ((p `plusPtr` 16 :: Ptr DependencyFlags))
    memoryBarrierCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pMemoryBarriers <- peek @(Ptr MemoryBarrier2) ((p `plusPtr` 24 :: Ptr (Ptr MemoryBarrier2)))
    pMemoryBarriers' <- generateM (fromIntegral memoryBarrierCount) (\i -> peekCStruct @MemoryBarrier2 ((pMemoryBarriers `advancePtrBytes` (48 * (i)) :: Ptr MemoryBarrier2)))
    bufferMemoryBarrierCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pBufferMemoryBarriers <- peek @(Ptr (BufferMemoryBarrier2 _)) ((p `plusPtr` 40 :: Ptr (Ptr (BufferMemoryBarrier2 _))))
    pBufferMemoryBarriers' <- generateM (fromIntegral bufferMemoryBarrierCount) (\i -> peekSomeCStruct (forgetExtensions ((pBufferMemoryBarriers `advancePtrBytes` (80 * (i)) :: Ptr (BufferMemoryBarrier2 _)))))
    imageMemoryBarrierCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pImageMemoryBarriers <- peek @(Ptr (ImageMemoryBarrier2 _)) ((p `plusPtr` 56 :: Ptr (Ptr (ImageMemoryBarrier2 _))))
    pImageMemoryBarriers' <- generateM (fromIntegral imageMemoryBarrierCount) (\i -> peekSomeCStruct (forgetExtensions ((pImageMemoryBarriers `advancePtrBytes` (96 * (i)) :: Ptr (ImageMemoryBarrier2 _)))))
    pure $ DependencyInfo
             dependencyFlags
             pMemoryBarriers'
             pBufferMemoryBarriers'
             pImageMemoryBarriers'

instance Zero DependencyInfo where
  zero = DependencyInfo
           zero
           mempty
           mempty
           mempty


-- | VkSemaphoreSubmitInfo - Structure specifying a semaphore signal or wait
-- operation
--
-- = Description
--
-- Whether this structure defines a semaphore wait or signal operation is
-- defined by how it is used.
--
-- == Valid Usage
--
-- -   #VUID-VkSemaphoreSubmitInfo-stageMask-03929# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkSemaphoreSubmitInfo-stageMask-03930# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkSemaphoreSubmitInfo-stageMask-03931# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-conditionalRendering conditionalRendering>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   #VUID-VkSemaphoreSubmitInfo-stageMask-03932# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   #VUID-VkSemaphoreSubmitInfo-stageMask-03933# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   #VUID-VkSemaphoreSubmitInfo-stageMask-03934# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader meshShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
-- -   #VUID-VkSemaphoreSubmitInfo-stageMask-03935# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader taskShader>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   #VUID-VkSemaphoreSubmitInfo-stageMask-07316# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     are enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   #VUID-VkSemaphoreSubmitInfo-stageMask-04957# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subpassShading subpassShading>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--
-- -   #VUID-VkSemaphoreSubmitInfo-stageMask-04995# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-invocationMask invocationMask>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
-- -   #VUID-VkSemaphoreSubmitInfo-stageMask-07946# If neither the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
--     extension or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline feature>
--     are enabled, @stageMask@ /must/ not contain
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   #VUID-VkSemaphoreSubmitInfo-device-03888# If the @device@ that
--     @semaphore@ was created on is not a device group, @deviceIndex@
--     /must/ be @0@
--
-- -   #VUID-VkSemaphoreSubmitInfo-device-03889# If the @device@ that
--     @semaphore@ was created on is a device group, @deviceIndex@ /must/
--     be a valid device index
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSemaphoreSubmitInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO'
--
-- -   #VUID-VkSemaphoreSubmitInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkSemaphoreSubmitInfo-semaphore-parameter# @semaphore@ /must/
--     be a valid 'Vulkan.Core10.Handles.Semaphore' handle
--
-- -   #VUID-VkSemaphoreSubmitInfo-stageMask-parameter# @stageMask@ /must/
--     be a valid combination of
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2',
-- 'Vulkan.Extensions.VK_ARM_render_pass_striped.RenderPassStripeSubmitInfoARM',
-- 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'SubmitInfo2'
data SemaphoreSubmitInfo = SemaphoreSubmitInfo
  { -- | @semaphore@ is a 'Vulkan.Core10.Handles.Semaphore' affected by this
    -- operation.
    semaphore :: Semaphore
  , -- | @value@ is either the value used to signal @semaphore@ or the value
    -- waited on by @semaphore@, if @semaphore@ is a timeline semaphore.
    -- Otherwise it is ignored.
    value :: Word64
  , -- | @stageMask@ is a
    -- 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2' mask of
    -- pipeline stages which limit the first synchronization scope of a
    -- semaphore signal operation, or second synchronization scope of a
    -- semaphore wait operation as described in the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-waiting semaphore wait operation>
    -- and
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>
    -- sections of
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization the synchronization chapter>.
    stageMask :: PipelineStageFlags2
  , -- | @deviceIndex@ is the index of the device within a device group that
    -- executes the semaphore wait or signal operation.
    deviceIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreSubmitInfo)
#endif
deriving instance Show SemaphoreSubmitInfo

instance ToCStruct SemaphoreSubmitInfo where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreSubmitInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (value)
    poke ((p `plusPtr` 32 :: Ptr PipelineStageFlags2)) (stageMask)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (deviceIndex)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_SUBMIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    f

instance FromCStruct SemaphoreSubmitInfo where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    value <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    stageMask <- peek @PipelineStageFlags2 ((p `plusPtr` 32 :: Ptr PipelineStageFlags2))
    deviceIndex <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pure $ SemaphoreSubmitInfo
             semaphore value stageMask deviceIndex

instance Storable SemaphoreSubmitInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SemaphoreSubmitInfo where
  zero = SemaphoreSubmitInfo
           zero
           zero
           zero
           zero


-- | VkCommandBufferSubmitInfo - Structure specifying a command buffer
-- submission
--
-- == Valid Usage
--
-- -   #VUID-VkCommandBufferSubmitInfo-commandBuffer-03890# @commandBuffer@
--     /must/ not have been allocated with
--     'Vulkan.Core10.Enums.CommandBufferLevel.COMMAND_BUFFER_LEVEL_SECONDARY'
--
-- -   #VUID-VkCommandBufferSubmitInfo-deviceMask-03891# If @deviceMask@ is
--     not @0@, it /must/ be a valid device mask
--
-- -   #VUID-VkCommandBufferSubmitInfo-commandBuffer-09445# If any render
--     pass instance in @commandBuffer@ was recorded with a
--     'Vulkan.Extensions.VK_ARM_render_pass_striped.RenderPassStripeBeginInfoARM'
--     structure in its pNext chain, a
--     'Vulkan.Extensions.VK_ARM_render_pass_striped.RenderPassStripeSubmitInfoARM'
--     /must/ be included in the @pNext@ chain
--
-- -   #VUID-VkCommandBufferSubmitInfo-pNext-09446# If a
--     'Vulkan.Extensions.VK_ARM_render_pass_striped.RenderPassStripeSubmitInfoARM'
--     is included in the @pNext@ chain, the value of
--     'Vulkan.Extensions.VK_ARM_render_pass_striped.RenderPassStripeSubmitInfoARM'::@stripeSemaphoreInfoCount@
--     /must/ be equal to the sum of the
--     'Vulkan.Extensions.VK_ARM_render_pass_striped.RenderPassStripeBeginInfoARM'::@stripeInfoCount@
--     parameters provided to render pass instances recorded in
--     @commandBuffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCommandBufferSubmitInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO'
--
-- -   #VUID-VkCommandBufferSubmitInfo-pNext-pNext# @pNext@ /must/ be
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_ARM_render_pass_striped.RenderPassStripeSubmitInfoARM'
--
-- -   #VUID-VkCommandBufferSubmitInfo-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkCommandBufferSubmitInfo-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'SubmitInfo2'
data CommandBufferSubmitInfo (es :: [Type]) = CommandBufferSubmitInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @commandBuffer@ is a 'Vulkan.Core10.Handles.CommandBuffer' to be
    -- submitted for execution.
    commandBuffer :: Ptr CommandBuffer_T
  , -- | @deviceMask@ is a bitmask indicating which devices in a device group
    -- execute the command buffer. A @deviceMask@ of @0@ is equivalent to
    -- setting all bits corresponding to valid devices in the group to @1@.
    deviceMask :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandBufferSubmitInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (CommandBufferSubmitInfo es)

instance Extensible CommandBufferSubmitInfo where
  extensibleTypeName = "CommandBufferSubmitInfo"
  setNext CommandBufferSubmitInfo{..} next' = CommandBufferSubmitInfo{next = next', ..}
  getNext CommandBufferSubmitInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends CommandBufferSubmitInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @RenderPassStripeSubmitInfoARM = Just f
    | otherwise = Nothing

instance ( Extendss CommandBufferSubmitInfo es
         , PokeChain es ) => ToCStruct (CommandBufferSubmitInfo es) where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandBufferSubmitInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CommandBuffer_T))) (commandBuffer)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (deviceMask)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_BUFFER_SUBMIT_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CommandBuffer_T))) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ f

instance ( Extendss CommandBufferSubmitInfo es
         , PeekChain es ) => FromCStruct (CommandBufferSubmitInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    commandBuffer <- peek @(Ptr CommandBuffer_T) ((p `plusPtr` 16 :: Ptr (Ptr CommandBuffer_T)))
    deviceMask <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ CommandBufferSubmitInfo
             next commandBuffer deviceMask

instance es ~ '[] => Zero (CommandBufferSubmitInfo es) where
  zero = CommandBufferSubmitInfo
           ()
           zero
           zero


-- | VkSubmitInfo2 - Structure specifying a queue submit operation
--
-- == Valid Usage
--
-- -   #VUID-VkSubmitInfo2-semaphore-03881# If the same semaphore is used
--     as the @semaphore@ member of both an element of
--     @pSignalSemaphoreInfos@ and @pWaitSemaphoreInfos@, and that
--     semaphore is a timeline semaphore, the @value@ member of the
--     @pSignalSemaphoreInfos@ element /must/ be greater than the @value@
--     member of the @pWaitSemaphoreInfos@ element
--
-- -   #VUID-VkSubmitInfo2-semaphore-03882# If the @semaphore@ member of
--     any element of @pSignalSemaphoreInfos@ is a timeline semaphore, the
--     @value@ member of that element /must/ have a value greater than the
--     current value of the semaphore when the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>
--     is executed
--
-- -   #VUID-VkSubmitInfo2-semaphore-03883# If the @semaphore@ member of
--     any element of @pSignalSemaphoreInfos@ is a timeline semaphore, the
--     @value@ member of that element /must/ have a value which does not
--     differ from the current value of the semaphore or the value of any
--     outstanding semaphore wait or signal operation on that semaphore by
--     more than
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxTimelineSemaphoreValueDifference maxTimelineSemaphoreValueDifference>
--
-- -   #VUID-VkSubmitInfo2-semaphore-03884# If the @semaphore@ member of
--     any element of @pWaitSemaphoreInfos@ is a timeline semaphore, the
--     @value@ member of that element /must/ have a value which does not
--     differ from the current value of the semaphore or the value of any
--     outstanding semaphore wait or signal operation on that semaphore by
--     more than
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxTimelineSemaphoreValueDifference maxTimelineSemaphoreValueDifference>
--
-- -   #VUID-VkSubmitInfo2-flags-03886# If @flags@ includes
--     'Vulkan.Core13.Enums.SubmitFlagBits.SUBMIT_PROTECTED_BIT', all
--     elements of @pCommandBuffers@ /must/ be protected command buffers
--
-- -   #VUID-VkSubmitInfo2-flags-03887# If @flags@ does not include
--     'Vulkan.Core13.Enums.SubmitFlagBits.SUBMIT_PROTECTED_BIT', each
--     element of @pCommandBuffers@ /must/ not be a protected command
--     buffer
--
-- -   #VUID-VkSubmitInfo2KHR-commandBuffer-06192# If any @commandBuffer@
--     member of an element of @pCommandBufferInfos@ contains any
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-suspension resumed render pass instances>,
--     they /must/ be suspended by a render pass instance earlier in
--     submission order within @pCommandBufferInfos@
--
-- -   #VUID-VkSubmitInfo2KHR-commandBuffer-06010# If any @commandBuffer@
--     member of an element of @pCommandBufferInfos@ contains any
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-suspension suspended render pass instances>,
--     they /must/ be resumed by a render pass instance later in submission
--     order within @pCommandBufferInfos@
--
-- -   #VUID-VkSubmitInfo2KHR-commandBuffer-06011# If any @commandBuffer@
--     member of an element of @pCommandBufferInfos@ contains any
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-suspension suspended render pass instances>,
--     there /must/ be no action or synchronization commands between that
--     render pass instance and the render pass instance that resumes it
--
-- -   #VUID-VkSubmitInfo2KHR-commandBuffer-06012# If any @commandBuffer@
--     member of an element of @pCommandBufferInfos@ contains any
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-suspension suspended render pass instances>,
--     there /must/ be no render pass instances between that render pass
--     instance and the render pass instance that resumes it
--
-- -   #VUID-VkSubmitInfo2KHR-variableSampleLocations-06013# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-variableSampleLocations variableSampleLocations>
--     limit is not supported, and any @commandBuffer@ member of an element
--     of @pCommandBufferInfos@ contains any
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-suspension suspended render pass instances>,
--     where a graphics pipeline has been bound, any pipelines bound in the
--     render pass instance that resumes it, or any subsequent render pass
--     instances that resume from that one and so on, /must/ use the same
--     sample locations
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubmitInfo2-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBMIT_INFO_2'
--
-- -   #VUID-VkSubmitInfo2-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_frame_boundary.FrameBoundaryEXT',
--     'Vulkan.Extensions.VK_NV_low_latency2.LatencySubmissionPresentIdNV',
--     'Vulkan.Extensions.VK_KHR_performance_query.PerformanceQuerySubmitInfoKHR',
--     'Vulkan.Extensions.VK_KHR_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoKHR',
--     or
--     'Vulkan.Extensions.VK_NV_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoNV'
--
-- -   #VUID-VkSubmitInfo2-sType-unique# The @sType@ value of each struct
--     in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkSubmitInfo2-flags-parameter# @flags@ /must/ be a valid
--     combination of 'Vulkan.Core13.Enums.SubmitFlagBits.SubmitFlagBits'
--     values
--
-- -   #VUID-VkSubmitInfo2-pWaitSemaphoreInfos-parameter# If
--     @waitSemaphoreInfoCount@ is not @0@, @pWaitSemaphoreInfos@ /must/ be
--     a valid pointer to an array of @waitSemaphoreInfoCount@ valid
--     'SemaphoreSubmitInfo' structures
--
-- -   #VUID-VkSubmitInfo2-pCommandBufferInfos-parameter# If
--     @commandBufferInfoCount@ is not @0@, @pCommandBufferInfos@ /must/ be
--     a valid pointer to an array of @commandBufferInfoCount@ valid
--     'CommandBufferSubmitInfo' structures
--
-- -   #VUID-VkSubmitInfo2-pSignalSemaphoreInfos-parameter# If
--     @signalSemaphoreInfoCount@ is not @0@, @pSignalSemaphoreInfos@
--     /must/ be a valid pointer to an array of @signalSemaphoreInfoCount@
--     valid 'SemaphoreSubmitInfo' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'CommandBufferSubmitInfo', 'SemaphoreSubmitInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core13.Enums.SubmitFlagBits.SubmitFlags', 'queueSubmit2',
-- 'Vulkan.Extensions.VK_KHR_synchronization2.queueSubmit2KHR'
data SubmitInfo2 (es :: [Type]) = SubmitInfo2
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core13.Enums.SubmitFlagBits.SubmitFlagBits'.
    flags :: SubmitFlags
  , -- | @pWaitSemaphoreInfos@ is a pointer to an array of 'SemaphoreSubmitInfo'
    -- structures defining
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-waiting semaphore wait operations>.
    waitSemaphoreInfos :: Vector SemaphoreSubmitInfo
  , -- | @pCommandBufferInfos@ is a pointer to an array of
    -- 'CommandBufferSubmitInfo' structures describing command buffers to
    -- execute in the batch.
    commandBufferInfos :: Vector (SomeStruct CommandBufferSubmitInfo)
  , -- | @pSignalSemaphoreInfos@ is a pointer to an array of
    -- 'SemaphoreSubmitInfo' describing
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operations>.
    signalSemaphoreInfos :: Vector SemaphoreSubmitInfo
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubmitInfo2 (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SubmitInfo2 es)

instance Extensible SubmitInfo2 where
  extensibleTypeName = "SubmitInfo2"
  setNext SubmitInfo2{..} next' = SubmitInfo2{next = next', ..}
  getNext SubmitInfo2{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SubmitInfo2 e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @LatencySubmissionPresentIdNV = Just f
    | Just Refl <- eqT @e @FrameBoundaryEXT = Just f
    | Just Refl <- eqT @e @PerformanceQuerySubmitInfoKHR = Just f
    | Just Refl <- eqT @e @Win32KeyedMutexAcquireReleaseInfoKHR = Just f
    | Just Refl <- eqT @e @Win32KeyedMutexAcquireReleaseInfoNV = Just f
    | otherwise = Nothing

instance ( Extendss SubmitInfo2 es
         , PokeChain es ) => ToCStruct (SubmitInfo2 es) where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubmitInfo2{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBMIT_INFO_2)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SubmitFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (waitSemaphoreInfos)) :: Word32))
    pPWaitSemaphoreInfos' <- ContT $ allocaBytes @SemaphoreSubmitInfo ((Data.Vector.length (waitSemaphoreInfos)) * 48)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphoreInfos' `plusPtr` (48 * (i)) :: Ptr SemaphoreSubmitInfo) (e)) (waitSemaphoreInfos)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr SemaphoreSubmitInfo))) (pPWaitSemaphoreInfos')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (commandBufferInfos)) :: Word32))
    pPCommandBufferInfos' <- ContT $ allocaBytes @(CommandBufferSubmitInfo _) ((Data.Vector.length (commandBufferInfos)) * 32)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCommandBufferInfos' `plusPtr` (32 * (i)) :: Ptr (CommandBufferSubmitInfo _))) (e) . ($ ())) (commandBufferInfos)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (CommandBufferSubmitInfo _)))) (pPCommandBufferInfos')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (signalSemaphoreInfos)) :: Word32))
    pPSignalSemaphoreInfos' <- ContT $ allocaBytes @SemaphoreSubmitInfo ((Data.Vector.length (signalSemaphoreInfos)) * 48)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphoreInfos' `plusPtr` (48 * (i)) :: Ptr SemaphoreSubmitInfo) (e)) (signalSemaphoreInfos)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr SemaphoreSubmitInfo))) (pPSignalSemaphoreInfos')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBMIT_INFO_2)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance ( Extendss SubmitInfo2 es
         , PeekChain es ) => FromCStruct (SubmitInfo2 es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @SubmitFlags ((p `plusPtr` 16 :: Ptr SubmitFlags))
    waitSemaphoreInfoCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pWaitSemaphoreInfos <- peek @(Ptr SemaphoreSubmitInfo) ((p `plusPtr` 24 :: Ptr (Ptr SemaphoreSubmitInfo)))
    pWaitSemaphoreInfos' <- generateM (fromIntegral waitSemaphoreInfoCount) (\i -> peekCStruct @SemaphoreSubmitInfo ((pWaitSemaphoreInfos `advancePtrBytes` (48 * (i)) :: Ptr SemaphoreSubmitInfo)))
    commandBufferInfoCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pCommandBufferInfos <- peek @(Ptr (CommandBufferSubmitInfo _)) ((p `plusPtr` 40 :: Ptr (Ptr (CommandBufferSubmitInfo _))))
    pCommandBufferInfos' <- generateM (fromIntegral commandBufferInfoCount) (\i -> peekSomeCStruct (forgetExtensions ((pCommandBufferInfos `advancePtrBytes` (32 * (i)) :: Ptr (CommandBufferSubmitInfo _)))))
    signalSemaphoreInfoCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pSignalSemaphoreInfos <- peek @(Ptr SemaphoreSubmitInfo) ((p `plusPtr` 56 :: Ptr (Ptr SemaphoreSubmitInfo)))
    pSignalSemaphoreInfos' <- generateM (fromIntegral signalSemaphoreInfoCount) (\i -> peekCStruct @SemaphoreSubmitInfo ((pSignalSemaphoreInfos `advancePtrBytes` (48 * (i)) :: Ptr SemaphoreSubmitInfo)))
    pure $ SubmitInfo2
             next
             flags
             pWaitSemaphoreInfos'
             pCommandBufferInfos'
             pSignalSemaphoreInfos'

instance es ~ '[] => Zero (SubmitInfo2 es) where
  zero = SubmitInfo2
           ()
           zero
           mempty
           mempty
           mempty


-- | VkPhysicalDeviceSynchronization2Features - Structure describing whether
-- the implementation supports v2 synchronization commands
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceSynchronization2Features' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceSynchronization2Features' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSynchronization2Features = PhysicalDeviceSynchronization2Features
  { -- | #extension-features-synchronization2# @synchronization2@ indicates
    -- whether the implementation supports the new set of synchronization
    -- commands introduced in @VK_KHR_synchronization2@.
    synchronization2 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSynchronization2Features)
#endif
deriving instance Show PhysicalDeviceSynchronization2Features

instance ToCStruct PhysicalDeviceSynchronization2Features where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSynchronization2Features{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (synchronization2))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SYNCHRONIZATION_2_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSynchronization2Features where
  peekCStruct p = do
    synchronization2 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceSynchronization2Features
             (bool32ToBool synchronization2)

instance Storable PhysicalDeviceSynchronization2Features where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSynchronization2Features where
  zero = PhysicalDeviceSynchronization2Features
           zero

