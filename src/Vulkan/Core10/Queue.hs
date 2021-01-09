{-# language CPP #-}
-- No documentation found for Chapter "Queue"
module Vulkan.Core10.Queue  ( getDeviceQueue
                            , queueSubmit
                            , queueWaitIdle
                            , queueWaitIdleSafe
                            , deviceWaitIdle
                            , deviceWaitIdleSafe
                            , SubmitInfo(..)
                            , Queue(..)
                            , PipelineStageFlagBits(..)
                            , PipelineStageFlags
                            ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
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
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (D3D12FenceSubmitInfoKHR)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkDeviceWaitIdle))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceQueue))
import Vulkan.Dynamic (DeviceCmds(pVkQueueSubmit))
import Vulkan.Dynamic (DeviceCmds(pVkQueueWaitIdle))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupSubmitInfo)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PerformanceQuerySubmitInfoKHR)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (ProtectedSubmitInfo)
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue(Queue))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (TimelineSemaphoreSubmitInfo)
import Vulkan.Exception (VulkanException(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_win32_keyed_mutex (Win32KeyedMutexAcquireReleaseInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_win32_keyed_mutex (Win32KeyedMutexAcquireReleaseInfoNV)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBMIT_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(..))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Handles (Queue(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceQueue
  :: FunPtr (Ptr Device_T -> Word32 -> Word32 -> Ptr (Ptr Queue_T) -> IO ()) -> Ptr Device_T -> Word32 -> Word32 -> Ptr (Ptr Queue_T) -> IO ()

-- | vkGetDeviceQueue - Get a queue handle from a device
--
-- = Description
--
-- 'getDeviceQueue' /must/ only be used to get queues that were created
-- with the @flags@ parameter of
-- 'Vulkan.Core10.Device.DeviceQueueCreateInfo' set to zero. To get queues
-- that were created with a non-zero @flags@ parameter use
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.getDeviceQueue2'.
--
-- == Valid Usage
--
-- -   #VUID-vkGetDeviceQueue-queueFamilyIndex-00384# @queueFamilyIndex@
--     /must/ be one of the queue family indices specified when @device@
--     was created, via the 'Vulkan.Core10.Device.DeviceQueueCreateInfo'
--     structure
--
-- -   #VUID-vkGetDeviceQueue-queueIndex-00385# @queueIndex@ /must/ be less
--     than the number of queues created for the specified queue family
--     index when @device@ was created, via the @queueCount@ member of the
--     'Vulkan.Core10.Device.DeviceQueueCreateInfo' structure
--
-- -   #VUID-vkGetDeviceQueue-flags-01841#
--     'Vulkan.Core10.Device.DeviceQueueCreateInfo'::@flags@ /must/ have
--     been set to zero when @device@ was created
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDeviceQueue-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDeviceQueue-pQueue-parameter# @pQueue@ /must/ be a valid
--     pointer to a 'Vulkan.Core10.Handles.Queue' handle
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Queue'
getDeviceQueue :: forall io
                . (MonadIO io)
               => -- | @device@ is the logical device that owns the queue.
                  Device
               -> -- | @queueFamilyIndex@ is the index of the queue family to which the queue
                  -- belongs.
                  ("queueFamilyIndex" ::: Word32)
               -> -- | @queueIndex@ is the index within this queue family of the queue to
                  -- retrieve.
                  ("queueIndex" ::: Word32)
               -> io (Queue)
getDeviceQueue device queueFamilyIndex queueIndex = liftIO . evalContT $ do
  let cmds = deviceCmds (device :: Device)
  let vkGetDeviceQueuePtr = pVkGetDeviceQueue cmds
  lift $ unless (vkGetDeviceQueuePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceQueue is null" Nothing Nothing
  let vkGetDeviceQueue' = mkVkGetDeviceQueue vkGetDeviceQueuePtr
  pPQueue <- ContT $ bracket (callocBytes @(Ptr Queue_T) 8) free
  lift $ traceAroundEvent "vkGetDeviceQueue" (vkGetDeviceQueue' (deviceHandle (device)) (queueFamilyIndex) (queueIndex) (pPQueue))
  pQueue <- lift $ peek @(Ptr Queue_T) pPQueue
  pure $ (((\h -> Queue h cmds ) pQueue))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueSubmit
  :: FunPtr (Ptr Queue_T -> Word32 -> Ptr (SomeStruct SubmitInfo) -> Fence -> IO Result) -> Ptr Queue_T -> Word32 -> Ptr (SomeStruct SubmitInfo) -> Fence -> IO Result

-- | vkQueueSubmit - Submits a sequence of semaphores or command buffers to a
-- queue
--
-- = Description
--
-- 'queueSubmit' is a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-submission queue submission command>,
-- with each batch defined by an element of @pSubmits@. Batches begin
-- execution in the order they appear in @pSubmits@, but /may/ complete out
-- of order.
--
-- Fence and semaphore operations submitted with 'queueSubmit' have
-- additional ordering constraints compared to other submission commands,
-- with dependencies involving previous and subsequent queue operations.
-- Information about these additional constraints can be found in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores semaphore>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-fences fence>
-- sections of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization the synchronization chapter>.
--
-- Details on the interaction of @pWaitDstStageMask@ with synchronization
-- are described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-waiting semaphore wait operation>
-- section of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization the synchronization chapter>.
--
-- The order that batches appear in @pSubmits@ is used to determine
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>,
-- and thus all the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-implicit implicit ordering guarantees>
-- that respect it. Other than these implicit ordering guarantees and any
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization explicit synchronization primitives>,
-- these batches /may/ overlap or otherwise execute out of order.
--
-- If any command buffer submitted to this queue is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>,
-- it is moved to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
-- Once execution of all submissions of a command buffer complete, it moves
-- from the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>,
-- back to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>.
-- If a command buffer was recorded with the
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT'
-- flag, it instead moves to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle invalid state>.
--
-- If 'queueSubmit' fails, it /may/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY' or
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'. If it does, the
-- implementation /must/ ensure that the state and contents of any
-- resources or synchronization primitives referenced by the submitted
-- command buffers and any semaphores referenced by @pSubmits@ is
-- unaffected by the call or its failure. If 'queueSubmit' fails in such a
-- way that the implementation is unable to make that guarantee, the
-- implementation /must/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'. See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>.
--
-- == Valid Usage
--
-- -   #VUID-vkQueueSubmit-fence-00063# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ be
--     unsignaled
--
-- -   #VUID-vkQueueSubmit-fence-00064# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ not be
--     associated with any other queue command that has not yet completed
--     execution on that queue
--
-- -   #VUID-vkQueueSubmit-pCommandBuffers-00065# Any calls to
--     'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent',
--     'Vulkan.Core10.CommandBufferBuilding.cmdResetEvent' or
--     'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents' that have been
--     recorded into any of the command buffer elements of the
--     @pCommandBuffers@ member of any element of @pSubmits@, /must/ not
--     reference any 'Vulkan.Core10.Handles.Event' that is referenced by
--     any of those commands in a command buffer that has been submitted to
--     another queue and is still in the /pending state/
--
-- -   #VUID-vkQueueSubmit-pWaitDstStageMask-00066# Any stage flag included
--     in any element of the @pWaitDstStageMask@ member of any element of
--     @pSubmits@ /must/ be a pipeline stage supported by one of the
--     capabilities of @queue@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   #VUID-vkQueueSubmit-pSignalSemaphores-00067# Each element of the
--     @pSignalSemaphores@ member of any element of @pSubmits@ /must/ be
--     unsignaled when the semaphore signal operation it defines is
--     executed on the device
--
-- -   #VUID-vkQueueSubmit-pWaitSemaphores-00068# When a semaphore wait
--     operation referring to a binary semaphore defined by any element of
--     the @pWaitSemaphores@ member of any element of @pSubmits@ executes
--     on @queue@, there /must/ be no other queues waiting on the same
--     semaphore
--
-- -   #VUID-vkQueueSubmit-pWaitSemaphores-03238# All elements of the
--     @pWaitSemaphores@ member of all elements of @pSubmits@ created with
--     a 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY' /must/
--     reference a semaphore signal operation that has been submitted for
--     execution and any semaphore signal operations on which it depends
--     (if any) /must/ have also been submitted for execution
--
-- -   #VUID-vkQueueSubmit-pCommandBuffers-00070# Each element of the
--     @pCommandBuffers@ member of each element of @pSubmits@ /must/ be in
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>
--
-- -   #VUID-vkQueueSubmit-pCommandBuffers-00071# If any element of the
--     @pCommandBuffers@ member of any element of @pSubmits@ was not
--     recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT',
--     it /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   #VUID-vkQueueSubmit-pCommandBuffers-00072# Any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-secondary secondary command buffers recorded>
--     into any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>
--
-- -   #VUID-vkQueueSubmit-pCommandBuffers-00073# If any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-secondary secondary command buffers recorded>
--     into any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ was not recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT',
--     it /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   #VUID-vkQueueSubmit-pCommandBuffers-00074# Each element of the
--     @pCommandBuffers@ member of each element of @pSubmits@ /must/ have
--     been allocated from a 'Vulkan.Core10.Handles.CommandPool' that was
--     created for the same queue family @queue@ belongs to
--
-- -   #VUID-vkQueueSubmit-pSubmits-02207# If any element of
--     @pSubmits->pCommandBuffers@ includes a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-acquire Queue Family Transfer Acquire Operation>,
--     there /must/ exist a previously submitted
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-release Queue Family Transfer Release Operation>
--     on a queue in the queue family identified by the acquire operation,
--     with parameters matching the acquire operation as defined in the
--     definition of such
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-acquire acquire operations>,
--     and which happens-before the acquire operation
--
-- -   #VUID-vkQueueSubmit-pCommandBuffers-03220# If a command recorded
--     into any element of @pCommandBuffers@ was a
--     'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery' whose
--     @queryPool@ was created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#profiling-lock profiling lock>
--     /must/ have been held continuously on the
--     'Vulkan.Core10.Handles.Device' that @queue@ was retrieved from,
--     throughout recording of those command buffers
--
-- -   #VUID-vkQueueSubmit-pSubmits-02808# Any resource created with
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE' that is
--     read by an operation specified by @pSubmits@ /must/ not be owned by
--     any queue family other than the one which @queue@ belongs to, at the
--     time it is executed
--
-- -   #VUID-vkQueueSubmit-pSubmits-04626# Any resource created with
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT' that is
--     accessed by an operation specified by @pSubmits@ /must/ have
--     included the queue family of @queue@ at resource creation time
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkQueueSubmit-queue-parameter# @queue@ /must/ be a valid
--     'Vulkan.Core10.Handles.Queue' handle
--
-- -   #VUID-vkQueueSubmit-pSubmits-parameter# If @submitCount@ is not @0@,
--     @pSubmits@ /must/ be a valid pointer to an array of @submitCount@
--     valid 'SubmitInfo' structures
--
-- -   #VUID-vkQueueSubmit-fence-parameter# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ be a valid
--     'Vulkan.Core10.Handles.Fence' handle
--
-- -   #VUID-vkQueueSubmit-commonparent# Both of @fence@, and @queue@ that
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | Any                                                                                                                   | -                                                                                                                                   |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
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
-- 'Vulkan.Core10.Handles.Fence', 'Vulkan.Core10.Handles.Queue',
-- 'SubmitInfo'
queueSubmit :: forall io
             . (MonadIO io)
            => -- | @queue@ is the queue that the command buffers will be submitted to.
               Queue
            -> -- | @pSubmits@ is a pointer to an array of 'SubmitInfo' structures, each
               -- specifying a command buffer submission batch.
               ("submits" ::: Vector (SomeStruct SubmitInfo))
            -> -- | @fence@ is an /optional/ handle to a fence to be signaled once all
               -- submitted command buffers have completed execution. If @fence@ is not
               -- 'Vulkan.Core10.APIConstants.NULL_HANDLE', it defines a
               -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-fences-signaling fence signal operation>.
               Fence
            -> io ()
queueSubmit queue submits fence = liftIO . evalContT $ do
  let vkQueueSubmitPtr = pVkQueueSubmit (deviceCmds (queue :: Queue))
  lift $ unless (vkQueueSubmitPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueSubmit is null" Nothing Nothing
  let vkQueueSubmit' = mkVkQueueSubmit vkQueueSubmitPtr
  pPSubmits <- ContT $ allocaBytesAligned @(SubmitInfo _) ((Data.Vector.length (submits)) * 72) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPSubmits `plusPtr` (72 * (i)) :: Ptr (SubmitInfo _))) (e) . ($ ())) (submits)
  r <- lift $ traceAroundEvent "vkQueueSubmit" (vkQueueSubmit' (queueHandle (queue)) ((fromIntegral (Data.Vector.length $ (submits)) :: Word32)) (forgetExtensions (pPSubmits)) (fence))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueWaitIdleUnsafe
  :: FunPtr (Ptr Queue_T -> IO Result) -> Ptr Queue_T -> IO Result

foreign import ccall
  "dynamic" mkVkQueueWaitIdleSafe
  :: FunPtr (Ptr Queue_T -> IO Result) -> Ptr Queue_T -> IO Result

-- | queueWaitIdle with selectable safeness
queueWaitIdleSafeOrUnsafe :: forall io
                           . (MonadIO io)
                          => (FunPtr (Ptr Queue_T -> IO Result) -> Ptr Queue_T -> IO Result)
                          -> -- | @queue@ is the queue on which to wait.
                             Queue
                          -> io ()
queueWaitIdleSafeOrUnsafe mkVkQueueWaitIdle queue = liftIO $ do
  let vkQueueWaitIdlePtr = pVkQueueWaitIdle (deviceCmds (queue :: Queue))
  unless (vkQueueWaitIdlePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueWaitIdle is null" Nothing Nothing
  let vkQueueWaitIdle' = mkVkQueueWaitIdle vkQueueWaitIdlePtr
  r <- traceAroundEvent "vkQueueWaitIdle" (vkQueueWaitIdle' (queueHandle (queue)))
  when (r < SUCCESS) (throwIO (VulkanException r))

-- | vkQueueWaitIdle - Wait for a queue to become idle
--
-- = Description
--
-- 'queueWaitIdle' is equivalent to submitting a fence to a queue and
-- waiting with an infinite timeout for that fence to signal.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkQueueWaitIdle-queue-parameter# @queue@ /must/ be a valid
--     'Vulkan.Core10.Handles.Queue' handle
--
-- == Host Synchronization
--
-- -   Host access to @queue@ /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | Any                                                                                                                   | -                                                                                                                                   |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
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
-- 'Vulkan.Core10.Handles.Queue'
queueWaitIdle :: forall io
               . (MonadIO io)
              => -- | @queue@ is the queue on which to wait.
                 Queue
              -> io ()
queueWaitIdle = queueWaitIdleSafeOrUnsafe mkVkQueueWaitIdleUnsafe

-- | A variant of 'queueWaitIdle' which makes a *safe* FFI call
queueWaitIdleSafe :: forall io
                   . (MonadIO io)
                  => -- | @queue@ is the queue on which to wait.
                     Queue
                  -> io ()
queueWaitIdleSafe = queueWaitIdleSafeOrUnsafe mkVkQueueWaitIdleSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDeviceWaitIdleUnsafe
  :: FunPtr (Ptr Device_T -> IO Result) -> Ptr Device_T -> IO Result

foreign import ccall
  "dynamic" mkVkDeviceWaitIdleSafe
  :: FunPtr (Ptr Device_T -> IO Result) -> Ptr Device_T -> IO Result

-- | deviceWaitIdle with selectable safeness
deviceWaitIdleSafeOrUnsafe :: forall io
                            . (MonadIO io)
                           => (FunPtr (Ptr Device_T -> IO Result) -> Ptr Device_T -> IO Result)
                           -> -- | @device@ is the logical device to idle.
                              Device
                           -> io ()
deviceWaitIdleSafeOrUnsafe mkVkDeviceWaitIdle device = liftIO $ do
  let vkDeviceWaitIdlePtr = pVkDeviceWaitIdle (deviceCmds (device :: Device))
  unless (vkDeviceWaitIdlePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDeviceWaitIdle is null" Nothing Nothing
  let vkDeviceWaitIdle' = mkVkDeviceWaitIdle vkDeviceWaitIdlePtr
  r <- traceAroundEvent "vkDeviceWaitIdle" (vkDeviceWaitIdle' (deviceHandle (device)))
  when (r < SUCCESS) (throwIO (VulkanException r))

-- | vkDeviceWaitIdle - Wait for a device to become idle
--
-- = Description
--
-- 'deviceWaitIdle' is equivalent to calling 'queueWaitIdle' for all queues
-- owned by @device@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDeviceWaitIdle-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- == Host Synchronization
--
-- -   Host access to all 'Vulkan.Core10.Handles.Queue' objects created
--     from @device@ /must/ be externally synchronized
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
-- 'Vulkan.Core10.Handles.Device'
deviceWaitIdle :: forall io
                . (MonadIO io)
               => -- | @device@ is the logical device to idle.
                  Device
               -> io ()
deviceWaitIdle = deviceWaitIdleSafeOrUnsafe mkVkDeviceWaitIdleUnsafe

-- | A variant of 'deviceWaitIdle' which makes a *safe* FFI call
deviceWaitIdleSafe :: forall io
                    . (MonadIO io)
                   => -- | @device@ is the logical device to idle.
                      Device
                   -> io ()
deviceWaitIdleSafe = deviceWaitIdleSafeOrUnsafe mkVkDeviceWaitIdleSafe


-- | VkSubmitInfo - Structure specifying a queue submit operation
--
-- = Description
--
-- The order that command buffers appear in @pCommandBuffers@ is used to
-- determine
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-submission-order submission order>,
-- and thus all the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-implicit implicit ordering guarantees>
-- that respect it. Other than these implicit ordering guarantees and any
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization explicit synchronization primitives>,
-- these command buffers /may/ overlap or otherwise execute out of order.
--
-- == Valid Usage
--
-- -   #VUID-VkSubmitInfo-pCommandBuffers-00075# Each element of
--     @pCommandBuffers@ /must/ not have been allocated with
--     'Vulkan.Core10.Enums.CommandBufferLevel.COMMAND_BUFFER_LEVEL_SECONDARY'
--
-- -   #VUID-VkSubmitInfo-pWaitDstStageMask-00076# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   #VUID-VkSubmitInfo-pWaitDstStageMask-00077# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   #VUID-VkSubmitInfo-pWaitDstStageMask-00078# Each element of
--     @pWaitDstStageMask@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'
--
-- -   #VUID-VkSubmitInfo-pWaitSemaphores-03239# If any element of
--     @pWaitSemaphores@ or @pSignalSemaphores@ was created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE', then
--     the @pNext@ chain /must/ include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'
--     structure
--
-- -   #VUID-VkSubmitInfo-pNext-03240# If the @pNext@ chain of this
--     structure includes a
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'
--     structure and any element of @pWaitSemaphores@ was created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE', then
--     its @waitSemaphoreValueCount@ member /must/ equal
--     @waitSemaphoreCount@
--
-- -   #VUID-VkSubmitInfo-pNext-03241# If the @pNext@ chain of this
--     structure includes a
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'
--     structure and any element of @pSignalSemaphores@ was created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE', then
--     its @signalSemaphoreValueCount@ member /must/ equal
--     @signalSemaphoreCount@
--
-- -   #VUID-VkSubmitInfo-pSignalSemaphores-03242# For each element of
--     @pSignalSemaphores@ created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' the
--     corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'::pSignalSemaphoreValues
--     /must/ have a value greater than the current value of the semaphore
--     when the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>
--     is executed
--
-- -   #VUID-VkSubmitInfo-pWaitSemaphores-03243# For each element of
--     @pWaitSemaphores@ created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' the
--     corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'::pWaitSemaphoreValues
--     /must/ have a value which does not differ from the current value of
--     the semaphore or the value of any outstanding semaphore wait or
--     signal operation on that semaphore by more than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxTimelineSemaphoreValueDifference maxTimelineSemaphoreValueDifference>
--
-- -   #VUID-VkSubmitInfo-pSignalSemaphores-03244# For each element of
--     @pSignalSemaphores@ created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' the
--     corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'::pSignalSemaphoreValues
--     /must/ have a value which does not differ from the current value of
--     the semaphore or the value of any outstanding semaphore wait or
--     signal operation on that semaphore by more than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxTimelineSemaphoreValueDifference maxTimelineSemaphoreValueDifference>
--
-- -   #VUID-VkSubmitInfo-pWaitDstStageMask-02089# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   #VUID-VkSubmitInfo-pWaitDstStageMask-02090# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   #VUID-VkSubmitInfo-pNext-04120# If the @pNext@ chain of this
--     structure does not include a
--     'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.ProtectedSubmitInfo'
--     structure with @protectedSubmit@ set to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then each element of the
--     @pCommandBuffers@ array /must/ be an unprotected command buffer
--
-- -   #VUID-VkSubmitInfo-pNext-04148# If the @pNext@ chain of this
--     structure includes a
--     'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.ProtectedSubmitInfo'
--     structure with @protectedSubmit@ set to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then each element of the
--     @pCommandBuffers@ array /must/ be an protected command buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubmitInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBMIT_INFO'
--
-- -   #VUID-VkSubmitInfo-pNext-pNext# Each @pNext@ member of any structure
--     (including this one) in the @pNext@ chain /must/ be either @NULL@ or
--     a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_external_semaphore_win32.D3D12FenceSubmitInfoKHR',
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupSubmitInfo',
--     'Vulkan.Extensions.VK_KHR_performance_query.PerformanceQuerySubmitInfoKHR',
--     'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.ProtectedSubmitInfo',
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo',
--     'Vulkan.Extensions.VK_KHR_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoKHR',
--     or
--     'Vulkan.Extensions.VK_NV_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoNV'
--
-- -   #VUID-VkSubmitInfo-sType-unique# The @sType@ value of each struct in
--     the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkSubmitInfo-pWaitSemaphores-parameter# If
--     @waitSemaphoreCount@ is not @0@, @pWaitSemaphores@ /must/ be a valid
--     pointer to an array of @waitSemaphoreCount@ valid
--     'Vulkan.Core10.Handles.Semaphore' handles
--
-- -   #VUID-VkSubmitInfo-pWaitDstStageMask-parameter# If
--     @waitSemaphoreCount@ is not @0@, @pWaitDstStageMask@ /must/ be a
--     valid pointer to an array of @waitSemaphoreCount@ valid combinations
--     of 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   #VUID-VkSubmitInfo-pWaitDstStageMask-requiredbitmask# Each element
--     of @pWaitDstStageMask@ /must/ not be @0@
--
-- -   #VUID-VkSubmitInfo-pCommandBuffers-parameter# If
--     @commandBufferCount@ is not @0@, @pCommandBuffers@ /must/ be a valid
--     pointer to an array of @commandBufferCount@ valid
--     'Vulkan.Core10.Handles.CommandBuffer' handles
--
-- -   #VUID-VkSubmitInfo-pSignalSemaphores-parameter# If
--     @signalSemaphoreCount@ is not @0@, @pSignalSemaphores@ /must/ be a
--     valid pointer to an array of @signalSemaphoreCount@ valid
--     'Vulkan.Core10.Handles.Semaphore' handles
--
-- -   #VUID-VkSubmitInfo-commonparent# Each of the elements of
--     @pCommandBuffers@, the elements of @pSignalSemaphores@, and the
--     elements of @pWaitSemaphores@ that are valid handles of non-ignored
--     parameters /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags',
-- 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'queueSubmit'
data SubmitInfo (es :: [Type]) = SubmitInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @pWaitSemaphores@ is a pointer to an array of
    -- 'Vulkan.Core10.Handles.Semaphore' handles upon which to wait before the
    -- command buffers for this batch begin execution. If semaphores to wait on
    -- are provided, they define a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-waiting semaphore wait operation>.
    waitSemaphores :: Vector Semaphore
  , -- | @pWaitDstStageMask@ is a pointer to an array of pipeline stages at which
    -- each corresponding semaphore wait will occur.
    waitDstStageMask :: Vector PipelineStageFlags
  , -- | @pCommandBuffers@ is a pointer to an array of
    -- 'Vulkan.Core10.Handles.CommandBuffer' handles to execute in the batch.
    commandBuffers :: Vector (Ptr CommandBuffer_T)
  , -- | @pSignalSemaphores@ is a pointer to an array of
    -- 'Vulkan.Core10.Handles.Semaphore' handles which will be signaled when
    -- the command buffers for this batch have completed execution. If
    -- semaphores to be signaled are provided, they define a
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>.
    signalSemaphores :: Vector Semaphore
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubmitInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SubmitInfo es)

instance Extensible SubmitInfo where
  extensibleTypeName = "SubmitInfo"
  setNext x next = x{next = next}
  getNext SubmitInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SubmitInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PerformanceQuerySubmitInfoKHR = Just f
    | Just Refl <- eqT @e @TimelineSemaphoreSubmitInfo = Just f
    | Just Refl <- eqT @e @ProtectedSubmitInfo = Just f
    | Just Refl <- eqT @e @DeviceGroupSubmitInfo = Just f
    | Just Refl <- eqT @e @D3D12FenceSubmitInfoKHR = Just f
    | Just Refl <- eqT @e @Win32KeyedMutexAcquireReleaseInfoKHR = Just f
    | Just Refl <- eqT @e @Win32KeyedMutexAcquireReleaseInfoNV = Just f
    | otherwise = Nothing

instance (Extendss SubmitInfo es, PokeChain es) => ToCStruct (SubmitInfo es) where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubmitInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBMIT_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    let pWaitSemaphoresLength = Data.Vector.length $ (waitSemaphores)
    lift $ unless ((Data.Vector.length $ (waitDstStageMask)) == pWaitSemaphoresLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pWaitDstStageMask and pWaitSemaphores must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral pWaitSemaphoresLength :: Word32))
    pPWaitSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (waitSemaphores)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (waitSemaphores)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Semaphore))) (pPWaitSemaphores')
    pPWaitDstStageMask' <- ContT $ allocaBytesAligned @PipelineStageFlags ((Data.Vector.length (waitDstStageMask)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitDstStageMask' `plusPtr` (4 * (i)) :: Ptr PipelineStageFlags) (e)) (waitDstStageMask)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr PipelineStageFlags))) (pPWaitDstStageMask')
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (commandBuffers)) :: Word32))
    pPCommandBuffers' <- ContT $ allocaBytesAligned @(Ptr CommandBuffer_T) ((Data.Vector.length (commandBuffers)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBuffers' `plusPtr` (8 * (i)) :: Ptr (Ptr CommandBuffer_T)) (e)) (commandBuffers)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (Ptr CommandBuffer_T)))) (pPCommandBuffers')
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (signalSemaphores)) :: Word32))
    pPSignalSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (signalSemaphores)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (signalSemaphores)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr Semaphore))) (pPSignalSemaphores')
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBMIT_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss SubmitInfo es, PeekChain es) => FromCStruct (SubmitInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    waitSemaphoreCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pWaitSemaphores <- peek @(Ptr Semaphore) ((p `plusPtr` 24 :: Ptr (Ptr Semaphore)))
    pWaitSemaphores' <- generateM (fromIntegral waitSemaphoreCount) (\i -> peek @Semaphore ((pWaitSemaphores `advancePtrBytes` (8 * (i)) :: Ptr Semaphore)))
    pWaitDstStageMask <- peek @(Ptr PipelineStageFlags) ((p `plusPtr` 32 :: Ptr (Ptr PipelineStageFlags)))
    pWaitDstStageMask' <- generateM (fromIntegral waitSemaphoreCount) (\i -> peek @PipelineStageFlags ((pWaitDstStageMask `advancePtrBytes` (4 * (i)) :: Ptr PipelineStageFlags)))
    commandBufferCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pCommandBuffers <- peek @(Ptr (Ptr CommandBuffer_T)) ((p `plusPtr` 48 :: Ptr (Ptr (Ptr CommandBuffer_T))))
    pCommandBuffers' <- generateM (fromIntegral commandBufferCount) (\i -> peek @(Ptr CommandBuffer_T) ((pCommandBuffers `advancePtrBytes` (8 * (i)) :: Ptr (Ptr CommandBuffer_T))))
    signalSemaphoreCount <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    pSignalSemaphores <- peek @(Ptr Semaphore) ((p `plusPtr` 64 :: Ptr (Ptr Semaphore)))
    pSignalSemaphores' <- generateM (fromIntegral signalSemaphoreCount) (\i -> peek @Semaphore ((pSignalSemaphores `advancePtrBytes` (8 * (i)) :: Ptr Semaphore)))
    pure $ SubmitInfo
             next pWaitSemaphores' pWaitDstStageMask' pCommandBuffers' pSignalSemaphores'

instance es ~ '[] => Zero (SubmitInfo es) where
  zero = SubmitInfo
           ()
           mempty
           mempty
           mempty
           mempty

