{-# language CPP #-}
module Vulkan.Core10.Queue  ( getDeviceQueue
                            , queueSubmit
                            , queueWaitIdle
                            , deviceWaitIdle
                            , SubmitInfo(..)
                            ) where

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
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
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
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_win32_keyed_mutex (Win32KeyedMutexAcquireReleaseInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_win32_keyed_mutex (Win32KeyedMutexAcquireReleaseInfoNV)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBMIT_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
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
-- -   @queueFamilyIndex@ /must/ be one of the queue family indices
--     specified when @device@ was created, via the
--     'Vulkan.Core10.Device.DeviceQueueCreateInfo' structure
--
-- -   @queueIndex@ /must/ be less than the number of queues created for
--     the specified queue family index when @device@ was created, via the
--     @queueCount@ member of the
--     'Vulkan.Core10.Device.DeviceQueueCreateInfo' structure
--
-- -   'Vulkan.Core10.Device.DeviceQueueCreateInfo'::@flags@ /must/ have
--     been set to zero when @device@ was created
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pQueue@ /must/ be a valid pointer to a
--     'Vulkan.Core10.Handles.Queue' handle
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
  lift $ vkGetDeviceQueue' (deviceHandle (device)) (queueFamilyIndex) (queueIndex) (pPQueue)
  pQueue <- lift $ peek @(Ptr Queue_T) pPQueue
  pure $ (((\h -> Queue h cmds ) pQueue))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueSubmit
  :: FunPtr (Ptr Queue_T -> Word32 -> Ptr (SubmitInfo a) -> Fence -> IO Result) -> Ptr Queue_T -> Word32 -> Ptr (SubmitInfo a) -> Fence -> IO Result

-- | vkQueueSubmit - Submits a sequence of semaphores or command buffers to a
-- queue
--
-- = Description
--
-- Note
--
-- Submission can be a high overhead operation, and applications /should/
-- attempt to batch work together into as few calls to 'queueSubmit' as
-- possible.
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
-- -   If @fence@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@
--     /must/ be unsignaled
--
-- -   If @fence@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@
--     /must/ not be associated with any other queue command that has not
--     yet completed execution on that queue
--
-- -   Any calls to 'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent',
--     'Vulkan.Core10.CommandBufferBuilding.cmdResetEvent' or
--     'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents' that have been
--     recorded into any of the command buffer elements of the
--     @pCommandBuffers@ member of any element of @pSubmits@, /must/ not
--     reference any 'Vulkan.Core10.Handles.Event' that is referenced by
--     any of those commands in a command buffer that has been submitted to
--     another queue and is still in the /pending state/
--
-- -   Any stage flag included in any element of the @pWaitDstStageMask@
--     member of any element of @pSubmits@ /must/ be a pipeline stage
--     supported by one of the capabilities of @queue@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>
--
-- -   Each element of the @pSignalSemaphores@ member of any element of
--     @pSubmits@ /must/ be unsignaled when the semaphore signal operation
--     it defines is executed on the device
--
-- -   When a semaphore wait operation referring to a binary semaphore
--     defined by any element of the @pWaitSemaphores@ member of any
--     element of @pSubmits@ executes on @queue@, there /must/ be no other
--     queues waiting on the same semaphore
--
-- -   All elements of the @pWaitSemaphores@ member of all elements of
--     @pSubmits@ created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_BINARY' /must/
--     reference a semaphore signal operation that has been submitted for
--     execution and any semaphore signal operations on which it depends
--     (if any) /must/ have also been submitted for execution
--
-- -   Each element of the @pCommandBuffers@ member of each element of
--     @pSubmits@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>
--
-- -   If any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ was not recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT',
--     it /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   Any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-secondary secondary command buffers recorded>
--     into any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>
--
-- -   If any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-secondary secondary command buffers recorded>
--     into any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ was not recorded with the
--     'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT',
--     it /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   Each element of the @pCommandBuffers@ member of each element of
--     @pSubmits@ /must/ have been allocated from a
--     'Vulkan.Core10.Handles.CommandPool' that was created for the same
--     queue family @queue@ belongs to
--
-- -   If any element of @pSubmits->pCommandBuffers@ includes a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-acquire Queue Family Transfer Acquire Operation>,
--     there /must/ exist a previously submitted
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-release Queue Family Transfer Release Operation>
--     on a queue in the queue family identified by the acquire operation,
--     with parameters matching the acquire operation as defined in the
--     definition of such
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers-acquire acquire operations>,
--     and which happens-before the acquire operation
--
-- -   If a command recorded into any element of @pCommandBuffers@ was a
--     'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery' whose
--     @queryPool@ was created with a @queryType@ of
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_PERFORMANCE_QUERY_KHR',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#profiling-lock profiling lock>
--     /must/ have been held continuously on the
--     'Vulkan.Core10.Handles.Device' that @queue@ was retrieved from,
--     throughout recording of those command buffers
--
-- -   Any resource created with
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_EXCLUSIVE' that is
--     read by an operation specified by @pSubmits@ /must/ not be owned by
--     any queue family other than the one which @queue@ belongs to, at the
--     time it is executed
--
-- == Valid Usage (Implicit)
--
-- -   @queue@ /must/ be a valid 'Vulkan.Core10.Handles.Queue' handle
--
-- -   If @submitCount@ is not @0@, @pSubmits@ /must/ be a valid pointer to
--     an array of @submitCount@ valid 'SubmitInfo' structures
--
-- -   If @fence@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@
--     /must/ be a valid 'Vulkan.Core10.Handles.Fence' handle
--
-- -   Both of @fence@, and @queue@ that are valid handles of non-ignored
--     parameters /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Device'
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
  r <- lift $ vkQueueSubmit' (queueHandle (queue)) ((fromIntegral (Data.Vector.length $ (submits)) :: Word32)) (pPSubmits) (fence)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueWaitIdle
  :: FunPtr (Ptr Queue_T -> IO Result) -> Ptr Queue_T -> IO Result

-- | vkQueueWaitIdle - Wait for a queue to become idle
--
-- = Description
--
-- 'queueWaitIdle' is equivalent to submitting a fence to a queue and
-- waiting with an infinite timeout for that fence to signal.
--
-- == Valid Usage (Implicit)
--
-- -   @queue@ /must/ be a valid 'Vulkan.Core10.Handles.Queue' handle
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
queueWaitIdle queue = liftIO $ do
  let vkQueueWaitIdlePtr = pVkQueueWaitIdle (deviceCmds (queue :: Queue))
  unless (vkQueueWaitIdlePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueWaitIdle is null" Nothing Nothing
  let vkQueueWaitIdle' = mkVkQueueWaitIdle vkQueueWaitIdlePtr
  r <- vkQueueWaitIdle' (queueHandle (queue))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDeviceWaitIdle
  :: FunPtr (Ptr Device_T -> IO Result) -> Ptr Device_T -> IO Result

-- | vkDeviceWaitIdle - Wait for a device to become idle
--
-- = Description
--
-- 'deviceWaitIdle' is equivalent to calling 'queueWaitIdle' for all queues
-- owned by @device@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
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
deviceWaitIdle device = liftIO $ do
  let vkDeviceWaitIdlePtr = pVkDeviceWaitIdle (deviceCmds (device :: Device))
  unless (vkDeviceWaitIdlePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDeviceWaitIdle is null" Nothing Nothing
  let vkDeviceWaitIdle' = mkVkDeviceWaitIdle vkDeviceWaitIdlePtr
  r <- vkDeviceWaitIdle' (deviceHandle (device))
  when (r < SUCCESS) (throwIO (VulkanException r))


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
-- -   Each element of @pCommandBuffers@ /must/ not have been allocated
--     with
--     'Vulkan.Core10.Enums.CommandBufferLevel.COMMAND_BUFFER_LEVEL_SECONDARY'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   Each element of @pWaitDstStageMask@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'
--
-- -   If any element of @pWaitSemaphores@ or @pSignalSemaphores@ was
--     created with a 'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE', then
--     the @pNext@ chain /must/ include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'
--     structure
--
-- -   If the @pNext@ chain of this structure includes a
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'
--     structure and any element of @pWaitSemaphores@ was created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE', then
--     its @waitSemaphoreValueCount@ member /must/ equal
--     @waitSemaphoreCount@
--
-- -   If the @pNext@ chain of this structure includes a
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'
--     structure and any element of @pSignalSemaphores@ was created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE', then
--     its @signalSemaphoreValueCount@ member /must/ equal
--     @signalSemaphoreCount@
--
-- -   For each element of @pSignalSemaphores@ created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' the
--     corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'::pSignalSemaphoreValues
--     /must/ have a value greater than the current value of the semaphore
--     when the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operation>
--     is executed
--
-- -   For each element of @pWaitSemaphores@ created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' the
--     corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'::pWaitSemaphoreValues
--     /must/ have a value which does not differ from the current value of
--     the semaphore or the value of any outstanding semaphore wait or
--     signal operation on that semaphore by more than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxTimelineSemaphoreValueDifference maxTimelineSemaphoreValueDifference>
--
-- -   For each element of @pSignalSemaphores@ created with a
--     'Vulkan.Core12.Enums.SemaphoreType.SemaphoreType' of
--     'Vulkan.Core12.Enums.SemaphoreType.SEMAPHORE_TYPE_TIMELINE' the
--     corresponding element of
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo'::pSignalSemaphoreValues
--     /must/ have a value which does not differ from the current value of
--     the semaphore or the value of any outstanding semaphore wait or
--     signal operation on that semaphore by more than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxTimelineSemaphoreValueDifference maxTimelineSemaphoreValueDifference>
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBMIT_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Vulkan.Extensions.VK_KHR_external_semaphore_win32.D3D12FenceSubmitInfoKHR',
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupSubmitInfo',
--     'Vulkan.Extensions.VK_KHR_performance_query.PerformanceQuerySubmitInfoKHR',
--     'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.ProtectedSubmitInfo',
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo',
--     'Vulkan.Extensions.VK_KHR_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoKHR',
--     or
--     'Vulkan.Extensions.VK_NV_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoNV'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   If @waitSemaphoreCount@ is not @0@, @pWaitSemaphores@ /must/ be a
--     valid pointer to an array of @waitSemaphoreCount@ valid
--     'Vulkan.Core10.Handles.Semaphore' handles
--
-- -   If @waitSemaphoreCount@ is not @0@, @pWaitDstStageMask@ /must/ be a
--     valid pointer to an array of @waitSemaphoreCount@ valid combinations
--     of 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits'
--     values
--
-- -   Each element of @pWaitDstStageMask@ /must/ not be @0@
--
-- -   If @commandBufferCount@ is not @0@, @pCommandBuffers@ /must/ be a
--     valid pointer to an array of @commandBufferCount@ valid
--     'Vulkan.Core10.Handles.CommandBuffer' handles
--
-- -   If @signalSemaphoreCount@ is not @0@, @pSignalSemaphores@ /must/ be
--     a valid pointer to an array of @signalSemaphoreCount@ valid
--     'Vulkan.Core10.Handles.Semaphore' handles
--
-- -   Each of the elements of @pCommandBuffers@, the elements of
--     @pSignalSemaphores@, and the elements of @pWaitSemaphores@ that are
--     valid handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlags',
-- 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'queueSubmit'
data SubmitInfo (es :: [Type]) = SubmitInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
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
deriving instance Show (Chain es) => Show (SubmitInfo es)

instance Extensible SubmitInfo where
  extensibleType = STRUCTURE_TYPE_SUBMIT_INFO
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
    pPWaitSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Semaphore))) (pPWaitSemaphores')
    pPWaitDstStageMask' <- ContT $ allocaBytesAligned @PipelineStageFlags ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitDstStageMask' `plusPtr` (4 * (i)) :: Ptr PipelineStageFlags) (e)) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr PipelineStageFlags))) (pPWaitDstStageMask')
    pPCommandBuffers' <- ContT $ allocaBytesAligned @(Ptr CommandBuffer_T) ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBuffers' `plusPtr` (8 * (i)) :: Ptr (Ptr CommandBuffer_T)) (e)) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (Ptr CommandBuffer_T)))) (pPCommandBuffers')
    pPSignalSemaphores' <- ContT $ allocaBytesAligned @Semaphore ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphores' `plusPtr` (8 * (i)) :: Ptr Semaphore) (e)) (mempty)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr Semaphore))) (pPSignalSemaphores')
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

