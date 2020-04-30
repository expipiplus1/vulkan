{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_deferred_host_operations  ( createDeferredOperationKHR
                                                                   , withDeferredOperationKHR
                                                                   , destroyDeferredOperationKHR
                                                                   , getDeferredOperationMaxConcurrencyKHR
                                                                   , getDeferredOperationResultKHR
                                                                   , deferredOperationJoinKHR
                                                                   , DeferredOperationInfoKHR(..)
                                                                   , KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION
                                                                   , pattern KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION
                                                                   , KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
                                                                   , pattern KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
                                                                   , DeferredOperationKHR(..)
                                                                   ) where

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Extensions.Handles (DeferredOperationKHR)
import Graphics.Vulkan.Extensions.Handles (DeferredOperationKHR(..))
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateDeferredOperationKHR))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDeferredOperationJoinKHR))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyDeferredOperationKHR))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetDeferredOperationMaxConcurrencyKHR))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetDeferredOperationResultKHR))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.Handles (DeferredOperationKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDeferredOperationKHR
  :: FunPtr (Ptr Device_T -> Ptr AllocationCallbacks -> Ptr DeferredOperationKHR -> IO Result) -> Ptr Device_T -> Ptr AllocationCallbacks -> Ptr DeferredOperationKHR -> IO Result

-- | vkCreateDeferredOperationKHR - Create a deferred operation handle
--
-- = Parameters
--
-- -   @device@ is the device which owns @operation@.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pDeferredOperation@ is a pointer to a handle in which the created
--     'Graphics.Vulkan.Extensions.Handles.DeferredOperationKHR' is
--     returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pDeferredOperation@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Graphics.Vulkan.Core10.Handles.Device'
createDeferredOperationKHR :: forall io . MonadIO io => Device -> ("allocator" ::: Maybe AllocationCallbacks) -> io (DeferredOperationKHR)
createDeferredOperationKHR device allocator = liftIO . evalContT $ do
  let vkCreateDeferredOperationKHR' = mkVkCreateDeferredOperationKHR (pVkCreateDeferredOperationKHR (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPDeferredOperation <- ContT $ bracket (callocBytes @DeferredOperationKHR 8) free
  r <- lift $ vkCreateDeferredOperationKHR' (deviceHandle (device)) pAllocator (pPDeferredOperation)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDeferredOperation <- lift $ peek @DeferredOperationKHR pPDeferredOperation
  pure $ (pDeferredOperation)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDeferredOperationKHR' and 'destroyDeferredOperationKHR'
--
-- To ensure that 'destroyDeferredOperationKHR' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withDeferredOperationKHR :: forall io r . MonadIO io => (io (DeferredOperationKHR) -> ((DeferredOperationKHR) -> io ()) -> r) -> Device -> Maybe AllocationCallbacks -> r
withDeferredOperationKHR b device pAllocator =
  b (createDeferredOperationKHR device pAllocator)
    (\(o0) -> destroyDeferredOperationKHR device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDeferredOperationKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> DeferredOperationKHR -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyDeferredOperationKHR - Destroy a deferred operation handle
--
-- = Parameters
--
-- -   @device@ is the device which owns @operation@.
--
-- -   @operation@ is the completed operation to be destroyed.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @operation@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @operation@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- -   @operation@ /must/ be completed
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @operation@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @operation@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Graphics.Vulkan.Core10.Handles.Device'
destroyDeferredOperationKHR :: forall io . MonadIO io => Device -> DeferredOperationKHR -> ("allocator" ::: Maybe AllocationCallbacks) -> io ()
destroyDeferredOperationKHR device operation allocator = liftIO . evalContT $ do
  let vkDestroyDeferredOperationKHR' = mkVkDestroyDeferredOperationKHR (pVkDestroyDeferredOperationKHR (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyDeferredOperationKHR' (deviceHandle (device)) (operation) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeferredOperationMaxConcurrencyKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> IO Word32) -> Ptr Device_T -> DeferredOperationKHR -> IO Word32

-- | vkGetDeferredOperationMaxConcurrencyKHR - Query the maximum concurrency
-- on a deferred operation
--
-- = Parameters
--
-- -   @device@ is the device which owns @operation@.
--
-- -   @operation@ is the deferred operation to be queried.
--
-- = Description
--
-- The returned value is the maximum number of threads that can usefully
-- execute a deferred operation concurrently, reported for the state of the
-- deferred operation at the point this command is called. This value is
-- intended to be used to better schedule work onto available threads.
-- Applications /can/ join any number of threads to the deferred operation
-- and expect it to eventually complete, though excessive joins /may/
-- return 'Graphics.Vulkan.Core10.Enums.Result.THREAD_IDLE_KHR'
-- immediately, performing no useful work.
--
-- If the deferred operation is currently joined to any threads, the value
-- returned by this command /may/ immediately be out of date.
--
-- Implementations /must/ not return zero.
--
-- Implementations /may/ return 232-1 to indicate that the maximum
-- concurrency is unknown and cannot be easily derived. Implementations
-- /may/ return values larger than the maximum concurrency available on the
-- host CPU. In these situations, an application /should/ clamp the return
-- value rather than oversubscribing the machine.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Graphics.Vulkan.Core10.Handles.Device'
getDeferredOperationMaxConcurrencyKHR :: forall io . MonadIO io => Device -> DeferredOperationKHR -> io (Word32)
getDeferredOperationMaxConcurrencyKHR device operation = liftIO $ do
  let vkGetDeferredOperationMaxConcurrencyKHR' = mkVkGetDeferredOperationMaxConcurrencyKHR (pVkGetDeferredOperationMaxConcurrencyKHR (deviceCmds (device :: Device)))
  r <- vkGetDeferredOperationMaxConcurrencyKHR' (deviceHandle (device)) (operation)
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeferredOperationResultKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> IO Result

-- | vkGetDeferredOperationResultKHR - Query the result of a deferred
-- operation
--
-- = Parameters
--
-- -   @device@ is the device which owns @operation@.
--
-- -   @operation@ is the operation whose deferred result is being queried.
--
-- = Description
--
-- If the deferred operation is pending, 'getDeferredOperationResultKHR'
-- returns 'Graphics.Vulkan.Core10.Enums.Result.NOT_READY'. Otherwise, it
-- returns the result of the deferred operation. This value /must/ be one
-- of the 'Graphics.Vulkan.Core10.Enums.Result.Result' values which could
-- have been returned by the original command if the operation had not been
-- deferred.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.NOT_READY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Graphics.Vulkan.Core10.Handles.Device'
getDeferredOperationResultKHR :: forall io . MonadIO io => Device -> DeferredOperationKHR -> io (Result)
getDeferredOperationResultKHR device operation = liftIO $ do
  let vkGetDeferredOperationResultKHR' = mkVkGetDeferredOperationResultKHR (pVkGetDeferredOperationResultKHR (deviceCmds (device :: Device)))
  r <- vkGetDeferredOperationResultKHR' (deviceHandle (device)) (operation)
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDeferredOperationJoinKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> IO Result

-- | vkDeferredOperationJoinKHR - Assign a thread to a deferred operation
--
-- = Parameters
--
-- -   @device@ is the device which owns @operation@.
--
-- -   @operation@ is the deferred operation that the calling thread should
--     work on.
--
-- = Description
--
-- The 'deferredOperationJoinKHR' command will execute a portion of the
-- deferred operation on the calling thread.
--
-- The return value will be one of the following:
--
-- -   A return value of 'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--     indicates that @operation@ is complete. The application /should/ use
--     'getDeferredOperationResultKHR' to retrieve the result of
--     @operation@.
--
-- -   A return value of
--     'Graphics.Vulkan.Core10.Enums.Result.THREAD_DONE_KHR' indicates that
--     the deferred operation is not complete, but there is no work
--     remaining to assign to threads. Future calls to
--     'deferredOperationJoinKHR' are not necessary and will simply harm
--     performance. This situation /may/ occur when other threads executing
--     'deferredOperationJoinKHR' are about to complete @operation@, and
--     the implementation is unable to partition the workload any further.
--
-- -   A return value of
--     'Graphics.Vulkan.Core10.Enums.Result.THREAD_IDLE_KHR' indicates that
--     the deferred operation is not complete, and there is no work for the
--     thread to do at the time of the call. This situation /may/ occur if
--     the operation encounters a temporary reduction in parallelism. By
--     returning 'Graphics.Vulkan.Core10.Enums.Result.THREAD_IDLE_KHR', the
--     implementation is signaling that it expects that more opportunities
--     for parallelism will emerge as execution progresses, and that future
--     calls to 'deferredOperationJoinKHR' /can/ be beneficial. In the
--     meantime, the application /can/ perform other work on the calling
--     thread.
--
-- Implementations /must/ guarantee forward progress by enforcing the
-- following invariants:
--
-- 1.  If only one thread has invoked 'deferredOperationJoinKHR' on a given
--     operation, that thread /must/ execute the operation to completion
--     and return 'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'.
--
-- 2.  If multiple threads have concurrently invoked
--     'deferredOperationJoinKHR' on the same operation, then at least one
--     of them /must/ complete the operation and return
--     'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @operation@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   @operation@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.THREAD_DONE_KHR'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.THREAD_IDLE_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Graphics.Vulkan.Core10.Handles.Device'
deferredOperationJoinKHR :: forall io . MonadIO io => Device -> DeferredOperationKHR -> io (Result)
deferredOperationJoinKHR device operation = liftIO $ do
  let vkDeferredOperationJoinKHR' = mkVkDeferredOperationJoinKHR (pVkDeferredOperationJoinKHR (deviceCmds (device :: Device)))
  r <- vkDeferredOperationJoinKHR' (deviceHandle (device)) (operation)
  when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


-- | VkDeferredOperationInfoKHR - Deferred operation request
--
-- = Description
--
-- The application /can/ request deferral of an operation by adding this
-- structure to the argument list of a command or by providing this in the
-- @pNext@ chain of a relevant structure for an operation when the
-- corresponding command is invoked. If this structure is not present, no
-- deferral is requested. If @operationHandle@ is
-- 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', no deferral is
-- requested and the command proceeds as if no 'DeferredOperationInfoKHR'
-- structure was provided.
--
-- When an application requests an operation deferral, the implementation
-- /may/ defer the operation. When deferral is requested and the
-- implementation defers any operation, the implementation /must/ return
-- 'Graphics.Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR' as the
-- success code if no errors occurred. When deferral is requested, the
-- implementation /should/ defer the operation when the workload is
-- significant, however if the implementation chooses not to defer any of
-- the requested operations and instead executes all of them immediately,
-- the implementation /must/ return
-- 'Graphics.Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR' as the
-- success code if no errors occurred.
--
-- A deferred operation is created /complete/ with an initial result value
-- of 'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'. The deferred operation
-- becomes /pending/ when an operation has been successfully deferred with
-- that @operationHandle@.
--
-- A deferred operation is considered pending until the deferred operation
-- completes. A pending deferred operation becomes /complete/ when it has
-- been fully executed by one or more threads. Pending deferred operations
-- will never complete until they are /joined/ by an application thread,
-- using 'deferredOperationJoinKHR'. Applications /can/ join multiple
-- threads to the same deferred operation, enabling concurrent execution of
-- subtasks within that operation.
--
-- The application /can/ query the status of a
-- 'Graphics.Vulkan.Extensions.Handles.DeferredOperationKHR' using the
-- 'getDeferredOperationMaxConcurrencyKHR' or
-- 'getDeferredOperationResultKHR' commands.
--
-- From the perspective of other commands - parameters to the original
-- command that are externally synchronized /must/ not be accessed before
-- the deferred operation completes, and the result of the deferred
-- operation (e.g. object creation) are not considered complete until the
-- deferred operation completes.
--
-- If the deferred operation is one which creates an object (for example, a
-- pipeline object), the implementation /must/ allocate that object as it
-- normally would, and return a valid handle to the application. This
-- object is a /pending/ object, and /must/ not be used by the application
-- until the deferred operation is completed (unless otherwise specified by
-- the deferral extension). When the deferred operation is complete, the
-- application /should/ call 'getDeferredOperationResultKHR' to obtain the
-- result of the operation. If 'getDeferredOperationResultKHR' indicates
-- failure, the application /must/ destroy the pending object using an
-- appropriate command, so that the implementation has an opportunity to
-- recover the handle. The application /must/ not perform this destruction
-- until the deferred operation is complete. Construction of the pending
-- object uses the same allocator which would have been used if the
-- operation had not been deferred.
--
-- == Valid Usage
--
-- -   Any previous deferred operation that was associated with
--     @operationHandle@ /must/ be complete
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DeferredOperationInfoKHR = DeferredOperationInfoKHR
  { -- | @operationHandle@ is a handle to a tracking object to associate with the
    -- deferred operation.
    operationHandle :: DeferredOperationKHR }
  deriving (Typeable)
deriving instance Show DeferredOperationInfoKHR

instance ToCStruct DeferredOperationInfoKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeferredOperationInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeferredOperationKHR)) (operationHandle)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeferredOperationKHR)) (zero)
    f

instance FromCStruct DeferredOperationInfoKHR where
  peekCStruct p = do
    operationHandle <- peek @DeferredOperationKHR ((p `plusPtr` 16 :: Ptr DeferredOperationKHR))
    pure $ DeferredOperationInfoKHR
             operationHandle

instance Storable DeferredOperationInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeferredOperationInfoKHR where
  zero = DeferredOperationInfoKHR
           zero


type KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION"
pattern KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION = 2


type KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME = "VK_KHR_deferred_host_operations"

-- No documentation found for TopLevel "VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME"
pattern KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME = "VK_KHR_deferred_host_operations"

