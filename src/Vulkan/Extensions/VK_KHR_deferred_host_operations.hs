{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_deferred_host_operations  ( createDeferredOperationKHR
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
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
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
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Extensions.Handles (DeferredOperationKHR)
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateDeferredOperationKHR))
import Vulkan.Dynamic (DeviceCmds(pVkDeferredOperationJoinKHR))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyDeferredOperationKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeferredOperationMaxConcurrencyKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeferredOperationResultKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDeferredOperationKHR
  :: FunPtr (Ptr Device_T -> Ptr AllocationCallbacks -> Ptr DeferredOperationKHR -> IO Result) -> Ptr Device_T -> Ptr AllocationCallbacks -> Ptr DeferredOperationKHR -> IO Result

-- | vkCreateDeferredOperationKHR - Create a deferred operation handle
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pDeferredOperation@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
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
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device'
createDeferredOperationKHR :: forall io
                            . (MonadIO io)
                           => -- | @device@ is the device which owns @operation@.
                              Device
                           -> -- | @pAllocator@ controls host memory allocation as described in the
                              -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                              -- chapter.
                              ("allocator" ::: Maybe AllocationCallbacks)
                           -> io (DeferredOperationKHR)
createDeferredOperationKHR device allocator = liftIO . evalContT $ do
  let vkCreateDeferredOperationKHRPtr = pVkCreateDeferredOperationKHR (deviceCmds (device :: Device))
  lift $ unless (vkCreateDeferredOperationKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDeferredOperationKHR is null" Nothing Nothing
  let vkCreateDeferredOperationKHR' = mkVkCreateDeferredOperationKHR vkCreateDeferredOperationKHRPtr
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
withDeferredOperationKHR :: forall io r . MonadIO io => Device -> Maybe AllocationCallbacks -> (io (DeferredOperationKHR) -> ((DeferredOperationKHR) -> io ()) -> r) -> r
withDeferredOperationKHR device pAllocator b =
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
-- == Valid Usage
--
-- -   If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @operation@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @operation@ was created, @pAllocator@ /must/ be @NULL@
--
-- -   @operation@ /must/ be completed
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @operation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @operation@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   If @operation@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @operation@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device'
destroyDeferredOperationKHR :: forall io
                             . (MonadIO io)
                            => -- | @device@ is the device which owns @operation@.
                               Device
                            -> -- | @operation@ is the completed operation to be destroyed.
                               DeferredOperationKHR
                            -> -- | @pAllocator@ controls host memory allocation as described in the
                               -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                               -- chapter.
                               ("allocator" ::: Maybe AllocationCallbacks)
                            -> io ()
destroyDeferredOperationKHR device operation allocator = liftIO . evalContT $ do
  let vkDestroyDeferredOperationKHRPtr = pVkDestroyDeferredOperationKHR (deviceCmds (device :: Device))
  lift $ unless (vkDestroyDeferredOperationKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDeferredOperationKHR is null" Nothing Nothing
  let vkDestroyDeferredOperationKHR' = mkVkDestroyDeferredOperationKHR vkDestroyDeferredOperationKHRPtr
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
-- = Description
--
-- The returned value is the maximum number of threads that can usefully
-- execute a deferred operation concurrently, reported for the state of the
-- deferred operation at the point this command is called. This value is
-- intended to be used to better schedule work onto available threads.
-- Applications /can/ join any number of threads to the deferred operation
-- and expect it to eventually complete, though excessive joins /may/
-- return 'Vulkan.Core10.Enums.Result.THREAD_IDLE_KHR' immediately,
-- performing no useful work.
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
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device'
getDeferredOperationMaxConcurrencyKHR :: forall io
                                       . (MonadIO io)
                                      => -- | @device@ is the device which owns @operation@.
                                         --
                                         -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                         Device
                                      -> -- | @operation@ is the deferred operation to be queried.
                                         --
                                         -- @operation@ /must/ be a valid
                                         -- 'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
                                         --
                                         -- @operation@ /must/ have been created, allocated, or retrieved from
                                         -- @device@
                                         DeferredOperationKHR
                                      -> io (Word32)
getDeferredOperationMaxConcurrencyKHR device operation = liftIO $ do
  let vkGetDeferredOperationMaxConcurrencyKHRPtr = pVkGetDeferredOperationMaxConcurrencyKHR (deviceCmds (device :: Device))
  unless (vkGetDeferredOperationMaxConcurrencyKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeferredOperationMaxConcurrencyKHR is null" Nothing Nothing
  let vkGetDeferredOperationMaxConcurrencyKHR' = mkVkGetDeferredOperationMaxConcurrencyKHR vkGetDeferredOperationMaxConcurrencyKHRPtr
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
-- = Description
--
-- If the deferred operation is pending, 'getDeferredOperationResultKHR'
-- returns 'Vulkan.Core10.Enums.Result.NOT_READY'. Otherwise, it returns
-- the result of the deferred operation. This value /must/ be one of the
-- 'Vulkan.Core10.Enums.Result.Result' values which could have been
-- returned by the original command if the operation had not been deferred.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.NOT_READY'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device'
getDeferredOperationResultKHR :: forall io
                               . (MonadIO io)
                              => -- | @device@ is the device which owns @operation@.
                                 --
                                 -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                 Device
                              -> -- | @operation@ is the operation whose deferred result is being queried.
                                 --
                                 -- @operation@ /must/ be a valid
                                 -- 'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
                                 --
                                 -- @operation@ /must/ have been created, allocated, or retrieved from
                                 -- @device@
                                 DeferredOperationKHR
                              -> io (Result)
getDeferredOperationResultKHR device operation = liftIO $ do
  let vkGetDeferredOperationResultKHRPtr = pVkGetDeferredOperationResultKHR (deviceCmds (device :: Device))
  unless (vkGetDeferredOperationResultKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeferredOperationResultKHR is null" Nothing Nothing
  let vkGetDeferredOperationResultKHR' = mkVkGetDeferredOperationResultKHR vkGetDeferredOperationResultKHRPtr
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
-- = Description
--
-- The 'deferredOperationJoinKHR' command will execute a portion of the
-- deferred operation on the calling thread.
--
-- The return value will be one of the following:
--
-- -   A return value of 'Vulkan.Core10.Enums.Result.SUCCESS' indicates
--     that @operation@ is complete. The application /should/ use
--     'getDeferredOperationResultKHR' to retrieve the result of
--     @operation@.
--
-- -   A return value of 'Vulkan.Core10.Enums.Result.THREAD_DONE_KHR'
--     indicates that the deferred operation is not complete, but there is
--     no work remaining to assign to threads. Future calls to
--     'deferredOperationJoinKHR' are not necessary and will simply harm
--     performance. This situation /may/ occur when other threads executing
--     'deferredOperationJoinKHR' are about to complete @operation@, and
--     the implementation is unable to partition the workload any further.
--
-- -   A return value of 'Vulkan.Core10.Enums.Result.THREAD_IDLE_KHR'
--     indicates that the deferred operation is not complete, and there is
--     no work for the thread to do at the time of the call. This situation
--     /may/ occur if the operation encounters a temporary reduction in
--     parallelism. By returning
--     'Vulkan.Core10.Enums.Result.THREAD_IDLE_KHR', the implementation is
--     signaling that it expects that more opportunities for parallelism
--     will emerge as execution progresses, and that future calls to
--     'deferredOperationJoinKHR' /can/ be beneficial. In the meantime, the
--     application /can/ perform other work on the calling thread.
--
-- Implementations /must/ guarantee forward progress by enforcing the
-- following invariants:
--
-- 1.  If only one thread has invoked 'deferredOperationJoinKHR' on a given
--     operation, that thread /must/ execute the operation to completion
--     and return 'Vulkan.Core10.Enums.Result.SUCCESS'.
--
-- 2.  If multiple threads have concurrently invoked
--     'deferredOperationJoinKHR' on the same operation, then at least one
--     of them /must/ complete the operation and return
--     'Vulkan.Core10.Enums.Result.SUCCESS'.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @operation@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   @operation@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.THREAD_DONE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.THREAD_IDLE_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device'
deferredOperationJoinKHR :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the device which owns @operation@.
                            Device
                         -> -- | @operation@ is the deferred operation that the calling thread should
                            -- work on.
                            DeferredOperationKHR
                         -> io (Result)
deferredOperationJoinKHR device operation = liftIO $ do
  let vkDeferredOperationJoinKHRPtr = pVkDeferredOperationJoinKHR (deviceCmds (device :: Device))
  unless (vkDeferredOperationJoinKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDeferredOperationJoinKHR is null" Nothing Nothing
  let vkDeferredOperationJoinKHR' = mkVkDeferredOperationJoinKHR vkDeferredOperationJoinKHRPtr
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
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', no deferral is requested and
-- the command proceeds as if no 'DeferredOperationInfoKHR' structure was
-- provided.
--
-- When an application requests an operation deferral, the implementation
-- /may/ defer the operation. When deferral is requested and the
-- implementation defers any operation, the implementation /must/ return
-- 'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR' as the success code
-- if no errors occurred. When deferral is requested, the implementation
-- /should/ defer the operation when the workload is significant, however
-- if the implementation chooses not to defer any of the requested
-- operations and instead executes all of them immediately, the
-- implementation /must/ return
-- 'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR' as the success
-- code if no errors occurred.
--
-- A deferred operation is created /complete/ with an initial result value
-- of 'Vulkan.Core10.Enums.Result.SUCCESS'. The deferred operation becomes
-- /pending/ when an operation has been successfully deferred with that
-- @operationHandle@.
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
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR' using the
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
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeferredOperationInfoKHR = DeferredOperationInfoKHR
  { -- | @operationHandle@ is a handle to a tracking object to associate with the
    -- deferred operation.
    operationHandle :: DeferredOperationKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeferredOperationInfoKHR)
#endif
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

