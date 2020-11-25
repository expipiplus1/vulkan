{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_deferred_host_operations"
module Vulkan.Extensions.VK_KHR_deferred_host_operations  ( createDeferredOperationKHR
                                                          , withDeferredOperationKHR
                                                          , destroyDeferredOperationKHR
                                                          , getDeferredOperationMaxConcurrencyKHR
                                                          , getDeferredOperationResultKHR
                                                          , deferredOperationJoinKHR
                                                          , KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION
                                                          , pattern KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION
                                                          , KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
                                                          , pattern KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME
                                                          , DeferredOperationKHR(..)
                                                          ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Foreign.Storable (Storable(peek))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
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
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDeferredOperationKHR
  :: FunPtr (Ptr Device_T -> Ptr AllocationCallbacks -> Ptr DeferredOperationKHR -> IO Result) -> Ptr Device_T -> Ptr AllocationCallbacks -> Ptr DeferredOperationKHR -> IO Result

-- No documentation found for TopLevel "vkCreateDeferredOperationKHR"
createDeferredOperationKHR :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkCreateDeferredOperationKHR" "device"
                              Device
                           -> -- No documentation found for Nested "vkCreateDeferredOperationKHR" "pAllocator"
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
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDeferredOperationKHR :: forall io r . MonadIO io => Device -> Maybe AllocationCallbacks -> (io DeferredOperationKHR -> (DeferredOperationKHR -> io ()) -> r) -> r
withDeferredOperationKHR device pAllocator b =
  b (createDeferredOperationKHR device pAllocator)
    (\(o0) -> destroyDeferredOperationKHR device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDeferredOperationKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> DeferredOperationKHR -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyDeferredOperationKHR"
destroyDeferredOperationKHR :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "vkDestroyDeferredOperationKHR" "device"
                               Device
                            -> -- No documentation found for Nested "vkDestroyDeferredOperationKHR" "operation"
                               DeferredOperationKHR
                            -> -- No documentation found for Nested "vkDestroyDeferredOperationKHR" "pAllocator"
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

-- No documentation found for TopLevel "vkGetDeferredOperationMaxConcurrencyKHR"
getDeferredOperationMaxConcurrencyKHR :: forall io
                                       . (MonadIO io)
                                      => -- No documentation found for Nested "vkGetDeferredOperationMaxConcurrencyKHR" "device"
                                         Device
                                      -> -- No documentation found for Nested "vkGetDeferredOperationMaxConcurrencyKHR" "operation"
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

-- No documentation found for TopLevel "vkGetDeferredOperationResultKHR"
getDeferredOperationResultKHR :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkGetDeferredOperationResultKHR" "device"
                                 Device
                              -> -- No documentation found for Nested "vkGetDeferredOperationResultKHR" "operation"
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

-- No documentation found for TopLevel "vkDeferredOperationJoinKHR"
deferredOperationJoinKHR :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkDeferredOperationJoinKHR" "device"
                            Device
                         -> -- No documentation found for Nested "vkDeferredOperationJoinKHR" "operation"
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


type KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION"
pattern KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DEFERRED_HOST_OPERATIONS_SPEC_VERSION = 4


type KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME = "VK_KHR_deferred_host_operations"

-- No documentation found for TopLevel "VK_KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME"
pattern KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DEFERRED_HOST_OPERATIONS_EXTENSION_NAME = "VK_KHR_deferred_host_operations"

