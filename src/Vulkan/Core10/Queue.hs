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
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(..))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Handles (Queue(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceQueue
  :: FunPtr (Ptr Device_T -> Word32 -> Word32 -> Ptr (Ptr Queue_T) -> IO ()) -> Ptr Device_T -> Word32 -> Word32 -> Ptr (Ptr Queue_T) -> IO ()

-- No documentation found for TopLevel "vkGetDeviceQueue"
getDeviceQueue :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vkGetDeviceQueue" "device"
                  Device
               -> -- No documentation found for Nested "vkGetDeviceQueue" "queueFamilyIndex"
                  ("queueFamilyIndex" ::: Word32)
               -> -- No documentation found for Nested "vkGetDeviceQueue" "queueIndex"
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
  :: FunPtr (Ptr Queue_T -> Word32 -> Ptr (SomeStruct SubmitInfo) -> Fence -> IO Result) -> Ptr Queue_T -> Word32 -> Ptr (SomeStruct SubmitInfo) -> Fence -> IO Result

-- No documentation found for TopLevel "vkQueueSubmit"
queueSubmit :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "vkQueueSubmit" "queue"
               Queue
            -> -- No documentation found for Nested "vkQueueSubmit" "pSubmits"
               ("submits" ::: Vector (SomeStruct SubmitInfo))
            -> -- No documentation found for Nested "vkQueueSubmit" "fence"
               Fence
            -> io ()
queueSubmit queue submits fence = liftIO . evalContT $ do
  let vkQueueSubmitPtr = pVkQueueSubmit (deviceCmds (queue :: Queue))
  lift $ unless (vkQueueSubmitPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueSubmit is null" Nothing Nothing
  let vkQueueSubmit' = mkVkQueueSubmit vkQueueSubmitPtr
  pPSubmits <- ContT $ allocaBytesAligned @(SubmitInfo _) ((Data.Vector.length (submits)) * 72) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPSubmits `plusPtr` (72 * (i)) :: Ptr (SubmitInfo _))) (e) . ($ ())) (submits)
  r <- lift $ vkQueueSubmit' (queueHandle (queue)) ((fromIntegral (Data.Vector.length $ (submits)) :: Word32)) (forgetExtensions (pPSubmits)) (fence)
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
                          => -- No documentation found for TopLevel ""
                             (FunPtr (Ptr Queue_T -> IO Result) -> Ptr Queue_T -> IO Result)
                          -> -- No documentation found for Nested "vkQueueWaitIdle" "queue"
                             Queue
                          -> io ()
queueWaitIdleSafeOrUnsafe mkVkQueueWaitIdle queue = liftIO $ do
  let vkQueueWaitIdlePtr = pVkQueueWaitIdle (deviceCmds (queue :: Queue))
  unless (vkQueueWaitIdlePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueWaitIdle is null" Nothing Nothing
  let vkQueueWaitIdle' = mkVkQueueWaitIdle vkQueueWaitIdlePtr
  r <- vkQueueWaitIdle' (queueHandle (queue))
  when (r < SUCCESS) (throwIO (VulkanException r))

-- No documentation found for TopLevel "vkQueueWaitIdle"
queueWaitIdle :: forall io
               . (MonadIO io)
              => -- No documentation found for Nested "vkQueueWaitIdle" "queue"
                 Queue
              -> io ()
queueWaitIdle = queueWaitIdleSafeOrUnsafe mkVkQueueWaitIdleUnsafe

-- | A variant of 'queueWaitIdle' which makes a *safe* FFI call
queueWaitIdleSafe :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkQueueWaitIdle" "queue"
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
                           => -- No documentation found for TopLevel ""
                              (FunPtr (Ptr Device_T -> IO Result) -> Ptr Device_T -> IO Result)
                           -> -- No documentation found for Nested "vkDeviceWaitIdle" "device"
                              Device
                           -> io ()
deviceWaitIdleSafeOrUnsafe mkVkDeviceWaitIdle device = liftIO $ do
  let vkDeviceWaitIdlePtr = pVkDeviceWaitIdle (deviceCmds (device :: Device))
  unless (vkDeviceWaitIdlePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDeviceWaitIdle is null" Nothing Nothing
  let vkDeviceWaitIdle' = mkVkDeviceWaitIdle vkDeviceWaitIdlePtr
  r <- vkDeviceWaitIdle' (deviceHandle (device))
  when (r < SUCCESS) (throwIO (VulkanException r))

-- No documentation found for TopLevel "vkDeviceWaitIdle"
deviceWaitIdle :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vkDeviceWaitIdle" "device"
                  Device
               -> io ()
deviceWaitIdle = deviceWaitIdleSafeOrUnsafe mkVkDeviceWaitIdleUnsafe

-- | A variant of 'deviceWaitIdle' which makes a *safe* FFI call
deviceWaitIdleSafe :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkDeviceWaitIdle" "device"
                      Device
                   -> io ()
deviceWaitIdleSafe = deviceWaitIdleSafeOrUnsafe mkVkDeviceWaitIdleSafe



-- No documentation found for TopLevel "VkSubmitInfo"
data SubmitInfo (es :: [Type]) = SubmitInfo
  { -- No documentation found for Nested "VkSubmitInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkSubmitInfo" "pWaitSemaphores"
    waitSemaphores :: Vector Semaphore
  , -- No documentation found for Nested "VkSubmitInfo" "pWaitDstStageMask"
    waitDstStageMask :: Vector PipelineStageFlags
  , -- No documentation found for Nested "VkSubmitInfo" "pCommandBuffers"
    commandBuffers :: Vector (Ptr CommandBuffer_T)
  , -- No documentation found for Nested "VkSubmitInfo" "pSignalSemaphores"
    signalSemaphores :: Vector Semaphore
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubmitInfo (es :: [Type]))
#endif
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

