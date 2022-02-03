{-# language CPP #-}
-- No documentation found for Chapter "Fence"
module Vulkan.Core10.Fence  ( createFence
                            , withFence
                            , destroyFence
                            , resetFences
                            , getFenceStatus
                            , waitForFences
                            , waitForFencesSafe
                            , FenceCreateInfo(..)
                            , Fence(..)
                            , FenceCreateFlagBits(..)
                            , FenceCreateFlags
                            ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateFence))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyFence))
import Vulkan.Dynamic (DeviceCmds(pVkGetFenceStatus))
import Vulkan.Dynamic (DeviceCmds(pVkResetFences))
import Vulkan.Dynamic (DeviceCmds(pVkWaitForFences))
import Vulkan.Core10.Handles (Device_T)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_fence (ExportFenceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_win32 (ExportFenceWin32HandleInfoKHR)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.Core10.Enums.FenceCreateFlagBits (FenceCreateFlags)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FENCE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.Core10.Enums.FenceCreateFlagBits (FenceCreateFlagBits(..))
import Vulkan.Core10.Enums.FenceCreateFlagBits (FenceCreateFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateFence
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct FenceCreateInfo) -> Ptr AllocationCallbacks -> Ptr Fence -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct FenceCreateInfo) -> Ptr AllocationCallbacks -> Ptr Fence -> IO Result

-- | vkCreateFence - Create a new fence object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateFence-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateFence-pCreateInfo-parameter# @pCreateInfo@ /must/ be a
--     valid pointer to a valid 'FenceCreateInfo' structure
--
-- -   #VUID-vkCreateFence-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateFence-pFence-parameter# @pFence@ /must/ be a valid
--     pointer to a 'Vulkan.Core10.Handles.Fence' handle
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Fence',
-- 'FenceCreateInfo'
createFence :: forall a io
             . (Extendss FenceCreateInfo a, PokeChain a, MonadIO io)
            => -- | @device@ is the logical device that creates the fence.
               Device
            -> -- | @pCreateInfo@ is a pointer to a 'FenceCreateInfo' structure containing
               -- information about how the fence is to be created.
               (FenceCreateInfo a)
            -> -- | @pAllocator@ controls host memory allocation as described in the
               -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
               -- chapter.
               ("allocator" ::: Maybe AllocationCallbacks)
            -> io (Fence)
createFence device createInfo allocator = liftIO . evalContT $ do
  let vkCreateFencePtr = pVkCreateFence (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateFencePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateFence is null" Nothing Nothing
  let vkCreateFence' = mkVkCreateFence vkCreateFencePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPFence <- ContT $ bracket (callocBytes @Fence 8) free
  r <- lift $ traceAroundEvent "vkCreateFence" (vkCreateFence' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPFence))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFence <- lift $ peek @Fence pPFence
  pure $ (pFence)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createFence' and 'destroyFence'
--
-- To ensure that 'destroyFence' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withFence :: forall a io r . (Extendss FenceCreateInfo a, PokeChain a, MonadIO io) => Device -> FenceCreateInfo a -> Maybe AllocationCallbacks -> (io Fence -> (Fence -> io ()) -> r) -> r
withFence device pCreateInfo pAllocator b =
  b (createFence device pCreateInfo pAllocator)
    (\(o0) -> destroyFence device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyFence
  :: FunPtr (Ptr Device_T -> Fence -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Fence -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyFence - Destroy a fence object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyFence-fence-01120# All
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-submission queue submission>
--     commands that refer to @fence@ /must/ have completed execution
--
-- -   #VUID-vkDestroyFence-fence-01121# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @fence@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyFence-fence-01122# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @fence@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyFence-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyFence-fence-parameter# If @fence@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @fence@ /must/ be a valid
--     'Vulkan.Core10.Handles.Fence' handle
--
-- -   #VUID-vkDestroyFence-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyFence-fence-parent# If @fence@ is a valid handle, it
--     /must/ have been created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @fence@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Fence'
destroyFence :: forall io
              . (MonadIO io)
             => -- | @device@ is the logical device that destroys the fence.
                Device
             -> -- | @fence@ is the handle of the fence to destroy.
                Fence
             -> -- | @pAllocator@ controls host memory allocation as described in the
                -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                -- chapter.
                ("allocator" ::: Maybe AllocationCallbacks)
             -> io ()
destroyFence device fence allocator = liftIO . evalContT $ do
  let vkDestroyFencePtr = pVkDestroyFence (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyFencePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyFence is null" Nothing Nothing
  let vkDestroyFence' = mkVkDestroyFence vkDestroyFencePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyFence" (vkDestroyFence' (deviceHandle (device)) (fence) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetFences
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr Fence -> IO Result) -> Ptr Device_T -> Word32 -> Ptr Fence -> IO Result

-- | vkResetFences - Resets one or more fence objects
--
-- = Description
--
-- If any member of @pFences@ currently has its
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-fences-importing payload imported>
-- with temporary permanence, that fence’s prior permanent payload is first
-- restored. The remaining operations described therefore operate on the
-- restored payload.
--
-- When 'resetFences' is executed on the host, it defines a /fence unsignal
-- operation/ for each fence, which resets the fence to the unsignaled
-- state.
--
-- If any member of @pFences@ is already in the unsignaled state when
-- 'resetFences' is executed, then 'resetFences' has no effect on that
-- fence.
--
-- == Valid Usage
--
-- -   #VUID-vkResetFences-pFences-01123# Each element of @pFences@ /must/
--     not be currently associated with any queue command that has not yet
--     completed execution on that queue
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkResetFences-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkResetFences-pFences-parameter# @pFences@ /must/ be a valid
--     pointer to an array of @fenceCount@ valid
--     'Vulkan.Core10.Handles.Fence' handles
--
-- -   #VUID-vkResetFences-fenceCount-arraylength# @fenceCount@ /must/ be
--     greater than @0@
--
-- -   #VUID-vkResetFences-pFences-parent# Each element of @pFences@ /must/
--     have been created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to each member of @pFences@ /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Fence'
resetFences :: forall io
             . (MonadIO io)
            => -- | @device@ is the logical device that owns the fences.
               Device
            -> -- | @pFences@ is a pointer to an array of fence handles to reset.
               ("fences" ::: Vector Fence)
            -> io ()
resetFences device fences = liftIO . evalContT $ do
  let vkResetFencesPtr = pVkResetFences (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkResetFencesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetFences is null" Nothing Nothing
  let vkResetFences' = mkVkResetFences vkResetFencesPtr
  pPFences <- ContT $ allocaBytes @Fence ((Data.Vector.length (fences)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPFences `plusPtr` (8 * (i)) :: Ptr Fence) (e)) (fences)
  r <- lift $ traceAroundEvent "vkResetFences" (vkResetFences' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (fences)) :: Word32)) (pPFences))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceStatus
  :: FunPtr (Ptr Device_T -> Fence -> IO Result) -> Ptr Device_T -> Fence -> IO Result

-- | vkGetFenceStatus - Return the status of a fence
--
-- = Description
--
-- Upon success, 'getFenceStatus' returns the status of the fence object,
-- with the following return codes:
--
-- +------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+
-- | Status                                         | Meaning                                                                                                                |
-- +================================================+========================================================================================================================+
-- | 'Vulkan.Core10.Enums.Result.SUCCESS'           | The fence specified by @fence@ is signaled.                                                                            |
-- +------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+
-- | 'Vulkan.Core10.Enums.Result.NOT_READY'         | The fence specified by @fence@ is unsignaled.                                                                          |
-- +------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+
-- | 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST' | The device has been lost. See                                                                                          |
-- |                                                | <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>. |
-- +------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+
--
-- Fence Object Status Codes
--
-- If a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-submission queue submission>
-- command is pending execution, then the value returned by this command
-- /may/ immediately be out of date.
--
-- If the device has been lost (see
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>),
-- 'getFenceStatus' /may/ return any of the above status codes. If the
-- device has been lost and 'getFenceStatus' is called repeatedly, it will
-- eventually return either 'Vulkan.Core10.Enums.Result.SUCCESS' or
-- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.NOT_READY'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Fence'
getFenceStatus :: forall io
                . (MonadIO io)
               => -- | @device@ is the logical device that owns the fence.
                  --
                  -- #VUID-vkGetFenceStatus-device-parameter# @device@ /must/ be a valid
                  -- 'Vulkan.Core10.Handles.Device' handle
                  Device
               -> -- | @fence@ is the handle of the fence to query.
                  --
                  -- #VUID-vkGetFenceStatus-fence-parameter# @fence@ /must/ be a valid
                  -- 'Vulkan.Core10.Handles.Fence' handle
                  --
                  -- #VUID-vkGetFenceStatus-fence-parent# @fence@ /must/ have been created,
                  -- allocated, or retrieved from @device@
                  Fence
               -> io (Result)
getFenceStatus device fence = liftIO $ do
  let vkGetFenceStatusPtr = pVkGetFenceStatus (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkGetFenceStatusPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetFenceStatus is null" Nothing Nothing
  let vkGetFenceStatus' = mkVkGetFenceStatus vkGetFenceStatusPtr
  r <- traceAroundEvent "vkGetFenceStatus" (vkGetFenceStatus' (deviceHandle (device)) (fence))
  when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWaitForFencesUnsafe
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr Fence -> Bool32 -> Word64 -> IO Result) -> Ptr Device_T -> Word32 -> Ptr Fence -> Bool32 -> Word64 -> IO Result

foreign import ccall
  "dynamic" mkVkWaitForFencesSafe
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr Fence -> Bool32 -> Word64 -> IO Result) -> Ptr Device_T -> Word32 -> Ptr Fence -> Bool32 -> Word64 -> IO Result

-- | waitForFences with selectable safeness
waitForFencesSafeOrUnsafe :: forall io
                           . (MonadIO io)
                          => (FunPtr (Ptr Device_T -> Word32 -> Ptr Fence -> Bool32 -> Word64 -> IO Result) -> Ptr Device_T -> Word32 -> Ptr Fence -> Bool32 -> Word64 -> IO Result)
                          -> -- | @device@ is the logical device that owns the fences.
                             Device
                          -> -- | @pFences@ is a pointer to an array of @fenceCount@ fence handles.
                             ("fences" ::: Vector Fence)
                          -> -- | @waitAll@ is the condition that /must/ be satisfied to successfully
                             -- unblock the wait. If @waitAll@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
                             -- then the condition is that all fences in @pFences@ are signaled.
                             -- Otherwise, the condition is that at least one fence in @pFences@ is
                             -- signaled.
                             ("waitAll" ::: Bool)
                          -> -- | @timeout@ is the timeout period in units of nanoseconds. @timeout@ is
                             -- adjusted to the closest value allowed by the implementation-dependent
                             -- timeout accuracy, which /may/ be substantially longer than one
                             -- nanosecond, and /may/ be longer than the requested period.
                             ("timeout" ::: Word64)
                          -> io (Result)
waitForFencesSafeOrUnsafe mkVkWaitForFences device fences waitAll timeout = liftIO . evalContT $ do
  let vkWaitForFencesPtr = pVkWaitForFences (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkWaitForFencesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkWaitForFences is null" Nothing Nothing
  let vkWaitForFences' = mkVkWaitForFences vkWaitForFencesPtr
  pPFences <- ContT $ allocaBytes @Fence ((Data.Vector.length (fences)) * 8)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPFences `plusPtr` (8 * (i)) :: Ptr Fence) (e)) (fences)
  r <- lift $ traceAroundEvent "vkWaitForFences" (vkWaitForFences' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (fences)) :: Word32)) (pPFences) (boolToBool32 (waitAll)) (timeout))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)

-- | vkWaitForFences - Wait for one or more fences to become signaled
--
-- = Description
--
-- If the condition is satisfied when 'waitForFences' is called, then
-- 'waitForFences' returns immediately. If the condition is not satisfied
-- at the time 'waitForFences' is called, then 'waitForFences' will block
-- and wait until the condition is satisfied or the @timeout@ has expired,
-- whichever is sooner.
--
-- If @timeout@ is zero, then 'waitForFences' does not wait, but simply
-- returns the current state of the fences.
-- 'Vulkan.Core10.Enums.Result.TIMEOUT' will be returned in this case if
-- the condition is not satisfied, even though no actual wait was
-- performed.
--
-- If the condition is satisfied before the @timeout@ has expired,
-- 'waitForFences' returns 'Vulkan.Core10.Enums.Result.SUCCESS'. Otherwise,
-- 'waitForFences' returns 'Vulkan.Core10.Enums.Result.TIMEOUT' after the
-- @timeout@ has expired.
--
-- If device loss occurs (see
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>)
-- before the timeout has expired, 'waitForFences' /must/ return in finite
-- time with either 'Vulkan.Core10.Enums.Result.SUCCESS' or
-- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'.
--
-- Note
--
-- While we guarantee that 'waitForFences' /must/ return in finite time, no
-- guarantees are made that it returns immediately upon device loss.
-- However, the client can reasonably expect that the delay will be on the
-- order of seconds and that calling 'waitForFences' will not result in a
-- permanently (or seemingly permanently) dead process.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkWaitForFences-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkWaitForFences-pFences-parameter# @pFences@ /must/ be a valid
--     pointer to an array of @fenceCount@ valid
--     'Vulkan.Core10.Handles.Fence' handles
--
-- -   #VUID-vkWaitForFences-fenceCount-arraylength# @fenceCount@ /must/ be
--     greater than @0@
--
-- -   #VUID-vkWaitForFences-pFences-parent# Each element of @pFences@
--     /must/ have been created, allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.TIMEOUT'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.Handles.Fence'
waitForFences :: forall io
               . (MonadIO io)
              => -- | @device@ is the logical device that owns the fences.
                 Device
              -> -- | @pFences@ is a pointer to an array of @fenceCount@ fence handles.
                 ("fences" ::: Vector Fence)
              -> -- | @waitAll@ is the condition that /must/ be satisfied to successfully
                 -- unblock the wait. If @waitAll@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
                 -- then the condition is that all fences in @pFences@ are signaled.
                 -- Otherwise, the condition is that at least one fence in @pFences@ is
                 -- signaled.
                 ("waitAll" ::: Bool)
              -> -- | @timeout@ is the timeout period in units of nanoseconds. @timeout@ is
                 -- adjusted to the closest value allowed by the implementation-dependent
                 -- timeout accuracy, which /may/ be substantially longer than one
                 -- nanosecond, and /may/ be longer than the requested period.
                 ("timeout" ::: Word64)
              -> io (Result)
waitForFences = waitForFencesSafeOrUnsafe mkVkWaitForFencesUnsafe

-- | A variant of 'waitForFences' which makes a *safe* FFI call
waitForFencesSafe :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the logical device that owns the fences.
                     Device
                  -> -- | @pFences@ is a pointer to an array of @fenceCount@ fence handles.
                     ("fences" ::: Vector Fence)
                  -> -- | @waitAll@ is the condition that /must/ be satisfied to successfully
                     -- unblock the wait. If @waitAll@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
                     -- then the condition is that all fences in @pFences@ are signaled.
                     -- Otherwise, the condition is that at least one fence in @pFences@ is
                     -- signaled.
                     ("waitAll" ::: Bool)
                  -> -- | @timeout@ is the timeout period in units of nanoseconds. @timeout@ is
                     -- adjusted to the closest value allowed by the implementation-dependent
                     -- timeout accuracy, which /may/ be substantially longer than one
                     -- nanosecond, and /may/ be longer than the requested period.
                     ("timeout" ::: Word64)
                  -> io (Result)
waitForFencesSafe = waitForFencesSafeOrUnsafe mkVkWaitForFencesSafe


-- | VkFenceCreateInfo - Structure specifying parameters of a newly created
-- fence
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkFenceCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FENCE_CREATE_INFO'
--
-- -   #VUID-VkFenceCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_fence.ExportFenceCreateInfo'
--     or
--     'Vulkan.Extensions.VK_KHR_external_fence_win32.ExportFenceWin32HandleInfoKHR'
--
-- -   #VUID-VkFenceCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkFenceCreateInfo-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core10.Enums.FenceCreateFlagBits.FenceCreateFlagBits' values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.FenceCreateFlagBits.FenceCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createFence'
data FenceCreateInfo (es :: [Type]) = FenceCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.FenceCreateFlagBits.FenceCreateFlagBits' specifying
    -- the initial state and behavior of the fence.
    flags :: FenceCreateFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FenceCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (FenceCreateInfo es)

instance Extensible FenceCreateInfo where
  extensibleTypeName = "FenceCreateInfo"
  setNext FenceCreateInfo{..} next' = FenceCreateInfo{next = next', ..}
  getNext FenceCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends FenceCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ExportFenceWin32HandleInfoKHR = Just f
    | Just Refl <- eqT @e @ExportFenceCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss FenceCreateInfo es, PokeChain es) => ToCStruct (FenceCreateInfo es) where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FenceCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FENCE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr FenceCreateFlags)) (flags)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FENCE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss FenceCreateInfo es, PeekChain es) => FromCStruct (FenceCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @FenceCreateFlags ((p `plusPtr` 16 :: Ptr FenceCreateFlags))
    pure $ FenceCreateInfo
             next flags

instance es ~ '[] => Zero (FenceCreateInfo es) where
  zero = FenceCreateInfo
           ()
           zero

