{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_maintenance1"
module Vulkan.Core11.Promoted_From_VK_KHR_maintenance1  ( trimCommandPool
                                                        , CommandPoolTrimFlags(..)
                                                        , Result(..)
                                                        , ImageCreateFlagBits(..)
                                                        , ImageCreateFlags
                                                        , FormatFeatureFlagBits(..)
                                                        , FormatFeatureFlags
                                                        ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.IO.Class (MonadIO)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Vulkan.Core10.Handles (CommandPool)
import Vulkan.Core10.Handles (CommandPool(..))
import Vulkan.Core11.Enums.CommandPoolTrimFlags (CommandPoolTrimFlags)
import Vulkan.Core11.Enums.CommandPoolTrimFlags (CommandPoolTrimFlags(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkTrimCommandPool))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.CommandPoolTrimFlags (CommandPoolTrimFlags(..))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlagBits(..))
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.Result (Result(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkTrimCommandPool
  :: FunPtr (Ptr Device_T -> CommandPool -> CommandPoolTrimFlags -> IO ()) -> Ptr Device_T -> CommandPool -> CommandPoolTrimFlags -> IO ()

-- | vkTrimCommandPool - Trim a command pool
--
-- = Description
--
-- Trimming a command pool recycles unused memory from the command pool
-- back to the system. Command buffers allocated from the pool are not
-- affected by the command.
--
-- Note
--
-- This command provides applications with some control over the internal
-- memory allocations used by command pools.
--
-- Unused memory normally arises from command buffers that have been
-- recorded and later reset, such that they are no longer using the memory.
-- On reset, a command buffer can return memory to its command pool, but
-- the only way to release memory from a command pool to the system
-- requires calling 'Vulkan.Core10.CommandPool.resetCommandPool', which
-- cannot be executed while any command buffers from that pool are still in
-- use. Subsequent recording operations into command buffers will re-use
-- this memory but since total memory requirements fluctuate over time,
-- unused memory can accumulate.
--
-- In this situation, trimming a command pool /may/ be useful to return
-- unused memory back to the system, returning the total outstanding memory
-- allocated by the pool back to a more “average” value.
--
-- Implementations utilize many internal allocation strategies that make it
-- impossible to guarantee that all unused memory is released back to the
-- system. For instance, an implementation of a command pool /may/ involve
-- allocating memory in bulk from the system and sub-allocating from that
-- memory. In such an implementation any live command buffer that holds a
-- reference to a bulk allocation would prevent that allocation from being
-- freed, even if only a small proportion of the bulk allocation is in use.
--
-- In most cases trimming will result in a reduction in allocated but
-- unused memory, but it does not guarantee the “ideal” behavior.
--
-- Trimming /may/ be an expensive operation, and /should/ not be called
-- frequently. Trimming /should/ be treated as a way to relieve memory
-- pressure after application-known points when there exists enough unused
-- memory that the cost of trimming is “worth” it.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkTrimCommandPool-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkTrimCommandPool-commandPool-parameter# @commandPool@ /must/
--     be a valid 'Vulkan.Core10.Handles.CommandPool' handle
--
-- -   #VUID-vkTrimCommandPool-flags-zerobitmask# @flags@ /must/ be @0@
--
-- -   #VUID-vkTrimCommandPool-commandPool-parent# @commandPool@ /must/
--     have been created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @commandPool@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandPool',
-- 'Vulkan.Core11.Enums.CommandPoolTrimFlags.CommandPoolTrimFlags',
-- 'Vulkan.Core10.Handles.Device'
trimCommandPool :: forall io
                 . (MonadIO io)
                => -- | @device@ is the logical device that owns the command pool.
                   Device
                -> -- | @commandPool@ is the command pool to trim.
                   CommandPool
                -> -- | @flags@ is reserved for future use.
                   CommandPoolTrimFlags
                -> io ()
trimCommandPool device commandPool flags = liftIO $ do
  let vkTrimCommandPoolPtr = pVkTrimCommandPool (deviceCmds (device :: Device))
  unless (vkTrimCommandPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkTrimCommandPool is null" Nothing Nothing
  let vkTrimCommandPool' = mkVkTrimCommandPool vkTrimCommandPoolPtr
  traceAroundEvent "vkTrimCommandPool" (vkTrimCommandPool' (deviceHandle (device)) (commandPool) (flags))
  pure $ ()

