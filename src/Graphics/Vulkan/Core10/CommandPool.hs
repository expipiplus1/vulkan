{-# language CPP #-}
module Graphics.Vulkan.Core10.CommandPool  ( createCommandPool
                                           , withCommandPool
                                           , destroyCommandPool
                                           , resetCommandPool
                                           , CommandPoolCreateInfo(..)
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
import Graphics.Vulkan.Core10.Handles (CommandPool)
import Graphics.Vulkan.Core10.Handles (CommandPool(..))
import Graphics.Vulkan.Core10.Enums.CommandPoolCreateFlagBits (CommandPoolCreateFlags)
import Graphics.Vulkan.Core10.Enums.CommandPoolResetFlagBits (CommandPoolResetFlagBits(..))
import Graphics.Vulkan.Core10.Enums.CommandPoolResetFlagBits (CommandPoolResetFlags)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateCommandPool))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyCommandPool))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkResetCommandPool))
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
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateCommandPool
  :: FunPtr (Ptr Device_T -> Ptr CommandPoolCreateInfo -> Ptr AllocationCallbacks -> Ptr CommandPool -> IO Result) -> Ptr Device_T -> Ptr CommandPoolCreateInfo -> Ptr AllocationCallbacks -> Ptr CommandPool -> IO Result

-- | vkCreateCommandPool - Create a new command pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the command pool.
--
-- -   @pCreateInfo@ is a pointer to a 'CommandPoolCreateInfo' structure
--     specifying the state of the command pool object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pCommandPool@ is a pointer to a
--     'Graphics.Vulkan.Core10.Handles.CommandPool' handle in which the
--     created pool is returned.
--
-- == Valid Usage
--
-- -   @pCreateInfo->queueFamilyIndex@ /must/ be the index of a queue
--     family available in the logical device @device@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'CommandPoolCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pCommandPool@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core10.Handles.CommandPool' handle
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
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.CommandPool', 'CommandPoolCreateInfo',
-- 'Graphics.Vulkan.Core10.Handles.Device'
createCommandPool :: forall io . MonadIO io => Device -> CommandPoolCreateInfo -> ("allocator" ::: Maybe AllocationCallbacks) -> io (CommandPool)
createCommandPool device createInfo allocator = liftIO . evalContT $ do
  let vkCreateCommandPool' = mkVkCreateCommandPool (pVkCreateCommandPool (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPCommandPool <- ContT $ bracket (callocBytes @CommandPool 8) free
  r <- lift $ vkCreateCommandPool' (deviceHandle (device)) pCreateInfo pAllocator (pPCommandPool)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCommandPool <- lift $ peek @CommandPool pPCommandPool
  pure $ (pCommandPool)

-- | A convenience wrapper to make a compatible pair of 'createCommandPool'
-- and 'destroyCommandPool'
--
-- To ensure that 'destroyCommandPool' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withCommandPool :: forall io r . MonadIO io => (io (CommandPool) -> ((CommandPool) -> io ()) -> r) -> Device -> CommandPoolCreateInfo -> Maybe AllocationCallbacks -> r
withCommandPool b device pCreateInfo pAllocator =
  b (createCommandPool device pCreateInfo pAllocator)
    (\(o0) -> destroyCommandPool device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyCommandPool
  :: FunPtr (Ptr Device_T -> CommandPool -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> CommandPool -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyCommandPool - Destroy a command pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the command pool.
--
-- -   @commandPool@ is the handle of the command pool to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- = Description
--
-- When a pool is destroyed, all command buffers allocated from the pool
-- are
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkFreeCommandBuffers freed>.
--
-- Any primary command buffer allocated from another
-- 'Graphics.Vulkan.Core10.Handles.CommandPool' that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- and has a secondary command buffer allocated from @commandPool@ recorded
-- into it, becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   All 'Graphics.Vulkan.Core10.Handles.CommandBuffer' objects allocated
--     from @commandPool@ /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @commandPool@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @commandPool@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   If @commandPool@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @commandPool@
--     /must/ be a valid 'Graphics.Vulkan.Core10.Handles.CommandPool'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   If @commandPool@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @commandPool@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.CommandPool',
-- 'Graphics.Vulkan.Core10.Handles.Device'
destroyCommandPool :: forall io . MonadIO io => Device -> CommandPool -> ("allocator" ::: Maybe AllocationCallbacks) -> io ()
destroyCommandPool device commandPool allocator = liftIO . evalContT $ do
  let vkDestroyCommandPool' = mkVkDestroyCommandPool (pVkDestroyCommandPool (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyCommandPool' (deviceHandle (device)) (commandPool) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetCommandPool
  :: FunPtr (Ptr Device_T -> CommandPool -> CommandPoolResetFlags -> IO Result) -> Ptr Device_T -> CommandPool -> CommandPoolResetFlags -> IO Result

-- | vkResetCommandPool - Reset a command pool
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the command pool.
--
-- -   @commandPool@ is the command pool to reset.
--
-- -   @flags@ is a bitmask of
--     'Graphics.Vulkan.Core10.Enums.CommandPoolResetFlagBits.CommandPoolResetFlagBits'
--     controlling the reset operation.
--
-- = Description
--
-- Resetting a command pool recycles all of the resources from all of the
-- command buffers allocated from the command pool back to the command
-- pool. All command buffers that have been allocated from the command pool
-- are put in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
--
-- Any primary command buffer allocated from another
-- 'Graphics.Vulkan.Core10.Handles.CommandPool' that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- and has a secondary command buffer allocated from @commandPool@ recorded
-- into it, becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   All 'Graphics.Vulkan.Core10.Handles.CommandBuffer' objects allocated
--     from @commandPool@ /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @commandPool@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.CommandPool' handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.CommandPoolResetFlagBits.CommandPoolResetFlagBits'
--     values
--
-- -   @commandPool@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @commandPool@ /must/ be externally synchronized
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
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.CommandPool',
-- 'Graphics.Vulkan.Core10.Enums.CommandPoolResetFlagBits.CommandPoolResetFlags',
-- 'Graphics.Vulkan.Core10.Handles.Device'
resetCommandPool :: forall io . MonadIO io => Device -> CommandPool -> CommandPoolResetFlags -> io ()
resetCommandPool device commandPool flags = liftIO $ do
  let vkResetCommandPool' = mkVkResetCommandPool (pVkResetCommandPool (deviceCmds (device :: Device)))
  r <- vkResetCommandPool' (deviceHandle (device)) (commandPool) (flags)
  when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkCommandPoolCreateInfo - Structure specifying parameters of a newly
-- created command pool
--
-- == Valid Usage
--
-- -   If the protected memory feature is not enabled, the
--     'Graphics.Vulkan.Core10.Enums.CommandPoolCreateFlagBits.COMMAND_POOL_CREATE_PROTECTED_BIT'
--     bit of @flags@ /must/ not be set.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.CommandPoolCreateFlagBits.CommandPoolCreateFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.CommandPoolCreateFlagBits.CommandPoolCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createCommandPool'
data CommandPoolCreateInfo = CommandPoolCreateInfo
  { -- | @flags@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.CommandPoolCreateFlagBits.CommandPoolCreateFlagBits'
    -- indicating usage behavior for the pool and command buffers allocated
    -- from it.
    flags :: CommandPoolCreateFlags
  , -- | @queueFamilyIndex@ designates a queue family as described in section
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-queueprops Queue Family Properties>.
    -- All command buffers allocated from this command pool /must/ be submitted
    -- on queues from the same queue family.
    queueFamilyIndex :: Word32
  }
  deriving (Typeable)
deriving instance Show CommandPoolCreateInfo

instance ToCStruct CommandPoolCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandPoolCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CommandPoolCreateFlags)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (queueFamilyIndex)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct CommandPoolCreateInfo where
  peekCStruct p = do
    flags <- peek @CommandPoolCreateFlags ((p `plusPtr` 16 :: Ptr CommandPoolCreateFlags))
    queueFamilyIndex <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ CommandPoolCreateInfo
             flags queueFamilyIndex

instance Storable CommandPoolCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CommandPoolCreateInfo where
  zero = CommandPoolCreateInfo
           zero
           zero

