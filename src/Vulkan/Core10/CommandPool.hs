{-# language CPP #-}
-- No documentation found for Chapter "CommandPool"
module Vulkan.Core10.CommandPool  ( createCommandPool
                                  , withCommandPool
                                  , destroyCommandPool
                                  , resetCommandPool
                                  , CommandPoolCreateInfo(..)
                                  , CommandPool(..)
                                  , CommandPoolCreateFlagBits(..)
                                  , CommandPoolCreateFlags
                                  , CommandPoolResetFlagBits(..)
                                  , CommandPoolResetFlags
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
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandPool)
import Vulkan.Core10.Handles (CommandPool(..))
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits (CommandPoolCreateFlags)
import {-# SOURCE #-} Vulkan.Core10.StaticMemoryFunctionality (CommandPoolMemoryReservationCreateInfo)
import Vulkan.Core10.Enums.CommandPoolResetFlagBits (CommandPoolResetFlagBits(..))
import Vulkan.Core10.Enums.CommandPoolResetFlagBits (CommandPoolResetFlags)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateCommandPool))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyCommandPool))
import Vulkan.Dynamic (DeviceCmds(pVkResetCommandPool))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (CommandPool(..))
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits (CommandPoolCreateFlagBits(..))
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits (CommandPoolCreateFlags)
import Vulkan.Core10.Enums.CommandPoolResetFlagBits (CommandPoolResetFlagBits(..))
import Vulkan.Core10.Enums.CommandPoolResetFlagBits (CommandPoolResetFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateCommandPool
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct CommandPoolCreateInfo) -> Ptr AllocationCallbacks -> Ptr CommandPool -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct CommandPoolCreateInfo) -> Ptr AllocationCallbacks -> Ptr CommandPool -> IO Result

-- | vkCreateCommandPool - Create a new command pool object
--
-- == Valid Usage
--
-- -   #VUID-vkCreateCommandPool-queueFamilyIndex-01937#
--     @pCreateInfo->queueFamilyIndex@ /must/ be the index of a queue
--     family available in the logical device @device@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateCommandPool-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateCommandPool-pCreateInfo-parameter# @pCreateInfo@
--     /must/ be a valid pointer to a valid 'CommandPoolCreateInfo'
--     structure
--
-- -   #VUID-vkCreateCommandPool-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateCommandPool-pCommandPool-parameter# @pCommandPool@
--     /must/ be a valid pointer to a 'Vulkan.Core10.Handles.CommandPool'
--     handle
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
-- 'Vulkan.Core10.Handles.CommandPool', 'CommandPoolCreateInfo',
-- 'Vulkan.Core10.Handles.Device'
createCommandPool :: forall a io
                   . (Extendss CommandPoolCreateInfo a, PokeChain a, MonadIO io)
                  => -- | @device@ is the logical device that creates the command pool.
                     Device
                  -> -- | @pCreateInfo@ is a pointer to a 'CommandPoolCreateInfo' structure
                     -- specifying the state of the command pool object.
                     (CommandPoolCreateInfo a)
                  -> -- | @pAllocator@ controls host memory allocation as described in the
                     -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                     -- chapter.
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io (CommandPool)
createCommandPool device createInfo allocator = liftIO . evalContT $ do
  let vkCreateCommandPoolPtr = pVkCreateCommandPool (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateCommandPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateCommandPool is null" Nothing Nothing
  let vkCreateCommandPool' = mkVkCreateCommandPool vkCreateCommandPoolPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPCommandPool <- ContT $ bracket (callocBytes @CommandPool 8) free
  r <- lift $ traceAroundEvent "vkCreateCommandPool" (vkCreateCommandPool'
                                                        (deviceHandle (device))
                                                        (forgetExtensions pCreateInfo)
                                                        pAllocator
                                                        (pPCommandPool))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCommandPool <- lift $ peek @CommandPool pPCommandPool
  pure $ (pCommandPool)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createCommandPool' and 'destroyCommandPool'
--
-- To ensure that 'destroyCommandPool' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withCommandPool :: forall a io r . (Extendss CommandPoolCreateInfo a, PokeChain a, MonadIO io) => Device -> CommandPoolCreateInfo a -> Maybe AllocationCallbacks -> (io CommandPool -> (CommandPool -> io ()) -> r) -> r
withCommandPool device pCreateInfo pAllocator b =
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
-- = Description
--
-- When a pool is destroyed, all command buffers allocated from the pool
-- are <vkFreeCommandBuffers.html freed>.
--
-- Any primary command buffer allocated from another
-- 'Vulkan.Core10.Handles.CommandPool' that is in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- and has a secondary command buffer allocated from @commandPool@ recorded
-- into it, becomes
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyCommandPool-commandPool-00041# All
--     'Vulkan.Core10.Handles.CommandBuffer' objects allocated from
--     @commandPool@ /must/ not be in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   #VUID-vkDestroyCommandPool-commandPool-00042# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @commandPool@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyCommandPool-commandPool-00043# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @commandPool@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyCommandPool-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyCommandPool-commandPool-parameter# If @commandPool@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @commandPool@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandPool' handle
--
-- -   #VUID-vkDestroyCommandPool-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyCommandPool-commandPool-parent# If @commandPool@ is a
--     valid handle, it /must/ have been created, allocated, or retrieved
--     from @device@
--
-- == Host Synchronization
--
-- -   Host access to @commandPool@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.CommandPool', 'Vulkan.Core10.Handles.Device'
destroyCommandPool :: forall io
                    . (MonadIO io)
                   => -- | @device@ is the logical device that destroys the command pool.
                      Device
                   -> -- | @commandPool@ is the handle of the command pool to destroy.
                      CommandPool
                   -> -- | @pAllocator@ controls host memory allocation as described in the
                      -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                      -- chapter.
                      ("allocator" ::: Maybe AllocationCallbacks)
                   -> io ()
destroyCommandPool device commandPool allocator = liftIO . evalContT $ do
  let vkDestroyCommandPoolPtr = pVkDestroyCommandPool (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyCommandPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyCommandPool is null" Nothing Nothing
  let vkDestroyCommandPool' = mkVkDestroyCommandPool vkDestroyCommandPoolPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyCommandPool" (vkDestroyCommandPool'
                                                    (deviceHandle (device))
                                                    (commandPool)
                                                    pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetCommandPool
  :: FunPtr (Ptr Device_T -> CommandPool -> CommandPoolResetFlags -> IO Result) -> Ptr Device_T -> CommandPool -> CommandPoolResetFlags -> IO Result

-- | vkResetCommandPool - Reset a command pool
--
-- = Description
--
-- Resetting a command pool recycles all of the resources from all of the
-- command buffers allocated from the command pool back to the command
-- pool. All command buffers that have been allocated from the command pool
-- are put in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle initial state>.
--
-- Any primary command buffer allocated from another
-- 'Vulkan.Core10.Handles.CommandPool' that is in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>
-- and has a secondary command buffer allocated from @commandPool@ recorded
-- into it, becomes
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   #VUID-vkResetCommandPool-commandPool-00040# All
--     'Vulkan.Core10.Handles.CommandBuffer' objects allocated from
--     @commandPool@ /must/ not be in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkResetCommandPool-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkResetCommandPool-commandPool-parameter# @commandPool@ /must/
--     be a valid 'Vulkan.Core10.Handles.CommandPool' handle
--
-- -   #VUID-vkResetCommandPool-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core10.Enums.CommandPoolResetFlagBits.CommandPoolResetFlagBits'
--     values
--
-- -   #VUID-vkResetCommandPool-commandPool-parent# @commandPool@ /must/
--     have been created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @commandPool@ /must/ be externally synchronized
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
-- 'Vulkan.Core10.Handles.CommandPool',
-- 'Vulkan.Core10.Enums.CommandPoolResetFlagBits.CommandPoolResetFlags',
-- 'Vulkan.Core10.Handles.Device'
resetCommandPool :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the logical device that owns the command pool.
                    Device
                 -> -- | @commandPool@ is the command pool to reset.
                    CommandPool
                 -> -- | @flags@ is a bitmask of
                    -- 'Vulkan.Core10.Enums.CommandPoolResetFlagBits.CommandPoolResetFlagBits'
                    -- controlling the reset operation.
                    CommandPoolResetFlags
                 -> io ()
resetCommandPool device commandPool flags = liftIO $ do
  let vkResetCommandPoolPtr = pVkResetCommandPool (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkResetCommandPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetCommandPool is null" Nothing Nothing
  let vkResetCommandPool' = mkVkResetCommandPool vkResetCommandPoolPtr
  r <- traceAroundEvent "vkResetCommandPool" (vkResetCommandPool'
                                                (deviceHandle (device))
                                                (commandPool)
                                                (flags))
  when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkCommandPoolCreateInfo - Structure specifying parameters of a newly
-- created command pool
--
-- == Valid Usage
--
-- -   #VUID-VkCommandPoolCreateInfo-flags-02860# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-protectedMemory protectedMemory>
--     feature is not enabled, the
--     'Vulkan.Core10.Enums.CommandPoolCreateFlagBits.COMMAND_POOL_CREATE_PROTECTED_BIT'
--     bit of @flags@ /must/ not be set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCommandPoolCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO'
--
-- -   #VUID-VkCommandPoolCreateInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkCommandPoolCreateInfo-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.CommandPoolCreateFlagBits.CommandPoolCreateFlagBits'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.CommandPoolCreateFlagBits.CommandPoolCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createCommandPool'
data CommandPoolCreateInfo (es :: [Type]) = CommandPoolCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.CommandPoolCreateFlagBits.CommandPoolCreateFlagBits'
    -- indicating usage behavior for the pool and command buffers allocated
    -- from it.
    flags :: CommandPoolCreateFlags
  , -- | @queueFamilyIndex@ designates a queue family as described in section
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-queueprops Queue Family Properties>.
    -- All command buffers allocated from this command pool /must/ be submitted
    -- on queues from the same queue family.
    queueFamilyIndex :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CommandPoolCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (CommandPoolCreateInfo es)

instance Extensible CommandPoolCreateInfo where
  extensibleTypeName = "CommandPoolCreateInfo"
  setNext CommandPoolCreateInfo{..} next' = CommandPoolCreateInfo{next = next', ..}
  getNext CommandPoolCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends CommandPoolCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @CommandPoolMemoryReservationCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss CommandPoolCreateInfo es
         , PokeChain es ) => ToCStruct (CommandPoolCreateInfo es) where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CommandPoolCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr CommandPoolCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (queueFamilyIndex)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ f

instance ( Extendss CommandPoolCreateInfo es
         , PeekChain es ) => FromCStruct (CommandPoolCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @CommandPoolCreateFlags ((p `plusPtr` 16 :: Ptr CommandPoolCreateFlags))
    queueFamilyIndex <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ CommandPoolCreateInfo
             next flags queueFamilyIndex

instance es ~ '[] => Zero (CommandPoolCreateInfo es) where
  zero = CommandPoolCreateInfo
           ()
           zero
           zero

