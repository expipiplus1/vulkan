{-# language CPP #-}
module Vulkan.Core10.QueueSemaphore  ( createSemaphore
                                     , withSemaphore
                                     , destroySemaphore
                                     , SemaphoreCreateInfo(..)
                                     ) where

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateSemaphore))
import Vulkan.Dynamic (DeviceCmds(pVkDestroySemaphore))
import Vulkan.Core10.Handles (Device_T)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore (ExportSemaphoreCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (ExportSemaphoreWin32HandleInfoKHR)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.Core10.Handles (Semaphore(..))
import Vulkan.Core10.Enums.SemaphoreCreateFlags (SemaphoreCreateFlags)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreTypeCreateInfo)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSemaphore
  :: FunPtr (Ptr Device_T -> Ptr (SemaphoreCreateInfo a) -> Ptr AllocationCallbacks -> Ptr Semaphore -> IO Result) -> Ptr Device_T -> Ptr (SemaphoreCreateInfo a) -> Ptr AllocationCallbacks -> Ptr Semaphore -> IO Result

-- | vkCreateSemaphore - Create a new queue semaphore object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the semaphore.
--
-- -   @pCreateInfo@ is a pointer to a 'SemaphoreCreateInfo' structure
--     containing information about how the semaphore is to be created.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pSemaphore@ is a pointer to a handle in which the resulting
--     semaphore object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'SemaphoreCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pSemaphore@ /must/ be a valid pointer to a
--     'Vulkan.Core10.Handles.Semaphore' handle
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
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Semaphore',
-- 'SemaphoreCreateInfo'
createSemaphore :: forall a io . (PokeChain a, MonadIO io) => Device -> SemaphoreCreateInfo a -> ("allocator" ::: Maybe AllocationCallbacks) -> io (Semaphore)
createSemaphore device createInfo allocator = liftIO . evalContT $ do
  let vkCreateSemaphore' = mkVkCreateSemaphore (pVkCreateSemaphore (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSemaphore <- ContT $ bracket (callocBytes @Semaphore 8) free
  r <- lift $ vkCreateSemaphore' (deviceHandle (device)) pCreateInfo pAllocator (pPSemaphore)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSemaphore <- lift $ peek @Semaphore pPSemaphore
  pure $ (pSemaphore)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSemaphore' and 'destroySemaphore'
--
-- To ensure that 'destroySemaphore' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withSemaphore :: forall a io r . (PokeChain a, MonadIO io) => Device -> SemaphoreCreateInfo a -> Maybe AllocationCallbacks -> (io (Semaphore) -> ((Semaphore) -> io ()) -> r) -> r
withSemaphore device pCreateInfo pAllocator b =
  b (createSemaphore device pCreateInfo pAllocator)
    (\(o0) -> destroySemaphore device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySemaphore
  :: FunPtr (Ptr Device_T -> Semaphore -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Semaphore -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroySemaphore - Destroy a semaphore object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the semaphore.
--
-- -   @semaphore@ is the handle of the semaphore to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted batches that refer to @semaphore@ /must/ have
--     completed execution
--
-- -   If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @semaphore@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @semaphore@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @semaphore@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @semaphore@ /must/ be a valid 'Vulkan.Core10.Handles.Semaphore'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   If @semaphore@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @semaphore@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Semaphore'
destroySemaphore :: forall io . MonadIO io => Device -> Semaphore -> ("allocator" ::: Maybe AllocationCallbacks) -> io ()
destroySemaphore device semaphore allocator = liftIO . evalContT $ do
  let vkDestroySemaphore' = mkVkDestroySemaphore (pVkDestroySemaphore (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroySemaphore' (deviceHandle (device)) (semaphore) pAllocator
  pure $ ()


-- | VkSemaphoreCreateInfo - Structure specifying parameters of a newly
-- created semaphore
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore.ExportSemaphoreCreateInfo',
--     'Vulkan.Extensions.VK_KHR_external_semaphore_win32.ExportSemaphoreWin32HandleInfoKHR',
--     or
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.SemaphoreCreateFlags.SemaphoreCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createSemaphore'
data SemaphoreCreateInfo (es :: [Type]) = SemaphoreCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: SemaphoreCreateFlags
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (SemaphoreCreateInfo es)

instance Extensible SemaphoreCreateInfo where
  extensibleType = STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
  setNext x next = x{next = next}
  getNext SemaphoreCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SemaphoreCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SemaphoreTypeCreateInfo = Just f
    | Just Refl <- eqT @e @ExportSemaphoreWin32HandleInfoKHR = Just f
    | Just Refl <- eqT @e @ExportSemaphoreCreateInfo = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (SemaphoreCreateInfo es) where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SemaphoreCreateFlags)) (flags)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance PeekChain es => FromCStruct (SemaphoreCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @SemaphoreCreateFlags ((p `plusPtr` 16 :: Ptr SemaphoreCreateFlags))
    pure $ SemaphoreCreateInfo
             next flags

instance es ~ '[] => Zero (SemaphoreCreateInfo es) where
  zero = SemaphoreCreateInfo
           ()
           zero

