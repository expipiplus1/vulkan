{-# language CPP #-}
-- No documentation found for Chapter "QueueSemaphore"
module Vulkan.Core10.QueueSemaphore  ( createSemaphore
                                     , withSemaphore
                                     , destroySemaphore
                                     , SemaphoreCreateInfo(..)
                                     , Semaphore(..)
                                     , SemaphoreCreateFlags(..)
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateSemaphore))
import Vulkan.Dynamic (DeviceCmds(pVkDestroySemaphore))
import Vulkan.Core10.Handles (Device_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalObjectCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore (ExportSemaphoreCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (ExportSemaphoreWin32HandleInfoKHR)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ImportMetalSharedEventInfoEXT)
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
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Semaphore(..))
import Vulkan.Core10.Enums.SemaphoreCreateFlags (SemaphoreCreateFlags(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSemaphore
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct SemaphoreCreateInfo) -> Ptr AllocationCallbacks -> Ptr Semaphore -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct SemaphoreCreateInfo) -> Ptr AllocationCallbacks -> Ptr Semaphore -> IO Result

-- | vkCreateSemaphore - Create a new queue semaphore object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateSemaphore-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateSemaphore-pCreateInfo-parameter# @pCreateInfo@ /must/
--     be a valid pointer to a valid 'SemaphoreCreateInfo' structure
--
-- -   #VUID-vkCreateSemaphore-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateSemaphore-pSemaphore-parameter# @pSemaphore@ /must/ be
--     a valid pointer to a 'Vulkan.Core10.Handles.Semaphore' handle
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
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Semaphore',
-- 'SemaphoreCreateInfo'
createSemaphore :: forall a io
                 . (Extendss SemaphoreCreateInfo a, PokeChain a, MonadIO io)
                => -- | @device@ is the logical device that creates the semaphore.
                   Device
                -> -- | @pCreateInfo@ is a pointer to a 'SemaphoreCreateInfo' structure
                   -- containing information about how the semaphore is to be created.
                   (SemaphoreCreateInfo a)
                -> -- | @pAllocator@ controls host memory allocation as described in the
                   -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                   -- chapter.
                   ("allocator" ::: Maybe AllocationCallbacks)
                -> io (Semaphore)
createSemaphore device createInfo allocator = liftIO . evalContT $ do
  let vkCreateSemaphorePtr = pVkCreateSemaphore (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateSemaphorePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateSemaphore is null" Nothing Nothing
  let vkCreateSemaphore' = mkVkCreateSemaphore vkCreateSemaphorePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSemaphore <- ContT $ bracket (callocBytes @Semaphore 8) free
  r <- lift $ traceAroundEvent "vkCreateSemaphore" (vkCreateSemaphore' (deviceHandle (device)) (forgetExtensions pCreateInfo) pAllocator (pPSemaphore))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSemaphore <- lift $ peek @Semaphore pPSemaphore
  pure $ (pSemaphore)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSemaphore' and 'destroySemaphore'
--
-- To ensure that 'destroySemaphore' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSemaphore :: forall a io r . (Extendss SemaphoreCreateInfo a, PokeChain a, MonadIO io) => Device -> SemaphoreCreateInfo a -> Maybe AllocationCallbacks -> (io Semaphore -> (Semaphore -> io ()) -> r) -> r
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
-- == Valid Usage
--
-- -   #VUID-vkDestroySemaphore-semaphore-01137# All submitted batches that
--     refer to @semaphore@ /must/ have completed execution
--
-- -   #VUID-vkDestroySemaphore-semaphore-01138# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @semaphore@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroySemaphore-semaphore-01139# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @semaphore@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroySemaphore-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroySemaphore-semaphore-parameter# If @semaphore@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @semaphore@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Semaphore' handle
--
-- -   #VUID-vkDestroySemaphore-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroySemaphore-semaphore-parent# If @semaphore@ is a valid
--     handle, it /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @semaphore@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Semaphore'
destroySemaphore :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the logical device that destroys the semaphore.
                    Device
                 -> -- | @semaphore@ is the handle of the semaphore to destroy.
                    Semaphore
                 -> -- | @pAllocator@ controls host memory allocation as described in the
                    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                    -- chapter.
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io ()
destroySemaphore device semaphore allocator = liftIO . evalContT $ do
  let vkDestroySemaphorePtr = pVkDestroySemaphore (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroySemaphorePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroySemaphore is null" Nothing Nothing
  let vkDestroySemaphore' = mkVkDestroySemaphore vkDestroySemaphorePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroySemaphore" (vkDestroySemaphore' (deviceHandle (device)) (semaphore) pAllocator)
  pure $ ()


-- | VkSemaphoreCreateInfo - Structure specifying parameters of a newly
-- created semaphore
--
-- == Valid Usage
--
-- -   #VUID-VkSemaphoreCreateInfo-pNext-06789# If the @pNext@ chain
--     includes a
--     'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectCreateInfoEXT'
--     structure, its @exportObjectType@ member /must/ be
--     'Vulkan.Extensions.VK_EXT_metal_objects.EXPORT_METAL_OBJECT_TYPE_METAL_SHARED_EVENT_BIT_EXT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSemaphoreCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO'
--
-- -   #VUID-VkSemaphoreCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectCreateInfoEXT',
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore.ExportSemaphoreCreateInfo',
--     'Vulkan.Extensions.VK_KHR_external_semaphore_win32.ExportSemaphoreWin32HandleInfoKHR',
--     'Vulkan.Extensions.VK_EXT_metal_objects.ImportMetalSharedEventInfoEXT',
--     or
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo'
--
-- -   #VUID-VkSemaphoreCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique, with the exception of
--     structures of type
--     'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectCreateInfoEXT'
--
-- -   #VUID-VkSemaphoreCreateInfo-flags-zerobitmask# @flags@ /must/ be @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.SemaphoreCreateFlags.SemaphoreCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createSemaphore'
data SemaphoreCreateInfo (es :: [Type]) = SemaphoreCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: SemaphoreCreateFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SemaphoreCreateInfo es)

instance Extensible SemaphoreCreateInfo where
  extensibleTypeName = "SemaphoreCreateInfo"
  setNext SemaphoreCreateInfo{..} next' = SemaphoreCreateInfo{next = next', ..}
  getNext SemaphoreCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SemaphoreCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImportMetalSharedEventInfoEXT = Just f
    | Just Refl <- eqT @e @ExportMetalObjectCreateInfoEXT = Just f
    | Just Refl <- eqT @e @SemaphoreTypeCreateInfo = Just f
    | Just Refl <- eqT @e @ExportSemaphoreWin32HandleInfoKHR = Just f
    | Just Refl <- eqT @e @ExportSemaphoreCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss SemaphoreCreateInfo es, PokeChain es) => ToCStruct (SemaphoreCreateInfo es) where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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

instance (Extendss SemaphoreCreateInfo es, PeekChain es) => FromCStruct (SemaphoreCreateInfo es) where
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

