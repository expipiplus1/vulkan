{-# language CPP #-}
module Graphics.Vulkan.Core10.Buffer  ( createBuffer
                                      , withBuffer
                                      , destroyBuffer
                                      , BufferCreateInfo(..)
                                      ) where

import Control.Exception.Base (bracket)
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
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.Core10.Handles (Buffer(..))
import Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address (BufferDeviceAddressCreateInfoEXT)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferOpaqueCaptureAddressCreateInfo)
import Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Graphics.Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationBufferCreateInfoNV)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateBuffer))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyBuffer))
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryBufferCreateInfo)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.SharingMode (SharingMode)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateBuffer
  :: FunPtr (Ptr Device_T -> Ptr (BufferCreateInfo a) -> Ptr AllocationCallbacks -> Ptr Buffer -> IO Result) -> Ptr Device_T -> Ptr (BufferCreateInfo a) -> Ptr AllocationCallbacks -> Ptr Buffer -> IO Result

-- | vkCreateBuffer - Create a new buffer object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     creates the buffer object.
--
-- -   @pCreateInfo@ is a pointer to a 'BufferCreateInfo' structure
--     containing parameters affecting creation of the buffer.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pBuffer@ is a pointer to a 'Graphics.Vulkan.Core10.Handles.Buffer'
--     handle in which the resulting buffer object is returned.
--
-- == Valid Usage
--
-- -   If the 'Graphics.Vulkan.Core10.BaseType.Flags' member of
--     @pCreateInfo@ includes
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT',
--     creating this 'Graphics.Vulkan.Core10.Handles.Buffer' /must/ not
--     cause the total required sparse memory for all currently valid
--     sparse resources on the device to exceed
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@sparseAddressSpaceSize@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'BufferCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pBuffer@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
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
--     -   'Graphics.Vulkan.Extensions.VK_KHR_buffer_device_address.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Buffer', 'BufferCreateInfo',
-- 'Graphics.Vulkan.Core10.Handles.Device'
createBuffer :: PokeChain a => Device -> BufferCreateInfo a -> ("allocator" ::: Maybe AllocationCallbacks) -> IO (Buffer)
createBuffer device createInfo allocator = evalContT $ do
  let vkCreateBuffer' = mkVkCreateBuffer (pVkCreateBuffer (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPBuffer <- ContT $ bracket (callocBytes @Buffer 8) free
  r <- lift $ vkCreateBuffer' (deviceHandle (device)) pCreateInfo pAllocator (pPBuffer)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pBuffer <- lift $ peek @Buffer pPBuffer
  pure $ (pBuffer)

-- | A safe wrapper for 'createBuffer' and 'destroyBuffer' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withBuffer :: PokeChain a => Device -> BufferCreateInfo a -> Maybe AllocationCallbacks -> ((Buffer) -> IO r) -> IO r
withBuffer device pCreateInfo pAllocator =
  bracket
    (createBuffer device pCreateInfo pAllocator)
    (\(o0) -> destroyBuffer device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyBuffer
  :: FunPtr (Ptr Device_T -> Buffer -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Buffer -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyBuffer - Destroy a buffer object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     destroys the buffer.
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' is the buffer to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to
--     'Graphics.Vulkan.Core10.Handles.Buffer', either directly or via a
--     'Graphics.Vulkan.Core10.Handles.BufferView', /must/ have completed
--     execution
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when 'Graphics.Vulkan.Core10.Handles.Buffer' was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when 'Graphics.Vulkan.Core10.Handles.Buffer' was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' is a valid handle, it
--     /must/ have been created, allocated, or retrieved from
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be
--     externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.Device'
destroyBuffer :: Device -> Buffer -> ("allocator" ::: Maybe AllocationCallbacks) -> IO ()
destroyBuffer device buffer allocator = evalContT $ do
  let vkDestroyBuffer' = mkVkDestroyBuffer (pVkDestroyBuffer (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyBuffer' (deviceHandle (device)) (buffer) pAllocator
  pure $ ()


-- | VkBufferCreateInfo - Structure specifying the parameters of a newly
-- created buffer object
--
-- == Valid Usage
--
-- -   @size@ /must/ be greater than @0@
--
-- -   If 'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode' is
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values
--
-- -   If 'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode' is
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @queueFamilyIndexCount@ /must/ be greater than @1@
--
-- -   If 'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode' is
--     'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     each element of @pQueueFamilyIndices@ /must/ be unique and /must/ be
--     less than @pQueueFamilyPropertyCount@ returned by either
--     'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     or
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2'
--     for the 'Graphics.Vulkan.Core10.Handles.PhysicalDevice' that was
--     used to create 'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseBinding sparse bindings>
--     feature is not enabled, 'Graphics.Vulkan.Core10.BaseType.Flags'
--     /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidencyBuffer sparse buffer residency>
--     feature is not enabled, 'Graphics.Vulkan.Core10.BaseType.Flags'
--     /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseResidencyAliased sparse aliased residency>
--     feature is not enabled, 'Graphics.Vulkan.Core10.BaseType.Flags'
--     /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_ALIASED_BIT'
--
-- -   If 'Graphics.Vulkan.Core10.BaseType.Flags' contains
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_ALIASED_BIT',
--     it /must/ also contain
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT'
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'
--     structure, its @handleTypes@ member /must/ only contain bits that
--     are also in
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalBufferProperties'::@externalMemoryProperties.compatibleHandleTypes@,
--     as returned by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.getPhysicalDeviceExternalBufferProperties'
--     with @pExternalBufferInfo->handleType@ equal to any one of the
--     handle types specified in
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'::@handleTypes@
--
-- -   If the protected memory feature is not enabled,
--     'Graphics.Vulkan.Core10.BaseType.Flags' /must/ not contain
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--
-- -   If any of the bits
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT',
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_ALIASED_BIT'
--     are set,
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--     /must/ not also be set
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationBufferCreateInfoNV'
--     structure, and the @dedicatedAllocation@ member of the chained
--     structure is 'Graphics.Vulkan.Core10.BaseType.TRUE', then
--     'Graphics.Vulkan.Core10.BaseType.Flags' /must/ not include
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT',
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_ALIASED_BIT'
--
-- -   If
--     'Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address.BufferDeviceAddressCreateInfoEXT'::'Graphics.Vulkan.Core10.BaseType.DeviceAddress'
--     is not zero, 'Graphics.Vulkan.Core10.BaseType.Flags' /must/ include
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
--
-- -   If
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferOpaqueCaptureAddressCreateInfo'::@opaqueCaptureAddress@
--     is not zero, 'Graphics.Vulkan.Core10.BaseType.Flags' /must/ include
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
--
-- -   If 'Graphics.Vulkan.Core10.BaseType.Flags' includes
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddressCaptureReplay bufferDeviceAddressCaptureReplay>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddressCaptureReplayEXT ::bufferDeviceAddressCaptureReplay>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address.BufferDeviceAddressCreateInfoEXT',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferOpaqueCaptureAddressCreateInfo',
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationBufferCreateInfoNV',
--     or
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be a valid
--     combination of
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlagBits'
--     values
--
-- -   @usage@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits'
--     values
--
-- -   @usage@ /must/ not be @0@
--
-- -   'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode' /must/ be a
--     valid 'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode' value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlags',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createBuffer'
data BufferCreateInfo (es :: [Type]) = BufferCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | 'Graphics.Vulkan.Core10.BaseType.Flags' is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlagBits'
    -- specifying additional parameters of the buffer.
    flags :: BufferCreateFlags
  , -- | @size@ is the size in bytes of the buffer to be created.
    size :: DeviceSize
  , -- | @usage@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits'
    -- specifying allowed usages of the buffer.
    usage :: BufferUsageFlags
  , -- | 'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode' is a
    -- 'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode' value specifying
    -- the sharing mode of the buffer when it will be accessed by multiple
    -- queue families.
    sharingMode :: SharingMode
  , -- | @pQueueFamilyIndices@ is a list of queue families that will access this
    -- buffer (ignored if
    -- 'Graphics.Vulkan.Core10.Enums.SharingMode.SharingMode' is not
    -- 'Graphics.Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT').
    queueFamilyIndices :: Vector Word32
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (BufferCreateInfo es)

instance Extensible BufferCreateInfo where
  extensibleType = STRUCTURE_TYPE_BUFFER_CREATE_INFO
  setNext x next = x{next = next}
  getNext BufferCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BufferCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @BufferDeviceAddressCreateInfoEXT = Just f
    | Just Refl <- eqT @e @BufferOpaqueCaptureAddressCreateInfo = Just f
    | Just Refl <- eqT @e @ExternalMemoryBufferCreateInfo = Just f
    | Just Refl <- eqT @e @DedicatedAllocationBufferCreateInfoNV = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (BufferCreateInfo es) where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr BufferCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (size)
    lift $ poke ((p `plusPtr` 32 :: Ptr BufferUsageFlags)) (usage)
    lift $ poke ((p `plusPtr` 36 :: Ptr SharingMode)) (sharingMode)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueFamilyIndices)) :: Word32))
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (queueFamilyIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (queueFamilyIndices)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr BufferUsageFlags)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr SharingMode)) (zero)
    pPQueueFamilyIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ f

instance PeekChain es => FromCStruct (BufferCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @BufferCreateFlags ((p `plusPtr` 16 :: Ptr BufferCreateFlags))
    size <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    usage <- peek @BufferUsageFlags ((p `plusPtr` 32 :: Ptr BufferUsageFlags))
    sharingMode <- peek @SharingMode ((p `plusPtr` 36 :: Ptr SharingMode))
    queueFamilyIndexCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pQueueFamilyIndices <- peek @(Ptr Word32) ((p `plusPtr` 48 :: Ptr (Ptr Word32)))
    pQueueFamilyIndices' <- generateM (fromIntegral queueFamilyIndexCount) (\i -> peek @Word32 ((pQueueFamilyIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ BufferCreateInfo
             next flags size usage sharingMode pQueueFamilyIndices'

instance es ~ '[] => Zero (BufferCreateInfo es) where
  zero = BufferCreateInfo
           ()
           zero
           zero
           zero
           zero
           mempty

