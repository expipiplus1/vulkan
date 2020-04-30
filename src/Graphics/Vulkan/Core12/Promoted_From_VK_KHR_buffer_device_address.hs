{-# language CPP #-}
module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address  ( getBufferOpaqueCaptureAddress
                                                                          , getBufferDeviceAddress
                                                                          , getDeviceMemoryOpaqueCaptureAddress
                                                                          , PhysicalDeviceBufferDeviceAddressFeatures(..)
                                                                          , BufferDeviceAddressInfo(..)
                                                                          , BufferOpaqueCaptureAddressCreateInfo(..)
                                                                          , MemoryOpaqueCaptureAddressAllocateInfo(..)
                                                                          , DeviceMemoryOpaqueCaptureAddressInfo(..)
                                                                          , StructureType(..)
                                                                          , Result(..)
                                                                          , BufferUsageFlagBits(..)
                                                                          , BufferUsageFlags
                                                                          , BufferCreateFlagBits(..)
                                                                          , BufferCreateFlags
                                                                          , MemoryAllocateFlagBits(..)
                                                                          , MemoryAllocateFlags
                                                                          ) where

import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Core10.BaseType (DeviceAddress)
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetBufferDeviceAddress))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetBufferOpaqueCaptureAddress))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetDeviceMemoryOpaqueCaptureAddress))
import Graphics.Vulkan.Core10.Handles (DeviceMemory)
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES))
import Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlagBits(..))
import Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlagBits(..))
import Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlagBits(..))
import Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferOpaqueCaptureAddress
  :: FunPtr (Ptr Device_T -> Ptr BufferDeviceAddressInfo -> IO Word64) -> Ptr Device_T -> Ptr BufferDeviceAddressInfo -> IO Word64

-- | vkGetBufferOpaqueCaptureAddress - Query an opaque capture address of a
-- buffer
--
-- = Parameters
--
-- -   @device@ is the logical device that the buffer was created on.
--
-- -   @pInfo@ is a pointer to a 'BufferDeviceAddressInfo' structure
--     specifying the buffer to retrieve an address for.
--
-- = Description
--
-- The 64-bit return value is an opaque capture address of the start of
-- @pInfo->buffer@.
--
-- If the buffer was created with a non-zero value of
-- 'BufferOpaqueCaptureAddressCreateInfo'::@opaqueCaptureAddress@ the
-- return value /must/ be the same address.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddress bufferDeviceAddress>
--     feature /must/ be enabled
--
-- -   If @device@ was created with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'BufferDeviceAddressInfo' structure
--
-- = See Also
--
-- 'BufferDeviceAddressInfo', 'Graphics.Vulkan.Core10.Handles.Device'
getBufferOpaqueCaptureAddress :: forall io . MonadIO io => Device -> BufferDeviceAddressInfo -> io (Word64)
getBufferOpaqueCaptureAddress device info = liftIO . evalContT $ do
  let vkGetBufferOpaqueCaptureAddress' = mkVkGetBufferOpaqueCaptureAddress (pVkGetBufferOpaqueCaptureAddress (deviceCmds (device :: Device)))
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkGetBufferOpaqueCaptureAddress' (deviceHandle (device)) pInfo
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferDeviceAddress
  :: FunPtr (Ptr Device_T -> Ptr BufferDeviceAddressInfo -> IO DeviceAddress) -> Ptr Device_T -> Ptr BufferDeviceAddressInfo -> IO DeviceAddress

-- | vkGetBufferDeviceAddress - Query an address of a buffer
--
-- = Parameters
--
-- -   @device@ is the logical device that the buffer was created on.
--
-- -   @pInfo@ is a pointer to a 'BufferDeviceAddressInfo' structure
--     specifying the buffer to retrieve an address for.
--
-- = Description
--
-- The 64-bit return value is an address of the start of @pInfo->buffer@.
-- The address range starting at this value and whose size is the size of
-- the buffer /can/ be used in a shader to access the memory bound to that
-- buffer, using the @SPV_KHR_physical_storage_buffer@ extension or the
-- equivalent @SPV_EXT_physical_storage_buffer@ extension and the
-- @PhysicalStorageBuffer@ storage class. For example, this value /can/ be
-- stored in a uniform buffer, and the shader /can/ read the value from the
-- uniform buffer and use it to do a dependent read\/write to this buffer.
-- A value of zero is reserved as a “null” pointer and /must/ not be
-- returned as a valid buffer device address. All loads, stores, and
-- atomics in a shader through @PhysicalStorageBuffer@ pointers /must/
-- access addresses in the address range of some buffer.
--
-- If the buffer was created with a non-zero value of
-- 'BufferOpaqueCaptureAddressCreateInfo'::@opaqueCaptureAddress@ or
-- 'Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address.BufferDeviceAddressCreateInfoEXT'::@deviceAddress@
-- the return value will be the same address that was returned at capture
-- time.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddress bufferDeviceAddress>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddressEXT ::bufferDeviceAddress>
--     feature /must/ be enabled
--
-- -   If @device@ was created with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDeviceEXT ::bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'BufferDeviceAddressInfo' structure
--
-- = See Also
--
-- 'BufferDeviceAddressInfo', 'Graphics.Vulkan.Core10.Handles.Device'
getBufferDeviceAddress :: forall io . MonadIO io => Device -> BufferDeviceAddressInfo -> io (DeviceAddress)
getBufferDeviceAddress device info = liftIO . evalContT $ do
  let vkGetBufferDeviceAddress' = mkVkGetBufferDeviceAddress (pVkGetBufferDeviceAddress (deviceCmds (device :: Device)))
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkGetBufferDeviceAddress' (deviceHandle (device)) pInfo
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceMemoryOpaqueCaptureAddress
  :: FunPtr (Ptr Device_T -> Ptr DeviceMemoryOpaqueCaptureAddressInfo -> IO Word64) -> Ptr Device_T -> Ptr DeviceMemoryOpaqueCaptureAddressInfo -> IO Word64

-- | vkGetDeviceMemoryOpaqueCaptureAddress - Query an opaque capture address
-- of a memory object
--
-- = Parameters
--
-- -   @device@ is the logical device that the memory object was allocated
--     on.
--
-- -   @pInfo@ is a pointer to a 'DeviceMemoryOpaqueCaptureAddressInfo'
--     structure specifying the memory object to retrieve an address for.
--
-- = Description
--
-- The 64-bit return value is an opaque address representing the start of
-- @pInfo->memory@.
--
-- If the memory object was allocated with a non-zero value of
-- 'MemoryOpaqueCaptureAddressAllocateInfo'::@opaqueCaptureAddress@, the
-- return value /must/ be the same address.
--
-- Note
--
-- The expected usage for these opaque addresses is only for trace
-- capture\/replay tools to store these addresses in a trace and
-- subsequently specify them during replay.
--
-- == Valid Usage
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddress bufferDeviceAddress>
--     feature /must/ be enabled
--
-- -   If @device@ was created with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'DeviceMemoryOpaqueCaptureAddressInfo' structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'DeviceMemoryOpaqueCaptureAddressInfo'
getDeviceMemoryOpaqueCaptureAddress :: forall io . MonadIO io => Device -> DeviceMemoryOpaqueCaptureAddressInfo -> io (Word64)
getDeviceMemoryOpaqueCaptureAddress device info = liftIO . evalContT $ do
  let vkGetDeviceMemoryOpaqueCaptureAddress' = mkVkGetDeviceMemoryOpaqueCaptureAddress (pVkGetDeviceMemoryOpaqueCaptureAddress (deviceCmds (device :: Device)))
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkGetDeviceMemoryOpaqueCaptureAddress' (deviceHandle (device)) pInfo
  pure $ (r)


-- | VkPhysicalDeviceBufferDeviceAddressFeatures - Structure describing
-- buffer address features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceBufferDeviceAddressFeatures' structure
-- describe the following features:
--
-- = Description
--
-- Note
--
-- @bufferDeviceAddressMultiDevice@ exists to allow certain legacy
-- platforms to be able to support @bufferDeviceAddress@ without needing to
-- support shared GPU virtual addresses for multi-device configurations.
--
-- See 'getBufferDeviceAddress' for more information.
--
-- If the 'PhysicalDeviceBufferDeviceAddressFeatures' structure is included
-- in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceBufferDeviceAddressFeatures' /can/ also be included in
-- the @pNext@ chain of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceBufferDeviceAddressFeatures = PhysicalDeviceBufferDeviceAddressFeatures
  { -- | @bufferDeviceAddress@ indicates that the implementation supports
    -- accessing buffer memory in shaders as storage buffers via an address
    -- queried from 'getBufferDeviceAddress'.
    bufferDeviceAddress :: Bool
  , -- | @bufferDeviceAddressCaptureReplay@ indicates that the implementation
    -- supports saving and reusing buffer and device addresses, e.g. for trace
    -- capture and replay.
    bufferDeviceAddressCaptureReplay :: Bool
  , -- | @bufferDeviceAddressMultiDevice@ indicates that the implementation
    -- supports the @bufferDeviceAddress@ and @rayTracing@ features for logical
    -- devices created with multiple physical devices. If this feature is not
    -- supported, buffer and acceleration structure addresses /must/ not be
    -- queried on a logical device created with more than one physical device.
    bufferDeviceAddressMultiDevice :: Bool
  }
  deriving (Typeable)
deriving instance Show PhysicalDeviceBufferDeviceAddressFeatures

instance ToCStruct PhysicalDeviceBufferDeviceAddressFeatures where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceBufferDeviceAddressFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (bufferDeviceAddress))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (bufferDeviceAddressCaptureReplay))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (bufferDeviceAddressMultiDevice))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceBufferDeviceAddressFeatures where
  peekCStruct p = do
    bufferDeviceAddress <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    bufferDeviceAddressCaptureReplay <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    bufferDeviceAddressMultiDevice <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceBufferDeviceAddressFeatures
             (bool32ToBool bufferDeviceAddress) (bool32ToBool bufferDeviceAddressCaptureReplay) (bool32ToBool bufferDeviceAddressMultiDevice)

instance Storable PhysicalDeviceBufferDeviceAddressFeatures where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceBufferDeviceAddressFeatures where
  zero = PhysicalDeviceBufferDeviceAddressFeatures
           zero
           zero
           zero


-- | VkBufferDeviceAddressInfo - Structure specifying the buffer to query an
-- address for
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse and was not created with the
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
--     flag, then it /must/ be bound completely and contiguously to a
--     single 'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @buffer@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @buffer@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Buffer'
--     handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getBufferDeviceAddress',
-- 'Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address.getBufferDeviceAddressEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_buffer_device_address.getBufferDeviceAddressKHR',
-- 'getBufferOpaqueCaptureAddress',
-- 'Graphics.Vulkan.Extensions.VK_KHR_buffer_device_address.getBufferOpaqueCaptureAddressKHR'
data BufferDeviceAddressInfo = BufferDeviceAddressInfo
  { -- | @buffer@ specifies the buffer whose address is being queried.
    buffer :: Buffer }
  deriving (Typeable)
deriving instance Show BufferDeviceAddressInfo

instance ToCStruct BufferDeviceAddressInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferDeviceAddressInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (buffer)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    f

instance FromCStruct BufferDeviceAddressInfo where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    pure $ BufferDeviceAddressInfo
             buffer

instance Storable BufferDeviceAddressInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferDeviceAddressInfo where
  zero = BufferDeviceAddressInfo
           zero


-- | VkBufferOpaqueCaptureAddressCreateInfo - Request a specific address for
-- a buffer
--
-- = Description
--
-- If @opaqueCaptureAddress@ is zero, no specific address is requested.
--
-- If @opaqueCaptureAddress@ is not zero, then it /should/ be an address
-- retrieved from 'getBufferOpaqueCaptureAddress' for an identically
-- created buffer on the same implementation.
--
-- If this structure is not present, it is as if @opaqueCaptureAddress@ is
-- zero.
--
-- Apps /should/ avoid creating buffers with app-provided addresses and
-- implementation-provided addresses in the same process, to reduce the
-- likelihood of
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS'
-- errors.
--
-- Note
--
-- The expected usage for this is that a trace capture\/replay tool will
-- add the
-- 'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
-- flag to all buffers that use
-- 'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT',
-- and during capture will save the queried opaque device addresses in the
-- trace. During replay, the buffers will be created specifying the
-- original address so any address values stored in the trace data will
-- remain valid.
--
-- Implementations are expected to separate such buffers in the GPU address
-- space so normal allocations will avoid using these addresses.
-- Apps\/tools should avoid mixing app-provided and implementation-provided
-- addresses for buffers created with
-- 'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT',
-- to avoid address space allocation conflicts.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data BufferOpaqueCaptureAddressCreateInfo = BufferOpaqueCaptureAddressCreateInfo
  { -- | @opaqueCaptureAddress@ is the opaque capture address requested for the
    -- buffer.
    opaqueCaptureAddress :: Word64 }
  deriving (Typeable)
deriving instance Show BufferOpaqueCaptureAddressCreateInfo

instance ToCStruct BufferOpaqueCaptureAddressCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferOpaqueCaptureAddressCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (opaqueCaptureAddress)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    f

instance FromCStruct BufferOpaqueCaptureAddressCreateInfo where
  peekCStruct p = do
    opaqueCaptureAddress <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    pure $ BufferOpaqueCaptureAddressCreateInfo
             opaqueCaptureAddress

instance Storable BufferOpaqueCaptureAddressCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferOpaqueCaptureAddressCreateInfo where
  zero = BufferOpaqueCaptureAddressCreateInfo
           zero


-- | VkMemoryOpaqueCaptureAddressAllocateInfo - Request a specific address
-- for a memory allocation
--
-- = Description
--
-- If @opaqueCaptureAddress@ is zero, no specific address is requested.
--
-- If @opaqueCaptureAddress@ is not zero, it /should/ be an address
-- retrieved from 'getDeviceMemoryOpaqueCaptureAddress' on an identically
-- created memory allocation on the same implementation.
--
-- Note
--
-- In most cases, it is expected that a non-zero @opaqueAddress@ is an
-- address retrieved from 'getDeviceMemoryOpaqueCaptureAddress' on an
-- identically created memory allocation. If this is not the case, it
-- likely that
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS'
-- errors will occur.
--
-- This is, however, not a strict requirement because trace capture\/replay
-- tools may need to adjust memory allocation parameters for imported
-- memory.
--
-- If this structure is not present, it is as if @opaqueCaptureAddress@ is
-- zero.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryOpaqueCaptureAddressAllocateInfo = MemoryOpaqueCaptureAddressAllocateInfo
  { -- | @opaqueCaptureAddress@ is the opaque capture address requested for the
    -- memory allocation.
    opaqueCaptureAddress :: Word64 }
  deriving (Typeable)
deriving instance Show MemoryOpaqueCaptureAddressAllocateInfo

instance ToCStruct MemoryOpaqueCaptureAddressAllocateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryOpaqueCaptureAddressAllocateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (opaqueCaptureAddress)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    f

instance FromCStruct MemoryOpaqueCaptureAddressAllocateInfo where
  peekCStruct p = do
    opaqueCaptureAddress <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    pure $ MemoryOpaqueCaptureAddressAllocateInfo
             opaqueCaptureAddress

instance Storable MemoryOpaqueCaptureAddressAllocateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryOpaqueCaptureAddressAllocateInfo where
  zero = MemoryOpaqueCaptureAddressAllocateInfo
           zero


-- | VkDeviceMemoryOpaqueCaptureAddressInfo - Structure specifying the memory
-- object to query an address for
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.DeviceMemory',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceMemoryOpaqueCaptureAddress',
-- 'Graphics.Vulkan.Extensions.VK_KHR_buffer_device_address.getDeviceMemoryOpaqueCaptureAddressKHR'
data DeviceMemoryOpaqueCaptureAddressInfo = DeviceMemoryOpaqueCaptureAddressInfo
  { -- | @memory@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.DeviceMemory'
    -- handle
    memory :: DeviceMemory }
  deriving (Typeable)
deriving instance Show DeviceMemoryOpaqueCaptureAddressInfo

instance ToCStruct DeviceMemoryOpaqueCaptureAddressInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceMemoryOpaqueCaptureAddressInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (zero)
    f

instance FromCStruct DeviceMemoryOpaqueCaptureAddressInfo where
  peekCStruct p = do
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    pure $ DeviceMemoryOpaqueCaptureAddressInfo
             memory

instance Storable DeviceMemoryOpaqueCaptureAddressInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceMemoryOpaqueCaptureAddressInfo where
  zero = DeviceMemoryOpaqueCaptureAddressInfo
           zero

