{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_buffer_device_address"
module Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address  ( getBufferOpaqueCaptureAddress
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

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
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
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkGetBufferDeviceAddress))
import Vulkan.Dynamic (DeviceCmds(pVkGetBufferOpaqueCaptureAddress))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceMemoryOpaqueCaptureAddress))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES))
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlagBits(..))
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlagBits(..))
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlagBits(..))
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferOpaqueCaptureAddress
  :: FunPtr (Ptr Device_T -> Ptr BufferDeviceAddressInfo -> IO Word64) -> Ptr Device_T -> Ptr BufferDeviceAddressInfo -> IO Word64

-- | vkGetBufferOpaqueCaptureAddress - Query an opaque capture address of a
-- buffer
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
-- -   #VUID-vkGetBufferOpaqueCaptureAddress-None-03326# The
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddress bufferDeviceAddress>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetBufferOpaqueCaptureAddress-device-03327# If @device@ was
--     created with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetBufferOpaqueCaptureAddress-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetBufferOpaqueCaptureAddress-pInfo-parameter# @pInfo@
--     /must/ be a valid pointer to a valid 'BufferDeviceAddressInfo'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'BufferDeviceAddressInfo', 'Vulkan.Core10.Handles.Device'
getBufferOpaqueCaptureAddress :: forall io
                               . (MonadIO io)
                              => -- | @device@ is the logical device that the buffer was created on.
                                 Device
                              -> -- | @pInfo@ is a pointer to a 'BufferDeviceAddressInfo' structure specifying
                                 -- the buffer to retrieve an address for.
                                 BufferDeviceAddressInfo
                              -> io (Word64)
getBufferOpaqueCaptureAddress device info = liftIO . evalContT $ do
  let vkGetBufferOpaqueCaptureAddressPtr = pVkGetBufferOpaqueCaptureAddress (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetBufferOpaqueCaptureAddressPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetBufferOpaqueCaptureAddress is null" Nothing Nothing
  let vkGetBufferOpaqueCaptureAddress' = mkVkGetBufferOpaqueCaptureAddress vkGetBufferOpaqueCaptureAddressPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetBufferOpaqueCaptureAddress" (vkGetBufferOpaqueCaptureAddress' (deviceHandle (device)) pInfo)
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferDeviceAddress
  :: FunPtr (Ptr Device_T -> Ptr BufferDeviceAddressInfo -> IO DeviceAddress) -> Ptr Device_T -> Ptr BufferDeviceAddressInfo -> IO DeviceAddress

-- | vkGetBufferDeviceAddress - Query an address of a buffer
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
-- 'Vulkan.Extensions.VK_EXT_buffer_device_address.BufferDeviceAddressCreateInfoEXT'::@deviceAddress@
-- the return value will be the same address that was returned at capture
-- time.
--
-- == Valid Usage
--
-- -   #VUID-vkGetBufferDeviceAddress-bufferDeviceAddress-03324# The
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddress bufferDeviceAddress>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressEXT ::bufferDeviceAddress>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetBufferDeviceAddress-device-03325# If @device@ was created
--     with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDeviceEXT ::bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetBufferDeviceAddress-device-parameter# @device@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetBufferDeviceAddress-pInfo-parameter# @pInfo@ /must/ be a
--     valid pointer to a valid 'BufferDeviceAddressInfo' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'BufferDeviceAddressInfo', 'Vulkan.Core10.Handles.Device'
getBufferDeviceAddress :: forall io
                        . (MonadIO io)
                       => -- | @device@ is the logical device that the buffer was created on.
                          Device
                       -> -- | @pInfo@ is a pointer to a 'BufferDeviceAddressInfo' structure specifying
                          -- the buffer to retrieve an address for.
                          BufferDeviceAddressInfo
                       -> io (DeviceAddress)
getBufferDeviceAddress device info = liftIO . evalContT $ do
  let vkGetBufferDeviceAddressPtr = pVkGetBufferDeviceAddress (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetBufferDeviceAddressPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetBufferDeviceAddress is null" Nothing Nothing
  let vkGetBufferDeviceAddress' = mkVkGetBufferDeviceAddress vkGetBufferDeviceAddressPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetBufferDeviceAddress" (vkGetBufferDeviceAddress' (deviceHandle (device)) pInfo)
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
-- -   #VUID-vkGetDeviceMemoryOpaqueCaptureAddress-None-03334# The
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddress bufferDeviceAddress>
--     feature /must/ be enabled
--
-- -   #VUID-vkGetDeviceMemoryOpaqueCaptureAddress-device-03335# If
--     @device@ was created with multiple physical devices, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressMultiDevice bufferDeviceAddressMultiDevice>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDeviceMemoryOpaqueCaptureAddress-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDeviceMemoryOpaqueCaptureAddress-pInfo-parameter# @pInfo@
--     /must/ be a valid pointer to a valid
--     'DeviceMemoryOpaqueCaptureAddressInfo' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceMemoryOpaqueCaptureAddressInfo'
getDeviceMemoryOpaqueCaptureAddress :: forall io
                                     . (MonadIO io)
                                    => -- | @device@ is the logical device that the memory object was allocated on.
                                       Device
                                    -> -- | @pInfo@ is a pointer to a 'DeviceMemoryOpaqueCaptureAddressInfo'
                                       -- structure specifying the memory object to retrieve an address for.
                                       DeviceMemoryOpaqueCaptureAddressInfo
                                    -> io (Word64)
getDeviceMemoryOpaqueCaptureAddress device info = liftIO . evalContT $ do
  let vkGetDeviceMemoryOpaqueCaptureAddressPtr = pVkGetDeviceMemoryOpaqueCaptureAddress (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceMemoryOpaqueCaptureAddressPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceMemoryOpaqueCaptureAddress is null" Nothing Nothing
  let vkGetDeviceMemoryOpaqueCaptureAddress' = mkVkGetDeviceMemoryOpaqueCaptureAddress vkGetDeviceMemoryOpaqueCaptureAddressPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ traceAroundEvent "vkGetDeviceMemoryOpaqueCaptureAddress" (vkGetDeviceMemoryOpaqueCaptureAddress' (deviceHandle (device)) pInfo)
  pure $ (r)


-- | VkPhysicalDeviceBufferDeviceAddressFeatures - Structure describing
-- buffer address features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
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
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceBufferDeviceAddressFeatures' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceBufferDeviceAddressFeatures = PhysicalDeviceBufferDeviceAddressFeatures
  { -- | #extension-features-bufferDeviceAddress# @bufferDeviceAddress@ indicates
    -- that the implementation supports accessing buffer memory in shaders as
    -- storage buffers via an address queried from 'getBufferDeviceAddress'.
    bufferDeviceAddress :: Bool
  , -- | #extension-features-bufferDeviceAddressCaptureReplay#
    -- @bufferDeviceAddressCaptureReplay@ indicates that the implementation
    -- supports saving and reusing buffer and device addresses, e.g. for trace
    -- capture and replay.
    bufferDeviceAddressCaptureReplay :: Bool
  , -- | #extension-features-bufferDeviceAddressMultiDevice#
    -- @bufferDeviceAddressMultiDevice@ indicates that the implementation
    -- supports the @bufferDeviceAddress@ , @rayTracingPipeline@ and @rayQuery@
    -- features for logical devices created with multiple physical devices. If
    -- this feature is not supported, buffer and acceleration structure
    -- addresses /must/ not be queried on a logical device created with more
    -- than one physical device.
    bufferDeviceAddressMultiDevice :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceBufferDeviceAddressFeatures)
#endif
deriving instance Show PhysicalDeviceBufferDeviceAddressFeatures

instance ToCStruct PhysicalDeviceBufferDeviceAddressFeatures where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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
-- -   #VUID-VkBufferDeviceAddressInfo-buffer-02600# If @buffer@ is
--     non-sparse and was not created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
--     flag, then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkBufferDeviceAddressInfo-buffer-02601# @buffer@ /must/ have
--     been created with
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferDeviceAddressInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO'
--
-- -   #VUID-VkBufferDeviceAddressInfo-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkBufferDeviceAddressInfo-buffer-parameter# @buffer@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getBufferDeviceAddress',
-- 'Vulkan.Extensions.VK_EXT_buffer_device_address.getBufferDeviceAddressEXT',
-- 'Vulkan.Extensions.VK_KHR_buffer_device_address.getBufferDeviceAddressKHR',
-- 'getBufferOpaqueCaptureAddress',
-- 'Vulkan.Extensions.VK_KHR_buffer_device_address.getBufferOpaqueCaptureAddressKHR'
data BufferDeviceAddressInfo = BufferDeviceAddressInfo
  { -- | @buffer@ specifies the buffer whose address is being queried.
    buffer :: Buffer }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferDeviceAddressInfo)
#endif
deriving instance Show BufferDeviceAddressInfo

instance ToCStruct BufferDeviceAddressInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS'
-- errors.
--
-- Note
--
-- The expected usage for this is that a trace capture\/replay tool will
-- add the
-- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
-- flag to all buffers that use
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT',
-- and during capture will save the queried opaque device addresses in the
-- trace. During replay, the buffers will be created specifying the
-- original address so any address values stored in the trace data will
-- remain valid.
--
-- Implementations are expected to separate such buffers in the GPU address
-- space so normal allocations will avoid using these addresses.
-- Apps\/tools should avoid mixing app-provided and implementation-provided
-- addresses for buffers created with
-- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT',
-- to avoid address space allocation conflicts.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferOpaqueCaptureAddressCreateInfo = BufferOpaqueCaptureAddressCreateInfo
  { -- | @opaqueCaptureAddress@ is the opaque capture address requested for the
    -- buffer.
    opaqueCaptureAddress :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferOpaqueCaptureAddressCreateInfo)
#endif
deriving instance Show BufferOpaqueCaptureAddressCreateInfo

instance ToCStruct BufferOpaqueCaptureAddressCreateInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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
-- identically created memory allocation. If this is not the case, it is
-- likely that
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS' errors
-- will occur.
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data MemoryOpaqueCaptureAddressAllocateInfo = MemoryOpaqueCaptureAddressAllocateInfo
  { -- | @opaqueCaptureAddress@ is the opaque capture address requested for the
    -- memory allocation.
    opaqueCaptureAddress :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryOpaqueCaptureAddressAllocateInfo)
#endif
deriving instance Show MemoryOpaqueCaptureAddressAllocateInfo

instance ToCStruct MemoryOpaqueCaptureAddressAllocateInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceMemoryOpaqueCaptureAddress',
-- 'Vulkan.Extensions.VK_KHR_buffer_device_address.getDeviceMemoryOpaqueCaptureAddressKHR'
data DeviceMemoryOpaqueCaptureAddressInfo = DeviceMemoryOpaqueCaptureAddressInfo
  { -- | @memory@ specifies the memory whose address is being queried.
    --
    -- #VUID-VkDeviceMemoryOpaqueCaptureAddressInfo-memory-03336# @memory@
    -- /must/ have been allocated with
    -- 'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT'
    --
    -- #VUID-VkDeviceMemoryOpaqueCaptureAddressInfo-memory-parameter# @memory@
    -- /must/ be a valid 'Vulkan.Core10.Handles.DeviceMemory' handle
    memory :: DeviceMemory }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceMemoryOpaqueCaptureAddressInfo)
#endif
deriving instance Show DeviceMemoryOpaqueCaptureAddressInfo

instance ToCStruct DeviceMemoryOpaqueCaptureAddressInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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

