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

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
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
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkGetBufferDeviceAddress))
import Vulkan.Dynamic (DeviceCmds(pVkGetBufferOpaqueCaptureAddress))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceMemoryOpaqueCaptureAddress))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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

-- No documentation found for TopLevel "vkGetBufferOpaqueCaptureAddress"
getBufferOpaqueCaptureAddress :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkGetBufferOpaqueCaptureAddress" "device"
                                 Device
                              -> -- No documentation found for Nested "vkGetBufferOpaqueCaptureAddress" "pInfo"
                                 BufferDeviceAddressInfo
                              -> io (Word64)
getBufferOpaqueCaptureAddress device info = liftIO . evalContT $ do
  let vkGetBufferOpaqueCaptureAddressPtr = pVkGetBufferOpaqueCaptureAddress (deviceCmds (device :: Device))
  lift $ unless (vkGetBufferOpaqueCaptureAddressPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetBufferOpaqueCaptureAddress is null" Nothing Nothing
  let vkGetBufferOpaqueCaptureAddress' = mkVkGetBufferOpaqueCaptureAddress vkGetBufferOpaqueCaptureAddressPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkGetBufferOpaqueCaptureAddress' (deviceHandle (device)) pInfo
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferDeviceAddress
  :: FunPtr (Ptr Device_T -> Ptr BufferDeviceAddressInfo -> IO DeviceAddress) -> Ptr Device_T -> Ptr BufferDeviceAddressInfo -> IO DeviceAddress

-- No documentation found for TopLevel "vkGetBufferDeviceAddress"
getBufferDeviceAddress :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkGetBufferDeviceAddress" "device"
                          Device
                       -> -- No documentation found for Nested "vkGetBufferDeviceAddress" "pInfo"
                          BufferDeviceAddressInfo
                       -> io (DeviceAddress)
getBufferDeviceAddress device info = liftIO . evalContT $ do
  let vkGetBufferDeviceAddressPtr = pVkGetBufferDeviceAddress (deviceCmds (device :: Device))
  lift $ unless (vkGetBufferDeviceAddressPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetBufferDeviceAddress is null" Nothing Nothing
  let vkGetBufferDeviceAddress' = mkVkGetBufferDeviceAddress vkGetBufferDeviceAddressPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkGetBufferDeviceAddress' (deviceHandle (device)) pInfo
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceMemoryOpaqueCaptureAddress
  :: FunPtr (Ptr Device_T -> Ptr DeviceMemoryOpaqueCaptureAddressInfo -> IO Word64) -> Ptr Device_T -> Ptr DeviceMemoryOpaqueCaptureAddressInfo -> IO Word64

-- No documentation found for TopLevel "vkGetDeviceMemoryOpaqueCaptureAddress"
getDeviceMemoryOpaqueCaptureAddress :: forall io
                                     . (MonadIO io)
                                    => -- No documentation found for Nested "vkGetDeviceMemoryOpaqueCaptureAddress" "device"
                                       Device
                                    -> -- No documentation found for Nested "vkGetDeviceMemoryOpaqueCaptureAddress" "pInfo"
                                       DeviceMemoryOpaqueCaptureAddressInfo
                                    -> io (Word64)
getDeviceMemoryOpaqueCaptureAddress device info = liftIO . evalContT $ do
  let vkGetDeviceMemoryOpaqueCaptureAddressPtr = pVkGetDeviceMemoryOpaqueCaptureAddress (deviceCmds (device :: Device))
  lift $ unless (vkGetDeviceMemoryOpaqueCaptureAddressPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceMemoryOpaqueCaptureAddress is null" Nothing Nothing
  let vkGetDeviceMemoryOpaqueCaptureAddress' = mkVkGetDeviceMemoryOpaqueCaptureAddress vkGetDeviceMemoryOpaqueCaptureAddressPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkGetDeviceMemoryOpaqueCaptureAddress' (deviceHandle (device)) pInfo
  pure $ (r)



-- No documentation found for TopLevel "VkPhysicalDeviceBufferDeviceAddressFeatures"
data PhysicalDeviceBufferDeviceAddressFeatures = PhysicalDeviceBufferDeviceAddressFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeatures" "bufferDeviceAddress"
    bufferDeviceAddress :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeatures" "bufferDeviceAddressCaptureReplay"
    bufferDeviceAddressCaptureReplay :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceBufferDeviceAddressFeatures" "bufferDeviceAddressMultiDevice"
    bufferDeviceAddressMultiDevice :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceBufferDeviceAddressFeatures)
#endif
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



-- No documentation found for TopLevel "VkBufferDeviceAddressInfo"
data BufferDeviceAddressInfo = BufferDeviceAddressInfo
  { -- No documentation found for Nested "VkBufferDeviceAddressInfo" "buffer"
    buffer :: Buffer }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferDeviceAddressInfo)
#endif
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



-- No documentation found for TopLevel "VkBufferOpaqueCaptureAddressCreateInfo"
data BufferOpaqueCaptureAddressCreateInfo = BufferOpaqueCaptureAddressCreateInfo
  { -- No documentation found for Nested "VkBufferOpaqueCaptureAddressCreateInfo" "opaqueCaptureAddress"
    opaqueCaptureAddress :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferOpaqueCaptureAddressCreateInfo)
#endif
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



-- No documentation found for TopLevel "VkMemoryOpaqueCaptureAddressAllocateInfo"
data MemoryOpaqueCaptureAddressAllocateInfo = MemoryOpaqueCaptureAddressAllocateInfo
  { -- No documentation found for Nested "VkMemoryOpaqueCaptureAddressAllocateInfo" "opaqueCaptureAddress"
    opaqueCaptureAddress :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryOpaqueCaptureAddressAllocateInfo)
#endif
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



-- No documentation found for TopLevel "VkDeviceMemoryOpaqueCaptureAddressInfo"
data DeviceMemoryOpaqueCaptureAddressInfo = DeviceMemoryOpaqueCaptureAddressInfo
  { -- No documentation found for Nested "VkDeviceMemoryOpaqueCaptureAddressInfo" "memory"
    memory :: DeviceMemory }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceMemoryOpaqueCaptureAddressInfo)
#endif
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

