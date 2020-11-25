{-# language CPP #-}
-- No documentation found for Chapter "Originally_Based_On_VK_KHR_protected_memory"
module Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory  ( getDeviceQueue2
                                                                  , ProtectedSubmitInfo(..)
                                                                  , PhysicalDeviceProtectedMemoryFeatures(..)
                                                                  , PhysicalDeviceProtectedMemoryProperties(..)
                                                                  , DeviceQueueInfo2(..)
                                                                  , StructureType(..)
                                                                  , QueueFlagBits(..)
                                                                  , QueueFlags
                                                                  , DeviceQueueCreateFlagBits(..)
                                                                  , DeviceQueueCreateFlags
                                                                  , MemoryPropertyFlagBits(..)
                                                                  , MemoryPropertyFlags
                                                                  , BufferCreateFlagBits(..)
                                                                  , BufferCreateFlags
                                                                  , ImageCreateFlagBits(..)
                                                                  , ImageCreateFlags
                                                                  , CommandPoolCreateFlagBits(..)
                                                                  , CommandPoolCreateFlags
                                                                  ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceQueue2))
import Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlags)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(Queue))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO))
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlagBits(..))
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits (CommandPoolCreateFlagBits(..))
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits (CommandPoolCreateFlags)
import Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlagBits(..))
import Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlags)
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.MemoryPropertyFlagBits (MemoryPropertyFlagBits(..))
import Vulkan.Core10.Enums.MemoryPropertyFlagBits (MemoryPropertyFlags)
import Vulkan.Core10.Enums.QueueFlagBits (QueueFlagBits(..))
import Vulkan.Core10.Enums.QueueFlagBits (QueueFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceQueue2
  :: FunPtr (Ptr Device_T -> Ptr DeviceQueueInfo2 -> Ptr (Ptr Queue_T) -> IO ()) -> Ptr Device_T -> Ptr DeviceQueueInfo2 -> Ptr (Ptr Queue_T) -> IO ()

-- No documentation found for TopLevel "vkGetDeviceQueue2"
getDeviceQueue2 :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkGetDeviceQueue2" "device"
                   Device
                -> -- No documentation found for Nested "vkGetDeviceQueue2" "pQueueInfo"
                   DeviceQueueInfo2
                -> io (Queue)
getDeviceQueue2 device queueInfo = liftIO . evalContT $ do
  let cmds = deviceCmds (device :: Device)
  let vkGetDeviceQueue2Ptr = pVkGetDeviceQueue2 cmds
  lift $ unless (vkGetDeviceQueue2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceQueue2 is null" Nothing Nothing
  let vkGetDeviceQueue2' = mkVkGetDeviceQueue2 vkGetDeviceQueue2Ptr
  pQueueInfo <- ContT $ withCStruct (queueInfo)
  pPQueue <- ContT $ bracket (callocBytes @(Ptr Queue_T) 8) free
  lift $ vkGetDeviceQueue2' (deviceHandle (device)) pQueueInfo (pPQueue)
  pQueue <- lift $ peek @(Ptr Queue_T) pPQueue
  pure $ (((\h -> Queue h cmds ) pQueue))



-- No documentation found for TopLevel "VkProtectedSubmitInfo"
data ProtectedSubmitInfo = ProtectedSubmitInfo
  { -- No documentation found for Nested "VkProtectedSubmitInfo" "protectedSubmit"
    protectedSubmit :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ProtectedSubmitInfo)
#endif
deriving instance Show ProtectedSubmitInfo

instance ToCStruct ProtectedSubmitInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ProtectedSubmitInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (protectedSubmit))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct ProtectedSubmitInfo where
  peekCStruct p = do
    protectedSubmit <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ ProtectedSubmitInfo
             (bool32ToBool protectedSubmit)


instance Storable ProtectedSubmitInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ProtectedSubmitInfo where
  zero = ProtectedSubmitInfo
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceProtectedMemoryFeatures"
data PhysicalDeviceProtectedMemoryFeatures = PhysicalDeviceProtectedMemoryFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceProtectedMemoryFeatures" "protectedMemory"
    protectedMemory :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceProtectedMemoryFeatures)
#endif
deriving instance Show PhysicalDeviceProtectedMemoryFeatures

instance ToCStruct PhysicalDeviceProtectedMemoryFeatures where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceProtectedMemoryFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (protectedMemory))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceProtectedMemoryFeatures where
  peekCStruct p = do
    protectedMemory <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceProtectedMemoryFeatures
             (bool32ToBool protectedMemory)


instance Storable PhysicalDeviceProtectedMemoryFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceProtectedMemoryFeatures where
  zero = PhysicalDeviceProtectedMemoryFeatures
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceProtectedMemoryProperties"
data PhysicalDeviceProtectedMemoryProperties = PhysicalDeviceProtectedMemoryProperties
  { -- No documentation found for Nested "VkPhysicalDeviceProtectedMemoryProperties" "protectedNoFault"
    protectedNoFault :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceProtectedMemoryProperties)
#endif
deriving instance Show PhysicalDeviceProtectedMemoryProperties

instance ToCStruct PhysicalDeviceProtectedMemoryProperties where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceProtectedMemoryProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (protectedNoFault))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceProtectedMemoryProperties where
  peekCStruct p = do
    protectedNoFault <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceProtectedMemoryProperties
             (bool32ToBool protectedNoFault)


instance Storable PhysicalDeviceProtectedMemoryProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceProtectedMemoryProperties where
  zero = PhysicalDeviceProtectedMemoryProperties
           zero



-- No documentation found for TopLevel "VkDeviceQueueInfo2"
data DeviceQueueInfo2 = DeviceQueueInfo2
  { -- No documentation found for Nested "VkDeviceQueueInfo2" "flags"
    flags :: DeviceQueueCreateFlags
  , -- No documentation found for Nested "VkDeviceQueueInfo2" "queueFamilyIndex"
    queueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkDeviceQueueInfo2" "queueIndex"
    queueIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceQueueInfo2)
#endif
deriving instance Show DeviceQueueInfo2

instance ToCStruct DeviceQueueInfo2 where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceQueueInfo2{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceQueueCreateFlags)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (queueFamilyIndex)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (queueIndex)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceQueueInfo2 where
  peekCStruct p = do
    flags <- peek @DeviceQueueCreateFlags ((p `plusPtr` 16 :: Ptr DeviceQueueCreateFlags))
    queueFamilyIndex <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    queueIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ DeviceQueueInfo2
             flags queueFamilyIndex queueIndex


instance Storable DeviceQueueInfo2 where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceQueueInfo2 where
  zero = DeviceQueueInfo2
           zero
           zero
           zero

