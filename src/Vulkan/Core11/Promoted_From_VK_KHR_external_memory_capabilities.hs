{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_external_memory_capabilities"
module Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities  ( getPhysicalDeviceExternalBufferProperties
                                                                        , ExternalMemoryProperties(..)
                                                                        , PhysicalDeviceExternalImageFormatInfo(..)
                                                                        , ExternalImageFormatProperties(..)
                                                                        , PhysicalDeviceExternalBufferInfo(..)
                                                                        , ExternalBufferProperties(..)
                                                                        , PhysicalDeviceIDProperties(..)
                                                                        , StructureType(..)
                                                                        , ExternalMemoryHandleTypeFlagBits(..)
                                                                        , ExternalMemoryHandleTypeFlags
                                                                        , ExternalMemoryFeatureFlagBits(..)
                                                                        , ExternalMemoryFeatureFlags
                                                                        , LUID_SIZE
                                                                        , pattern LUID_SIZE
                                                                        ) where

import Vulkan.CStruct.Utils (FixedArray)
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
import Data.Word (Word32)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlags)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceExternalBufferProperties))
import Vulkan.Core10.APIConstants (LUID_SIZE)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES))
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlagBits(..))
import Vulkan.Core11.Enums.ExternalMemoryFeatureFlagBits (ExternalMemoryFeatureFlags)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(..))
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlags)
import Vulkan.Core10.APIConstants (LUID_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core10.APIConstants (pattern LUID_SIZE)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalBufferProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr PhysicalDeviceExternalBufferInfo -> Ptr ExternalBufferProperties -> IO ()) -> Ptr PhysicalDevice_T -> Ptr PhysicalDeviceExternalBufferInfo -> Ptr ExternalBufferProperties -> IO ()

-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalBufferProperties"
getPhysicalDeviceExternalBufferProperties :: forall io
                                           . (MonadIO io)
                                          => -- No documentation found for Nested "vkGetPhysicalDeviceExternalBufferProperties" "physicalDevice"
                                             PhysicalDevice
                                          -> -- No documentation found for Nested "vkGetPhysicalDeviceExternalBufferProperties" "pExternalBufferInfo"
                                             PhysicalDeviceExternalBufferInfo
                                          -> io (ExternalBufferProperties)
getPhysicalDeviceExternalBufferProperties physicalDevice externalBufferInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceExternalBufferPropertiesPtr = pVkGetPhysicalDeviceExternalBufferProperties (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceExternalBufferPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceExternalBufferProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceExternalBufferProperties' = mkVkGetPhysicalDeviceExternalBufferProperties vkGetPhysicalDeviceExternalBufferPropertiesPtr
  pExternalBufferInfo <- ContT $ withCStruct (externalBufferInfo)
  pPExternalBufferProperties <- ContT (withZeroCStruct @ExternalBufferProperties)
  lift $ vkGetPhysicalDeviceExternalBufferProperties' (physicalDeviceHandle (physicalDevice)) pExternalBufferInfo (pPExternalBufferProperties)
  pExternalBufferProperties <- lift $ peekCStruct @ExternalBufferProperties pPExternalBufferProperties
  pure $ (pExternalBufferProperties)



-- No documentation found for TopLevel "VkExternalMemoryProperties"
data ExternalMemoryProperties = ExternalMemoryProperties
  { -- No documentation found for Nested "VkExternalMemoryProperties" "externalMemoryFeatures"
    externalMemoryFeatures :: ExternalMemoryFeatureFlags
  , -- No documentation found for Nested "VkExternalMemoryProperties" "exportFromImportedHandleTypes"
    exportFromImportedHandleTypes :: ExternalMemoryHandleTypeFlags
  , -- No documentation found for Nested "VkExternalMemoryProperties" "compatibleHandleTypes"
    compatibleHandleTypes :: ExternalMemoryHandleTypeFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalMemoryProperties)
#endif
deriving instance Show ExternalMemoryProperties

instance ToCStruct ExternalMemoryProperties where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalMemoryProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ExternalMemoryFeatureFlags)) (externalMemoryFeatures)
    poke ((p `plusPtr` 4 :: Ptr ExternalMemoryHandleTypeFlags)) (exportFromImportedHandleTypes)
    poke ((p `plusPtr` 8 :: Ptr ExternalMemoryHandleTypeFlags)) (compatibleHandleTypes)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ExternalMemoryFeatureFlags)) (zero)
    poke ((p `plusPtr` 8 :: Ptr ExternalMemoryHandleTypeFlags)) (zero)
    f

instance FromCStruct ExternalMemoryProperties where
  peekCStruct p = do
    externalMemoryFeatures <- peek @ExternalMemoryFeatureFlags ((p `plusPtr` 0 :: Ptr ExternalMemoryFeatureFlags))
    exportFromImportedHandleTypes <- peek @ExternalMemoryHandleTypeFlags ((p `plusPtr` 4 :: Ptr ExternalMemoryHandleTypeFlags))
    compatibleHandleTypes <- peek @ExternalMemoryHandleTypeFlags ((p `plusPtr` 8 :: Ptr ExternalMemoryHandleTypeFlags))
    pure $ ExternalMemoryProperties
             externalMemoryFeatures exportFromImportedHandleTypes compatibleHandleTypes


instance Storable ExternalMemoryProperties where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalMemoryProperties where
  zero = ExternalMemoryProperties
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceExternalImageFormatInfo"
data PhysicalDeviceExternalImageFormatInfo = PhysicalDeviceExternalImageFormatInfo
  { -- No documentation found for Nested "VkPhysicalDeviceExternalImageFormatInfo" "handleType"
    handleType :: ExternalMemoryHandleTypeFlagBits }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalImageFormatInfo)
#endif
deriving instance Show PhysicalDeviceExternalImageFormatInfo

instance ToCStruct PhysicalDeviceExternalImageFormatInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalImageFormatInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PhysicalDeviceExternalImageFormatInfo where
  peekCStruct p = do
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pure $ PhysicalDeviceExternalImageFormatInfo
             handleType


instance Storable PhysicalDeviceExternalImageFormatInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalImageFormatInfo where
  zero = PhysicalDeviceExternalImageFormatInfo
           zero



-- No documentation found for TopLevel "VkExternalImageFormatProperties"
data ExternalImageFormatProperties = ExternalImageFormatProperties
  { -- No documentation found for Nested "VkExternalImageFormatProperties" "externalMemoryProperties"
    externalMemoryProperties :: ExternalMemoryProperties }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalImageFormatProperties)
#endif
deriving instance Show ExternalImageFormatProperties

instance ToCStruct ExternalImageFormatProperties where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalImageFormatProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties)) (externalMemoryProperties)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties)) (zero)
    f

instance FromCStruct ExternalImageFormatProperties where
  peekCStruct p = do
    externalMemoryProperties <- peekCStruct @ExternalMemoryProperties ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties))
    pure $ ExternalImageFormatProperties
             externalMemoryProperties


instance Storable ExternalImageFormatProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalImageFormatProperties where
  zero = ExternalImageFormatProperties
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceExternalBufferInfo"
data PhysicalDeviceExternalBufferInfo = PhysicalDeviceExternalBufferInfo
  { -- No documentation found for Nested "VkPhysicalDeviceExternalBufferInfo" "flags"
    flags :: BufferCreateFlags
  , -- No documentation found for Nested "VkPhysicalDeviceExternalBufferInfo" "usage"
    usage :: BufferUsageFlags
  , -- No documentation found for Nested "VkPhysicalDeviceExternalBufferInfo" "handleType"
    handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalBufferInfo)
#endif
deriving instance Show PhysicalDeviceExternalBufferInfo

instance ToCStruct PhysicalDeviceExternalBufferInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalBufferInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferCreateFlags)) (flags)
    poke ((p `plusPtr` 20 :: Ptr BufferUsageFlags)) (usage)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr BufferUsageFlags)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (zero)
    f

instance FromCStruct PhysicalDeviceExternalBufferInfo where
  peekCStruct p = do
    flags <- peek @BufferCreateFlags ((p `plusPtr` 16 :: Ptr BufferCreateFlags))
    usage <- peek @BufferUsageFlags ((p `plusPtr` 20 :: Ptr BufferUsageFlags))
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pure $ PhysicalDeviceExternalBufferInfo
             flags usage handleType


instance Storable PhysicalDeviceExternalBufferInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalBufferInfo where
  zero = PhysicalDeviceExternalBufferInfo
           zero
           zero
           zero



-- No documentation found for TopLevel "VkExternalBufferProperties"
data ExternalBufferProperties = ExternalBufferProperties
  { -- No documentation found for Nested "VkExternalBufferProperties" "externalMemoryProperties"
    externalMemoryProperties :: ExternalMemoryProperties }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalBufferProperties)
#endif
deriving instance Show ExternalBufferProperties

instance ToCStruct ExternalBufferProperties where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalBufferProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties)) (externalMemoryProperties)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties)) (zero)
    f

instance FromCStruct ExternalBufferProperties where
  peekCStruct p = do
    externalMemoryProperties <- peekCStruct @ExternalMemoryProperties ((p `plusPtr` 16 :: Ptr ExternalMemoryProperties))
    pure $ ExternalBufferProperties
             externalMemoryProperties


instance Storable ExternalBufferProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalBufferProperties where
  zero = ExternalBufferProperties
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceIDProperties"
data PhysicalDeviceIDProperties = PhysicalDeviceIDProperties
  { -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "deviceUUID"
    deviceUUID :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "driverUUID"
    driverUUID :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "deviceLUID"
    deviceLUID :: ByteString
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "deviceNodeMask"
    deviceNodeMask :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceIDProperties" "deviceLUIDValid"
    deviceLUIDValid :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceIDProperties)
#endif
deriving instance Show PhysicalDeviceIDProperties

instance ToCStruct PhysicalDeviceIDProperties where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceIDProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (deviceUUID)
    pokeFixedLengthByteString ((p `plusPtr` 32 :: Ptr (FixedArray UUID_SIZE Word8))) (driverUUID)
    pokeFixedLengthByteString ((p `plusPtr` 48 :: Ptr (FixedArray LUID_SIZE Word8))) (deviceLUID)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (deviceNodeMask)
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (deviceLUIDValid))
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    pokeFixedLengthByteString ((p `plusPtr` 32 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    pokeFixedLengthByteString ((p `plusPtr` 48 :: Ptr (FixedArray LUID_SIZE Word8))) (mempty)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceIDProperties where
  peekCStruct p = do
    deviceUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8)))
    driverUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 32 :: Ptr (FixedArray UUID_SIZE Word8)))
    deviceLUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 48 :: Ptr (FixedArray LUID_SIZE Word8)))
    deviceNodeMask <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    deviceLUIDValid <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    pure $ PhysicalDeviceIDProperties
             deviceUUID driverUUID deviceLUID deviceNodeMask (bool32ToBool deviceLUIDValid)


instance Storable PhysicalDeviceIDProperties where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceIDProperties where
  zero = PhysicalDeviceIDProperties
           mempty
           mempty
           mempty
           zero
           zero

