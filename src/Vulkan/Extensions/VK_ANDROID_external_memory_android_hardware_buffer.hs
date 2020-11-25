{-# language CPP #-}
-- No documentation found for Chapter "VK_ANDROID_external_memory_android_hardware_buffer"
module Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer  ( getAndroidHardwareBufferPropertiesANDROID
                                                                             , getMemoryAndroidHardwareBufferANDROID
                                                                             , ImportAndroidHardwareBufferInfoANDROID(..)
                                                                             , AndroidHardwareBufferUsageANDROID(..)
                                                                             , AndroidHardwareBufferPropertiesANDROID(..)
                                                                             , MemoryGetAndroidHardwareBufferInfoANDROID(..)
                                                                             , AndroidHardwareBufferFormatPropertiesANDROID(..)
                                                                             , ExternalFormatANDROID(..)
                                                                             , ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
                                                                             , pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION
                                                                             , ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
                                                                             , pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME
                                                                             , AHardwareBuffer
                                                                             ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core11.Enums.ChromaLocation (ChromaLocation)
import Vulkan.Core10.ImageView (ComponentMapping)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetAndroidHardwareBufferPropertiesANDROID))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryAndroidHardwareBufferANDROID))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.FormatFeatureFlagBits (FormatFeatureFlags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core11.Enums.SamplerYcbcrModelConversion (SamplerYcbcrModelConversion)
import Vulkan.Core11.Enums.SamplerYcbcrRange (SamplerYcbcrRange)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAndroidHardwareBufferPropertiesANDROID
  :: FunPtr (Ptr Device_T -> Ptr AHardwareBuffer -> Ptr (SomeStruct AndroidHardwareBufferPropertiesANDROID) -> IO Result) -> Ptr Device_T -> Ptr AHardwareBuffer -> Ptr (SomeStruct AndroidHardwareBufferPropertiesANDROID) -> IO Result

-- No documentation found for TopLevel "vkGetAndroidHardwareBufferPropertiesANDROID"
getAndroidHardwareBufferPropertiesANDROID :: forall a io
                                           . (Extendss AndroidHardwareBufferPropertiesANDROID a, PokeChain a, PeekChain a, MonadIO io)
                                          => -- No documentation found for Nested "vkGetAndroidHardwareBufferPropertiesANDROID" "device"
                                             Device
                                          -> -- No documentation found for Nested "vkGetAndroidHardwareBufferPropertiesANDROID" "buffer"
                                             (Ptr AHardwareBuffer)
                                          -> io (AndroidHardwareBufferPropertiesANDROID a)
getAndroidHardwareBufferPropertiesANDROID device buffer = liftIO . evalContT $ do
  let vkGetAndroidHardwareBufferPropertiesANDROIDPtr = pVkGetAndroidHardwareBufferPropertiesANDROID (deviceCmds (device :: Device))
  lift $ unless (vkGetAndroidHardwareBufferPropertiesANDROIDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetAndroidHardwareBufferPropertiesANDROID is null" Nothing Nothing
  let vkGetAndroidHardwareBufferPropertiesANDROID' = mkVkGetAndroidHardwareBufferPropertiesANDROID vkGetAndroidHardwareBufferPropertiesANDROIDPtr
  pPProperties <- ContT (withZeroCStruct @(AndroidHardwareBufferPropertiesANDROID _))
  r <- lift $ vkGetAndroidHardwareBufferPropertiesANDROID' (deviceHandle (device)) (buffer) (forgetExtensions (pPProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pProperties <- lift $ peekCStruct @(AndroidHardwareBufferPropertiesANDROID _) pPProperties
  pure $ (pProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryAndroidHardwareBufferANDROID
  :: FunPtr (Ptr Device_T -> Ptr MemoryGetAndroidHardwareBufferInfoANDROID -> Ptr (Ptr AHardwareBuffer) -> IO Result) -> Ptr Device_T -> Ptr MemoryGetAndroidHardwareBufferInfoANDROID -> Ptr (Ptr AHardwareBuffer) -> IO Result

-- No documentation found for TopLevel "vkGetMemoryAndroidHardwareBufferANDROID"
getMemoryAndroidHardwareBufferANDROID :: forall io
                                       . (MonadIO io)
                                      => -- No documentation found for Nested "vkGetMemoryAndroidHardwareBufferANDROID" "device"
                                         Device
                                      -> -- No documentation found for Nested "vkGetMemoryAndroidHardwareBufferANDROID" "pInfo"
                                         MemoryGetAndroidHardwareBufferInfoANDROID
                                      -> io (Ptr AHardwareBuffer)
getMemoryAndroidHardwareBufferANDROID device info = liftIO . evalContT $ do
  let vkGetMemoryAndroidHardwareBufferANDROIDPtr = pVkGetMemoryAndroidHardwareBufferANDROID (deviceCmds (device :: Device))
  lift $ unless (vkGetMemoryAndroidHardwareBufferANDROIDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryAndroidHardwareBufferANDROID is null" Nothing Nothing
  let vkGetMemoryAndroidHardwareBufferANDROID' = mkVkGetMemoryAndroidHardwareBufferANDROID vkGetMemoryAndroidHardwareBufferANDROIDPtr
  pInfo <- ContT $ withCStruct (info)
  pPBuffer <- ContT $ bracket (callocBytes @(Ptr AHardwareBuffer) 8) free
  r <- lift $ vkGetMemoryAndroidHardwareBufferANDROID' (deviceHandle (device)) pInfo (pPBuffer)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pBuffer <- lift $ peek @(Ptr AHardwareBuffer) pPBuffer
  pure $ (pBuffer)



-- No documentation found for TopLevel "VkImportAndroidHardwareBufferInfoANDROID"
data ImportAndroidHardwareBufferInfoANDROID = ImportAndroidHardwareBufferInfoANDROID
  { -- No documentation found for Nested "VkImportAndroidHardwareBufferInfoANDROID" "buffer"
    buffer :: Ptr AHardwareBuffer }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportAndroidHardwareBufferInfoANDROID)
#endif
deriving instance Show ImportAndroidHardwareBufferInfoANDROID

instance ToCStruct ImportAndroidHardwareBufferInfoANDROID where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportAndroidHardwareBufferInfoANDROID{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr AHardwareBuffer))) (buffer)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr AHardwareBuffer))) (zero)
    f

instance FromCStruct ImportAndroidHardwareBufferInfoANDROID where
  peekCStruct p = do
    buffer <- peek @(Ptr AHardwareBuffer) ((p `plusPtr` 16 :: Ptr (Ptr AHardwareBuffer)))
    pure $ ImportAndroidHardwareBufferInfoANDROID
             buffer


instance Storable ImportAndroidHardwareBufferInfoANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportAndroidHardwareBufferInfoANDROID where
  zero = ImportAndroidHardwareBufferInfoANDROID
           zero



-- No documentation found for TopLevel "VkAndroidHardwareBufferUsageANDROID"
data AndroidHardwareBufferUsageANDROID = AndroidHardwareBufferUsageANDROID
  { -- No documentation found for Nested "VkAndroidHardwareBufferUsageANDROID" "androidHardwareBufferUsage"
    androidHardwareBufferUsage :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AndroidHardwareBufferUsageANDROID)
#endif
deriving instance Show AndroidHardwareBufferUsageANDROID

instance ToCStruct AndroidHardwareBufferUsageANDROID where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AndroidHardwareBufferUsageANDROID{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (androidHardwareBufferUsage)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    f

instance FromCStruct AndroidHardwareBufferUsageANDROID where
  peekCStruct p = do
    androidHardwareBufferUsage <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    pure $ AndroidHardwareBufferUsageANDROID
             androidHardwareBufferUsage


instance Storable AndroidHardwareBufferUsageANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AndroidHardwareBufferUsageANDROID where
  zero = AndroidHardwareBufferUsageANDROID
           zero



-- No documentation found for TopLevel "VkAndroidHardwareBufferPropertiesANDROID"
data AndroidHardwareBufferPropertiesANDROID (es :: [Type]) = AndroidHardwareBufferPropertiesANDROID
  { -- No documentation found for Nested "VkAndroidHardwareBufferPropertiesANDROID" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkAndroidHardwareBufferPropertiesANDROID" "allocationSize"
    allocationSize :: DeviceSize
  , -- No documentation found for Nested "VkAndroidHardwareBufferPropertiesANDROID" "memoryTypeBits"
    memoryTypeBits :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AndroidHardwareBufferPropertiesANDROID (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (AndroidHardwareBufferPropertiesANDROID es)

instance Extensible AndroidHardwareBufferPropertiesANDROID where
  extensibleType = STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  setNext x next = x{next = next}
  getNext AndroidHardwareBufferPropertiesANDROID{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends AndroidHardwareBufferPropertiesANDROID e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @AndroidHardwareBufferFormatPropertiesANDROID = Just f
    | otherwise = Nothing

instance (Extendss AndroidHardwareBufferPropertiesANDROID es, PokeChain es) => ToCStruct (AndroidHardwareBufferPropertiesANDROID es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AndroidHardwareBufferPropertiesANDROID{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (allocationSize)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (memoryTypeBits)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ f

instance (Extendss AndroidHardwareBufferPropertiesANDROID es, PeekChain es) => FromCStruct (AndroidHardwareBufferPropertiesANDROID es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    allocationSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ AndroidHardwareBufferPropertiesANDROID
             next allocationSize memoryTypeBits

instance es ~ '[] => Zero (AndroidHardwareBufferPropertiesANDROID es) where
  zero = AndroidHardwareBufferPropertiesANDROID
           ()
           zero
           zero



-- No documentation found for TopLevel "VkMemoryGetAndroidHardwareBufferInfoANDROID"
data MemoryGetAndroidHardwareBufferInfoANDROID = MemoryGetAndroidHardwareBufferInfoANDROID
  { -- No documentation found for Nested "VkMemoryGetAndroidHardwareBufferInfoANDROID" "memory"
    memory :: DeviceMemory }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryGetAndroidHardwareBufferInfoANDROID)
#endif
deriving instance Show MemoryGetAndroidHardwareBufferInfoANDROID

instance ToCStruct MemoryGetAndroidHardwareBufferInfoANDROID where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryGetAndroidHardwareBufferInfoANDROID{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (zero)
    f

instance FromCStruct MemoryGetAndroidHardwareBufferInfoANDROID where
  peekCStruct p = do
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    pure $ MemoryGetAndroidHardwareBufferInfoANDROID
             memory


instance Storable MemoryGetAndroidHardwareBufferInfoANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryGetAndroidHardwareBufferInfoANDROID where
  zero = MemoryGetAndroidHardwareBufferInfoANDROID
           zero



-- No documentation found for TopLevel "VkAndroidHardwareBufferFormatPropertiesANDROID"
data AndroidHardwareBufferFormatPropertiesANDROID = AndroidHardwareBufferFormatPropertiesANDROID
  { -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "format"
    format :: Format
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "externalFormat"
    externalFormat :: Word64
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "formatFeatures"
    formatFeatures :: FormatFeatureFlags
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "samplerYcbcrConversionComponents"
    samplerYcbcrConversionComponents :: ComponentMapping
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "suggestedYcbcrModel"
    suggestedYcbcrModel :: SamplerYcbcrModelConversion
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "suggestedYcbcrRange"
    suggestedYcbcrRange :: SamplerYcbcrRange
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "suggestedXChromaOffset"
    suggestedXChromaOffset :: ChromaLocation
  , -- No documentation found for Nested "VkAndroidHardwareBufferFormatPropertiesANDROID" "suggestedYChromaOffset"
    suggestedYChromaOffset :: ChromaLocation
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AndroidHardwareBufferFormatPropertiesANDROID)
#endif
deriving instance Show AndroidHardwareBufferFormatPropertiesANDROID

instance ToCStruct AndroidHardwareBufferFormatPropertiesANDROID where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AndroidHardwareBufferFormatPropertiesANDROID{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (format)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (externalFormat)
    poke ((p `plusPtr` 32 :: Ptr FormatFeatureFlags)) (formatFeatures)
    poke ((p `plusPtr` 36 :: Ptr ComponentMapping)) (samplerYcbcrConversionComponents)
    poke ((p `plusPtr` 52 :: Ptr SamplerYcbcrModelConversion)) (suggestedYcbcrModel)
    poke ((p `plusPtr` 56 :: Ptr SamplerYcbcrRange)) (suggestedYcbcrRange)
    poke ((p `plusPtr` 60 :: Ptr ChromaLocation)) (suggestedXChromaOffset)
    poke ((p `plusPtr` 64 :: Ptr ChromaLocation)) (suggestedYChromaOffset)
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr FormatFeatureFlags)) (zero)
    poke ((p `plusPtr` 36 :: Ptr ComponentMapping)) (zero)
    poke ((p `plusPtr` 52 :: Ptr SamplerYcbcrModelConversion)) (zero)
    poke ((p `plusPtr` 56 :: Ptr SamplerYcbcrRange)) (zero)
    poke ((p `plusPtr` 60 :: Ptr ChromaLocation)) (zero)
    poke ((p `plusPtr` 64 :: Ptr ChromaLocation)) (zero)
    f

instance FromCStruct AndroidHardwareBufferFormatPropertiesANDROID where
  peekCStruct p = do
    format <- peek @Format ((p `plusPtr` 16 :: Ptr Format))
    externalFormat <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    formatFeatures <- peek @FormatFeatureFlags ((p `plusPtr` 32 :: Ptr FormatFeatureFlags))
    samplerYcbcrConversionComponents <- peekCStruct @ComponentMapping ((p `plusPtr` 36 :: Ptr ComponentMapping))
    suggestedYcbcrModel <- peek @SamplerYcbcrModelConversion ((p `plusPtr` 52 :: Ptr SamplerYcbcrModelConversion))
    suggestedYcbcrRange <- peek @SamplerYcbcrRange ((p `plusPtr` 56 :: Ptr SamplerYcbcrRange))
    suggestedXChromaOffset <- peek @ChromaLocation ((p `plusPtr` 60 :: Ptr ChromaLocation))
    suggestedYChromaOffset <- peek @ChromaLocation ((p `plusPtr` 64 :: Ptr ChromaLocation))
    pure $ AndroidHardwareBufferFormatPropertiesANDROID
             format externalFormat formatFeatures samplerYcbcrConversionComponents suggestedYcbcrModel suggestedYcbcrRange suggestedXChromaOffset suggestedYChromaOffset


instance Storable AndroidHardwareBufferFormatPropertiesANDROID where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AndroidHardwareBufferFormatPropertiesANDROID where
  zero = AndroidHardwareBufferFormatPropertiesANDROID
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkExternalFormatANDROID"
data ExternalFormatANDROID = ExternalFormatANDROID
  { -- No documentation found for Nested "VkExternalFormatANDROID" "externalFormat"
    externalFormat :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExternalFormatANDROID)
#endif
deriving instance Show ExternalFormatANDROID

instance ToCStruct ExternalFormatANDROID where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExternalFormatANDROID{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (externalFormat)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    f

instance FromCStruct ExternalFormatANDROID where
  peekCStruct p = do
    externalFormat <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    pure $ ExternalFormatANDROID
             externalFormat


instance Storable ExternalFormatANDROID where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExternalFormatANDROID where
  zero = ExternalFormatANDROID
           zero


type ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION"
pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION :: forall a . Integral a => a
pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_SPEC_VERSION = 3


type ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME = "VK_ANDROID_external_memory_android_hardware_buffer"

-- No documentation found for TopLevel "VK_ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME"
pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ANDROID_EXTERNAL_MEMORY_ANDROID_HARDWARE_BUFFER_EXTENSION_NAME = "VK_ANDROID_external_memory_android_hardware_buffer"


data AHardwareBuffer

