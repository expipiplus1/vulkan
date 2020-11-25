{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_external_memory_host"
module Vulkan.Extensions.VK_EXT_external_memory_host  ( getMemoryHostPointerPropertiesEXT
                                                      , ImportMemoryHostPointerInfoEXT(..)
                                                      , MemoryHostPointerPropertiesEXT(..)
                                                      , PhysicalDeviceExternalMemoryHostPropertiesEXT(..)
                                                      , EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION
                                                      , pattern EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION
                                                      , EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
                                                      , pattern EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME
                                                      ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryHostPointerPropertiesEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryHostPointerPropertiesEXT
  :: FunPtr (Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> Ptr () -> Ptr MemoryHostPointerPropertiesEXT -> IO Result) -> Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> Ptr () -> Ptr MemoryHostPointerPropertiesEXT -> IO Result

-- No documentation found for TopLevel "vkGetMemoryHostPointerPropertiesEXT"
getMemoryHostPointerPropertiesEXT :: forall io
                                   . (MonadIO io)
                                  => -- No documentation found for Nested "vkGetMemoryHostPointerPropertiesEXT" "device"
                                     Device
                                  -> -- No documentation found for Nested "vkGetMemoryHostPointerPropertiesEXT" "handleType"
                                     ExternalMemoryHandleTypeFlagBits
                                  -> -- No documentation found for Nested "vkGetMemoryHostPointerPropertiesEXT" "pHostPointer"
                                     ("hostPointer" ::: Ptr ())
                                  -> io (MemoryHostPointerPropertiesEXT)
getMemoryHostPointerPropertiesEXT device handleType hostPointer = liftIO . evalContT $ do
  let vkGetMemoryHostPointerPropertiesEXTPtr = pVkGetMemoryHostPointerPropertiesEXT (deviceCmds (device :: Device))
  lift $ unless (vkGetMemoryHostPointerPropertiesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryHostPointerPropertiesEXT is null" Nothing Nothing
  let vkGetMemoryHostPointerPropertiesEXT' = mkVkGetMemoryHostPointerPropertiesEXT vkGetMemoryHostPointerPropertiesEXTPtr
  pPMemoryHostPointerProperties <- ContT (withZeroCStruct @MemoryHostPointerPropertiesEXT)
  r <- lift $ vkGetMemoryHostPointerPropertiesEXT' (deviceHandle (device)) (handleType) (hostPointer) (pPMemoryHostPointerProperties)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryHostPointerProperties <- lift $ peekCStruct @MemoryHostPointerPropertiesEXT pPMemoryHostPointerProperties
  pure $ (pMemoryHostPointerProperties)



-- No documentation found for TopLevel "VkImportMemoryHostPointerInfoEXT"
data ImportMemoryHostPointerInfoEXT = ImportMemoryHostPointerInfoEXT
  { -- No documentation found for Nested "VkImportMemoryHostPointerInfoEXT" "handleType"
    handleType :: ExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportMemoryHostPointerInfoEXT" "pHostPointer"
    hostPointer :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMemoryHostPointerInfoEXT)
#endif
deriving instance Show ImportMemoryHostPointerInfoEXT

instance ToCStruct ImportMemoryHostPointerInfoEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMemoryHostPointerInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (hostPointer)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct ImportMemoryHostPointerInfoEXT where
  peekCStruct p = do
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pHostPointer <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ ImportMemoryHostPointerInfoEXT
             handleType pHostPointer


instance Storable ImportMemoryHostPointerInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMemoryHostPointerInfoEXT where
  zero = ImportMemoryHostPointerInfoEXT
           zero
           zero



-- No documentation found for TopLevel "VkMemoryHostPointerPropertiesEXT"
data MemoryHostPointerPropertiesEXT = MemoryHostPointerPropertiesEXT
  { -- No documentation found for Nested "VkMemoryHostPointerPropertiesEXT" "memoryTypeBits"
    memoryTypeBits :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryHostPointerPropertiesEXT)
#endif
deriving instance Show MemoryHostPointerPropertiesEXT

instance ToCStruct MemoryHostPointerPropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryHostPointerPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (memoryTypeBits)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct MemoryHostPointerPropertiesEXT where
  peekCStruct p = do
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ MemoryHostPointerPropertiesEXT
             memoryTypeBits


instance Storable MemoryHostPointerPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryHostPointerPropertiesEXT where
  zero = MemoryHostPointerPropertiesEXT
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceExternalMemoryHostPropertiesEXT"
data PhysicalDeviceExternalMemoryHostPropertiesEXT = PhysicalDeviceExternalMemoryHostPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceExternalMemoryHostPropertiesEXT" "minImportedHostPointerAlignment"
    minImportedHostPointerAlignment :: DeviceSize }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalMemoryHostPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceExternalMemoryHostPropertiesEXT

instance ToCStruct PhysicalDeviceExternalMemoryHostPropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalMemoryHostPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (minImportedHostPointerAlignment)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct PhysicalDeviceExternalMemoryHostPropertiesEXT where
  peekCStruct p = do
    minImportedHostPointerAlignment <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ PhysicalDeviceExternalMemoryHostPropertiesEXT
             minImportedHostPointerAlignment


instance Storable PhysicalDeviceExternalMemoryHostPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalMemoryHostPropertiesEXT where
  zero = PhysicalDeviceExternalMemoryHostPropertiesEXT
           zero


type EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION"
pattern EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_EXTERNAL_MEMORY_HOST_SPEC_VERSION = 1


type EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME = "VK_EXT_external_memory_host"

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME"
pattern EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_EXTERNAL_MEMORY_HOST_EXTENSION_NAME = "VK_EXT_external_memory_host"

