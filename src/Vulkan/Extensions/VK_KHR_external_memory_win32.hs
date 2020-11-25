{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_external_memory_win32"
module Vulkan.Extensions.VK_KHR_external_memory_win32  ( getMemoryWin32HandleKHR
                                                       , getMemoryWin32HandlePropertiesKHR
                                                       , ImportMemoryWin32HandleInfoKHR(..)
                                                       , ExportMemoryWin32HandleInfoKHR(..)
                                                       , MemoryWin32HandlePropertiesKHR(..)
                                                       , MemoryGetWin32HandleInfoKHR(..)
                                                       , KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
                                                       , pattern KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION
                                                       , KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
                                                       , pattern KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME
                                                       , LPCWSTR
                                                       , HANDLE
                                                       , DWORD
                                                       , SECURITY_ATTRIBUTES
                                                       ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
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
import Foreign.C.Types (CWchar)
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
import Vulkan.Extensions.VK_NV_external_memory_win32 (DWORD)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryWin32HandleKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryWin32HandlePropertiesKHR))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (HANDLE)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (SECURITY_ATTRIBUTES)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_NV_external_memory_win32 (DWORD)
import Vulkan.Extensions.VK_NV_external_memory_win32 (HANDLE)
import Vulkan.Extensions.VK_NV_external_memory_win32 (SECURITY_ATTRIBUTES)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryWin32HandleKHR
  :: FunPtr (Ptr Device_T -> Ptr MemoryGetWin32HandleInfoKHR -> Ptr HANDLE -> IO Result) -> Ptr Device_T -> Ptr MemoryGetWin32HandleInfoKHR -> Ptr HANDLE -> IO Result

-- No documentation found for TopLevel "vkGetMemoryWin32HandleKHR"
getMemoryWin32HandleKHR :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkGetMemoryWin32HandleKHR" "device"
                           Device
                        -> -- No documentation found for Nested "vkGetMemoryWin32HandleKHR" "pGetWin32HandleInfo"
                           MemoryGetWin32HandleInfoKHR
                        -> io (HANDLE)
getMemoryWin32HandleKHR device getWin32HandleInfo = liftIO . evalContT $ do
  let vkGetMemoryWin32HandleKHRPtr = pVkGetMemoryWin32HandleKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetMemoryWin32HandleKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryWin32HandleKHR is null" Nothing Nothing
  let vkGetMemoryWin32HandleKHR' = mkVkGetMemoryWin32HandleKHR vkGetMemoryWin32HandleKHRPtr
  pGetWin32HandleInfo <- ContT $ withCStruct (getWin32HandleInfo)
  pPHandle <- ContT $ bracket (callocBytes @HANDLE 8) free
  r <- lift $ vkGetMemoryWin32HandleKHR' (deviceHandle (device)) pGetWin32HandleInfo (pPHandle)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pHandle <- lift $ peek @HANDLE pPHandle
  pure $ (pHandle)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryWin32HandlePropertiesKHR
  :: FunPtr (Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> HANDLE -> Ptr MemoryWin32HandlePropertiesKHR -> IO Result) -> Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> HANDLE -> Ptr MemoryWin32HandlePropertiesKHR -> IO Result

-- No documentation found for TopLevel "vkGetMemoryWin32HandlePropertiesKHR"
getMemoryWin32HandlePropertiesKHR :: forall io
                                   . (MonadIO io)
                                  => -- No documentation found for Nested "vkGetMemoryWin32HandlePropertiesKHR" "device"
                                     Device
                                  -> -- No documentation found for Nested "vkGetMemoryWin32HandlePropertiesKHR" "handleType"
                                     ExternalMemoryHandleTypeFlagBits
                                  -> -- No documentation found for Nested "vkGetMemoryWin32HandlePropertiesKHR" "handle"
                                     HANDLE
                                  -> io (MemoryWin32HandlePropertiesKHR)
getMemoryWin32HandlePropertiesKHR device handleType handle = liftIO . evalContT $ do
  let vkGetMemoryWin32HandlePropertiesKHRPtr = pVkGetMemoryWin32HandlePropertiesKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetMemoryWin32HandlePropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryWin32HandlePropertiesKHR is null" Nothing Nothing
  let vkGetMemoryWin32HandlePropertiesKHR' = mkVkGetMemoryWin32HandlePropertiesKHR vkGetMemoryWin32HandlePropertiesKHRPtr
  pPMemoryWin32HandleProperties <- ContT (withZeroCStruct @MemoryWin32HandlePropertiesKHR)
  r <- lift $ vkGetMemoryWin32HandlePropertiesKHR' (deviceHandle (device)) (handleType) (handle) (pPMemoryWin32HandleProperties)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemoryWin32HandleProperties <- lift $ peekCStruct @MemoryWin32HandlePropertiesKHR pPMemoryWin32HandleProperties
  pure $ (pMemoryWin32HandleProperties)



-- No documentation found for TopLevel "VkImportMemoryWin32HandleInfoKHR"
data ImportMemoryWin32HandleInfoKHR = ImportMemoryWin32HandleInfoKHR
  { -- No documentation found for Nested "VkImportMemoryWin32HandleInfoKHR" "handleType"
    handleType :: ExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportMemoryWin32HandleInfoKHR" "handle"
    handle :: HANDLE
  , -- No documentation found for Nested "VkImportMemoryWin32HandleInfoKHR" "name"
    name :: LPCWSTR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMemoryWin32HandleInfoKHR)
#endif
deriving instance Show ImportMemoryWin32HandleInfoKHR

instance ToCStruct ImportMemoryWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMemoryWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 24 :: Ptr HANDLE)) (handle)
    poke ((p `plusPtr` 32 :: Ptr LPCWSTR)) (name)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ImportMemoryWin32HandleInfoKHR where
  peekCStruct p = do
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits))
    handle <- peek @HANDLE ((p `plusPtr` 24 :: Ptr HANDLE))
    name <- peek @LPCWSTR ((p `plusPtr` 32 :: Ptr LPCWSTR))
    pure $ ImportMemoryWin32HandleInfoKHR
             handleType handle name


instance Storable ImportMemoryWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMemoryWin32HandleInfoKHR where
  zero = ImportMemoryWin32HandleInfoKHR
           zero
           zero
           zero



-- No documentation found for TopLevel "VkExportMemoryWin32HandleInfoKHR"
data ExportMemoryWin32HandleInfoKHR = ExportMemoryWin32HandleInfoKHR
  { -- No documentation found for Nested "VkExportMemoryWin32HandleInfoKHR" "pAttributes"
    attributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "VkExportMemoryWin32HandleInfoKHR" "dwAccess"
    dwAccess :: DWORD
  , -- No documentation found for Nested "VkExportMemoryWin32HandleInfoKHR" "name"
    name :: LPCWSTR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMemoryWin32HandleInfoKHR)
#endif
deriving instance Show ExportMemoryWin32HandleInfoKHR

instance ToCStruct ExportMemoryWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMemoryWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES))) (attributes)
    poke ((p `plusPtr` 24 :: Ptr DWORD)) (dwAccess)
    poke ((p `plusPtr` 32 :: Ptr LPCWSTR)) (name)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr DWORD)) (zero)
    poke ((p `plusPtr` 32 :: Ptr LPCWSTR)) (zero)
    f

instance FromCStruct ExportMemoryWin32HandleInfoKHR where
  peekCStruct p = do
    pAttributes <- peek @(Ptr SECURITY_ATTRIBUTES) ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES)))
    dwAccess <- peek @DWORD ((p `plusPtr` 24 :: Ptr DWORD))
    name <- peek @LPCWSTR ((p `plusPtr` 32 :: Ptr LPCWSTR))
    pure $ ExportMemoryWin32HandleInfoKHR
             pAttributes dwAccess name


instance Storable ExportMemoryWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMemoryWin32HandleInfoKHR where
  zero = ExportMemoryWin32HandleInfoKHR
           zero
           zero
           zero



-- No documentation found for TopLevel "VkMemoryWin32HandlePropertiesKHR"
data MemoryWin32HandlePropertiesKHR = MemoryWin32HandlePropertiesKHR
  { -- No documentation found for Nested "VkMemoryWin32HandlePropertiesKHR" "memoryTypeBits"
    memoryTypeBits :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryWin32HandlePropertiesKHR)
#endif
deriving instance Show MemoryWin32HandlePropertiesKHR

instance ToCStruct MemoryWin32HandlePropertiesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryWin32HandlePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (memoryTypeBits)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct MemoryWin32HandlePropertiesKHR where
  peekCStruct p = do
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ MemoryWin32HandlePropertiesKHR
             memoryTypeBits


instance Storable MemoryWin32HandlePropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryWin32HandlePropertiesKHR where
  zero = MemoryWin32HandlePropertiesKHR
           zero



-- No documentation found for TopLevel "VkMemoryGetWin32HandleInfoKHR"
data MemoryGetWin32HandleInfoKHR = MemoryGetWin32HandleInfoKHR
  { -- No documentation found for Nested "VkMemoryGetWin32HandleInfoKHR" "memory"
    memory :: DeviceMemory
  , -- No documentation found for Nested "VkMemoryGetWin32HandleInfoKHR" "handleType"
    handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryGetWin32HandleInfoKHR)
#endif
deriving instance Show MemoryGetWin32HandleInfoKHR

instance ToCStruct MemoryGetWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryGetWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (zero)
    f

instance FromCStruct MemoryGetWin32HandleInfoKHR where
  peekCStruct p = do
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pure $ MemoryGetWin32HandleInfoKHR
             memory handleType


instance Storable MemoryGetWin32HandleInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryGetWin32HandleInfoKHR where
  zero = MemoryGetWin32HandleInfoKHR
           zero
           zero


type KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION"
pattern KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_MEMORY_WIN32_SPEC_VERSION = 1


type KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_KHR_external_memory_win32"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME"
pattern KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_MEMORY_WIN32_EXTENSION_NAME = "VK_KHR_external_memory_win32"


type LPCWSTR = Ptr CWchar

