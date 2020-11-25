{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_external_fence_win32"
module Vulkan.Extensions.VK_KHR_external_fence_win32  ( getFenceWin32HandleKHR
                                                      , importFenceWin32HandleKHR
                                                      , ImportFenceWin32HandleInfoKHR(..)
                                                      , ExportFenceWin32HandleInfoKHR(..)
                                                      , FenceGetWin32HandleInfoKHR(..)
                                                      , KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
                                                      , pattern KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION
                                                      , KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
                                                      , pattern KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME
                                                      , HANDLE
                                                      , DWORD
                                                      , LPCWSTR
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
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (DWORD)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetFenceWin32HandleKHR))
import Vulkan.Dynamic (DeviceCmds(pVkImportFenceWin32HandleKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlagBits)
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (HANDLE)
import Vulkan.Extensions.VK_KHR_external_memory_win32 (LPCWSTR)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (SECURITY_ATTRIBUTES)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_NV_external_memory_win32 (DWORD)
import Vulkan.Extensions.VK_NV_external_memory_win32 (HANDLE)
import Vulkan.Extensions.VK_KHR_external_memory_win32 (LPCWSTR)
import Vulkan.Extensions.VK_NV_external_memory_win32 (SECURITY_ATTRIBUTES)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceWin32HandleKHR
  :: FunPtr (Ptr Device_T -> Ptr FenceGetWin32HandleInfoKHR -> Ptr HANDLE -> IO Result) -> Ptr Device_T -> Ptr FenceGetWin32HandleInfoKHR -> Ptr HANDLE -> IO Result

-- No documentation found for TopLevel "vkGetFenceWin32HandleKHR"
getFenceWin32HandleKHR :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkGetFenceWin32HandleKHR" "device"
                          Device
                       -> -- No documentation found for Nested "vkGetFenceWin32HandleKHR" "pGetWin32HandleInfo"
                          FenceGetWin32HandleInfoKHR
                       -> io (HANDLE)
getFenceWin32HandleKHR device getWin32HandleInfo = liftIO . evalContT $ do
  let vkGetFenceWin32HandleKHRPtr = pVkGetFenceWin32HandleKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetFenceWin32HandleKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetFenceWin32HandleKHR is null" Nothing Nothing
  let vkGetFenceWin32HandleKHR' = mkVkGetFenceWin32HandleKHR vkGetFenceWin32HandleKHRPtr
  pGetWin32HandleInfo <- ContT $ withCStruct (getWin32HandleInfo)
  pPHandle <- ContT $ bracket (callocBytes @HANDLE 8) free
  r <- lift $ vkGetFenceWin32HandleKHR' (deviceHandle (device)) pGetWin32HandleInfo (pPHandle)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pHandle <- lift $ peek @HANDLE pPHandle
  pure $ (pHandle)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportFenceWin32HandleKHR
  :: FunPtr (Ptr Device_T -> Ptr ImportFenceWin32HandleInfoKHR -> IO Result) -> Ptr Device_T -> Ptr ImportFenceWin32HandleInfoKHR -> IO Result

-- No documentation found for TopLevel "vkImportFenceWin32HandleKHR"
importFenceWin32HandleKHR :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkImportFenceWin32HandleKHR" "device"
                             Device
                          -> -- No documentation found for Nested "vkImportFenceWin32HandleKHR" "pImportFenceWin32HandleInfo"
                             ImportFenceWin32HandleInfoKHR
                          -> io ()
importFenceWin32HandleKHR device importFenceWin32HandleInfo = liftIO . evalContT $ do
  let vkImportFenceWin32HandleKHRPtr = pVkImportFenceWin32HandleKHR (deviceCmds (device :: Device))
  lift $ unless (vkImportFenceWin32HandleKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkImportFenceWin32HandleKHR is null" Nothing Nothing
  let vkImportFenceWin32HandleKHR' = mkVkImportFenceWin32HandleKHR vkImportFenceWin32HandleKHRPtr
  pImportFenceWin32HandleInfo <- ContT $ withCStruct (importFenceWin32HandleInfo)
  r <- lift $ vkImportFenceWin32HandleKHR' (deviceHandle (device)) pImportFenceWin32HandleInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkImportFenceWin32HandleInfoKHR"
data ImportFenceWin32HandleInfoKHR = ImportFenceWin32HandleInfoKHR
  { -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "fence"
    fence :: Fence
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "flags"
    flags :: FenceImportFlags
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "handleType"
    handleType :: ExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "handle"
    handle :: HANDLE
  , -- No documentation found for Nested "VkImportFenceWin32HandleInfoKHR" "name"
    name :: LPCWSTR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportFenceWin32HandleInfoKHR)
#endif
deriving instance Show ImportFenceWin32HandleInfoKHR

instance ToCStruct ImportFenceWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportFenceWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (fence)
    poke ((p `plusPtr` 24 :: Ptr FenceImportFlags)) (flags)
    poke ((p `plusPtr` 28 :: Ptr ExternalFenceHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 32 :: Ptr HANDLE)) (handle)
    poke ((p `plusPtr` 40 :: Ptr LPCWSTR)) (name)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (zero)
    f

instance FromCStruct ImportFenceWin32HandleInfoKHR where
  peekCStruct p = do
    fence <- peek @Fence ((p `plusPtr` 16 :: Ptr Fence))
    flags <- peek @FenceImportFlags ((p `plusPtr` 24 :: Ptr FenceImportFlags))
    handleType <- peek @ExternalFenceHandleTypeFlagBits ((p `plusPtr` 28 :: Ptr ExternalFenceHandleTypeFlagBits))
    handle <- peek @HANDLE ((p `plusPtr` 32 :: Ptr HANDLE))
    name <- peek @LPCWSTR ((p `plusPtr` 40 :: Ptr LPCWSTR))
    pure $ ImportFenceWin32HandleInfoKHR
             fence flags handleType handle name


instance Storable ImportFenceWin32HandleInfoKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportFenceWin32HandleInfoKHR where
  zero = ImportFenceWin32HandleInfoKHR
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkExportFenceWin32HandleInfoKHR"
data ExportFenceWin32HandleInfoKHR = ExportFenceWin32HandleInfoKHR
  { -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "pAttributes"
    attributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "dwAccess"
    dwAccess :: DWORD
  , -- No documentation found for Nested "VkExportFenceWin32HandleInfoKHR" "name"
    name :: LPCWSTR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportFenceWin32HandleInfoKHR)
#endif
deriving instance Show ExportFenceWin32HandleInfoKHR

instance ToCStruct ExportFenceWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportFenceWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES))) (attributes)
    poke ((p `plusPtr` 24 :: Ptr DWORD)) (dwAccess)
    poke ((p `plusPtr` 32 :: Ptr LPCWSTR)) (name)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr DWORD)) (zero)
    poke ((p `plusPtr` 32 :: Ptr LPCWSTR)) (zero)
    f

instance FromCStruct ExportFenceWin32HandleInfoKHR where
  peekCStruct p = do
    pAttributes <- peek @(Ptr SECURITY_ATTRIBUTES) ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES)))
    dwAccess <- peek @DWORD ((p `plusPtr` 24 :: Ptr DWORD))
    name <- peek @LPCWSTR ((p `plusPtr` 32 :: Ptr LPCWSTR))
    pure $ ExportFenceWin32HandleInfoKHR
             pAttributes dwAccess name


instance Storable ExportFenceWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportFenceWin32HandleInfoKHR where
  zero = ExportFenceWin32HandleInfoKHR
           zero
           zero
           zero



-- No documentation found for TopLevel "VkFenceGetWin32HandleInfoKHR"
data FenceGetWin32HandleInfoKHR = FenceGetWin32HandleInfoKHR
  { -- No documentation found for Nested "VkFenceGetWin32HandleInfoKHR" "fence"
    fence :: Fence
  , -- No documentation found for Nested "VkFenceGetWin32HandleInfoKHR" "handleType"
    handleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FenceGetWin32HandleInfoKHR)
#endif
deriving instance Show FenceGetWin32HandleInfoKHR

instance ToCStruct FenceGetWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FenceGetWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (fence)
    poke ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits)) (zero)
    f

instance FromCStruct FenceGetWin32HandleInfoKHR where
  peekCStruct p = do
    fence <- peek @Fence ((p `plusPtr` 16 :: Ptr Fence))
    handleType <- peek @ExternalFenceHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits))
    pure $ FenceGetWin32HandleInfoKHR
             fence handleType


instance Storable FenceGetWin32HandleInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FenceGetWin32HandleInfoKHR where
  zero = FenceGetWin32HandleInfoKHR
           zero
           zero


type KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION"
pattern KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_FENCE_WIN32_SPEC_VERSION = 1


type KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME = "VK_KHR_external_fence_win32"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME"
pattern KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_FENCE_WIN32_EXTENSION_NAME = "VK_KHR_external_fence_win32"

