{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_external_semaphore_fd"
module Vulkan.Extensions.VK_KHR_external_semaphore_fd  ( getSemaphoreFdKHR
                                                       , importSemaphoreFdKHR
                                                       , ImportSemaphoreFdInfoKHR(..)
                                                       , SemaphoreGetFdInfoKHR(..)
                                                       , KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
                                                       , pattern KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION
                                                       , KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
                                                       , pattern KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME
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
import Foreign.C.Types (CInt(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CInt)
import Foreign.C.Types (CInt(CInt))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetSemaphoreFdKHR))
import Vulkan.Dynamic (DeviceCmds(pVkImportSemaphoreFdKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSemaphoreFdKHR
  :: FunPtr (Ptr Device_T -> Ptr SemaphoreGetFdInfoKHR -> Ptr CInt -> IO Result) -> Ptr Device_T -> Ptr SemaphoreGetFdInfoKHR -> Ptr CInt -> IO Result

-- No documentation found for TopLevel "vkGetSemaphoreFdKHR"
getSemaphoreFdKHR :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkGetSemaphoreFdKHR" "device"
                     Device
                  -> -- No documentation found for Nested "vkGetSemaphoreFdKHR" "pGetFdInfo"
                     SemaphoreGetFdInfoKHR
                  -> io (("fd" ::: Int32))
getSemaphoreFdKHR device getFdInfo = liftIO . evalContT $ do
  let vkGetSemaphoreFdKHRPtr = pVkGetSemaphoreFdKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetSemaphoreFdKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSemaphoreFdKHR is null" Nothing Nothing
  let vkGetSemaphoreFdKHR' = mkVkGetSemaphoreFdKHR vkGetSemaphoreFdKHRPtr
  pGetFdInfo <- ContT $ withCStruct (getFdInfo)
  pPFd <- ContT $ bracket (callocBytes @CInt 4) free
  r <- lift $ vkGetSemaphoreFdKHR' (deviceHandle (device)) pGetFdInfo (pPFd)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFd <- lift $ peek @CInt pPFd
  pure $ (((\(CInt a) -> a) pFd))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportSemaphoreFdKHR
  :: FunPtr (Ptr Device_T -> Ptr ImportSemaphoreFdInfoKHR -> IO Result) -> Ptr Device_T -> Ptr ImportSemaphoreFdInfoKHR -> IO Result

-- No documentation found for TopLevel "vkImportSemaphoreFdKHR"
importSemaphoreFdKHR :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkImportSemaphoreFdKHR" "device"
                        Device
                     -> -- No documentation found for Nested "vkImportSemaphoreFdKHR" "pImportSemaphoreFdInfo"
                        ImportSemaphoreFdInfoKHR
                     -> io ()
importSemaphoreFdKHR device importSemaphoreFdInfo = liftIO . evalContT $ do
  let vkImportSemaphoreFdKHRPtr = pVkImportSemaphoreFdKHR (deviceCmds (device :: Device))
  lift $ unless (vkImportSemaphoreFdKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkImportSemaphoreFdKHR is null" Nothing Nothing
  let vkImportSemaphoreFdKHR' = mkVkImportSemaphoreFdKHR vkImportSemaphoreFdKHRPtr
  pImportSemaphoreFdInfo <- ContT $ withCStruct (importSemaphoreFdInfo)
  r <- lift $ vkImportSemaphoreFdKHR' (deviceHandle (device)) pImportSemaphoreFdInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkImportSemaphoreFdInfoKHR"
data ImportSemaphoreFdInfoKHR = ImportSemaphoreFdInfoKHR
  { -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "semaphore"
    semaphore :: Semaphore
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "flags"
    flags :: SemaphoreImportFlags
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "handleType"
    handleType :: ExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportSemaphoreFdInfoKHR" "fd"
    fd :: Int32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportSemaphoreFdInfoKHR)
#endif
deriving instance Show ImportSemaphoreFdInfoKHR

instance ToCStruct ImportSemaphoreFdInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportSemaphoreFdInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr SemaphoreImportFlags)) (flags)
    poke ((p `plusPtr` 28 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 32 :: Ptr CInt)) (CInt (fd))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (zero)
    poke ((p `plusPtr` 32 :: Ptr CInt)) (CInt (zero))
    f

instance FromCStruct ImportSemaphoreFdInfoKHR where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    flags <- peek @SemaphoreImportFlags ((p `plusPtr` 24 :: Ptr SemaphoreImportFlags))
    handleType <- peek @ExternalSemaphoreHandleTypeFlagBits ((p `plusPtr` 28 :: Ptr ExternalSemaphoreHandleTypeFlagBits))
    fd <- peek @CInt ((p `plusPtr` 32 :: Ptr CInt))
    pure $ ImportSemaphoreFdInfoKHR
             semaphore flags handleType ((\(CInt a) -> a) fd)


instance Storable ImportSemaphoreFdInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportSemaphoreFdInfoKHR where
  zero = ImportSemaphoreFdInfoKHR
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkSemaphoreGetFdInfoKHR"
data SemaphoreGetFdInfoKHR = SemaphoreGetFdInfoKHR
  { -- No documentation found for Nested "VkSemaphoreGetFdInfoKHR" "semaphore"
    semaphore :: Semaphore
  , -- No documentation found for Nested "VkSemaphoreGetFdInfoKHR" "handleType"
    handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreGetFdInfoKHR)
#endif
deriving instance Show SemaphoreGetFdInfoKHR

instance ToCStruct SemaphoreGetFdInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreGetFdInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (zero)
    f

instance FromCStruct SemaphoreGetFdInfoKHR where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    handleType <- peek @ExternalSemaphoreHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits))
    pure $ SemaphoreGetFdInfoKHR
             semaphore handleType


instance Storable SemaphoreGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SemaphoreGetFdInfoKHR where
  zero = SemaphoreGetFdInfoKHR
           zero
           zero


type KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION"
pattern KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_SEMAPHORE_FD_SPEC_VERSION = 1


type KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME = "VK_KHR_external_semaphore_fd"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME"
pattern KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_SEMAPHORE_FD_EXTENSION_NAME = "VK_KHR_external_semaphore_fd"

