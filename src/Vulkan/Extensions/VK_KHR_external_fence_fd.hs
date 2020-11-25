{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_external_fence_fd"
module Vulkan.Extensions.VK_KHR_external_fence_fd  ( getFenceFdKHR
                                                   , importFenceFdKHR
                                                   , ImportFenceFdInfoKHR(..)
                                                   , FenceGetFdInfoKHR(..)
                                                   , KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
                                                   , pattern KHR_EXTERNAL_FENCE_FD_SPEC_VERSION
                                                   , KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
                                                   , pattern KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME
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
import Vulkan.Dynamic (DeviceCmds(pVkGetFenceFdKHR))
import Vulkan.Dynamic (DeviceCmds(pVkImportFenceFdKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlagBits)
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core11.Enums.FenceImportFlagBits (FenceImportFlags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceFdKHR
  :: FunPtr (Ptr Device_T -> Ptr FenceGetFdInfoKHR -> Ptr CInt -> IO Result) -> Ptr Device_T -> Ptr FenceGetFdInfoKHR -> Ptr CInt -> IO Result

-- No documentation found for TopLevel "vkGetFenceFdKHR"
getFenceFdKHR :: forall io
               . (MonadIO io)
              => -- No documentation found for Nested "vkGetFenceFdKHR" "device"
                 Device
              -> -- No documentation found for Nested "vkGetFenceFdKHR" "pGetFdInfo"
                 FenceGetFdInfoKHR
              -> io (("fd" ::: Int32))
getFenceFdKHR device getFdInfo = liftIO . evalContT $ do
  let vkGetFenceFdKHRPtr = pVkGetFenceFdKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetFenceFdKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetFenceFdKHR is null" Nothing Nothing
  let vkGetFenceFdKHR' = mkVkGetFenceFdKHR vkGetFenceFdKHRPtr
  pGetFdInfo <- ContT $ withCStruct (getFdInfo)
  pPFd <- ContT $ bracket (callocBytes @CInt 4) free
  r <- lift $ vkGetFenceFdKHR' (deviceHandle (device)) pGetFdInfo (pPFd)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFd <- lift $ peek @CInt pPFd
  pure $ (((\(CInt a) -> a) pFd))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportFenceFdKHR
  :: FunPtr (Ptr Device_T -> Ptr ImportFenceFdInfoKHR -> IO Result) -> Ptr Device_T -> Ptr ImportFenceFdInfoKHR -> IO Result

-- No documentation found for TopLevel "vkImportFenceFdKHR"
importFenceFdKHR :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkImportFenceFdKHR" "device"
                    Device
                 -> -- No documentation found for Nested "vkImportFenceFdKHR" "pImportFenceFdInfo"
                    ImportFenceFdInfoKHR
                 -> io ()
importFenceFdKHR device importFenceFdInfo = liftIO . evalContT $ do
  let vkImportFenceFdKHRPtr = pVkImportFenceFdKHR (deviceCmds (device :: Device))
  lift $ unless (vkImportFenceFdKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkImportFenceFdKHR is null" Nothing Nothing
  let vkImportFenceFdKHR' = mkVkImportFenceFdKHR vkImportFenceFdKHRPtr
  pImportFenceFdInfo <- ContT $ withCStruct (importFenceFdInfo)
  r <- lift $ vkImportFenceFdKHR' (deviceHandle (device)) pImportFenceFdInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkImportFenceFdInfoKHR"
data ImportFenceFdInfoKHR = ImportFenceFdInfoKHR
  { -- No documentation found for Nested "VkImportFenceFdInfoKHR" "fence"
    fence :: Fence
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "flags"
    flags :: FenceImportFlags
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "handleType"
    handleType :: ExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportFenceFdInfoKHR" "fd"
    fd :: Int32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportFenceFdInfoKHR)
#endif
deriving instance Show ImportFenceFdInfoKHR

instance ToCStruct ImportFenceFdInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportFenceFdInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (fence)
    poke ((p `plusPtr` 24 :: Ptr FenceImportFlags)) (flags)
    poke ((p `plusPtr` 28 :: Ptr ExternalFenceHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 32 :: Ptr CInt)) (CInt (fd))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ExternalFenceHandleTypeFlagBits)) (zero)
    poke ((p `plusPtr` 32 :: Ptr CInt)) (CInt (zero))
    f

instance FromCStruct ImportFenceFdInfoKHR where
  peekCStruct p = do
    fence <- peek @Fence ((p `plusPtr` 16 :: Ptr Fence))
    flags <- peek @FenceImportFlags ((p `plusPtr` 24 :: Ptr FenceImportFlags))
    handleType <- peek @ExternalFenceHandleTypeFlagBits ((p `plusPtr` 28 :: Ptr ExternalFenceHandleTypeFlagBits))
    fd <- peek @CInt ((p `plusPtr` 32 :: Ptr CInt))
    pure $ ImportFenceFdInfoKHR
             fence flags handleType ((\(CInt a) -> a) fd)


instance Storable ImportFenceFdInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportFenceFdInfoKHR where
  zero = ImportFenceFdInfoKHR
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkFenceGetFdInfoKHR"
data FenceGetFdInfoKHR = FenceGetFdInfoKHR
  { -- No documentation found for Nested "VkFenceGetFdInfoKHR" "fence"
    fence :: Fence
  , -- No documentation found for Nested "VkFenceGetFdInfoKHR" "handleType"
    handleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FenceGetFdInfoKHR)
#endif
deriving instance Show FenceGetFdInfoKHR

instance ToCStruct FenceGetFdInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FenceGetFdInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (fence)
    poke ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits)) (zero)
    f

instance FromCStruct FenceGetFdInfoKHR where
  peekCStruct p = do
    fence <- peek @Fence ((p `plusPtr` 16 :: Ptr Fence))
    handleType <- peek @ExternalFenceHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits))
    pure $ FenceGetFdInfoKHR
             fence handleType


instance Storable FenceGetFdInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FenceGetFdInfoKHR where
  zero = FenceGetFdInfoKHR
           zero
           zero


type KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_FD_SPEC_VERSION"
pattern KHR_EXTERNAL_FENCE_FD_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_FENCE_FD_SPEC_VERSION = 1


type KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME = "VK_KHR_external_fence_fd"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME"
pattern KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_FENCE_FD_EXTENSION_NAME = "VK_KHR_external_fence_fd"

