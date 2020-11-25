{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_external_semaphore_win32"
module Vulkan.Extensions.VK_KHR_external_semaphore_win32  ( getSemaphoreWin32HandleKHR
                                                          , importSemaphoreWin32HandleKHR
                                                          , ImportSemaphoreWin32HandleInfoKHR(..)
                                                          , ExportSemaphoreWin32HandleInfoKHR(..)
                                                          , D3D12FenceSubmitInfoKHR(..)
                                                          , SemaphoreGetWin32HandleInfoKHR(..)
                                                          , KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION
                                                          , pattern KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION
                                                          , KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
                                                          , pattern KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME
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
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Extensions.VK_NV_external_memory_win32 (DWORD)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetSemaphoreWin32HandleKHR))
import Vulkan.Dynamic (DeviceCmds(pVkImportSemaphoreWin32HandleKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (HANDLE)
import Vulkan.Extensions.VK_KHR_external_memory_win32 (LPCWSTR)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.VK_NV_external_memory_win32 (SECURITY_ATTRIBUTES)
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.Core11.Enums.SemaphoreImportFlagBits (SemaphoreImportFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_NV_external_memory_win32 (DWORD)
import Vulkan.Extensions.VK_NV_external_memory_win32 (HANDLE)
import Vulkan.Extensions.VK_KHR_external_memory_win32 (LPCWSTR)
import Vulkan.Extensions.VK_NV_external_memory_win32 (SECURITY_ATTRIBUTES)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSemaphoreWin32HandleKHR
  :: FunPtr (Ptr Device_T -> Ptr SemaphoreGetWin32HandleInfoKHR -> Ptr HANDLE -> IO Result) -> Ptr Device_T -> Ptr SemaphoreGetWin32HandleInfoKHR -> Ptr HANDLE -> IO Result

-- No documentation found for TopLevel "vkGetSemaphoreWin32HandleKHR"
getSemaphoreWin32HandleKHR :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkGetSemaphoreWin32HandleKHR" "device"
                              Device
                           -> -- No documentation found for Nested "vkGetSemaphoreWin32HandleKHR" "pGetWin32HandleInfo"
                              SemaphoreGetWin32HandleInfoKHR
                           -> io (HANDLE)
getSemaphoreWin32HandleKHR device getWin32HandleInfo = liftIO . evalContT $ do
  let vkGetSemaphoreWin32HandleKHRPtr = pVkGetSemaphoreWin32HandleKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetSemaphoreWin32HandleKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSemaphoreWin32HandleKHR is null" Nothing Nothing
  let vkGetSemaphoreWin32HandleKHR' = mkVkGetSemaphoreWin32HandleKHR vkGetSemaphoreWin32HandleKHRPtr
  pGetWin32HandleInfo <- ContT $ withCStruct (getWin32HandleInfo)
  pPHandle <- ContT $ bracket (callocBytes @HANDLE 8) free
  r <- lift $ vkGetSemaphoreWin32HandleKHR' (deviceHandle (device)) pGetWin32HandleInfo (pPHandle)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pHandle <- lift $ peek @HANDLE pPHandle
  pure $ (pHandle)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportSemaphoreWin32HandleKHR
  :: FunPtr (Ptr Device_T -> Ptr ImportSemaphoreWin32HandleInfoKHR -> IO Result) -> Ptr Device_T -> Ptr ImportSemaphoreWin32HandleInfoKHR -> IO Result

-- No documentation found for TopLevel "vkImportSemaphoreWin32HandleKHR"
importSemaphoreWin32HandleKHR :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkImportSemaphoreWin32HandleKHR" "device"
                                 Device
                              -> -- No documentation found for Nested "vkImportSemaphoreWin32HandleKHR" "pImportSemaphoreWin32HandleInfo"
                                 ImportSemaphoreWin32HandleInfoKHR
                              -> io ()
importSemaphoreWin32HandleKHR device importSemaphoreWin32HandleInfo = liftIO . evalContT $ do
  let vkImportSemaphoreWin32HandleKHRPtr = pVkImportSemaphoreWin32HandleKHR (deviceCmds (device :: Device))
  lift $ unless (vkImportSemaphoreWin32HandleKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkImportSemaphoreWin32HandleKHR is null" Nothing Nothing
  let vkImportSemaphoreWin32HandleKHR' = mkVkImportSemaphoreWin32HandleKHR vkImportSemaphoreWin32HandleKHRPtr
  pImportSemaphoreWin32HandleInfo <- ContT $ withCStruct (importSemaphoreWin32HandleInfo)
  r <- lift $ vkImportSemaphoreWin32HandleKHR' (deviceHandle (device)) pImportSemaphoreWin32HandleInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkImportSemaphoreWin32HandleInfoKHR"
data ImportSemaphoreWin32HandleInfoKHR = ImportSemaphoreWin32HandleInfoKHR
  { -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "semaphore"
    semaphore :: Semaphore
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "flags"
    flags :: SemaphoreImportFlags
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "handleType"
    handleType :: ExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "handle"
    handle :: HANDLE
  , -- No documentation found for Nested "VkImportSemaphoreWin32HandleInfoKHR" "name"
    name :: LPCWSTR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportSemaphoreWin32HandleInfoKHR)
#endif
deriving instance Show ImportSemaphoreWin32HandleInfoKHR

instance ToCStruct ImportSemaphoreWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportSemaphoreWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr SemaphoreImportFlags)) (flags)
    poke ((p `plusPtr` 28 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 32 :: Ptr HANDLE)) (handle)
    poke ((p `plusPtr` 40 :: Ptr LPCWSTR)) (name)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    f

instance FromCStruct ImportSemaphoreWin32HandleInfoKHR where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    flags <- peek @SemaphoreImportFlags ((p `plusPtr` 24 :: Ptr SemaphoreImportFlags))
    handleType <- peek @ExternalSemaphoreHandleTypeFlagBits ((p `plusPtr` 28 :: Ptr ExternalSemaphoreHandleTypeFlagBits))
    handle <- peek @HANDLE ((p `plusPtr` 32 :: Ptr HANDLE))
    name <- peek @LPCWSTR ((p `plusPtr` 40 :: Ptr LPCWSTR))
    pure $ ImportSemaphoreWin32HandleInfoKHR
             semaphore flags handleType handle name


instance Storable ImportSemaphoreWin32HandleInfoKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportSemaphoreWin32HandleInfoKHR where
  zero = ImportSemaphoreWin32HandleInfoKHR
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkExportSemaphoreWin32HandleInfoKHR"
data ExportSemaphoreWin32HandleInfoKHR = ExportSemaphoreWin32HandleInfoKHR
  { -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "pAttributes"
    attributes :: Ptr SECURITY_ATTRIBUTES
  , -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "dwAccess"
    dwAccess :: DWORD
  , -- No documentation found for Nested "VkExportSemaphoreWin32HandleInfoKHR" "name"
    name :: LPCWSTR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportSemaphoreWin32HandleInfoKHR)
#endif
deriving instance Show ExportSemaphoreWin32HandleInfoKHR

instance ToCStruct ExportSemaphoreWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportSemaphoreWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES))) (attributes)
    poke ((p `plusPtr` 24 :: Ptr DWORD)) (dwAccess)
    poke ((p `plusPtr` 32 :: Ptr LPCWSTR)) (name)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr DWORD)) (zero)
    poke ((p `plusPtr` 32 :: Ptr LPCWSTR)) (zero)
    f

instance FromCStruct ExportSemaphoreWin32HandleInfoKHR where
  peekCStruct p = do
    pAttributes <- peek @(Ptr SECURITY_ATTRIBUTES) ((p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES)))
    dwAccess <- peek @DWORD ((p `plusPtr` 24 :: Ptr DWORD))
    name <- peek @LPCWSTR ((p `plusPtr` 32 :: Ptr LPCWSTR))
    pure $ ExportSemaphoreWin32HandleInfoKHR
             pAttributes dwAccess name


instance Storable ExportSemaphoreWin32HandleInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportSemaphoreWin32HandleInfoKHR where
  zero = ExportSemaphoreWin32HandleInfoKHR
           zero
           zero
           zero



-- No documentation found for TopLevel "VkD3D12FenceSubmitInfoKHR"
data D3D12FenceSubmitInfoKHR = D3D12FenceSubmitInfoKHR
  { -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "waitSemaphoreValuesCount"
    waitSemaphoreValuesCount :: Word32
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "pWaitSemaphoreValues"
    waitSemaphoreValues :: Vector Word64
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "signalSemaphoreValuesCount"
    signalSemaphoreValuesCount :: Word32
  , -- No documentation found for Nested "VkD3D12FenceSubmitInfoKHR" "pSignalSemaphoreValues"
    signalSemaphoreValues :: Vector Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (D3D12FenceSubmitInfoKHR)
#endif
deriving instance Show D3D12FenceSubmitInfoKHR

instance ToCStruct D3D12FenceSubmitInfoKHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p D3D12FenceSubmitInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let pWaitSemaphoreValuesLength = Data.Vector.length $ (waitSemaphoreValues)
    waitSemaphoreValuesCount'' <- lift $ if (waitSemaphoreValuesCount) == 0
      then pure $ fromIntegral pWaitSemaphoreValuesLength
      else do
        unless (fromIntegral pWaitSemaphoreValuesLength == (waitSemaphoreValuesCount) || pWaitSemaphoreValuesLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pWaitSemaphoreValues must be empty or have 'waitSemaphoreValuesCount' elements" Nothing Nothing
        pure (waitSemaphoreValuesCount)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (waitSemaphoreValuesCount'')
    pWaitSemaphoreValues'' <- if Data.Vector.null (waitSemaphoreValues)
      then pure nullPtr
      else do
        pPWaitSemaphoreValues <- ContT $ allocaBytesAligned @Word64 (((Data.Vector.length (waitSemaphoreValues))) * 8) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPWaitSemaphoreValues `plusPtr` (8 * (i)) :: Ptr Word64) (e)) ((waitSemaphoreValues))
        pure $ pPWaitSemaphoreValues
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word64))) pWaitSemaphoreValues''
    let pSignalSemaphoreValuesLength = Data.Vector.length $ (signalSemaphoreValues)
    signalSemaphoreValuesCount'' <- lift $ if (signalSemaphoreValuesCount) == 0
      then pure $ fromIntegral pSignalSemaphoreValuesLength
      else do
        unless (fromIntegral pSignalSemaphoreValuesLength == (signalSemaphoreValuesCount) || pSignalSemaphoreValuesLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pSignalSemaphoreValues must be empty or have 'signalSemaphoreValuesCount' elements" Nothing Nothing
        pure (signalSemaphoreValuesCount)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (signalSemaphoreValuesCount'')
    pSignalSemaphoreValues'' <- if Data.Vector.null (signalSemaphoreValues)
      then pure nullPtr
      else do
        pPSignalSemaphoreValues <- ContT $ allocaBytesAligned @Word64 (((Data.Vector.length (signalSemaphoreValues))) * 8) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pPSignalSemaphoreValues `plusPtr` (8 * (i)) :: Ptr Word64) (e)) ((signalSemaphoreValues))
        pure $ pPSignalSemaphoreValues
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Word64))) pSignalSemaphoreValues''
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct D3D12FenceSubmitInfoKHR where
  peekCStruct p = do
    waitSemaphoreValuesCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pWaitSemaphoreValues <- peek @(Ptr Word64) ((p `plusPtr` 24 :: Ptr (Ptr Word64)))
    let pWaitSemaphoreValuesLength = if pWaitSemaphoreValues == nullPtr then 0 else (fromIntegral waitSemaphoreValuesCount)
    pWaitSemaphoreValues' <- generateM pWaitSemaphoreValuesLength (\i -> peek @Word64 ((pWaitSemaphoreValues `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    signalSemaphoreValuesCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pSignalSemaphoreValues <- peek @(Ptr Word64) ((p `plusPtr` 40 :: Ptr (Ptr Word64)))
    let pSignalSemaphoreValuesLength = if pSignalSemaphoreValues == nullPtr then 0 else (fromIntegral signalSemaphoreValuesCount)
    pSignalSemaphoreValues' <- generateM pSignalSemaphoreValuesLength (\i -> peek @Word64 ((pSignalSemaphoreValues `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
    pure $ D3D12FenceSubmitInfoKHR
             waitSemaphoreValuesCount pWaitSemaphoreValues' signalSemaphoreValuesCount pSignalSemaphoreValues'

instance Zero D3D12FenceSubmitInfoKHR where
  zero = D3D12FenceSubmitInfoKHR
           zero
           mempty
           zero
           mempty



-- No documentation found for TopLevel "VkSemaphoreGetWin32HandleInfoKHR"
data SemaphoreGetWin32HandleInfoKHR = SemaphoreGetWin32HandleInfoKHR
  { -- No documentation found for Nested "VkSemaphoreGetWin32HandleInfoKHR" "semaphore"
    semaphore :: Semaphore
  , -- No documentation found for Nested "VkSemaphoreGetWin32HandleInfoKHR" "handleType"
    handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreGetWin32HandleInfoKHR)
#endif
deriving instance Show SemaphoreGetWin32HandleInfoKHR

instance ToCStruct SemaphoreGetWin32HandleInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreGetWin32HandleInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (zero)
    f

instance FromCStruct SemaphoreGetWin32HandleInfoKHR where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    handleType <- peek @ExternalSemaphoreHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits))
    pure $ SemaphoreGetWin32HandleInfoKHR
             semaphore handleType


instance Storable SemaphoreGetWin32HandleInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SemaphoreGetWin32HandleInfoKHR where
  zero = SemaphoreGetWin32HandleInfoKHR
           zero
           zero


type KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION"
pattern KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_EXTERNAL_SEMAPHORE_WIN32_SPEC_VERSION = 1


type KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME = "VK_KHR_external_semaphore_win32"

-- No documentation found for TopLevel "VK_KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME"
pattern KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_EXTERNAL_SEMAPHORE_WIN32_EXTENSION_NAME = "VK_KHR_external_semaphore_win32"

