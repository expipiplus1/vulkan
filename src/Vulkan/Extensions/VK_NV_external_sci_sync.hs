{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_external_sci_sync"
module Vulkan.Extensions.VK_NV_external_sci_sync  ( getFenceSciSyncFenceNV
                                                  , getFenceSciSyncObjNV
                                                  , importFenceSciSyncFenceNV
                                                  , importFenceSciSyncObjNV
                                                  , getSemaphoreSciSyncObjNV
                                                  , importSemaphoreSciSyncObjNV
                                                  , getPhysicalDeviceSciSyncAttributesNV
                                                  , ExportFenceSciSyncInfoNV(..)
                                                  , ImportFenceSciSyncInfoNV(..)
                                                  , FenceGetSciSyncInfoNV(..)
                                                  , ExportSemaphoreSciSyncInfoNV(..)
                                                  , ImportSemaphoreSciSyncInfoNV(..)
                                                  , SemaphoreGetSciSyncInfoNV(..)
                                                  , SciSyncAttributesInfoNV(..)
                                                  , PhysicalDeviceExternalSciSyncFeaturesNV(..)
                                                  , SciSyncClientTypeNV( SCI_SYNC_CLIENT_TYPE_SIGNALER_NV
                                                                       , SCI_SYNC_CLIENT_TYPE_WAITER_NV
                                                                       , SCI_SYNC_CLIENT_TYPE_SIGNALER_WAITER_NV
                                                                       , ..
                                                                       )
                                                  , SciSyncPrimitiveTypeNV( SCI_SYNC_PRIMITIVE_TYPE_FENCE_NV
                                                                          , SCI_SYNC_PRIMITIVE_TYPE_SEMAPHORE_NV
                                                                          , ..
                                                                          )
                                                  , NV_EXTERNAL_SCI_SYNC_SPEC_VERSION
                                                  , pattern NV_EXTERNAL_SCI_SYNC_SPEC_VERSION
                                                  , NV_EXTERNAL_SCI_SYNC_EXTENSION_NAME
                                                  , pattern NV_EXTERNAL_SCI_SYNC_EXTENSION_NAME
                                                  , NvSciSyncAttrList
                                                  ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
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
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetFenceSciSyncFenceNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetFenceSciSyncObjNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetSemaphoreSciSyncObjNV))
import Vulkan.Dynamic (DeviceCmds(pVkImportFenceSciSyncFenceNV))
import Vulkan.Dynamic (DeviceCmds(pVkImportFenceSciSyncObjNV))
import Vulkan.Dynamic (DeviceCmds(pVkImportSemaphoreSciSyncObjNV))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalFenceHandleTypeFlagBits (ExternalFenceHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalSemaphoreHandleTypeFlagBits (ExternalSemaphoreHandleTypeFlagBits)
import Vulkan.Core10.Handles (Fence)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSciSyncAttributesNV))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_FENCE_SCI_SYNC_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_SEMAPHORE_SCI_SYNC_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FENCE_GET_SCI_SYNC_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_FENCE_SCI_SYNC_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_SEMAPHORE_SCI_SYNC_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SCI_SYNC_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SCI_SYNC_ATTRIBUTES_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_GET_SCI_SYNC_INFO_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceSciSyncFenceNV
  :: FunPtr (Ptr Device_T -> Ptr FenceGetSciSyncInfoNV -> Ptr () -> IO Result) -> Ptr Device_T -> Ptr FenceGetSciSyncInfoNV -> Ptr () -> IO Result

-- No documentation found for TopLevel "vkGetFenceSciSyncFenceNV"
getFenceSciSyncFenceNV :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkGetFenceSciSyncFenceNV" "device"
                          Device
                       -> -- No documentation found for Nested "vkGetFenceSciSyncFenceNV" "pGetSciSyncHandleInfo"
                          ("getSciSyncHandleInfo" ::: FenceGetSciSyncInfoNV)
                       -> -- No documentation found for Nested "vkGetFenceSciSyncFenceNV" "pHandle"
                          ("handle" ::: Ptr ())
                       -> io ()
getFenceSciSyncFenceNV device
                         getSciSyncHandleInfo
                         handle = liftIO . evalContT $ do
  let vkGetFenceSciSyncFenceNVPtr = pVkGetFenceSciSyncFenceNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetFenceSciSyncFenceNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetFenceSciSyncFenceNV is null" Nothing Nothing
  let vkGetFenceSciSyncFenceNV' = mkVkGetFenceSciSyncFenceNV vkGetFenceSciSyncFenceNVPtr
  pGetSciSyncHandleInfo <- ContT $ withCStruct (getSciSyncHandleInfo)
  r <- lift $ traceAroundEvent "vkGetFenceSciSyncFenceNV" (vkGetFenceSciSyncFenceNV'
                                                             (deviceHandle (device))
                                                             pGetSciSyncHandleInfo
                                                             (handle))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceSciSyncObjNV
  :: FunPtr (Ptr Device_T -> Ptr FenceGetSciSyncInfoNV -> Ptr () -> IO Result) -> Ptr Device_T -> Ptr FenceGetSciSyncInfoNV -> Ptr () -> IO Result

-- No documentation found for TopLevel "vkGetFenceSciSyncObjNV"
getFenceSciSyncObjNV :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkGetFenceSciSyncObjNV" "device"
                        Device
                     -> -- No documentation found for Nested "vkGetFenceSciSyncObjNV" "pGetSciSyncHandleInfo"
                        ("getSciSyncHandleInfo" ::: FenceGetSciSyncInfoNV)
                     -> -- No documentation found for Nested "vkGetFenceSciSyncObjNV" "pHandle"
                        ("handle" ::: Ptr ())
                     -> io ()
getFenceSciSyncObjNV device
                       getSciSyncHandleInfo
                       handle = liftIO . evalContT $ do
  let vkGetFenceSciSyncObjNVPtr = pVkGetFenceSciSyncObjNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetFenceSciSyncObjNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetFenceSciSyncObjNV is null" Nothing Nothing
  let vkGetFenceSciSyncObjNV' = mkVkGetFenceSciSyncObjNV vkGetFenceSciSyncObjNVPtr
  pGetSciSyncHandleInfo <- ContT $ withCStruct (getSciSyncHandleInfo)
  r <- lift $ traceAroundEvent "vkGetFenceSciSyncObjNV" (vkGetFenceSciSyncObjNV'
                                                           (deviceHandle (device))
                                                           pGetSciSyncHandleInfo
                                                           (handle))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportFenceSciSyncFenceNV
  :: FunPtr (Ptr Device_T -> Ptr ImportFenceSciSyncInfoNV -> IO Result) -> Ptr Device_T -> Ptr ImportFenceSciSyncInfoNV -> IO Result

-- No documentation found for TopLevel "vkImportFenceSciSyncFenceNV"
importFenceSciSyncFenceNV :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkImportFenceSciSyncFenceNV" "device"
                             Device
                          -> -- No documentation found for Nested "vkImportFenceSciSyncFenceNV" "pImportFenceSciSyncInfo"
                             ImportFenceSciSyncInfoNV
                          -> io ()
importFenceSciSyncFenceNV device
                            importFenceSciSyncInfo = liftIO . evalContT $ do
  let vkImportFenceSciSyncFenceNVPtr = pVkImportFenceSciSyncFenceNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkImportFenceSciSyncFenceNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkImportFenceSciSyncFenceNV is null" Nothing Nothing
  let vkImportFenceSciSyncFenceNV' = mkVkImportFenceSciSyncFenceNV vkImportFenceSciSyncFenceNVPtr
  pImportFenceSciSyncInfo <- ContT $ withCStruct (importFenceSciSyncInfo)
  r <- lift $ traceAroundEvent "vkImportFenceSciSyncFenceNV" (vkImportFenceSciSyncFenceNV'
                                                                (deviceHandle (device))
                                                                pImportFenceSciSyncInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportFenceSciSyncObjNV
  :: FunPtr (Ptr Device_T -> Ptr ImportFenceSciSyncInfoNV -> IO Result) -> Ptr Device_T -> Ptr ImportFenceSciSyncInfoNV -> IO Result

-- No documentation found for TopLevel "vkImportFenceSciSyncObjNV"
importFenceSciSyncObjNV :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkImportFenceSciSyncObjNV" "device"
                           Device
                        -> -- No documentation found for Nested "vkImportFenceSciSyncObjNV" "pImportFenceSciSyncInfo"
                           ImportFenceSciSyncInfoNV
                        -> io ()
importFenceSciSyncObjNV device importFenceSciSyncInfo = liftIO . evalContT $ do
  let vkImportFenceSciSyncObjNVPtr = pVkImportFenceSciSyncObjNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkImportFenceSciSyncObjNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkImportFenceSciSyncObjNV is null" Nothing Nothing
  let vkImportFenceSciSyncObjNV' = mkVkImportFenceSciSyncObjNV vkImportFenceSciSyncObjNVPtr
  pImportFenceSciSyncInfo <- ContT $ withCStruct (importFenceSciSyncInfo)
  r <- lift $ traceAroundEvent "vkImportFenceSciSyncObjNV" (vkImportFenceSciSyncObjNV'
                                                              (deviceHandle (device))
                                                              pImportFenceSciSyncInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSemaphoreSciSyncObjNV
  :: FunPtr (Ptr Device_T -> Ptr SemaphoreGetSciSyncInfoNV -> Ptr () -> IO Result) -> Ptr Device_T -> Ptr SemaphoreGetSciSyncInfoNV -> Ptr () -> IO Result

-- No documentation found for TopLevel "vkGetSemaphoreSciSyncObjNV"
getSemaphoreSciSyncObjNV :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkGetSemaphoreSciSyncObjNV" "device"
                            Device
                         -> -- No documentation found for Nested "vkGetSemaphoreSciSyncObjNV" "pGetSciSyncInfo"
                            SemaphoreGetSciSyncInfoNV
                         -> -- No documentation found for Nested "vkGetSemaphoreSciSyncObjNV" "pHandle"
                            ("handle" ::: Ptr ())
                         -> io ()
getSemaphoreSciSyncObjNV device getSciSyncInfo handle = liftIO . evalContT $ do
  let vkGetSemaphoreSciSyncObjNVPtr = pVkGetSemaphoreSciSyncObjNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetSemaphoreSciSyncObjNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSemaphoreSciSyncObjNV is null" Nothing Nothing
  let vkGetSemaphoreSciSyncObjNV' = mkVkGetSemaphoreSciSyncObjNV vkGetSemaphoreSciSyncObjNVPtr
  pGetSciSyncInfo <- ContT $ withCStruct (getSciSyncInfo)
  r <- lift $ traceAroundEvent "vkGetSemaphoreSciSyncObjNV" (vkGetSemaphoreSciSyncObjNV'
                                                               (deviceHandle (device))
                                                               pGetSciSyncInfo
                                                               (handle))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportSemaphoreSciSyncObjNV
  :: FunPtr (Ptr Device_T -> Ptr ImportSemaphoreSciSyncInfoNV -> IO Result) -> Ptr Device_T -> Ptr ImportSemaphoreSciSyncInfoNV -> IO Result

-- No documentation found for TopLevel "vkImportSemaphoreSciSyncObjNV"
importSemaphoreSciSyncObjNV :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "vkImportSemaphoreSciSyncObjNV" "device"
                               Device
                            -> -- No documentation found for Nested "vkImportSemaphoreSciSyncObjNV" "pImportSemaphoreSciSyncInfo"
                               ImportSemaphoreSciSyncInfoNV
                            -> io ()
importSemaphoreSciSyncObjNV device
                              importSemaphoreSciSyncInfo = liftIO . evalContT $ do
  let vkImportSemaphoreSciSyncObjNVPtr = pVkImportSemaphoreSciSyncObjNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkImportSemaphoreSciSyncObjNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkImportSemaphoreSciSyncObjNV is null" Nothing Nothing
  let vkImportSemaphoreSciSyncObjNV' = mkVkImportSemaphoreSciSyncObjNV vkImportSemaphoreSciSyncObjNVPtr
  pImportSemaphoreSciSyncInfo <- ContT $ withCStruct (importSemaphoreSciSyncInfo)
  r <- lift $ traceAroundEvent "vkImportSemaphoreSciSyncObjNV" (vkImportSemaphoreSciSyncObjNV'
                                                                  (deviceHandle (device))
                                                                  pImportSemaphoreSciSyncInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSciSyncAttributesNV
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr SciSyncAttributesInfoNV -> NvSciSyncAttrList -> IO Result) -> Ptr PhysicalDevice_T -> Ptr SciSyncAttributesInfoNV -> NvSciSyncAttrList -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceSciSyncAttributesNV"
getPhysicalDeviceSciSyncAttributesNV :: forall io
                                      . (MonadIO io)
                                     => -- No documentation found for Nested "vkGetPhysicalDeviceSciSyncAttributesNV" "physicalDevice"
                                        PhysicalDevice
                                     -> -- No documentation found for Nested "vkGetPhysicalDeviceSciSyncAttributesNV" "pSciSyncAttributesInfo"
                                        SciSyncAttributesInfoNV
                                     -> -- No documentation found for Nested "vkGetPhysicalDeviceSciSyncAttributesNV" "pAttributes"
                                        ("attributes" ::: NvSciSyncAttrList)
                                     -> io ()
getPhysicalDeviceSciSyncAttributesNV physicalDevice
                                       sciSyncAttributesInfo
                                       attributes = liftIO . evalContT $ do
  let vkGetPhysicalDeviceSciSyncAttributesNVPtr = pVkGetPhysicalDeviceSciSyncAttributesNV (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceSciSyncAttributesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSciSyncAttributesNV is null" Nothing Nothing
  let vkGetPhysicalDeviceSciSyncAttributesNV' = mkVkGetPhysicalDeviceSciSyncAttributesNV vkGetPhysicalDeviceSciSyncAttributesNVPtr
  pSciSyncAttributesInfo <- ContT $ withCStruct (sciSyncAttributesInfo)
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceSciSyncAttributesNV" (vkGetPhysicalDeviceSciSyncAttributesNV'
                                                                           (physicalDeviceHandle (physicalDevice))
                                                                           pSciSyncAttributesInfo
                                                                           (attributes))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- No documentation found for TopLevel "VkExportFenceSciSyncInfoNV"
data ExportFenceSciSyncInfoNV = ExportFenceSciSyncInfoNV
  { -- No documentation found for Nested "VkExportFenceSciSyncInfoNV" "pAttributes"
    attributes :: NvSciSyncAttrList }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportFenceSciSyncInfoNV)
#endif
deriving instance Show ExportFenceSciSyncInfoNV

instance ToCStruct ExportFenceSciSyncInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportFenceSciSyncInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_FENCE_SCI_SYNC_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr NvSciSyncAttrList)) (attributes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_FENCE_SCI_SYNC_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr NvSciSyncAttrList)) (zero)
    f

instance FromCStruct ExportFenceSciSyncInfoNV where
  peekCStruct p = do
    pAttributes <- peek @NvSciSyncAttrList ((p `plusPtr` 16 :: Ptr NvSciSyncAttrList))
    pure $ ExportFenceSciSyncInfoNV
             pAttributes

instance Storable ExportFenceSciSyncInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportFenceSciSyncInfoNV where
  zero = ExportFenceSciSyncInfoNV
           zero


-- No documentation found for TopLevel "VkImportFenceSciSyncInfoNV"
data ImportFenceSciSyncInfoNV = ImportFenceSciSyncInfoNV
  { -- No documentation found for Nested "VkImportFenceSciSyncInfoNV" "fence"
    fence :: Fence
  , -- No documentation found for Nested "VkImportFenceSciSyncInfoNV" "handleType"
    handleType :: ExternalFenceHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportFenceSciSyncInfoNV" "handle"
    handle :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportFenceSciSyncInfoNV)
#endif
deriving instance Show ImportFenceSciSyncInfoNV

instance ToCStruct ImportFenceSciSyncInfoNV where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportFenceSciSyncInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_FENCE_SCI_SYNC_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (fence)
    poke ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (handle)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_FENCE_SCI_SYNC_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits)) (zero)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct ImportFenceSciSyncInfoNV where
  peekCStruct p = do
    fence <- peek @Fence ((p `plusPtr` 16 :: Ptr Fence))
    handleType <- peek @ExternalFenceHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits))
    handle <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pure $ ImportFenceSciSyncInfoNV
             fence handleType handle

instance Storable ImportFenceSciSyncInfoNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportFenceSciSyncInfoNV where
  zero = ImportFenceSciSyncInfoNV
           zero
           zero
           zero


-- No documentation found for TopLevel "VkFenceGetSciSyncInfoNV"
data FenceGetSciSyncInfoNV = FenceGetSciSyncInfoNV
  { -- No documentation found for Nested "VkFenceGetSciSyncInfoNV" "fence"
    fence :: Fence
  , -- No documentation found for Nested "VkFenceGetSciSyncInfoNV" "handleType"
    handleType :: ExternalFenceHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FenceGetSciSyncInfoNV)
#endif
deriving instance Show FenceGetSciSyncInfoNV

instance ToCStruct FenceGetSciSyncInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FenceGetSciSyncInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FENCE_GET_SCI_SYNC_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (fence)
    poke ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FENCE_GET_SCI_SYNC_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Fence)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits)) (zero)
    f

instance FromCStruct FenceGetSciSyncInfoNV where
  peekCStruct p = do
    fence <- peek @Fence ((p `plusPtr` 16 :: Ptr Fence))
    handleType <- peek @ExternalFenceHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalFenceHandleTypeFlagBits))
    pure $ FenceGetSciSyncInfoNV
             fence handleType

instance Storable FenceGetSciSyncInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FenceGetSciSyncInfoNV where
  zero = FenceGetSciSyncInfoNV
           zero
           zero


-- No documentation found for TopLevel "VkExportSemaphoreSciSyncInfoNV"
data ExportSemaphoreSciSyncInfoNV = ExportSemaphoreSciSyncInfoNV
  { -- No documentation found for Nested "VkExportSemaphoreSciSyncInfoNV" "pAttributes"
    attributes :: NvSciSyncAttrList }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportSemaphoreSciSyncInfoNV)
#endif
deriving instance Show ExportSemaphoreSciSyncInfoNV

instance ToCStruct ExportSemaphoreSciSyncInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportSemaphoreSciSyncInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_SEMAPHORE_SCI_SYNC_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr NvSciSyncAttrList)) (attributes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_SEMAPHORE_SCI_SYNC_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr NvSciSyncAttrList)) (zero)
    f

instance FromCStruct ExportSemaphoreSciSyncInfoNV where
  peekCStruct p = do
    pAttributes <- peek @NvSciSyncAttrList ((p `plusPtr` 16 :: Ptr NvSciSyncAttrList))
    pure $ ExportSemaphoreSciSyncInfoNV
             pAttributes

instance Storable ExportSemaphoreSciSyncInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportSemaphoreSciSyncInfoNV where
  zero = ExportSemaphoreSciSyncInfoNV
           zero


-- No documentation found for TopLevel "VkImportSemaphoreSciSyncInfoNV"
data ImportSemaphoreSciSyncInfoNV = ImportSemaphoreSciSyncInfoNV
  { -- No documentation found for Nested "VkImportSemaphoreSciSyncInfoNV" "semaphore"
    semaphore :: Semaphore
  , -- No documentation found for Nested "VkImportSemaphoreSciSyncInfoNV" "handleType"
    handleType :: ExternalSemaphoreHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportSemaphoreSciSyncInfoNV" "handle"
    handle :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportSemaphoreSciSyncInfoNV)
#endif
deriving instance Show ImportSemaphoreSciSyncInfoNV

instance ToCStruct ImportSemaphoreSciSyncInfoNV where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportSemaphoreSciSyncInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_SEMAPHORE_SCI_SYNC_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (handle)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_SEMAPHORE_SCI_SYNC_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (zero)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct ImportSemaphoreSciSyncInfoNV where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    handleType <- peek @ExternalSemaphoreHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits))
    handle <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pure $ ImportSemaphoreSciSyncInfoNV
             semaphore handleType handle

instance Storable ImportSemaphoreSciSyncInfoNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportSemaphoreSciSyncInfoNV where
  zero = ImportSemaphoreSciSyncInfoNV
           zero
           zero
           zero


-- No documentation found for TopLevel "VkSemaphoreGetSciSyncInfoNV"
data SemaphoreGetSciSyncInfoNV = SemaphoreGetSciSyncInfoNV
  { -- No documentation found for Nested "VkSemaphoreGetSciSyncInfoNV" "semaphore"
    semaphore :: Semaphore
  , -- No documentation found for Nested "VkSemaphoreGetSciSyncInfoNV" "handleType"
    handleType :: ExternalSemaphoreHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreGetSciSyncInfoNV)
#endif
deriving instance Show SemaphoreGetSciSyncInfoNV

instance ToCStruct SemaphoreGetSciSyncInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreGetSciSyncInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_GET_SCI_SYNC_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (semaphore)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_GET_SCI_SYNC_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits)) (zero)
    f

instance FromCStruct SemaphoreGetSciSyncInfoNV where
  peekCStruct p = do
    semaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    handleType <- peek @ExternalSemaphoreHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalSemaphoreHandleTypeFlagBits))
    pure $ SemaphoreGetSciSyncInfoNV
             semaphore handleType

instance Storable SemaphoreGetSciSyncInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SemaphoreGetSciSyncInfoNV where
  zero = SemaphoreGetSciSyncInfoNV
           zero
           zero


-- No documentation found for TopLevel "VkSciSyncAttributesInfoNV"
data SciSyncAttributesInfoNV = SciSyncAttributesInfoNV
  { -- No documentation found for Nested "VkSciSyncAttributesInfoNV" "clientType"
    clientType :: SciSyncClientTypeNV
  , -- No documentation found for Nested "VkSciSyncAttributesInfoNV" "primitiveType"
    primitiveType :: SciSyncPrimitiveTypeNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SciSyncAttributesInfoNV)
#endif
deriving instance Show SciSyncAttributesInfoNV

instance ToCStruct SciSyncAttributesInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SciSyncAttributesInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SCI_SYNC_ATTRIBUTES_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SciSyncClientTypeNV)) (clientType)
    poke ((p `plusPtr` 20 :: Ptr SciSyncPrimitiveTypeNV)) (primitiveType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SCI_SYNC_ATTRIBUTES_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SciSyncClientTypeNV)) (zero)
    poke ((p `plusPtr` 20 :: Ptr SciSyncPrimitiveTypeNV)) (zero)
    f

instance FromCStruct SciSyncAttributesInfoNV where
  peekCStruct p = do
    clientType <- peek @SciSyncClientTypeNV ((p `plusPtr` 16 :: Ptr SciSyncClientTypeNV))
    primitiveType <- peek @SciSyncPrimitiveTypeNV ((p `plusPtr` 20 :: Ptr SciSyncPrimitiveTypeNV))
    pure $ SciSyncAttributesInfoNV
             clientType primitiveType

instance Storable SciSyncAttributesInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SciSyncAttributesInfoNV where
  zero = SciSyncAttributesInfoNV
           zero
           zero


-- No documentation found for TopLevel "VkPhysicalDeviceExternalSciSyncFeaturesNV"
data PhysicalDeviceExternalSciSyncFeaturesNV = PhysicalDeviceExternalSciSyncFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceExternalSciSyncFeaturesNV" "sciSyncFence"
    sciSyncFence :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceExternalSciSyncFeaturesNV" "sciSyncSemaphore"
    sciSyncSemaphore :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceExternalSciSyncFeaturesNV" "sciSyncImport"
    sciSyncImport :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceExternalSciSyncFeaturesNV" "sciSyncExport"
    sciSyncExport :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalSciSyncFeaturesNV)
#endif
deriving instance Show PhysicalDeviceExternalSciSyncFeaturesNV

instance ToCStruct PhysicalDeviceExternalSciSyncFeaturesNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalSciSyncFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SCI_SYNC_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (sciSyncFence))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (sciSyncSemaphore))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (sciSyncImport))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (sciSyncExport))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SCI_SYNC_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExternalSciSyncFeaturesNV where
  peekCStruct p = do
    sciSyncFence <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    sciSyncSemaphore <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    sciSyncImport <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    sciSyncExport <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pure $ PhysicalDeviceExternalSciSyncFeaturesNV
             (bool32ToBool sciSyncFence)
             (bool32ToBool sciSyncSemaphore)
             (bool32ToBool sciSyncImport)
             (bool32ToBool sciSyncExport)

instance Storable PhysicalDeviceExternalSciSyncFeaturesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalSciSyncFeaturesNV where
  zero = PhysicalDeviceExternalSciSyncFeaturesNV
           zero
           zero
           zero
           zero


-- No documentation found for TopLevel "VkSciSyncClientTypeNV"
newtype SciSyncClientTypeNV = SciSyncClientTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSciSyncClientTypeNV" "VK_SCI_SYNC_CLIENT_TYPE_SIGNALER_NV"
pattern SCI_SYNC_CLIENT_TYPE_SIGNALER_NV = SciSyncClientTypeNV 0

-- No documentation found for Nested "VkSciSyncClientTypeNV" "VK_SCI_SYNC_CLIENT_TYPE_WAITER_NV"
pattern SCI_SYNC_CLIENT_TYPE_WAITER_NV = SciSyncClientTypeNV 1

-- No documentation found for Nested "VkSciSyncClientTypeNV" "VK_SCI_SYNC_CLIENT_TYPE_SIGNALER_WAITER_NV"
pattern SCI_SYNC_CLIENT_TYPE_SIGNALER_WAITER_NV = SciSyncClientTypeNV 2

{-# COMPLETE
  SCI_SYNC_CLIENT_TYPE_SIGNALER_NV
  , SCI_SYNC_CLIENT_TYPE_WAITER_NV
  , SCI_SYNC_CLIENT_TYPE_SIGNALER_WAITER_NV ::
    SciSyncClientTypeNV
  #-}

conNameSciSyncClientTypeNV :: String
conNameSciSyncClientTypeNV = "SciSyncClientTypeNV"

enumPrefixSciSyncClientTypeNV :: String
enumPrefixSciSyncClientTypeNV = "SCI_SYNC_CLIENT_TYPE_"

showTableSciSyncClientTypeNV :: [(SciSyncClientTypeNV, String)]
showTableSciSyncClientTypeNV =
  [
    ( SCI_SYNC_CLIENT_TYPE_SIGNALER_NV
    , "SIGNALER_NV"
    )
  , (SCI_SYNC_CLIENT_TYPE_WAITER_NV, "WAITER_NV")
  ,
    ( SCI_SYNC_CLIENT_TYPE_SIGNALER_WAITER_NV
    , "SIGNALER_WAITER_NV"
    )
  ]

instance Show SciSyncClientTypeNV where
  showsPrec =
    enumShowsPrec
      enumPrefixSciSyncClientTypeNV
      showTableSciSyncClientTypeNV
      conNameSciSyncClientTypeNV
      (\(SciSyncClientTypeNV x) -> x)
      (showsPrec 11)

instance Read SciSyncClientTypeNV where
  readPrec =
    enumReadPrec
      enumPrefixSciSyncClientTypeNV
      showTableSciSyncClientTypeNV
      conNameSciSyncClientTypeNV
      SciSyncClientTypeNV

-- No documentation found for TopLevel "VkSciSyncPrimitiveTypeNV"
newtype SciSyncPrimitiveTypeNV = SciSyncPrimitiveTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSciSyncPrimitiveTypeNV" "VK_SCI_SYNC_PRIMITIVE_TYPE_FENCE_NV"
pattern SCI_SYNC_PRIMITIVE_TYPE_FENCE_NV = SciSyncPrimitiveTypeNV 0

-- No documentation found for Nested "VkSciSyncPrimitiveTypeNV" "VK_SCI_SYNC_PRIMITIVE_TYPE_SEMAPHORE_NV"
pattern SCI_SYNC_PRIMITIVE_TYPE_SEMAPHORE_NV = SciSyncPrimitiveTypeNV 1

{-# COMPLETE
  SCI_SYNC_PRIMITIVE_TYPE_FENCE_NV
  , SCI_SYNC_PRIMITIVE_TYPE_SEMAPHORE_NV ::
    SciSyncPrimitiveTypeNV
  #-}

conNameSciSyncPrimitiveTypeNV :: String
conNameSciSyncPrimitiveTypeNV = "SciSyncPrimitiveTypeNV"

enumPrefixSciSyncPrimitiveTypeNV :: String
enumPrefixSciSyncPrimitiveTypeNV = "SCI_SYNC_PRIMITIVE_TYPE_"

showTableSciSyncPrimitiveTypeNV :: [(SciSyncPrimitiveTypeNV, String)]
showTableSciSyncPrimitiveTypeNV =
  [
    ( SCI_SYNC_PRIMITIVE_TYPE_FENCE_NV
    , "FENCE_NV"
    )
  ,
    ( SCI_SYNC_PRIMITIVE_TYPE_SEMAPHORE_NV
    , "SEMAPHORE_NV"
    )
  ]

instance Show SciSyncPrimitiveTypeNV where
  showsPrec =
    enumShowsPrec
      enumPrefixSciSyncPrimitiveTypeNV
      showTableSciSyncPrimitiveTypeNV
      conNameSciSyncPrimitiveTypeNV
      (\(SciSyncPrimitiveTypeNV x) -> x)
      (showsPrec 11)

instance Read SciSyncPrimitiveTypeNV where
  readPrec =
    enumReadPrec
      enumPrefixSciSyncPrimitiveTypeNV
      showTableSciSyncPrimitiveTypeNV
      conNameSciSyncPrimitiveTypeNV
      SciSyncPrimitiveTypeNV

type NV_EXTERNAL_SCI_SYNC_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_EXTERNAL_SCI_SYNC_SPEC_VERSION"
pattern NV_EXTERNAL_SCI_SYNC_SPEC_VERSION :: forall a . Integral a => a
pattern NV_EXTERNAL_SCI_SYNC_SPEC_VERSION = 2


type NV_EXTERNAL_SCI_SYNC_EXTENSION_NAME = "VK_NV_external_sci_sync"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_SCI_SYNC_EXTENSION_NAME"
pattern NV_EXTERNAL_SCI_SYNC_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_EXTERNAL_SCI_SYNC_EXTENSION_NAME = "VK_NV_external_sci_sync"


type NvSciSyncAttrList = Ptr ()

