{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_external_memory_sci_buf"
module Vulkan.Extensions.VK_NV_external_memory_sci_buf  ( getMemorySciBufNV
                                                        , getPhysicalDeviceExternalMemorySciBufPropertiesNV
                                                        , getPhysicalDeviceSciBufAttributesNV
                                                        , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SCI_BUF_FEATURES_NV
                                                        , ExportMemorySciBufInfoNV(..)
                                                        , ImportMemorySciBufInfoNV(..)
                                                        , MemoryGetSciBufInfoNV(..)
                                                        , MemorySciBufPropertiesNV(..)
                                                        , PhysicalDeviceExternalMemorySciBufFeaturesNV(..)
                                                        , PhysicalDeviceExternalSciBufFeaturesNV
                                                        , NV_EXTERNAL_MEMORY_SCI_BUF_SPEC_VERSION
                                                        , pattern NV_EXTERNAL_MEMORY_SCI_BUF_SPEC_VERSION
                                                        , NV_EXTERNAL_MEMORY_SCI_BUF_EXTENSION_NAME
                                                        , pattern NV_EXTERNAL_MEMORY_SCI_BUF_EXTENSION_NAME
                                                        , NvSciBufAttrList
                                                        , NvSciBufObj
                                                        ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
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
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemorySciBufNV))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceExternalMemorySciBufPropertiesNV))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceSciBufAttributesNV))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EXPORT_MEMORY_SCI_BUF_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMPORT_MEMORY_SCI_BUF_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_GET_SCI_BUF_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_SCI_BUF_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_SCI_BUF_FEATURES_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemorySciBufNV
  :: FunPtr (Ptr Device_T -> Ptr MemoryGetSciBufInfoNV -> Ptr NvSciBufObj -> IO Result) -> Ptr Device_T -> Ptr MemoryGetSciBufInfoNV -> Ptr NvSciBufObj -> IO Result

-- No documentation found for TopLevel "vkGetMemorySciBufNV"
getMemorySciBufNV :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkGetMemorySciBufNV" "device"
                     Device
                  -> -- No documentation found for Nested "vkGetMemorySciBufNV" "pGetSciBufInfo"
                     MemoryGetSciBufInfoNV
                  -> io (("handle" ::: NvSciBufObj))
getMemorySciBufNV device getSciBufInfo = liftIO . evalContT $ do
  let vkGetMemorySciBufNVPtr = pVkGetMemorySciBufNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetMemorySciBufNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemorySciBufNV is null" Nothing Nothing
  let vkGetMemorySciBufNV' = mkVkGetMemorySciBufNV vkGetMemorySciBufNVPtr
  pGetSciBufInfo <- ContT $ withCStruct (getSciBufInfo)
  pPHandle <- ContT $ bracket (callocBytes @NvSciBufObj 8) free
  r <- lift $ traceAroundEvent "vkGetMemorySciBufNV" (vkGetMemorySciBufNV'
                                                        (deviceHandle (device))
                                                        pGetSciBufInfo
                                                        (pPHandle))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pHandle <- lift $ peek @NvSciBufObj pPHandle
  pure $ (pHandle)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalMemorySciBufPropertiesNV
  :: FunPtr (Ptr PhysicalDevice_T -> ExternalMemoryHandleTypeFlagBits -> NvSciBufObj -> Ptr MemorySciBufPropertiesNV -> IO Result) -> Ptr PhysicalDevice_T -> ExternalMemoryHandleTypeFlagBits -> NvSciBufObj -> Ptr MemorySciBufPropertiesNV -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceExternalMemorySciBufPropertiesNV"
getPhysicalDeviceExternalMemorySciBufPropertiesNV :: forall io
                                                   . (MonadIO io)
                                                  => -- No documentation found for Nested "vkGetPhysicalDeviceExternalMemorySciBufPropertiesNV" "physicalDevice"
                                                     PhysicalDevice
                                                  -> -- No documentation found for Nested "vkGetPhysicalDeviceExternalMemorySciBufPropertiesNV" "handleType"
                                                     ExternalMemoryHandleTypeFlagBits
                                                  -> -- No documentation found for Nested "vkGetPhysicalDeviceExternalMemorySciBufPropertiesNV" "handle"
                                                     ("handle" ::: NvSciBufObj)
                                                  -> io (MemorySciBufPropertiesNV)
getPhysicalDeviceExternalMemorySciBufPropertiesNV physicalDevice
                                                    handleType
                                                    handle = liftIO . evalContT $ do
  let vkGetPhysicalDeviceExternalMemorySciBufPropertiesNVPtr = pVkGetPhysicalDeviceExternalMemorySciBufPropertiesNV (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceExternalMemorySciBufPropertiesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceExternalMemorySciBufPropertiesNV is null" Nothing Nothing
  let vkGetPhysicalDeviceExternalMemorySciBufPropertiesNV' = mkVkGetPhysicalDeviceExternalMemorySciBufPropertiesNV vkGetPhysicalDeviceExternalMemorySciBufPropertiesNVPtr
  pPMemorySciBufProperties <- ContT (withZeroCStruct @MemorySciBufPropertiesNV)
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceExternalMemorySciBufPropertiesNV" (vkGetPhysicalDeviceExternalMemorySciBufPropertiesNV'
                                                                                        (physicalDeviceHandle (physicalDevice))
                                                                                        (handleType)
                                                                                        (handle)
                                                                                        (pPMemorySciBufProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMemorySciBufProperties <- lift $ peekCStruct @MemorySciBufPropertiesNV pPMemorySciBufProperties
  pure $ (pMemorySciBufProperties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSciBufAttributesNV
  :: FunPtr (Ptr PhysicalDevice_T -> NvSciBufAttrList -> IO Result) -> Ptr PhysicalDevice_T -> NvSciBufAttrList -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceSciBufAttributesNV"
getPhysicalDeviceSciBufAttributesNV :: forall io
                                     . (MonadIO io)
                                    => -- No documentation found for Nested "vkGetPhysicalDeviceSciBufAttributesNV" "physicalDevice"
                                       PhysicalDevice
                                    -> -- No documentation found for Nested "vkGetPhysicalDeviceSciBufAttributesNV" "pAttributes"
                                       ("attributes" ::: NvSciBufAttrList)
                                    -> io ()
getPhysicalDeviceSciBufAttributesNV physicalDevice attributes = liftIO $ do
  let vkGetPhysicalDeviceSciBufAttributesNVPtr = pVkGetPhysicalDeviceSciBufAttributesNV (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  unless (vkGetPhysicalDeviceSciBufAttributesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceSciBufAttributesNV is null" Nothing Nothing
  let vkGetPhysicalDeviceSciBufAttributesNV' = mkVkGetPhysicalDeviceSciBufAttributesNV vkGetPhysicalDeviceSciBufAttributesNVPtr
  r <- traceAroundEvent "vkGetPhysicalDeviceSciBufAttributesNV" (vkGetPhysicalDeviceSciBufAttributesNV'
                                                                   (physicalDeviceHandle (physicalDevice))
                                                                   (attributes))
  when (r < SUCCESS) (throwIO (VulkanException r))


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SCI_BUF_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SCI_BUF_FEATURES_NV = STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_SCI_BUF_FEATURES_NV


-- No documentation found for TopLevel "VkExportMemorySciBufInfoNV"
data ExportMemorySciBufInfoNV = ExportMemorySciBufInfoNV
  { -- No documentation found for Nested "VkExportMemorySciBufInfoNV" "pAttributes"
    attributes :: NvSciBufAttrList }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ExportMemorySciBufInfoNV)
#endif
deriving instance Show ExportMemorySciBufInfoNV

instance ToCStruct ExportMemorySciBufInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExportMemorySciBufInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_SCI_BUF_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr NvSciBufAttrList)) (attributes)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EXPORT_MEMORY_SCI_BUF_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr NvSciBufAttrList)) (zero)
    f

instance FromCStruct ExportMemorySciBufInfoNV where
  peekCStruct p = do
    pAttributes <- peek @NvSciBufAttrList ((p `plusPtr` 16 :: Ptr NvSciBufAttrList))
    pure $ ExportMemorySciBufInfoNV
             pAttributes

instance Storable ExportMemorySciBufInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExportMemorySciBufInfoNV where
  zero = ExportMemorySciBufInfoNV
           zero


-- No documentation found for TopLevel "VkImportMemorySciBufInfoNV"
data ImportMemorySciBufInfoNV = ImportMemorySciBufInfoNV
  { -- No documentation found for Nested "VkImportMemorySciBufInfoNV" "handleType"
    handleType :: ExternalMemoryHandleTypeFlagBits
  , -- No documentation found for Nested "VkImportMemorySciBufInfoNV" "handle"
    handle :: NvSciBufObj
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImportMemorySciBufInfoNV)
#endif
deriving instance Show ImportMemorySciBufInfoNV

instance ToCStruct ImportMemorySciBufInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImportMemorySciBufInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_SCI_BUF_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    poke ((p `plusPtr` 24 :: Ptr NvSciBufObj)) (handle)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMPORT_MEMORY_SCI_BUF_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits)) (zero)
    poke ((p `plusPtr` 24 :: Ptr NvSciBufObj)) (zero)
    f

instance FromCStruct ImportMemorySciBufInfoNV where
  peekCStruct p = do
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 16 :: Ptr ExternalMemoryHandleTypeFlagBits))
    handle <- peek @NvSciBufObj ((p `plusPtr` 24 :: Ptr NvSciBufObj))
    pure $ ImportMemorySciBufInfoNV
             handleType handle

instance Storable ImportMemorySciBufInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImportMemorySciBufInfoNV where
  zero = ImportMemorySciBufInfoNV
           zero
           zero


-- No documentation found for TopLevel "VkMemoryGetSciBufInfoNV"
data MemoryGetSciBufInfoNV = MemoryGetSciBufInfoNV
  { -- No documentation found for Nested "VkMemoryGetSciBufInfoNV" "memory"
    memory :: DeviceMemory
  , -- No documentation found for Nested "VkMemoryGetSciBufInfoNV" "handleType"
    handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryGetSciBufInfoNV)
#endif
deriving instance Show MemoryGetSciBufInfoNV

instance ToCStruct MemoryGetSciBufInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryGetSciBufInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_SCI_BUF_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_SCI_BUF_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (zero)
    f

instance FromCStruct MemoryGetSciBufInfoNV where
  peekCStruct p = do
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pure $ MemoryGetSciBufInfoNV
             memory handleType

instance Storable MemoryGetSciBufInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryGetSciBufInfoNV where
  zero = MemoryGetSciBufInfoNV
           zero
           zero


-- No documentation found for TopLevel "VkMemorySciBufPropertiesNV"
data MemorySciBufPropertiesNV = MemorySciBufPropertiesNV
  { -- No documentation found for Nested "VkMemorySciBufPropertiesNV" "memoryTypeBits"
    memoryTypeBits :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemorySciBufPropertiesNV)
#endif
deriving instance Show MemorySciBufPropertiesNV

instance ToCStruct MemorySciBufPropertiesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemorySciBufPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_SCI_BUF_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (memoryTypeBits)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_SCI_BUF_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct MemorySciBufPropertiesNV where
  peekCStruct p = do
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ MemorySciBufPropertiesNV
             memoryTypeBits

instance Storable MemorySciBufPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemorySciBufPropertiesNV where
  zero = MemorySciBufPropertiesNV
           zero


-- No documentation found for TopLevel "VkPhysicalDeviceExternalMemorySciBufFeaturesNV"
data PhysicalDeviceExternalMemorySciBufFeaturesNV = PhysicalDeviceExternalMemorySciBufFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceExternalMemorySciBufFeaturesNV" "sciBufImport"
    sciBufImport :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceExternalMemorySciBufFeaturesNV" "sciBufExport"
    sciBufExport :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalMemorySciBufFeaturesNV)
#endif
deriving instance Show PhysicalDeviceExternalMemorySciBufFeaturesNV

instance ToCStruct PhysicalDeviceExternalMemorySciBufFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalMemorySciBufFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_SCI_BUF_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (sciBufImport))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (sciBufExport))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_SCI_BUF_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExternalMemorySciBufFeaturesNV where
  peekCStruct p = do
    sciBufImport <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    sciBufExport <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceExternalMemorySciBufFeaturesNV
             (bool32ToBool sciBufImport) (bool32ToBool sciBufExport)

instance Storable PhysicalDeviceExternalMemorySciBufFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalMemorySciBufFeaturesNV where
  zero = PhysicalDeviceExternalMemorySciBufFeaturesNV
           zero
           zero


-- No documentation found for TopLevel "VkPhysicalDeviceExternalSciBufFeaturesNV"
type PhysicalDeviceExternalSciBufFeaturesNV = PhysicalDeviceExternalMemorySciBufFeaturesNV


type NV_EXTERNAL_MEMORY_SCI_BUF_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_SCI_BUF_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_SCI_BUF_SPEC_VERSION :: forall a . Integral a => a
pattern NV_EXTERNAL_MEMORY_SCI_BUF_SPEC_VERSION = 2


type NV_EXTERNAL_MEMORY_SCI_BUF_EXTENSION_NAME = "VK_NV_external_memory_sci_buf"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_SCI_BUF_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_SCI_BUF_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_SCI_BUF_EXTENSION_NAME = "VK_NV_external_memory_sci_buf"


type NvSciBufAttrList = Ptr ()


type NvSciBufObj = Ptr ()

