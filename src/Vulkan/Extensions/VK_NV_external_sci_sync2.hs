{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_external_sci_sync2"
module Vulkan.Extensions.VK_NV_external_sci_sync2  ( createSemaphoreSciSyncPoolNV
                                                   , withSemaphoreSciSyncPoolNV
                                                   , destroySemaphoreSciSyncPoolNV
                                                   , PhysicalDeviceExternalSciSync2FeaturesNV(..)
                                                   , SemaphoreSciSyncPoolCreateInfoNV(..)
                                                   , SemaphoreSciSyncCreateInfoNV(..)
                                                   , DeviceSemaphoreSciSyncPoolReservationCreateInfoNV(..)
                                                   , NV_EXTERNAL_SCI_SYNC_2_SPEC_VERSION
                                                   , pattern NV_EXTERNAL_SCI_SYNC_2_SPEC_VERSION
                                                   , NV_EXTERNAL_SCI_SYNC_2_EXTENSION_NAME
                                                   , pattern NV_EXTERNAL_SCI_SYNC_2_EXTENSION_NAME
                                                   , NvSciSyncObj
                                                   , NvSciSyncFence
                                                   , SemaphoreSciSyncPoolNV(..)
                                                   , ExportFenceSciSyncInfoNV(..)
                                                   , ImportFenceSciSyncInfoNV(..)
                                                   , FenceGetSciSyncInfoNV(..)
                                                   , SciSyncAttributesInfoNV(..)
                                                   , getFenceSciSyncFenceNV
                                                   , getFenceSciSyncObjNV
                                                   , importFenceSciSyncFenceNV
                                                   , importFenceSciSyncObjNV
                                                   , getPhysicalDeviceSciSyncAttributesNV
                                                   , SciSyncClientTypeNV(..)
                                                   , SciSyncPrimitiveTypeNV(..)
                                                   , NvSciSyncAttrList
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
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateSemaphoreSciSyncPoolNV))
import Vulkan.Dynamic (DeviceCmds(pVkDestroySemaphoreSciSyncPoolNV))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Extensions.Handles (SemaphoreSciSyncPoolNV)
import Vulkan.Extensions.Handles (SemaphoreSciSyncPoolNV(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_SEMAPHORE_SCI_SYNC_POOL_RESERVATION_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SCI_SYNC_2_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_SCI_SYNC_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SEMAPHORE_SCI_SYNC_POOL_CREATE_INFO_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_NV_external_sci_sync (getFenceSciSyncFenceNV)
import Vulkan.Extensions.VK_NV_external_sci_sync (getFenceSciSyncObjNV)
import Vulkan.Extensions.VK_NV_external_sci_sync (getPhysicalDeviceSciSyncAttributesNV)
import Vulkan.Extensions.VK_NV_external_sci_sync (importFenceSciSyncFenceNV)
import Vulkan.Extensions.VK_NV_external_sci_sync (importFenceSciSyncObjNV)
import Vulkan.Extensions.VK_NV_external_sci_sync (ExportFenceSciSyncInfoNV(..))
import Vulkan.Extensions.VK_NV_external_sci_sync (FenceGetSciSyncInfoNV(..))
import Vulkan.Extensions.VK_NV_external_sci_sync (ImportFenceSciSyncInfoNV(..))
import Vulkan.Extensions.VK_NV_external_sci_sync (NvSciSyncAttrList)
import Vulkan.Extensions.VK_NV_external_sci_sync (SciSyncAttributesInfoNV(..))
import Vulkan.Extensions.VK_NV_external_sci_sync (SciSyncClientTypeNV(..))
import Vulkan.Extensions.VK_NV_external_sci_sync (SciSyncPrimitiveTypeNV(..))
import Vulkan.Extensions.Handles (SemaphoreSciSyncPoolNV(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSemaphoreSciSyncPoolNV
  :: FunPtr (Ptr Device_T -> Ptr SemaphoreSciSyncPoolCreateInfoNV -> Ptr AllocationCallbacks -> Ptr SemaphoreSciSyncPoolNV -> IO Result) -> Ptr Device_T -> Ptr SemaphoreSciSyncPoolCreateInfoNV -> Ptr AllocationCallbacks -> Ptr SemaphoreSciSyncPoolNV -> IO Result

-- No documentation found for TopLevel "vkCreateSemaphoreSciSyncPoolNV"
createSemaphoreSciSyncPoolNV :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkCreateSemaphoreSciSyncPoolNV" "device"
                                Device
                             -> -- No documentation found for Nested "vkCreateSemaphoreSciSyncPoolNV" "pCreateInfo"
                                SemaphoreSciSyncPoolCreateInfoNV
                             -> -- No documentation found for Nested "vkCreateSemaphoreSciSyncPoolNV" "pAllocator"
                                ("allocator" ::: Maybe AllocationCallbacks)
                             -> io (("semaphorePool" ::: SemaphoreSciSyncPoolNV))
createSemaphoreSciSyncPoolNV device
                               createInfo
                               allocator = liftIO . evalContT $ do
  let vkCreateSemaphoreSciSyncPoolNVPtr = pVkCreateSemaphoreSciSyncPoolNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateSemaphoreSciSyncPoolNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateSemaphoreSciSyncPoolNV is null" Nothing Nothing
  let vkCreateSemaphoreSciSyncPoolNV' = mkVkCreateSemaphoreSciSyncPoolNV vkCreateSemaphoreSciSyncPoolNVPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPSemaphorePool <- ContT $ bracket (callocBytes @SemaphoreSciSyncPoolNV 8) free
  r <- lift $ traceAroundEvent "vkCreateSemaphoreSciSyncPoolNV" (vkCreateSemaphoreSciSyncPoolNV'
                                                                   (deviceHandle (device))
                                                                   pCreateInfo
                                                                   pAllocator
                                                                   (pPSemaphorePool))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSemaphorePool <- lift $ peek @SemaphoreSciSyncPoolNV pPSemaphorePool
  pure $ (pSemaphorePool)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSemaphoreSciSyncPoolNV' and 'destroySemaphoreSciSyncPoolNV'
--
-- To ensure that 'destroySemaphoreSciSyncPoolNV' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSemaphoreSciSyncPoolNV :: forall io r . MonadIO io => Device -> SemaphoreSciSyncPoolCreateInfoNV -> Maybe AllocationCallbacks -> (io SemaphoreSciSyncPoolNV -> (SemaphoreSciSyncPoolNV -> io ()) -> r) -> r
withSemaphoreSciSyncPoolNV device pCreateInfo pAllocator b =
  b (createSemaphoreSciSyncPoolNV device pCreateInfo pAllocator)
    (\(o0) -> destroySemaphoreSciSyncPoolNV device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySemaphoreSciSyncPoolNV
  :: FunPtr (Ptr Device_T -> SemaphoreSciSyncPoolNV -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> SemaphoreSciSyncPoolNV -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroySemaphoreSciSyncPoolNV"
destroySemaphoreSciSyncPoolNV :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkDestroySemaphoreSciSyncPoolNV" "device"
                                 Device
                              -> -- No documentation found for Nested "vkDestroySemaphoreSciSyncPoolNV" "semaphorePool"
                                 ("semaphorePool" ::: SemaphoreSciSyncPoolNV)
                              -> -- No documentation found for Nested "vkDestroySemaphoreSciSyncPoolNV" "pAllocator"
                                 ("allocator" ::: Maybe AllocationCallbacks)
                              -> io ()
destroySemaphoreSciSyncPoolNV device
                                semaphorePool
                                allocator = liftIO . evalContT $ do
  let vkDestroySemaphoreSciSyncPoolNVPtr = pVkDestroySemaphoreSciSyncPoolNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroySemaphoreSciSyncPoolNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroySemaphoreSciSyncPoolNV is null" Nothing Nothing
  let vkDestroySemaphoreSciSyncPoolNV' = mkVkDestroySemaphoreSciSyncPoolNV vkDestroySemaphoreSciSyncPoolNVPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroySemaphoreSciSyncPoolNV" (vkDestroySemaphoreSciSyncPoolNV'
                                                               (deviceHandle (device))
                                                               (semaphorePool)
                                                               pAllocator)
  pure $ ()


-- No documentation found for TopLevel "VkPhysicalDeviceExternalSciSync2FeaturesNV"
data PhysicalDeviceExternalSciSync2FeaturesNV = PhysicalDeviceExternalSciSync2FeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceExternalSciSync2FeaturesNV" "sciSyncFence"
    sciSyncFence :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceExternalSciSync2FeaturesNV" "sciSyncSemaphore2"
    sciSyncSemaphore2 :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceExternalSciSync2FeaturesNV" "sciSyncImport"
    sciSyncImport :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceExternalSciSync2FeaturesNV" "sciSyncExport"
    sciSyncExport :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalSciSync2FeaturesNV)
#endif
deriving instance Show PhysicalDeviceExternalSciSync2FeaturesNV

instance ToCStruct PhysicalDeviceExternalSciSync2FeaturesNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalSciSync2FeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SCI_SYNC_2_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (sciSyncFence))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (sciSyncSemaphore2))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (sciSyncImport))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (sciSyncExport))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SCI_SYNC_2_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExternalSciSync2FeaturesNV where
  peekCStruct p = do
    sciSyncFence <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    sciSyncSemaphore2 <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    sciSyncImport <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    sciSyncExport <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pure $ PhysicalDeviceExternalSciSync2FeaturesNV
             (bool32ToBool sciSyncFence)
             (bool32ToBool sciSyncSemaphore2)
             (bool32ToBool sciSyncImport)
             (bool32ToBool sciSyncExport)

instance Storable PhysicalDeviceExternalSciSync2FeaturesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalSciSync2FeaturesNV where
  zero = PhysicalDeviceExternalSciSync2FeaturesNV
           zero
           zero
           zero
           zero


-- No documentation found for TopLevel "VkSemaphoreSciSyncPoolCreateInfoNV"
data SemaphoreSciSyncPoolCreateInfoNV = SemaphoreSciSyncPoolCreateInfoNV
  { -- No documentation found for Nested "VkSemaphoreSciSyncPoolCreateInfoNV" "handle"
    handle :: NvSciSyncObj }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreSciSyncPoolCreateInfoNV)
#endif
deriving instance Show SemaphoreSciSyncPoolCreateInfoNV

instance ToCStruct SemaphoreSciSyncPoolCreateInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreSciSyncPoolCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_SCI_SYNC_POOL_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr NvSciSyncObj)) (handle)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_SCI_SYNC_POOL_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr NvSciSyncObj)) (zero)
    f

instance FromCStruct SemaphoreSciSyncPoolCreateInfoNV where
  peekCStruct p = do
    handle <- peek @NvSciSyncObj ((p `plusPtr` 16 :: Ptr NvSciSyncObj))
    pure $ SemaphoreSciSyncPoolCreateInfoNV
             handle

instance Storable SemaphoreSciSyncPoolCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SemaphoreSciSyncPoolCreateInfoNV where
  zero = SemaphoreSciSyncPoolCreateInfoNV
           zero


-- No documentation found for TopLevel "VkSemaphoreSciSyncCreateInfoNV"
data SemaphoreSciSyncCreateInfoNV = SemaphoreSciSyncCreateInfoNV
  { -- No documentation found for Nested "VkSemaphoreSciSyncCreateInfoNV" "semaphorePool"
    semaphorePool :: SemaphoreSciSyncPoolNV
  , -- No documentation found for Nested "VkSemaphoreSciSyncCreateInfoNV" "pFence"
    fence :: Ptr NvSciSyncFence
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SemaphoreSciSyncCreateInfoNV)
#endif
deriving instance Show SemaphoreSciSyncCreateInfoNV

instance ToCStruct SemaphoreSciSyncCreateInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SemaphoreSciSyncCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_SCI_SYNC_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SemaphoreSciSyncPoolNV)) (semaphorePool)
    poke ((p `plusPtr` 24 :: Ptr (Ptr NvSciSyncFence))) (fence)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SEMAPHORE_SCI_SYNC_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SemaphoreSciSyncPoolNV)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr NvSciSyncFence))) (zero)
    f

instance FromCStruct SemaphoreSciSyncCreateInfoNV where
  peekCStruct p = do
    semaphorePool <- peek @SemaphoreSciSyncPoolNV ((p `plusPtr` 16 :: Ptr SemaphoreSciSyncPoolNV))
    pFence <- peek @(Ptr NvSciSyncFence) ((p `plusPtr` 24 :: Ptr (Ptr NvSciSyncFence)))
    pure $ SemaphoreSciSyncCreateInfoNV
             semaphorePool pFence

instance Storable SemaphoreSciSyncCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SemaphoreSciSyncCreateInfoNV where
  zero = SemaphoreSciSyncCreateInfoNV
           zero
           zero


-- No documentation found for TopLevel "VkDeviceSemaphoreSciSyncPoolReservationCreateInfoNV"
data DeviceSemaphoreSciSyncPoolReservationCreateInfoNV = DeviceSemaphoreSciSyncPoolReservationCreateInfoNV
  { -- No documentation found for Nested "VkDeviceSemaphoreSciSyncPoolReservationCreateInfoNV" "semaphoreSciSyncPoolRequestCount"
    semaphoreSciSyncPoolRequestCount :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceSemaphoreSciSyncPoolReservationCreateInfoNV)
#endif
deriving instance Show DeviceSemaphoreSciSyncPoolReservationCreateInfoNV

instance ToCStruct DeviceSemaphoreSciSyncPoolReservationCreateInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceSemaphoreSciSyncPoolReservationCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_SEMAPHORE_SCI_SYNC_POOL_RESERVATION_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (semaphoreSciSyncPoolRequestCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_SEMAPHORE_SCI_SYNC_POOL_RESERVATION_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceSemaphoreSciSyncPoolReservationCreateInfoNV where
  peekCStruct p = do
    semaphoreSciSyncPoolRequestCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DeviceSemaphoreSciSyncPoolReservationCreateInfoNV
             semaphoreSciSyncPoolRequestCount

instance Storable DeviceSemaphoreSciSyncPoolReservationCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceSemaphoreSciSyncPoolReservationCreateInfoNV where
  zero = DeviceSemaphoreSciSyncPoolReservationCreateInfoNV
           zero


type NV_EXTERNAL_SCI_SYNC_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_EXTERNAL_SCI_SYNC_2_SPEC_VERSION"
pattern NV_EXTERNAL_SCI_SYNC_2_SPEC_VERSION :: forall a . Integral a => a
pattern NV_EXTERNAL_SCI_SYNC_2_SPEC_VERSION = 1


type NV_EXTERNAL_SCI_SYNC_2_EXTENSION_NAME = "VK_NV_external_sci_sync2"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_SCI_SYNC_2_EXTENSION_NAME"
pattern NV_EXTERNAL_SCI_SYNC_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_EXTERNAL_SCI_SYNC_2_EXTENSION_NAME = "VK_NV_external_sci_sync2"


type NvSciSyncObj = Ptr ()


data NvSciSyncFence

