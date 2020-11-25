{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_performance_query"
module Vulkan.Extensions.VK_KHR_performance_query  ( enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
                                                   , getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
                                                   , acquireProfilingLockKHR
                                                   , releaseProfilingLockKHR
                                                   , pattern QUERY_SCOPE_COMMAND_BUFFER_KHR
                                                   , pattern QUERY_SCOPE_RENDER_PASS_KHR
                                                   , pattern QUERY_SCOPE_COMMAND_KHR
                                                   , pattern PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR
                                                   , pattern PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR
                                                   , PhysicalDevicePerformanceQueryFeaturesKHR(..)
                                                   , PhysicalDevicePerformanceQueryPropertiesKHR(..)
                                                   , PerformanceCounterKHR(..)
                                                   , PerformanceCounterDescriptionKHR(..)
                                                   , QueryPoolPerformanceCreateInfoKHR(..)
                                                   , AcquireProfilingLockInfoKHR(..)
                                                   , PerformanceQuerySubmitInfoKHR(..)
                                                   , PerformanceCounterResultKHR(..)
                                                   , PerformanceCounterScopeKHR( PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR
                                                                               , PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR
                                                                               , PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR
                                                                               , ..
                                                                               )
                                                   , PerformanceCounterUnitKHR( PERFORMANCE_COUNTER_UNIT_GENERIC_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_BYTES_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_KELVIN_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_WATTS_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_VOLTS_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_AMPS_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_HERTZ_KHR
                                                                              , PERFORMANCE_COUNTER_UNIT_CYCLES_KHR
                                                                              , ..
                                                                              )
                                                   , PerformanceCounterStorageKHR( PERFORMANCE_COUNTER_STORAGE_INT32_KHR
                                                                                 , PERFORMANCE_COUNTER_STORAGE_INT64_KHR
                                                                                 , PERFORMANCE_COUNTER_STORAGE_UINT32_KHR
                                                                                 , PERFORMANCE_COUNTER_STORAGE_UINT64_KHR
                                                                                 , PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR
                                                                                 , PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR
                                                                                 , ..
                                                                                 )
                                                   , PerformanceCounterDescriptionFlagsKHR
                                                   , PerformanceCounterDescriptionFlagBitsKHR( PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR
                                                                                             , PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR
                                                                                             , ..
                                                                                             )
                                                   , AcquireProfilingLockFlagsKHR
                                                   , AcquireProfilingLockFlagBitsKHR(..)
                                                   , KHR_PERFORMANCE_QUERY_SPEC_VERSION
                                                   , pattern KHR_PERFORMANCE_QUERY_SPEC_VERSION
                                                   , KHR_PERFORMANCE_QUERY_EXTENSION_NAME
                                                   , pattern KHR_PERFORMANCE_QUERY_EXTENSION_NAME
                                                   ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CDouble)
import Foreign.C.Types (CDouble(CDouble))
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Data.Int (Int64)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkAcquireProfilingLockKHR))
import Vulkan.Dynamic (DeviceCmds(pVkReleaseProfilingLockKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR))
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr Word32 -> Ptr PerformanceCounterKHR -> Ptr PerformanceCounterDescriptionKHR -> IO Result) -> Ptr PhysicalDevice_T -> Word32 -> Ptr Word32 -> Ptr PerformanceCounterKHR -> Ptr PerformanceCounterDescriptionKHR -> IO Result

-- No documentation found for TopLevel "vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR"
enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR :: forall io
                                                               . (MonadIO io)
                                                              => -- No documentation found for Nested "vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR" "physicalDevice"
                                                                 PhysicalDevice
                                                              -> -- No documentation found for Nested "vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR" "queueFamilyIndex"
                                                                 ("queueFamilyIndex" ::: Word32)
                                                              -> io (Result, ("counters" ::: Vector PerformanceCounterKHR), ("counterDescriptions" ::: Vector PerformanceCounterDescriptionKHR))
enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR physicalDevice queueFamilyIndex = liftIO . evalContT $ do
  let vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHRPtr = pVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR is null" Nothing Nothing
  let vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR' = mkVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPCounterCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR' physicalDevice' (queueFamilyIndex) (pPCounterCount) (nullPtr) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCounterCount <- lift $ peek @Word32 pPCounterCount
  pPCounters <- ContT $ bracket (callocBytes @PerformanceCounterKHR ((fromIntegral (pCounterCount)) * 48)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPCounters `advancePtrBytes` (i * 48) :: Ptr PerformanceCounterKHR) . ($ ())) [0..(fromIntegral (pCounterCount)) - 1]
  pPCounterDescriptions <- ContT $ bracket (callocBytes @PerformanceCounterDescriptionKHR ((fromIntegral (pCounterCount)) * 792)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPCounterDescriptions `advancePtrBytes` (i * 792) :: Ptr PerformanceCounterDescriptionKHR) . ($ ())) [0..(fromIntegral (pCounterCount)) - 1]
  r' <- lift $ vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR' physicalDevice' (queueFamilyIndex) (pPCounterCount) ((pPCounters)) ((pPCounterDescriptions))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pCounterCount' <- lift $ peek @Word32 pPCounterCount
  let x33 = pCounterCount'
  pCounters' <- lift $ generateM (fromIntegral x33) (\i -> peekCStruct @PerformanceCounterKHR (((pPCounters) `advancePtrBytes` (48 * (i)) :: Ptr PerformanceCounterKHR)))
  pCounterDescriptions' <- lift $ generateM (fromIntegral x33) (\i -> peekCStruct @PerformanceCounterDescriptionKHR (((pPCounterDescriptions) `advancePtrBytes` (792 * (i)) :: Ptr PerformanceCounterDescriptionKHR)))
  pure $ ((r'), pCounters', pCounterDescriptions')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr QueryPoolPerformanceCreateInfoKHR -> Ptr Word32 -> IO ()) -> Ptr PhysicalDevice_T -> Ptr QueryPoolPerformanceCreateInfoKHR -> Ptr Word32 -> IO ()

-- No documentation found for TopLevel "vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR"
getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR :: forall io
                                                       . (MonadIO io)
                                                      => -- No documentation found for Nested "vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR" "physicalDevice"
                                                         PhysicalDevice
                                                      -> -- No documentation found for Nested "vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR" "pPerformanceQueryCreateInfo"
                                                         ("performanceQueryCreateInfo" ::: QueryPoolPerformanceCreateInfoKHR)
                                                      -> io (("numPasses" ::: Word32))
getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR physicalDevice performanceQueryCreateInfo = liftIO . evalContT $ do
  let vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHRPtr = pVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR' = mkVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHRPtr
  pPerformanceQueryCreateInfo <- ContT $ withCStruct (performanceQueryCreateInfo)
  pPNumPasses <- ContT $ bracket (callocBytes @Word32 4) free
  lift $ vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR' (physicalDeviceHandle (physicalDevice)) pPerformanceQueryCreateInfo (pPNumPasses)
  pNumPasses <- lift $ peek @Word32 pPNumPasses
  pure $ (pNumPasses)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireProfilingLockKHR
  :: FunPtr (Ptr Device_T -> Ptr AcquireProfilingLockInfoKHR -> IO Result) -> Ptr Device_T -> Ptr AcquireProfilingLockInfoKHR -> IO Result

-- No documentation found for TopLevel "vkAcquireProfilingLockKHR"
acquireProfilingLockKHR :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkAcquireProfilingLockKHR" "device"
                           Device
                        -> -- No documentation found for Nested "vkAcquireProfilingLockKHR" "pInfo"
                           AcquireProfilingLockInfoKHR
                        -> io ()
acquireProfilingLockKHR device info = liftIO . evalContT $ do
  let vkAcquireProfilingLockKHRPtr = pVkAcquireProfilingLockKHR (deviceCmds (device :: Device))
  lift $ unless (vkAcquireProfilingLockKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkAcquireProfilingLockKHR is null" Nothing Nothing
  let vkAcquireProfilingLockKHR' = mkVkAcquireProfilingLockKHR vkAcquireProfilingLockKHRPtr
  pInfo <- ContT $ withCStruct (info)
  r <- lift $ vkAcquireProfilingLockKHR' (deviceHandle (device)) pInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseProfilingLockKHR
  :: FunPtr (Ptr Device_T -> IO ()) -> Ptr Device_T -> IO ()

-- No documentation found for TopLevel "vkReleaseProfilingLockKHR"
releaseProfilingLockKHR :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkReleaseProfilingLockKHR" "device"
                           Device
                        -> io ()
releaseProfilingLockKHR device = liftIO $ do
  let vkReleaseProfilingLockKHRPtr = pVkReleaseProfilingLockKHR (deviceCmds (device :: Device))
  unless (vkReleaseProfilingLockKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkReleaseProfilingLockKHR is null" Nothing Nothing
  let vkReleaseProfilingLockKHR' = mkVkReleaseProfilingLockKHR vkReleaseProfilingLockKHRPtr
  vkReleaseProfilingLockKHR' (deviceHandle (device))
  pure $ ()


-- No documentation found for TopLevel "VK_QUERY_SCOPE_COMMAND_BUFFER_KHR"
pattern QUERY_SCOPE_COMMAND_BUFFER_KHR = PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR


-- No documentation found for TopLevel "VK_QUERY_SCOPE_RENDER_PASS_KHR"
pattern QUERY_SCOPE_RENDER_PASS_KHR = PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR


-- No documentation found for TopLevel "VK_QUERY_SCOPE_COMMAND_KHR"
pattern QUERY_SCOPE_COMMAND_KHR = PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR


-- No documentation found for TopLevel "VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR"
pattern PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR = PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR


-- No documentation found for TopLevel "VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR"
pattern PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR = PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR



-- No documentation found for TopLevel "VkPhysicalDevicePerformanceQueryFeaturesKHR"
data PhysicalDevicePerformanceQueryFeaturesKHR = PhysicalDevicePerformanceQueryFeaturesKHR
  { -- No documentation found for Nested "VkPhysicalDevicePerformanceQueryFeaturesKHR" "performanceCounterQueryPools"
    performanceCounterQueryPools :: Bool
  , -- No documentation found for Nested "VkPhysicalDevicePerformanceQueryFeaturesKHR" "performanceCounterMultipleQueryPools"
    performanceCounterMultipleQueryPools :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePerformanceQueryFeaturesKHR)
#endif
deriving instance Show PhysicalDevicePerformanceQueryFeaturesKHR

instance ToCStruct PhysicalDevicePerformanceQueryFeaturesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePerformanceQueryFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (performanceCounterQueryPools))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (performanceCounterMultipleQueryPools))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePerformanceQueryFeaturesKHR where
  peekCStruct p = do
    performanceCounterQueryPools <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    performanceCounterMultipleQueryPools <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDevicePerformanceQueryFeaturesKHR
             (bool32ToBool performanceCounterQueryPools) (bool32ToBool performanceCounterMultipleQueryPools)


instance Storable PhysicalDevicePerformanceQueryFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePerformanceQueryFeaturesKHR where
  zero = PhysicalDevicePerformanceQueryFeaturesKHR
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDevicePerformanceQueryPropertiesKHR"
data PhysicalDevicePerformanceQueryPropertiesKHR = PhysicalDevicePerformanceQueryPropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDevicePerformanceQueryPropertiesKHR" "allowCommandBufferQueryCopies"
    allowCommandBufferQueryCopies :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePerformanceQueryPropertiesKHR)
#endif
deriving instance Show PhysicalDevicePerformanceQueryPropertiesKHR

instance ToCStruct PhysicalDevicePerformanceQueryPropertiesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePerformanceQueryPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (allowCommandBufferQueryCopies))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePerformanceQueryPropertiesKHR where
  peekCStruct p = do
    allowCommandBufferQueryCopies <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePerformanceQueryPropertiesKHR
             (bool32ToBool allowCommandBufferQueryCopies)


instance Storable PhysicalDevicePerformanceQueryPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePerformanceQueryPropertiesKHR where
  zero = PhysicalDevicePerformanceQueryPropertiesKHR
           zero



-- No documentation found for TopLevel "VkPerformanceCounterKHR"
data PerformanceCounterKHR = PerformanceCounterKHR
  { -- No documentation found for Nested "VkPerformanceCounterKHR" "unit"
    unit :: PerformanceCounterUnitKHR
  , -- No documentation found for Nested "VkPerformanceCounterKHR" "scope"
    scope :: PerformanceCounterScopeKHR
  , -- No documentation found for Nested "VkPerformanceCounterKHR" "storage"
    storage :: PerformanceCounterStorageKHR
  , -- No documentation found for Nested "VkPerformanceCounterKHR" "uuid"
    uuid :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerformanceCounterKHR)
#endif
deriving instance Show PerformanceCounterKHR

instance ToCStruct PerformanceCounterKHR where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerformanceCounterKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerformanceCounterUnitKHR)) (unit)
    poke ((p `plusPtr` 20 :: Ptr PerformanceCounterScopeKHR)) (scope)
    poke ((p `plusPtr` 24 :: Ptr PerformanceCounterStorageKHR)) (storage)
    pokeFixedLengthByteString ((p `plusPtr` 28 :: Ptr (FixedArray UUID_SIZE Word8))) (uuid)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerformanceCounterUnitKHR)) (zero)
    poke ((p `plusPtr` 20 :: Ptr PerformanceCounterScopeKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PerformanceCounterStorageKHR)) (zero)
    pokeFixedLengthByteString ((p `plusPtr` 28 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    f

instance FromCStruct PerformanceCounterKHR where
  peekCStruct p = do
    unit <- peek @PerformanceCounterUnitKHR ((p `plusPtr` 16 :: Ptr PerformanceCounterUnitKHR))
    scope <- peek @PerformanceCounterScopeKHR ((p `plusPtr` 20 :: Ptr PerformanceCounterScopeKHR))
    storage <- peek @PerformanceCounterStorageKHR ((p `plusPtr` 24 :: Ptr PerformanceCounterStorageKHR))
    uuid <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 28 :: Ptr (FixedArray UUID_SIZE Word8)))
    pure $ PerformanceCounterKHR
             unit scope storage uuid


instance Storable PerformanceCounterKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerformanceCounterKHR where
  zero = PerformanceCounterKHR
           zero
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkPerformanceCounterDescriptionKHR"
data PerformanceCounterDescriptionKHR = PerformanceCounterDescriptionKHR
  { -- No documentation found for Nested "VkPerformanceCounterDescriptionKHR" "flags"
    flags :: PerformanceCounterDescriptionFlagsKHR
  , -- No documentation found for Nested "VkPerformanceCounterDescriptionKHR" "name"
    name :: ByteString
  , -- No documentation found for Nested "VkPerformanceCounterDescriptionKHR" "category"
    category :: ByteString
  , -- No documentation found for Nested "VkPerformanceCounterDescriptionKHR" "description"
    description :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerformanceCounterDescriptionKHR)
#endif
deriving instance Show PerformanceCounterDescriptionKHR

instance ToCStruct PerformanceCounterDescriptionKHR where
  withCStruct x f = allocaBytesAligned 792 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerformanceCounterDescriptionKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerformanceCounterDescriptionFlagsKHR)) (flags)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (name)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (category)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 532 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    f
  cStructSize = 792
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 532 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    f

instance FromCStruct PerformanceCounterDescriptionKHR where
  peekCStruct p = do
    flags <- peek @PerformanceCounterDescriptionFlagsKHR ((p `plusPtr` 16 :: Ptr PerformanceCounterDescriptionFlagsKHR))
    name <- packCString (lowerArrayPtr ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    category <- packCString (lowerArrayPtr ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 532 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    pure $ PerformanceCounterDescriptionKHR
             flags name category description


instance Storable PerformanceCounterDescriptionKHR where
  sizeOf ~_ = 792
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerformanceCounterDescriptionKHR where
  zero = PerformanceCounterDescriptionKHR
           zero
           mempty
           mempty
           mempty



-- No documentation found for TopLevel "VkQueryPoolPerformanceCreateInfoKHR"
data QueryPoolPerformanceCreateInfoKHR = QueryPoolPerformanceCreateInfoKHR
  { -- No documentation found for Nested "VkQueryPoolPerformanceCreateInfoKHR" "queueFamilyIndex"
    queueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkQueryPoolPerformanceCreateInfoKHR" "pCounterIndices"
    counterIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueryPoolPerformanceCreateInfoKHR)
#endif
deriving instance Show QueryPoolPerformanceCreateInfoKHR

instance ToCStruct QueryPoolPerformanceCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueryPoolPerformanceCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (queueFamilyIndex)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (counterIndices)) :: Word32))
    pPCounterIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (counterIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (counterIndices)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPCounterIndices')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    pPCounterIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCounterIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPCounterIndices')
    lift $ f

instance FromCStruct QueryPoolPerformanceCreateInfoKHR where
  peekCStruct p = do
    queueFamilyIndex <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    counterIndexCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pCounterIndices <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pCounterIndices' <- generateM (fromIntegral counterIndexCount) (\i -> peek @Word32 ((pCounterIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ QueryPoolPerformanceCreateInfoKHR
             queueFamilyIndex pCounterIndices'

instance Zero QueryPoolPerformanceCreateInfoKHR where
  zero = QueryPoolPerformanceCreateInfoKHR
           zero
           mempty



-- No documentation found for TopLevel "VkAcquireProfilingLockInfoKHR"
data AcquireProfilingLockInfoKHR = AcquireProfilingLockInfoKHR
  { -- No documentation found for Nested "VkAcquireProfilingLockInfoKHR" "flags"
    flags :: AcquireProfilingLockFlagsKHR
  , -- No documentation found for Nested "VkAcquireProfilingLockInfoKHR" "timeout"
    timeout :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AcquireProfilingLockInfoKHR)
#endif
deriving instance Show AcquireProfilingLockInfoKHR

instance ToCStruct AcquireProfilingLockInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AcquireProfilingLockInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AcquireProfilingLockFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (timeout)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct AcquireProfilingLockInfoKHR where
  peekCStruct p = do
    flags <- peek @AcquireProfilingLockFlagsKHR ((p `plusPtr` 16 :: Ptr AcquireProfilingLockFlagsKHR))
    timeout <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pure $ AcquireProfilingLockInfoKHR
             flags timeout


instance Storable AcquireProfilingLockInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AcquireProfilingLockInfoKHR where
  zero = AcquireProfilingLockInfoKHR
           zero
           zero



-- No documentation found for TopLevel "VkPerformanceQuerySubmitInfoKHR"
data PerformanceQuerySubmitInfoKHR = PerformanceQuerySubmitInfoKHR
  { -- No documentation found for Nested "VkPerformanceQuerySubmitInfoKHR" "counterPassIndex"
    counterPassIndex :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerformanceQuerySubmitInfoKHR)
#endif
deriving instance Show PerformanceQuerySubmitInfoKHR

instance ToCStruct PerformanceQuerySubmitInfoKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerformanceQuerySubmitInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (counterPassIndex)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PerformanceQuerySubmitInfoKHR where
  peekCStruct p = do
    counterPassIndex <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PerformanceQuerySubmitInfoKHR
             counterPassIndex


instance Storable PerformanceQuerySubmitInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerformanceQuerySubmitInfoKHR where
  zero = PerformanceQuerySubmitInfoKHR
           zero


data PerformanceCounterResultKHR
  = Int32Counter Int32
  | Int64Counter Int64
  | Uint32Counter Word32
  | Uint64Counter Word64
  | Float32Counter Float
  | Float64Counter Double
  deriving (Show)

instance ToCStruct PerformanceCounterResultKHR where
  withCStruct x f = allocaBytesAligned 8 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr PerformanceCounterResultKHR -> PerformanceCounterResultKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    Int32Counter v -> lift $ poke (castPtr @_ @Int32 p) (v)
    Int64Counter v -> lift $ poke (castPtr @_ @Int64 p) (v)
    Uint32Counter v -> lift $ poke (castPtr @_ @Word32 p) (v)
    Uint64Counter v -> lift $ poke (castPtr @_ @Word64 p) (v)
    Float32Counter v -> lift $ poke (castPtr @_ @CFloat p) (CFloat (v))
    Float64Counter v -> lift $ poke (castPtr @_ @CDouble p) (CDouble (v))
  pokeZeroCStruct :: Ptr PerformanceCounterResultKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero PerformanceCounterResultKHR where
  zero = Int64Counter zero


-- No documentation found for TopLevel "VkPerformanceCounterScopeKHR"
newtype PerformanceCounterScopeKHR = PerformanceCounterScopeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPerformanceCounterScopeKHR" "VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR"
pattern PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR = PerformanceCounterScopeKHR 0
-- No documentation found for Nested "VkPerformanceCounterScopeKHR" "VK_PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR"
pattern PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR    = PerformanceCounterScopeKHR 1
-- No documentation found for Nested "VkPerformanceCounterScopeKHR" "VK_PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR"
pattern PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR        = PerformanceCounterScopeKHR 2
{-# complete PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR,
             PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR,
             PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR :: PerformanceCounterScopeKHR #-}

conNamePerformanceCounterScopeKHR :: String
conNamePerformanceCounterScopeKHR = "PerformanceCounterScopeKHR"

enumPrefixPerformanceCounterScopeKHR :: String
enumPrefixPerformanceCounterScopeKHR = "PERFORMANCE_COUNTER_SCOPE_"

showTablePerformanceCounterScopeKHR :: [(PerformanceCounterScopeKHR, String)]
showTablePerformanceCounterScopeKHR =
  [ (PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR, "COMMAND_BUFFER_KHR")
  , (PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR   , "RENDER_PASS_KHR")
  , (PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR       , "COMMAND_KHR")
  ]


instance Show PerformanceCounterScopeKHR where
showsPrec = enumShowsPrec enumPrefixPerformanceCounterScopeKHR
                          showTablePerformanceCounterScopeKHR
                          conNamePerformanceCounterScopeKHR
                          (\(PerformanceCounterScopeKHR x) -> x)
                          (showsPrec 11)


instance Read PerformanceCounterScopeKHR where
  readPrec = enumReadPrec enumPrefixPerformanceCounterScopeKHR
                          showTablePerformanceCounterScopeKHR
                          conNamePerformanceCounterScopeKHR
                          PerformanceCounterScopeKHR


-- No documentation found for TopLevel "VkPerformanceCounterUnitKHR"
newtype PerformanceCounterUnitKHR = PerformanceCounterUnitKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPerformanceCounterUnitKHR" "VK_PERFORMANCE_COUNTER_UNIT_GENERIC_KHR"
pattern PERFORMANCE_COUNTER_UNIT_GENERIC_KHR          = PerformanceCounterUnitKHR 0
-- No documentation found for Nested "VkPerformanceCounterUnitKHR" "VK_PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR"
pattern PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR       = PerformanceCounterUnitKHR 1
-- No documentation found for Nested "VkPerformanceCounterUnitKHR" "VK_PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR"
pattern PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR      = PerformanceCounterUnitKHR 2
-- No documentation found for Nested "VkPerformanceCounterUnitKHR" "VK_PERFORMANCE_COUNTER_UNIT_BYTES_KHR"
pattern PERFORMANCE_COUNTER_UNIT_BYTES_KHR            = PerformanceCounterUnitKHR 3
-- No documentation found for Nested "VkPerformanceCounterUnitKHR" "VK_PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR"
pattern PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR = PerformanceCounterUnitKHR 4
-- No documentation found for Nested "VkPerformanceCounterUnitKHR" "VK_PERFORMANCE_COUNTER_UNIT_KELVIN_KHR"
pattern PERFORMANCE_COUNTER_UNIT_KELVIN_KHR           = PerformanceCounterUnitKHR 5
-- No documentation found for Nested "VkPerformanceCounterUnitKHR" "VK_PERFORMANCE_COUNTER_UNIT_WATTS_KHR"
pattern PERFORMANCE_COUNTER_UNIT_WATTS_KHR            = PerformanceCounterUnitKHR 6
-- No documentation found for Nested "VkPerformanceCounterUnitKHR" "VK_PERFORMANCE_COUNTER_UNIT_VOLTS_KHR"
pattern PERFORMANCE_COUNTER_UNIT_VOLTS_KHR            = PerformanceCounterUnitKHR 7
-- No documentation found for Nested "VkPerformanceCounterUnitKHR" "VK_PERFORMANCE_COUNTER_UNIT_AMPS_KHR"
pattern PERFORMANCE_COUNTER_UNIT_AMPS_KHR             = PerformanceCounterUnitKHR 8
-- No documentation found for Nested "VkPerformanceCounterUnitKHR" "VK_PERFORMANCE_COUNTER_UNIT_HERTZ_KHR"
pattern PERFORMANCE_COUNTER_UNIT_HERTZ_KHR            = PerformanceCounterUnitKHR 9
-- No documentation found for Nested "VkPerformanceCounterUnitKHR" "VK_PERFORMANCE_COUNTER_UNIT_CYCLES_KHR"
pattern PERFORMANCE_COUNTER_UNIT_CYCLES_KHR           = PerformanceCounterUnitKHR 10
{-# complete PERFORMANCE_COUNTER_UNIT_GENERIC_KHR,
             PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR,
             PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR,
             PERFORMANCE_COUNTER_UNIT_BYTES_KHR,
             PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR,
             PERFORMANCE_COUNTER_UNIT_KELVIN_KHR,
             PERFORMANCE_COUNTER_UNIT_WATTS_KHR,
             PERFORMANCE_COUNTER_UNIT_VOLTS_KHR,
             PERFORMANCE_COUNTER_UNIT_AMPS_KHR,
             PERFORMANCE_COUNTER_UNIT_HERTZ_KHR,
             PERFORMANCE_COUNTER_UNIT_CYCLES_KHR :: PerformanceCounterUnitKHR #-}

conNamePerformanceCounterUnitKHR :: String
conNamePerformanceCounterUnitKHR = "PerformanceCounterUnitKHR"

enumPrefixPerformanceCounterUnitKHR :: String
enumPrefixPerformanceCounterUnitKHR = "PERFORMANCE_COUNTER_UNIT_"

showTablePerformanceCounterUnitKHR :: [(PerformanceCounterUnitKHR, String)]
showTablePerformanceCounterUnitKHR =
  [ (PERFORMANCE_COUNTER_UNIT_GENERIC_KHR         , "GENERIC_KHR")
  , (PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR      , "PERCENTAGE_KHR")
  , (PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR     , "NANOSECONDS_KHR")
  , (PERFORMANCE_COUNTER_UNIT_BYTES_KHR           , "BYTES_KHR")
  , (PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR, "BYTES_PER_SECOND_KHR")
  , (PERFORMANCE_COUNTER_UNIT_KELVIN_KHR          , "KELVIN_KHR")
  , (PERFORMANCE_COUNTER_UNIT_WATTS_KHR           , "WATTS_KHR")
  , (PERFORMANCE_COUNTER_UNIT_VOLTS_KHR           , "VOLTS_KHR")
  , (PERFORMANCE_COUNTER_UNIT_AMPS_KHR            , "AMPS_KHR")
  , (PERFORMANCE_COUNTER_UNIT_HERTZ_KHR           , "HERTZ_KHR")
  , (PERFORMANCE_COUNTER_UNIT_CYCLES_KHR          , "CYCLES_KHR")
  ]


instance Show PerformanceCounterUnitKHR where
showsPrec = enumShowsPrec enumPrefixPerformanceCounterUnitKHR
                          showTablePerformanceCounterUnitKHR
                          conNamePerformanceCounterUnitKHR
                          (\(PerformanceCounterUnitKHR x) -> x)
                          (showsPrec 11)


instance Read PerformanceCounterUnitKHR where
  readPrec = enumReadPrec enumPrefixPerformanceCounterUnitKHR
                          showTablePerformanceCounterUnitKHR
                          conNamePerformanceCounterUnitKHR
                          PerformanceCounterUnitKHR


-- No documentation found for TopLevel "VkPerformanceCounterStorageKHR"
newtype PerformanceCounterStorageKHR = PerformanceCounterStorageKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPerformanceCounterStorageKHR" "VK_PERFORMANCE_COUNTER_STORAGE_INT32_KHR"
pattern PERFORMANCE_COUNTER_STORAGE_INT32_KHR   = PerformanceCounterStorageKHR 0
-- No documentation found for Nested "VkPerformanceCounterStorageKHR" "VK_PERFORMANCE_COUNTER_STORAGE_INT64_KHR"
pattern PERFORMANCE_COUNTER_STORAGE_INT64_KHR   = PerformanceCounterStorageKHR 1
-- No documentation found for Nested "VkPerformanceCounterStorageKHR" "VK_PERFORMANCE_COUNTER_STORAGE_UINT32_KHR"
pattern PERFORMANCE_COUNTER_STORAGE_UINT32_KHR  = PerformanceCounterStorageKHR 2
-- No documentation found for Nested "VkPerformanceCounterStorageKHR" "VK_PERFORMANCE_COUNTER_STORAGE_UINT64_KHR"
pattern PERFORMANCE_COUNTER_STORAGE_UINT64_KHR  = PerformanceCounterStorageKHR 3
-- No documentation found for Nested "VkPerformanceCounterStorageKHR" "VK_PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR"
pattern PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR = PerformanceCounterStorageKHR 4
-- No documentation found for Nested "VkPerformanceCounterStorageKHR" "VK_PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR"
pattern PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR = PerformanceCounterStorageKHR 5
{-# complete PERFORMANCE_COUNTER_STORAGE_INT32_KHR,
             PERFORMANCE_COUNTER_STORAGE_INT64_KHR,
             PERFORMANCE_COUNTER_STORAGE_UINT32_KHR,
             PERFORMANCE_COUNTER_STORAGE_UINT64_KHR,
             PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR,
             PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR :: PerformanceCounterStorageKHR #-}

conNamePerformanceCounterStorageKHR :: String
conNamePerformanceCounterStorageKHR = "PerformanceCounterStorageKHR"

enumPrefixPerformanceCounterStorageKHR :: String
enumPrefixPerformanceCounterStorageKHR = "PERFORMANCE_COUNTER_STORAGE_"

showTablePerformanceCounterStorageKHR :: [(PerformanceCounterStorageKHR, String)]
showTablePerformanceCounterStorageKHR =
  [ (PERFORMANCE_COUNTER_STORAGE_INT32_KHR  , "INT32_KHR")
  , (PERFORMANCE_COUNTER_STORAGE_INT64_KHR  , "INT64_KHR")
  , (PERFORMANCE_COUNTER_STORAGE_UINT32_KHR , "UINT32_KHR")
  , (PERFORMANCE_COUNTER_STORAGE_UINT64_KHR , "UINT64_KHR")
  , (PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR, "FLOAT32_KHR")
  , (PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR, "FLOAT64_KHR")
  ]


instance Show PerformanceCounterStorageKHR where
showsPrec = enumShowsPrec enumPrefixPerformanceCounterStorageKHR
                          showTablePerformanceCounterStorageKHR
                          conNamePerformanceCounterStorageKHR
                          (\(PerformanceCounterStorageKHR x) -> x)
                          (showsPrec 11)


instance Read PerformanceCounterStorageKHR where
  readPrec = enumReadPrec enumPrefixPerformanceCounterStorageKHR
                          showTablePerformanceCounterStorageKHR
                          conNamePerformanceCounterStorageKHR
                          PerformanceCounterStorageKHR


type PerformanceCounterDescriptionFlagsKHR = PerformanceCounterDescriptionFlagBitsKHR

-- No documentation found for TopLevel "VkPerformanceCounterDescriptionFlagBitsKHR"
newtype PerformanceCounterDescriptionFlagBitsKHR = PerformanceCounterDescriptionFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPerformanceCounterDescriptionFlagBitsKHR" "VK_PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR"
pattern PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR =
  PerformanceCounterDescriptionFlagBitsKHR 0x00000001
-- No documentation found for Nested "VkPerformanceCounterDescriptionFlagBitsKHR" "VK_PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR"
pattern PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR =
  PerformanceCounterDescriptionFlagBitsKHR 0x00000002

conNamePerformanceCounterDescriptionFlagBitsKHR :: String
conNamePerformanceCounterDescriptionFlagBitsKHR = "PerformanceCounterDescriptionFlagBitsKHR"

enumPrefixPerformanceCounterDescriptionFlagBitsKHR :: String
enumPrefixPerformanceCounterDescriptionFlagBitsKHR = "PERFORMANCE_COUNTER_DESCRIPTION_"

showTablePerformanceCounterDescriptionFlagBitsKHR :: [(PerformanceCounterDescriptionFlagBitsKHR, String)]
showTablePerformanceCounterDescriptionFlagBitsKHR =
  [ (PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_BIT_KHR, "PERFORMANCE_IMPACTING_BIT_KHR")
  , (PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_BIT_KHR, "CONCURRENTLY_IMPACTED_BIT_KHR")
  ]


instance Show PerformanceCounterDescriptionFlagBitsKHR where
showsPrec = enumShowsPrec enumPrefixPerformanceCounterDescriptionFlagBitsKHR
                          showTablePerformanceCounterDescriptionFlagBitsKHR
                          conNamePerformanceCounterDescriptionFlagBitsKHR
                          (\(PerformanceCounterDescriptionFlagBitsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read PerformanceCounterDescriptionFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixPerformanceCounterDescriptionFlagBitsKHR
                          showTablePerformanceCounterDescriptionFlagBitsKHR
                          conNamePerformanceCounterDescriptionFlagBitsKHR
                          PerformanceCounterDescriptionFlagBitsKHR


type AcquireProfilingLockFlagsKHR = AcquireProfilingLockFlagBitsKHR

-- No documentation found for TopLevel "VkAcquireProfilingLockFlagBitsKHR"
newtype AcquireProfilingLockFlagBitsKHR = AcquireProfilingLockFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameAcquireProfilingLockFlagBitsKHR :: String
conNameAcquireProfilingLockFlagBitsKHR = "AcquireProfilingLockFlagBitsKHR"

enumPrefixAcquireProfilingLockFlagBitsKHR :: String
enumPrefixAcquireProfilingLockFlagBitsKHR = ""

showTableAcquireProfilingLockFlagBitsKHR :: [(AcquireProfilingLockFlagBitsKHR, String)]
showTableAcquireProfilingLockFlagBitsKHR = []


instance Show AcquireProfilingLockFlagBitsKHR where
showsPrec = enumShowsPrec enumPrefixAcquireProfilingLockFlagBitsKHR
                          showTableAcquireProfilingLockFlagBitsKHR
                          conNameAcquireProfilingLockFlagBitsKHR
                          (\(AcquireProfilingLockFlagBitsKHR x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read AcquireProfilingLockFlagBitsKHR where
  readPrec = enumReadPrec enumPrefixAcquireProfilingLockFlagBitsKHR
                          showTableAcquireProfilingLockFlagBitsKHR
                          conNameAcquireProfilingLockFlagBitsKHR
                          AcquireProfilingLockFlagBitsKHR


type KHR_PERFORMANCE_QUERY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PERFORMANCE_QUERY_SPEC_VERSION"
pattern KHR_PERFORMANCE_QUERY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PERFORMANCE_QUERY_SPEC_VERSION = 1


type KHR_PERFORMANCE_QUERY_EXTENSION_NAME = "VK_KHR_performance_query"

-- No documentation found for TopLevel "VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME"
pattern KHR_PERFORMANCE_QUERY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PERFORMANCE_QUERY_EXTENSION_NAME = "VK_KHR_performance_query"

