{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_performance_query  ( enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR
                                                   , getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR
                                                   , acquireProfilingLockKHR
                                                   , releaseProfilingLockKHR
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
                                                   , PerformanceCounterDescriptionFlagBitsKHR( PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR
                                                                                             , PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR
                                                                                             , ..
                                                                                             )
                                                   , PerformanceCounterDescriptionFlagsKHR
                                                   , AcquireProfilingLockFlagBitsKHR(..)
                                                   , AcquireProfilingLockFlagsKHR
                                                   , KHR_PERFORMANCE_QUERY_SPEC_VERSION
                                                   , pattern KHR_PERFORMANCE_QUERY_SPEC_VERSION
                                                   , KHR_PERFORMANCE_QUERY_EXTENSION_NAME
                                                   , pattern KHR_PERFORMANCE_QUERY_EXTENSION_NAME
                                                   ) where

import Vulkan.CStruct.Utils (FixedArray)
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
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
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
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Word (Word8)
import Text.Read.Lex (Lexeme(Ident))
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

-- | vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR -
-- Reports properties of the performance query counters available on a
-- queue family of a device
--
-- = Description
--
-- If @pCounters@ is @NULL@ and @pCounterDescriptions@ is @NULL@, then the
-- number of counters available is returned in @pCounterCount@. Otherwise,
-- @pCounterCount@ /must/ point to a variable set by the user to the number
-- of elements in the @pCounters@, @pCounterDescriptions@, or both arrays
-- and on return the variable is overwritten with the number of structures
-- actually written out. If @pCounterCount@ is less than the number of
-- counters available, at most @pCounterCount@ structures will be written
-- and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS'.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   @pCounterCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pCounterCount@ is not @0@, and
--     @pCounters@ is not @NULL@, @pCounters@ /must/ be a valid pointer to
--     an array of @pCounterCount@ 'PerformanceCounterKHR' structures
--
-- -   If the value referenced by @pCounterCount@ is not @0@, and
--     @pCounterDescriptions@ is not @NULL@, @pCounterDescriptions@ /must/
--     be a valid pointer to an array of @pCounterCount@
--     'PerformanceCounterDescriptionKHR' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'PerformanceCounterDescriptionKHR', 'PerformanceCounterKHR',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR :: forall io
                                                               . (MonadIO io)
                                                              => -- | @physicalDevice@ is the handle to the physical device whose queue family
                                                                 -- performance query counter properties will be queried.
                                                                 PhysicalDevice
                                                              -> -- | @queueFamilyIndex@ is the index into the queue family of the physical
                                                                 -- device we want to get properties for.
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

-- | vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR - Reports the
-- number of passes require for a performance query pool type
--
-- = Description
--
-- The @pPerformanceQueryCreateInfo@ member
-- 'QueryPoolPerformanceCreateInfoKHR'::@queueFamilyIndex@ /must/ be a
-- queue family of @physicalDevice@. The number of passes required to
-- capture the counters specified in the @pPerformanceQueryCreateInfo@
-- member 'QueryPoolPerformanceCreateInfoKHR'::@pCounters@ is returned in
-- @pNumPasses@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'QueryPoolPerformanceCreateInfoKHR'
getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR :: forall io
                                                       . (MonadIO io)
                                                      => -- | @physicalDevice@ is the handle to the physical device whose queue family
                                                         -- performance query counter properties will be queried.
                                                         --
                                                         -- @physicalDevice@ /must/ be a valid
                                                         -- 'Vulkan.Core10.Handles.PhysicalDevice' handle
                                                         PhysicalDevice
                                                      -> -- | @pPerformanceQueryCreateInfo@ is a pointer to a
                                                         -- 'QueryPoolPerformanceCreateInfoKHR' of the performance query that is to
                                                         -- be created.
                                                         --
                                                         -- @pPerformanceQueryCreateInfo@ /must/ be a valid pointer to a valid
                                                         -- 'QueryPoolPerformanceCreateInfoKHR' structure
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

-- | vkAcquireProfilingLockKHR - Acquires the profiling lock
--
-- = Description
--
-- Implementations /may/ allow multiple actors to hold the profiling lock
-- concurrently.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.TIMEOUT'
--
-- = See Also
--
-- 'AcquireProfilingLockInfoKHR', 'Vulkan.Core10.Handles.Device'
acquireProfilingLockKHR :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the logical device to profile.
                           --
                           -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                           Device
                        -> -- | @pInfo@ is a pointer to a 'AcquireProfilingLockInfoKHR' structure which
                           -- contains information about how the profiling is to be acquired.
                           --
                           -- @pInfo@ /must/ be a valid pointer to a valid
                           -- 'AcquireProfilingLockInfoKHR' structure
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

-- | vkReleaseProfilingLockKHR - Releases the profiling lock
--
-- == Valid Usage
--
-- -   The profiling lock of @device@ /must/ have been held via a previous
--     successful call to 'acquireProfilingLockKHR'
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device'
releaseProfilingLockKHR :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the logical device to cease profiling on.
                           Device
                        -> io ()
releaseProfilingLockKHR device = liftIO $ do
  let vkReleaseProfilingLockKHRPtr = pVkReleaseProfilingLockKHR (deviceCmds (device :: Device))
  unless (vkReleaseProfilingLockKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkReleaseProfilingLockKHR is null" Nothing Nothing
  let vkReleaseProfilingLockKHR' = mkVkReleaseProfilingLockKHR vkReleaseProfilingLockKHRPtr
  vkReleaseProfilingLockKHR' (deviceHandle (device))
  pure $ ()


-- | VkPhysicalDevicePerformanceQueryFeaturesKHR - Structure describing
-- performance query support for an implementation
--
-- = Members
--
-- The members of the 'PhysicalDevicePerformanceQueryFeaturesKHR' structure
-- describe the following implementation-dependent features:
--
-- == Valid Usage (Implicit)
--
-- To query supported performance counter query pool features, call
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2'
-- with a 'PhysicalDevicePerformanceQueryFeaturesKHR' structure included in
-- the @pNext@ chain of its @pFeatures@ parameter. The
-- 'PhysicalDevicePerformanceQueryFeaturesKHR' structure /can/ also be
-- included in the @pNext@ chain of a
-- 'Vulkan.Core10.Device.DeviceCreateInfo' structure, in which case it
-- controls which additional features are enabled in the device.
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePerformanceQueryFeaturesKHR = PhysicalDevicePerformanceQueryFeaturesKHR
  { -- | @performanceCounterQueryPools@ is 'Vulkan.Core10.FundamentalTypes.TRUE'
    -- if the physical device supports performance counter query pools.
    performanceCounterQueryPools :: Bool
  , -- | @performanceCounterMultipleQueryPools@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE'\` if the physical device supports
    -- using multiple performance query pools in a primary command buffer and
    -- secondary command buffers executed within it.
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


-- | VkPhysicalDevicePerformanceQueryPropertiesKHR - Structure describing
-- performance query properties for an implementation
--
-- = Members
--
-- The members of the 'PhysicalDevicePerformanceQueryPropertiesKHR'
-- structure describe the following implementation-dependent properties:
--
-- == Valid Usage (Implicit)
--
-- If the 'PhysicalDevicePerformanceQueryPropertiesKHR' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent properties.
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePerformanceQueryPropertiesKHR = PhysicalDevicePerformanceQueryPropertiesKHR
  { -- | @allowCommandBufferQueryCopies@ is 'Vulkan.Core10.FundamentalTypes.TRUE'
    -- if the performance query pools are allowed to be used with
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults'.
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


-- | VkPerformanceCounterKHR - Structure providing information about a
-- counter
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PerformanceCounterScopeKHR', 'PerformanceCounterStorageKHR',
-- 'PerformanceCounterUnitKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR'
data PerformanceCounterKHR = PerformanceCounterKHR
  { -- | @unit@ is a 'PerformanceCounterUnitKHR' specifying the unit that the
    -- counter data will record.
    unit :: PerformanceCounterUnitKHR
  , -- | @scope@ is a 'PerformanceCounterScopeKHR' specifying the scope that the
    -- counter belongs to.
    scope :: PerformanceCounterScopeKHR
  , -- | @storage@ is a 'PerformanceCounterStorageKHR' specifying the storage
    -- type that the counter’s data uses.
    storage :: PerformanceCounterStorageKHR
  , -- | @uuid@ is an array of size 'Vulkan.Core10.APIConstants.UUID_SIZE',
    -- containing 8-bit values that represent a universally unique identifier
    -- for the counter of the physical device.
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


-- | VkPerformanceCounterDescriptionKHR - Structure providing more detailed
-- information about a counter
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PerformanceCounterDescriptionFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR'
data PerformanceCounterDescriptionKHR = PerformanceCounterDescriptionKHR
  { -- | @flags@ is a bitmask of 'PerformanceCounterDescriptionFlagBitsKHR'
    -- indicating the usage behavior for the counter.
    flags :: PerformanceCounterDescriptionFlagsKHR
  , -- | @name@ is an array of size
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE', containing a
    -- null-terminated UTF-8 string specifying the name of the counter.
    name :: ByteString
  , -- | @category@ is an array of size
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE', containing a
    -- null-terminated UTF-8 string specifying the category of the counter.
    category :: ByteString
  , -- | @description@ is an array of size
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE', containing a
    -- null-terminated UTF-8 string specifying the description of the counter.
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


-- | VkQueryPoolPerformanceCreateInfoKHR - Structure specifying parameters of
-- a newly created performance query pool
--
-- == Valid Usage
--
-- -   @queueFamilyIndex@ /must/ be a valid queue family index of the
--     device
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-features-performanceCounterQueryPools performanceCounterQueryPools>
--     feature /must/ be enabled
--
-- -   Each element of @pCounterIndices@ /must/ be in the range of counters
--     reported by
--     'enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR' for
--     the queue family specified in @queueFamilyIndex@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR'
--
-- -   @pCounterIndices@ /must/ be a valid pointer to an array of
--     @counterIndexCount@ @uint32_t@ values
--
-- -   @counterIndexCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'
data QueryPoolPerformanceCreateInfoKHR = QueryPoolPerformanceCreateInfoKHR
  { -- | @queueFamilyIndex@ is the queue family index to create this performance
    -- query pool for.
    queueFamilyIndex :: Word32
  , -- | @pCounterIndices@ is the array of indices into the
    -- 'enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR'::@pCounters@
    -- to enable in this performance query pool.
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


-- | VkAcquireProfilingLockInfoKHR - Structure specifying parameters to
-- acquire the profiling lock
--
-- == Valid Usage (Implicit)
--
-- If @timeout@ is 0, 'acquireProfilingLockKHR' will not block while
-- attempting to acquire the profling lock. If @timeout@ is @UINT64_MAX@,
-- the function will not return until the profiling lock was acquired.
--
-- = See Also
--
-- 'AcquireProfilingLockFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'acquireProfilingLockKHR'
data AcquireProfilingLockInfoKHR = AcquireProfilingLockInfoKHR
  { -- | @flags@ is reserved for future use.
    --
    -- @flags@ /must/ be @0@
    flags :: AcquireProfilingLockFlagsKHR
  , -- | @timeout@ indicates how long the function waits, in nanoseconds, if the
    -- profiling lock is not available.
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


-- | VkPerformanceQuerySubmitInfoKHR - Structure indicating which counter
-- pass index is active for performance queries
--
-- = Description
--
-- If the 'Vulkan.Core10.Queue.SubmitInfo'::@pNext@ chain does not include
-- this structure, the batch defaults to use counter pass index 0.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PerformanceQuerySubmitInfoKHR = PerformanceQuerySubmitInfoKHR
  { -- | @counterPassIndex@ specifies which counter pass index is active.
    --
    -- @counterPassIndex@ /must/ be less than the number of counter passes
    -- required by any queries within the batch. The required number of counter
    -- passes for a performance query is obtained by calling
    -- 'getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR'
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


-- | VkPerformanceCounterScopeKHR - Supported counter scope types
--
-- = See Also
--
-- 'PerformanceCounterKHR'
newtype PerformanceCounterScopeKHR = PerformanceCounterScopeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR' - the performance counter
-- scope is a single complete command buffer.
pattern PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR = PerformanceCounterScopeKHR 0
-- | 'PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR' - the performance counter
-- scope is zero or more complete render passes. The performance query
-- containing the performance counter /must/ begin and end outside a render
-- pass instance.
pattern PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR = PerformanceCounterScopeKHR 1
-- | 'PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR' - the performance counter scope
-- is zero or more commands.
pattern PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR = PerformanceCounterScopeKHR 2
{-# complete PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR,
             PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR,
             PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR :: PerformanceCounterScopeKHR #-}

instance Show PerformanceCounterScopeKHR where
  showsPrec p = \case
    PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR -> showString "PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR"
    PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR -> showString "PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR"
    PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR -> showString "PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR"
    PerformanceCounterScopeKHR x -> showParen (p >= 11) (showString "PerformanceCounterScopeKHR " . showsPrec 11 x)

instance Read PerformanceCounterScopeKHR where
  readPrec = parens (choose [("PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR", pure PERFORMANCE_COUNTER_SCOPE_COMMAND_BUFFER_KHR)
                            , ("PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR", pure PERFORMANCE_COUNTER_SCOPE_RENDER_PASS_KHR)
                            , ("PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR", pure PERFORMANCE_COUNTER_SCOPE_COMMAND_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "PerformanceCounterScopeKHR")
                       v <- step readPrec
                       pure (PerformanceCounterScopeKHR v)))


-- | VkPerformanceCounterUnitKHR - Supported counter unit types
--
-- = See Also
--
-- 'PerformanceCounterKHR'
newtype PerformanceCounterUnitKHR = PerformanceCounterUnitKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PERFORMANCE_COUNTER_UNIT_GENERIC_KHR' - the performance counter unit is
-- a generic data point.
pattern PERFORMANCE_COUNTER_UNIT_GENERIC_KHR = PerformanceCounterUnitKHR 0
-- | 'PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR' - the performance counter unit
-- is a percentage (%).
pattern PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR = PerformanceCounterUnitKHR 1
-- | 'PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR' - the performance counter
-- unit is a value of nanoseconds (ns).
pattern PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR = PerformanceCounterUnitKHR 2
-- | 'PERFORMANCE_COUNTER_UNIT_BYTES_KHR' - the performance counter unit is a
-- value of bytes.
pattern PERFORMANCE_COUNTER_UNIT_BYTES_KHR = PerformanceCounterUnitKHR 3
-- | 'PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR' - the performance
-- counter unit is a value of bytes\/s.
pattern PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR = PerformanceCounterUnitKHR 4
-- | 'PERFORMANCE_COUNTER_UNIT_KELVIN_KHR' - the performance counter unit is
-- a temperature reported in Kelvin.
pattern PERFORMANCE_COUNTER_UNIT_KELVIN_KHR = PerformanceCounterUnitKHR 5
-- | 'PERFORMANCE_COUNTER_UNIT_WATTS_KHR' - the performance counter unit is a
-- value of watts (W).
pattern PERFORMANCE_COUNTER_UNIT_WATTS_KHR = PerformanceCounterUnitKHR 6
-- | 'PERFORMANCE_COUNTER_UNIT_VOLTS_KHR' - the performance counter unit is a
-- value of volts (V).
pattern PERFORMANCE_COUNTER_UNIT_VOLTS_KHR = PerformanceCounterUnitKHR 7
-- | 'PERFORMANCE_COUNTER_UNIT_AMPS_KHR' - the performance counter unit is a
-- value of amps (A).
pattern PERFORMANCE_COUNTER_UNIT_AMPS_KHR = PerformanceCounterUnitKHR 8
-- | 'PERFORMANCE_COUNTER_UNIT_HERTZ_KHR' - the performance counter unit is a
-- value of hertz (Hz).
pattern PERFORMANCE_COUNTER_UNIT_HERTZ_KHR = PerformanceCounterUnitKHR 9
-- | 'PERFORMANCE_COUNTER_UNIT_CYCLES_KHR' - the performance counter unit is
-- a value of cycles.
pattern PERFORMANCE_COUNTER_UNIT_CYCLES_KHR = PerformanceCounterUnitKHR 10
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

instance Show PerformanceCounterUnitKHR where
  showsPrec p = \case
    PERFORMANCE_COUNTER_UNIT_GENERIC_KHR -> showString "PERFORMANCE_COUNTER_UNIT_GENERIC_KHR"
    PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR -> showString "PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR"
    PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR -> showString "PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR"
    PERFORMANCE_COUNTER_UNIT_BYTES_KHR -> showString "PERFORMANCE_COUNTER_UNIT_BYTES_KHR"
    PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR -> showString "PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR"
    PERFORMANCE_COUNTER_UNIT_KELVIN_KHR -> showString "PERFORMANCE_COUNTER_UNIT_KELVIN_KHR"
    PERFORMANCE_COUNTER_UNIT_WATTS_KHR -> showString "PERFORMANCE_COUNTER_UNIT_WATTS_KHR"
    PERFORMANCE_COUNTER_UNIT_VOLTS_KHR -> showString "PERFORMANCE_COUNTER_UNIT_VOLTS_KHR"
    PERFORMANCE_COUNTER_UNIT_AMPS_KHR -> showString "PERFORMANCE_COUNTER_UNIT_AMPS_KHR"
    PERFORMANCE_COUNTER_UNIT_HERTZ_KHR -> showString "PERFORMANCE_COUNTER_UNIT_HERTZ_KHR"
    PERFORMANCE_COUNTER_UNIT_CYCLES_KHR -> showString "PERFORMANCE_COUNTER_UNIT_CYCLES_KHR"
    PerformanceCounterUnitKHR x -> showParen (p >= 11) (showString "PerformanceCounterUnitKHR " . showsPrec 11 x)

instance Read PerformanceCounterUnitKHR where
  readPrec = parens (choose [("PERFORMANCE_COUNTER_UNIT_GENERIC_KHR", pure PERFORMANCE_COUNTER_UNIT_GENERIC_KHR)
                            , ("PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR", pure PERFORMANCE_COUNTER_UNIT_PERCENTAGE_KHR)
                            , ("PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR", pure PERFORMANCE_COUNTER_UNIT_NANOSECONDS_KHR)
                            , ("PERFORMANCE_COUNTER_UNIT_BYTES_KHR", pure PERFORMANCE_COUNTER_UNIT_BYTES_KHR)
                            , ("PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR", pure PERFORMANCE_COUNTER_UNIT_BYTES_PER_SECOND_KHR)
                            , ("PERFORMANCE_COUNTER_UNIT_KELVIN_KHR", pure PERFORMANCE_COUNTER_UNIT_KELVIN_KHR)
                            , ("PERFORMANCE_COUNTER_UNIT_WATTS_KHR", pure PERFORMANCE_COUNTER_UNIT_WATTS_KHR)
                            , ("PERFORMANCE_COUNTER_UNIT_VOLTS_KHR", pure PERFORMANCE_COUNTER_UNIT_VOLTS_KHR)
                            , ("PERFORMANCE_COUNTER_UNIT_AMPS_KHR", pure PERFORMANCE_COUNTER_UNIT_AMPS_KHR)
                            , ("PERFORMANCE_COUNTER_UNIT_HERTZ_KHR", pure PERFORMANCE_COUNTER_UNIT_HERTZ_KHR)
                            , ("PERFORMANCE_COUNTER_UNIT_CYCLES_KHR", pure PERFORMANCE_COUNTER_UNIT_CYCLES_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "PerformanceCounterUnitKHR")
                       v <- step readPrec
                       pure (PerformanceCounterUnitKHR v)))


-- | VkPerformanceCounterStorageKHR - Supported counter storage types
--
-- = See Also
--
-- 'PerformanceCounterKHR'
newtype PerformanceCounterStorageKHR = PerformanceCounterStorageKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PERFORMANCE_COUNTER_STORAGE_INT32_KHR' - the performance counter
-- storage is a 32-bit signed integer.
pattern PERFORMANCE_COUNTER_STORAGE_INT32_KHR = PerformanceCounterStorageKHR 0
-- | 'PERFORMANCE_COUNTER_STORAGE_INT64_KHR' - the performance counter
-- storage is a 64-bit signed integer.
pattern PERFORMANCE_COUNTER_STORAGE_INT64_KHR = PerformanceCounterStorageKHR 1
-- | 'PERFORMANCE_COUNTER_STORAGE_UINT32_KHR' - the performance counter
-- storage is a 32-bit unsigned integer.
pattern PERFORMANCE_COUNTER_STORAGE_UINT32_KHR = PerformanceCounterStorageKHR 2
-- | 'PERFORMANCE_COUNTER_STORAGE_UINT64_KHR' - the performance counter
-- storage is a 64-bit unsigned integer.
pattern PERFORMANCE_COUNTER_STORAGE_UINT64_KHR = PerformanceCounterStorageKHR 3
-- | 'PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR' - the performance counter
-- storage is a 32-bit floating-point.
pattern PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR = PerformanceCounterStorageKHR 4
-- | 'PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR' - the performance counter
-- storage is a 64-bit floating-point.
pattern PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR = PerformanceCounterStorageKHR 5
{-# complete PERFORMANCE_COUNTER_STORAGE_INT32_KHR,
             PERFORMANCE_COUNTER_STORAGE_INT64_KHR,
             PERFORMANCE_COUNTER_STORAGE_UINT32_KHR,
             PERFORMANCE_COUNTER_STORAGE_UINT64_KHR,
             PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR,
             PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR :: PerformanceCounterStorageKHR #-}

instance Show PerformanceCounterStorageKHR where
  showsPrec p = \case
    PERFORMANCE_COUNTER_STORAGE_INT32_KHR -> showString "PERFORMANCE_COUNTER_STORAGE_INT32_KHR"
    PERFORMANCE_COUNTER_STORAGE_INT64_KHR -> showString "PERFORMANCE_COUNTER_STORAGE_INT64_KHR"
    PERFORMANCE_COUNTER_STORAGE_UINT32_KHR -> showString "PERFORMANCE_COUNTER_STORAGE_UINT32_KHR"
    PERFORMANCE_COUNTER_STORAGE_UINT64_KHR -> showString "PERFORMANCE_COUNTER_STORAGE_UINT64_KHR"
    PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR -> showString "PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR"
    PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR -> showString "PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR"
    PerformanceCounterStorageKHR x -> showParen (p >= 11) (showString "PerformanceCounterStorageKHR " . showsPrec 11 x)

instance Read PerformanceCounterStorageKHR where
  readPrec = parens (choose [("PERFORMANCE_COUNTER_STORAGE_INT32_KHR", pure PERFORMANCE_COUNTER_STORAGE_INT32_KHR)
                            , ("PERFORMANCE_COUNTER_STORAGE_INT64_KHR", pure PERFORMANCE_COUNTER_STORAGE_INT64_KHR)
                            , ("PERFORMANCE_COUNTER_STORAGE_UINT32_KHR", pure PERFORMANCE_COUNTER_STORAGE_UINT32_KHR)
                            , ("PERFORMANCE_COUNTER_STORAGE_UINT64_KHR", pure PERFORMANCE_COUNTER_STORAGE_UINT64_KHR)
                            , ("PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR", pure PERFORMANCE_COUNTER_STORAGE_FLOAT32_KHR)
                            , ("PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR", pure PERFORMANCE_COUNTER_STORAGE_FLOAT64_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "PerformanceCounterStorageKHR")
                       v <- step readPrec
                       pure (PerformanceCounterStorageKHR v)))


-- | VkPerformanceCounterDescriptionFlagBitsKHR - Bitmask specifying usage
-- behavior for a counter
--
-- = See Also
--
-- 'PerformanceCounterDescriptionFlagsKHR'
newtype PerformanceCounterDescriptionFlagBitsKHR = PerformanceCounterDescriptionFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR' specifies
-- that recording the counter /may/ have a noticeable performance impact.
pattern PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR = PerformanceCounterDescriptionFlagBitsKHR 0x00000001
-- | 'PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR' specifies
-- that concurrently recording the counter while other submitted command
-- buffers are running /may/ impact the accuracy of the recording.
pattern PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR = PerformanceCounterDescriptionFlagBitsKHR 0x00000002

type PerformanceCounterDescriptionFlagsKHR = PerformanceCounterDescriptionFlagBitsKHR

instance Show PerformanceCounterDescriptionFlagBitsKHR where
  showsPrec p = \case
    PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR -> showString "PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR"
    PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR -> showString "PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR"
    PerformanceCounterDescriptionFlagBitsKHR x -> showParen (p >= 11) (showString "PerformanceCounterDescriptionFlagBitsKHR 0x" . showHex x)

instance Read PerformanceCounterDescriptionFlagBitsKHR where
  readPrec = parens (choose [("PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR", pure PERFORMANCE_COUNTER_DESCRIPTION_PERFORMANCE_IMPACTING_KHR)
                            , ("PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR", pure PERFORMANCE_COUNTER_DESCRIPTION_CONCURRENTLY_IMPACTED_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "PerformanceCounterDescriptionFlagBitsKHR")
                       v <- step readPrec
                       pure (PerformanceCounterDescriptionFlagBitsKHR v)))


-- | VkAcquireProfilingLockFlagBitsKHR - Reserved for future use
--
-- = See Also
--
-- 'AcquireProfilingLockFlagsKHR'
newtype AcquireProfilingLockFlagBitsKHR = AcquireProfilingLockFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



type AcquireProfilingLockFlagsKHR = AcquireProfilingLockFlagBitsKHR

instance Show AcquireProfilingLockFlagBitsKHR where
  showsPrec p = \case
    AcquireProfilingLockFlagBitsKHR x -> showParen (p >= 11) (showString "AcquireProfilingLockFlagBitsKHR 0x" . showHex x)

instance Read AcquireProfilingLockFlagBitsKHR where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "AcquireProfilingLockFlagBitsKHR")
                       v <- step readPrec
                       pure (AcquireProfilingLockFlagBitsKHR v)))


type KHR_PERFORMANCE_QUERY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PERFORMANCE_QUERY_SPEC_VERSION"
pattern KHR_PERFORMANCE_QUERY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PERFORMANCE_QUERY_SPEC_VERSION = 1


type KHR_PERFORMANCE_QUERY_EXTENSION_NAME = "VK_KHR_performance_query"

-- No documentation found for TopLevel "VK_KHR_PERFORMANCE_QUERY_EXTENSION_NAME"
pattern KHR_PERFORMANCE_QUERY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PERFORMANCE_QUERY_EXTENSION_NAME = "VK_KHR_performance_query"

