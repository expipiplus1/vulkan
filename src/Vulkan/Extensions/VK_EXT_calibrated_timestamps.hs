{-# language CPP #-}
-- | = Name
--
-- VK_EXT_calibrated_timestamps - device extension
--
-- == VK_EXT_calibrated_timestamps
--
-- [__Name String__]
--     @VK_EXT_calibrated_timestamps@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     185
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_calibrated_timestamps] @drakos-amd%0A*Here describe the issue or question you have about the VK_EXT_calibrated_timestamps extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-10-04
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Alan Harrison, AMD
--
--     -   Derrick Owens, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Jason Ekstrand, Intel
--
--     -   Keith Packard, Valve
--
-- == Description
--
-- This extension provides an interface to query calibrated timestamps
-- obtained quasi simultaneously from two time domains.
--
-- == New Commands
--
-- -   'getCalibratedTimestampsEXT'
--
-- -   'getPhysicalDeviceCalibrateableTimeDomainsEXT'
--
-- == New Structures
--
-- -   'CalibratedTimestampInfoEXT'
--
-- == New Enums
--
-- -   'TimeDomainEXT'
--
-- == New Enum Constants
--
-- -   'EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME'
--
-- -   'EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT'
--
-- == Issues
--
-- 1) Is the device timestamp value returned in the same time domain as the
-- timestamp values written by
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp'?
--
-- __RESOLVED__: Yes.
--
-- 2) What time domain is the host timestamp returned in?
--
-- __RESOLVED__: A query is provided to determine the calibrateable time
-- domains. The expected host time domain used on Windows is that of
-- QueryPerformanceCounter, and on Linux that of CLOCK_MONOTONIC.
--
-- 3) Should we support other time domain combinations than just one host
-- and the device time domain?
--
-- __RESOLVED__: Supporting that would need the application to query the
-- set of supported time domains, while supporting only one host and the
-- device time domain would only need a query for the host time domain
-- type. The proposed API chooses the general approach for the sake of
-- extensibility.
--
-- 4) Should we use CLOCK_MONOTONIC_RAW instead of CLOCK_MONOTONIC?
--
-- __RESOLVED__: CLOCK_MONOTONIC is usable in a wider set of situations,
-- however, it is subject to NTP adjustments so some use cases may prefer
-- CLOCK_MONOTONIC_RAW. Thus this extension allows both to be exposed.
--
-- 5) How can the application extrapolate future device timestamp values
-- from the calibrated timestamp value?
--
-- __RESOLVED__:
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@timestampPeriod@
-- makes it possible to calculate future device timestamps as follows:
--
-- > futureTimestamp = calibratedTimestamp + deltaNanoseconds / timestampPeriod
--
-- 6) In what queue are timestamp values in time domain
-- 'TIME_DOMAIN_DEVICE_EXT' captured by 'getCalibratedTimestampsEXT'?
--
-- __RESOLVED__: An implementation supporting this extension will have all
-- its VkQueue share the same time domain.
--
-- 6) Can the host and device timestamp values drift apart over longer
-- periods of time?
--
-- __RESOLVED__: Yes, especially as some time domains by definition allow
-- for that to happen (e.g. CLOCK_MONOTONIC is subject to NTP adjustments).
-- Thus it is recommended that applications re-calibrate from time to time.
--
-- 7) Should we add a query for reporting the maximum deviation of the
-- timestamp values returned by calibrated timestamp queries?
--
-- __RESOLVED__: A global query seems inappropriate and difficult to
-- enforce. However, it is possible to return the maximum deviation any
-- single calibrated timestamp query can have by sampling one of the time
-- domains twice as follows:
--
-- > timestampX = timestampX_before = SampleTimeDomain(X)
-- > for each time domain Y != X
-- >     timestampY = SampleTimeDomain(Y)
-- > timestampX_after = SampleTimeDomain(X)
-- > maxDeviation = timestampX_after - timestampX_before
--
-- 8) Can the maximum deviation reported ever be zero?
--
-- __RESOLVED__: Unless the tick of each clock corresponding to the set of
-- time domains coincides and all clocks can literally be sampled
-- simultaneously, there is not really a possibility for the maximum
-- deviation to be zero, so by convention the maximum deviation is always
-- at least the maximum of the length of the ticks of the set of time
-- domains calibrated and thus can never be zero.
--
-- == Version History
--
-- -   Revision 2, 2021-03-16 (Lionel Landwerlin)
--
--     -   Specify requirement on device timestamps
--
-- -   Revision 1, 2018-10-04 (Daniel Rakos)
--
--     -   Internal revisions.
--
-- == See Also
--
-- 'CalibratedTimestampInfoEXT', 'TimeDomainEXT',
-- 'getCalibratedTimestampsEXT',
-- 'getPhysicalDeviceCalibrateableTimeDomainsEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_calibrated_timestamps Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_calibrated_timestamps  ( getPhysicalDeviceCalibrateableTimeDomainsEXT
                                                       , getCalibratedTimestampsEXT
                                                       , CalibratedTimestampInfoEXT(..)
                                                       , TimeDomainEXT( TIME_DOMAIN_DEVICE_EXT
                                                                      , TIME_DOMAIN_CLOCK_MONOTONIC_EXT
                                                                      , TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT
                                                                      , TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT
                                                                      , ..
                                                                      )
                                                       , EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION
                                                       , pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION
                                                       , EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
                                                       , pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
                                                       ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetCalibratedTimestampsEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceCalibrateableTimeDomainsEXT))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr TimeDomainEXT -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr TimeDomainEXT -> IO Result

-- | vkGetPhysicalDeviceCalibrateableTimeDomainsEXT - Query calibrateable
-- time domains
--
-- = Description
--
-- If @pTimeDomains@ is @NULL@, then the number of calibrateable time
-- domains supported for the given @physicalDevice@ is returned in
-- @pTimeDomainCount@. Otherwise, @pTimeDomainCount@ /must/ point to a
-- variable set by the user to the number of elements in the @pTimeDomains@
-- array, and on return the variable is overwritten with the number of
-- values actually written to @pTimeDomains@. If the value of
-- @pTimeDomainCount@ is less than the number of calibrateable time domains
-- supported, at most @pTimeDomainCount@ values will be written to
-- @pTimeDomains@, and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be
-- returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate
-- that not all the available time domains were returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceCalibrateableTimeDomainsEXT-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceCalibrateableTimeDomainsEXT-pTimeDomainCount-parameter#
--     @pTimeDomainCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceCalibrateableTimeDomainsEXT-pTimeDomains-parameter#
--     If the value referenced by @pTimeDomainCount@ is not @0@, and
--     @pTimeDomains@ is not @NULL@, @pTimeDomains@ /must/ be a valid
--     pointer to an array of @pTimeDomainCount@ 'TimeDomainEXT' values
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_calibrated_timestamps VK_EXT_calibrated_timestamps>,
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'TimeDomainEXT'
getPhysicalDeviceCalibrateableTimeDomainsEXT :: forall io
                                              . (MonadIO io)
                                             => -- | @physicalDevice@ is the physical device from which to query the set of
                                                -- calibrateable time domains.
                                                PhysicalDevice
                                             -> io (Result, ("timeDomains" ::: Vector TimeDomainEXT))
getPhysicalDeviceCalibrateableTimeDomainsEXT physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceCalibrateableTimeDomainsEXTPtr = pVkGetPhysicalDeviceCalibrateableTimeDomainsEXT (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceCalibrateableTimeDomainsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceCalibrateableTimeDomainsEXT is null" Nothing Nothing
  let vkGetPhysicalDeviceCalibrateableTimeDomainsEXT' = mkVkGetPhysicalDeviceCalibrateableTimeDomainsEXT vkGetPhysicalDeviceCalibrateableTimeDomainsEXTPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPTimeDomainCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT" (vkGetPhysicalDeviceCalibrateableTimeDomainsEXT' physicalDevice' (pPTimeDomainCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pTimeDomainCount <- lift $ peek @Word32 pPTimeDomainCount
  pPTimeDomains <- ContT $ bracket (callocBytes @TimeDomainEXT ((fromIntegral (pTimeDomainCount)) * 4)) free
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT" (vkGetPhysicalDeviceCalibrateableTimeDomainsEXT' physicalDevice' (pPTimeDomainCount) (pPTimeDomains))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pTimeDomainCount' <- lift $ peek @Word32 pPTimeDomainCount
  pTimeDomains' <- lift $ generateM (fromIntegral (pTimeDomainCount')) (\i -> peek @TimeDomainEXT ((pPTimeDomains `advancePtrBytes` (4 * (i)) :: Ptr TimeDomainEXT)))
  pure $ ((r'), pTimeDomains')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetCalibratedTimestampsEXT
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr CalibratedTimestampInfoEXT -> Ptr Word64 -> Ptr Word64 -> IO Result) -> Ptr Device_T -> Word32 -> Ptr CalibratedTimestampInfoEXT -> Ptr Word64 -> Ptr Word64 -> IO Result

-- | vkGetCalibratedTimestampsEXT - Query calibrated timestamps
--
-- = Description
--
-- Note
--
-- The maximum deviation /may/ vary between calls to
-- 'getCalibratedTimestampsEXT' even for the same set of time domains due
-- to implementation and platform specific reasons. It is the application’s
-- responsibility to assess whether the returned maximum deviation makes
-- the timestamp values suitable for any particular purpose and /can/
-- choose to re-issue the timestamp calibration call pursuing a lower
-- deviation value.
--
-- Calibrated timestamp values /can/ be extrapolated to estimate future
-- coinciding timestamp values, however, depending on the nature of the
-- time domains and other properties of the platform extrapolating values
-- over a sufficiently long period of time /may/ no longer be accurate
-- enough to fit any particular purpose, so applications are expected to
-- re-calibrate the timestamps on a regular basis.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_calibrated_timestamps VK_EXT_calibrated_timestamps>,
-- 'CalibratedTimestampInfoEXT', 'Vulkan.Core10.Handles.Device'
getCalibratedTimestampsEXT :: forall io
                            . (MonadIO io)
                           => -- | @device@ is the logical device used to perform the query.
                              --
                              -- #VUID-vkGetCalibratedTimestampsEXT-device-parameter# @device@ /must/ be
                              -- a valid 'Vulkan.Core10.Handles.Device' handle
                              Device
                           -> -- | @pTimestampInfos@ is a pointer to an array of @timestampCount@
                              -- 'CalibratedTimestampInfoEXT' structures, describing the time domains the
                              -- calibrated timestamps should be captured from.
                              --
                              -- #VUID-vkGetCalibratedTimestampsEXT-pTimestampInfos-parameter#
                              -- @pTimestampInfos@ /must/ be a valid pointer to an array of
                              -- @timestampCount@ valid 'CalibratedTimestampInfoEXT' structures
                              ("timestampInfos" ::: Vector CalibratedTimestampInfoEXT)
                           -> io (("timestamps" ::: Vector Word64), ("maxDeviation" ::: Word64))
getCalibratedTimestampsEXT device timestampInfos = liftIO . evalContT $ do
  let vkGetCalibratedTimestampsEXTPtr = pVkGetCalibratedTimestampsEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetCalibratedTimestampsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetCalibratedTimestampsEXT is null" Nothing Nothing
  let vkGetCalibratedTimestampsEXT' = mkVkGetCalibratedTimestampsEXT vkGetCalibratedTimestampsEXTPtr
  pPTimestampInfos <- ContT $ allocaBytes @CalibratedTimestampInfoEXT ((Data.Vector.length (timestampInfos)) * 24)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPTimestampInfos `plusPtr` (24 * (i)) :: Ptr CalibratedTimestampInfoEXT) (e)) (timestampInfos)
  pPTimestamps <- ContT $ bracket (callocBytes @Word64 ((fromIntegral ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32))) * 8)) free
  pPMaxDeviation <- ContT $ bracket (callocBytes @Word64 8) free
  r <- lift $ traceAroundEvent "vkGetCalibratedTimestampsEXT" (vkGetCalibratedTimestampsEXT' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32)) (pPTimestampInfos) (pPTimestamps) (pPMaxDeviation))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pTimestamps <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32))) (\i -> peek @Word64 ((pPTimestamps `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
  pMaxDeviation <- lift $ peek @Word64 pPMaxDeviation
  pure $ (pTimestamps, pMaxDeviation)


-- | VkCalibratedTimestampInfoEXT - Structure specifying the input parameters
-- of a calibrated timestamp query
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_calibrated_timestamps VK_EXT_calibrated_timestamps>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'TimeDomainEXT',
-- 'getCalibratedTimestampsEXT'
data CalibratedTimestampInfoEXT = CalibratedTimestampInfoEXT
  { -- | @timeDomain@ is a 'TimeDomainEXT' value specifying the time domain from
    -- which the calibrated timestamp value should be returned.
    --
    -- #VUID-VkCalibratedTimestampInfoEXT-timeDomain-02354# @timeDomain@ /must/
    -- be one of the 'TimeDomainEXT' values returned by
    -- 'getPhysicalDeviceCalibrateableTimeDomainsEXT'
    --
    -- #VUID-VkCalibratedTimestampInfoEXT-timeDomain-parameter# @timeDomain@
    -- /must/ be a valid 'TimeDomainEXT' value
    timeDomain :: TimeDomainEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CalibratedTimestampInfoEXT)
#endif
deriving instance Show CalibratedTimestampInfoEXT

instance ToCStruct CalibratedTimestampInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CalibratedTimestampInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TimeDomainEXT)) (timeDomain)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TimeDomainEXT)) (zero)
    f

instance FromCStruct CalibratedTimestampInfoEXT where
  peekCStruct p = do
    timeDomain <- peek @TimeDomainEXT ((p `plusPtr` 16 :: Ptr TimeDomainEXT))
    pure $ CalibratedTimestampInfoEXT
             timeDomain

instance Storable CalibratedTimestampInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CalibratedTimestampInfoEXT where
  zero = CalibratedTimestampInfoEXT
           zero


-- | VkTimeDomainEXT - Supported time domains
--
-- = Description
--
-- Note
--
-- An implementation supporting @VK_EXT_calibrated_timestamps@ will use the
-- same time domain for all its 'Vulkan.Core10.Handles.Queue' so that
-- timestamp values reported for 'TIME_DOMAIN_DEVICE_EXT' can be matched to
-- any timestamp captured through
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp' or
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWriteTimestamp2'
-- .
--
-- > struct timespec tv;
-- > clock_gettime(CLOCK_MONOTONIC, &tv);
-- > return tv.tv_nsec + tv.tv_sec*1000000000ull;
--
-- > struct timespec tv;
-- > clock_gettime(CLOCK_MONOTONIC_RAW, &tv);
-- > return tv.tv_nsec + tv.tv_sec*1000000000ull;
--
-- > LARGE_INTEGER counter;
-- > QueryPerformanceCounter(&counter);
-- > return counter.QuadPart;
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_calibrated_timestamps VK_EXT_calibrated_timestamps>,
-- 'CalibratedTimestampInfoEXT',
-- 'getPhysicalDeviceCalibrateableTimeDomainsEXT'
newtype TimeDomainEXT = TimeDomainEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'TIME_DOMAIN_DEVICE_EXT' specifies the device time domain. Timestamp
-- values in this time domain use the same units and are comparable with
-- device timestamp values captured using
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp' or
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWriteTimestamp2'
-- and are defined to be incrementing according to the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-timestampPeriod timestampPeriod>
-- of the device.
pattern TIME_DOMAIN_DEVICE_EXT                    = TimeDomainEXT 0
-- | 'TIME_DOMAIN_CLOCK_MONOTONIC_EXT' specifies the CLOCK_MONOTONIC time
-- domain available on POSIX platforms. Timestamp values in this time
-- domain are in units of nanoseconds and are comparable with platform
-- timestamp values captured using the POSIX clock_gettime API as computed
-- by this example:
pattern TIME_DOMAIN_CLOCK_MONOTONIC_EXT           = TimeDomainEXT 1
-- | 'TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT' specifies the CLOCK_MONOTONIC_RAW
-- time domain available on POSIX platforms. Timestamp values in this time
-- domain are in units of nanoseconds and are comparable with platform
-- timestamp values captured using the POSIX clock_gettime API as computed
-- by this example:
pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT       = TimeDomainEXT 2
-- | 'TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT' specifies the performance
-- counter (QPC) time domain available on Windows. Timestamp values in this
-- time domain are in the same units as those provided by the Windows
-- QueryPerformanceCounter API and are comparable with platform timestamp
-- values captured using that API as computed by this example:
pattern TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT = TimeDomainEXT 3
{-# complete TIME_DOMAIN_DEVICE_EXT,
             TIME_DOMAIN_CLOCK_MONOTONIC_EXT,
             TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT,
             TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT :: TimeDomainEXT #-}

conNameTimeDomainEXT :: String
conNameTimeDomainEXT = "TimeDomainEXT"

enumPrefixTimeDomainEXT :: String
enumPrefixTimeDomainEXT = "TIME_DOMAIN_"

showTableTimeDomainEXT :: [(TimeDomainEXT, String)]
showTableTimeDomainEXT =
  [ (TIME_DOMAIN_DEVICE_EXT                   , "DEVICE_EXT")
  , (TIME_DOMAIN_CLOCK_MONOTONIC_EXT          , "CLOCK_MONOTONIC_EXT")
  , (TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT      , "CLOCK_MONOTONIC_RAW_EXT")
  , (TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT, "QUERY_PERFORMANCE_COUNTER_EXT")
  ]

instance Show TimeDomainEXT where
  showsPrec = enumShowsPrec enumPrefixTimeDomainEXT
                            showTableTimeDomainEXT
                            conNameTimeDomainEXT
                            (\(TimeDomainEXT x) -> x)
                            (showsPrec 11)

instance Read TimeDomainEXT where
  readPrec = enumReadPrec enumPrefixTimeDomainEXT showTableTimeDomainEXT conNameTimeDomainEXT TimeDomainEXT


type EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION"
pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 2


type EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_EXT_calibrated_timestamps"

-- No documentation found for TopLevel "VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME"
pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_EXT_calibrated_timestamps"

