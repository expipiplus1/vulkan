{-# language CPP #-}
-- | = Name
--
-- VK_KHR_calibrated_timestamps - device extension
--
-- == VK_KHR_calibrated_timestamps
--
-- [__Name String__]
--     @VK_KHR_calibrated_timestamps@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     544
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_calibrated_timestamps] @aqnuep%0A*Here describe the issue or question you have about the VK_KHR_calibrated_timestamps extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_calibrated_timestamps.adoc VK_EXT_calibrated_timestamps>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-07-12
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
--     -   Daniel Rakos, RasterGrid
--
--     -   Faith Ekstrand, Intel
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
-- -   'getCalibratedTimestampsKHR'
--
-- -   'getPhysicalDeviceCalibrateableTimeDomainsKHR'
--
-- == New Structures
--
-- -   'CalibratedTimestampInfoKHR'
--
-- == New Enums
--
-- -   'TimeDomainKHR'
--
-- == New Enum Constants
--
-- -   'KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME'
--
-- -   'KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2023-07-12 (Daniel Rakos)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'CalibratedTimestampInfoKHR', 'TimeDomainKHR',
-- 'getCalibratedTimestampsKHR',
-- 'getPhysicalDeviceCalibrateableTimeDomainsKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_calibrated_timestamps Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_calibrated_timestamps  ( getPhysicalDeviceCalibrateableTimeDomainsKHR
                                                       , getCalibratedTimestampsKHR
                                                       , CalibratedTimestampInfoKHR(..)
                                                       , TimeDomainKHR( TIME_DOMAIN_DEVICE_KHR
                                                                      , TIME_DOMAIN_CLOCK_MONOTONIC_KHR
                                                                      , TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR
                                                                      , TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR
                                                                      , ..
                                                                      )
                                                       , KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION
                                                       , pattern KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION
                                                       , KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
                                                       , pattern KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
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
import Vulkan.Dynamic (DeviceCmds(pVkGetCalibratedTimestampsKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceCalibrateableTimeDomainsKHR))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceCalibrateableTimeDomainsKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr TimeDomainKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr TimeDomainKHR -> IO Result

-- | vkGetPhysicalDeviceCalibrateableTimeDomainsKHR - Query calibrateable
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
-- -   #VUID-vkGetPhysicalDeviceCalibrateableTimeDomainsKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceCalibrateableTimeDomainsKHR-pTimeDomainCount-parameter#
--     @pTimeDomainCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceCalibrateableTimeDomainsKHR-pTimeDomains-parameter#
--     If the value referenced by @pTimeDomainCount@ is not @0@, and
--     @pTimeDomains@ is not @NULL@, @pTimeDomains@ /must/ be a valid
--     pointer to an array of @pTimeDomainCount@ 'TimeDomainKHR' values
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_calibrated_timestamps VK_KHR_calibrated_timestamps>,
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'TimeDomainKHR'
getPhysicalDeviceCalibrateableTimeDomainsKHR :: forall io
                                              . (MonadIO io)
                                             => -- | @physicalDevice@ is the physical device from which to query the set of
                                                -- calibrateable time domains.
                                                PhysicalDevice
                                             -> io (Result, ("timeDomains" ::: Vector TimeDomainKHR))
getPhysicalDeviceCalibrateableTimeDomainsKHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceCalibrateableTimeDomainsKHRPtr = pVkGetPhysicalDeviceCalibrateableTimeDomainsKHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceCalibrateableTimeDomainsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceCalibrateableTimeDomainsKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceCalibrateableTimeDomainsKHR' = mkVkGetPhysicalDeviceCalibrateableTimeDomainsKHR vkGetPhysicalDeviceCalibrateableTimeDomainsKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPTimeDomainCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceCalibrateableTimeDomainsKHR" (vkGetPhysicalDeviceCalibrateableTimeDomainsKHR'
                                                                                   physicalDevice'
                                                                                   (pPTimeDomainCount)
                                                                                   (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pTimeDomainCount <- lift $ peek @Word32 pPTimeDomainCount
  pPTimeDomains <- ContT $ bracket (callocBytes @TimeDomainKHR ((fromIntegral (pTimeDomainCount)) * 4)) free
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceCalibrateableTimeDomainsKHR" (vkGetPhysicalDeviceCalibrateableTimeDomainsKHR'
                                                                                    physicalDevice'
                                                                                    (pPTimeDomainCount)
                                                                                    (pPTimeDomains))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pTimeDomainCount' <- lift $ peek @Word32 pPTimeDomainCount
  pTimeDomains' <- lift $ generateM (fromIntegral (pTimeDomainCount')) (\i -> peek @TimeDomainKHR ((pPTimeDomains `advancePtrBytes` (4 * (i)) :: Ptr TimeDomainKHR)))
  pure $ ((r'), pTimeDomains')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetCalibratedTimestampsKHR
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr CalibratedTimestampInfoKHR -> Ptr Word64 -> Ptr Word64 -> IO Result) -> Ptr Device_T -> Word32 -> Ptr CalibratedTimestampInfoKHR -> Ptr Word64 -> Ptr Word64 -> IO Result

-- | vkGetCalibratedTimestampsKHR - Query calibrated timestamps
--
-- = Description
--
-- Note
--
-- The maximum deviation /may/ vary between calls to
-- 'getCalibratedTimestampsKHR' even for the same set of time domains due
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
-- == Valid Usage
--
-- -   #VUID-vkGetCalibratedTimestampsEXT-timeDomain-09246# The
--     @timeDomain@ value of each
--     'Vulkan.Extensions.VK_EXT_calibrated_timestamps.CalibratedTimestampInfoEXT'
--     in @pTimestampInfos@ /must/ be unique
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetCalibratedTimestampsKHR-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetCalibratedTimestampsKHR-pTimestampInfos-parameter#
--     @pTimestampInfos@ /must/ be a valid pointer to an array of
--     @timestampCount@ valid 'CalibratedTimestampInfoKHR' structures
--
-- -   #VUID-vkGetCalibratedTimestampsKHR-pTimestamps-parameter#
--     @pTimestamps@ /must/ be a valid pointer to an array of
--     @timestampCount@ @uint64_t@ values
--
-- -   #VUID-vkGetCalibratedTimestampsKHR-pMaxDeviation-parameter#
--     @pMaxDeviation@ /must/ be a valid pointer to a @uint64_t@ value
--
-- -   #VUID-vkGetCalibratedTimestampsKHR-timestampCount-arraylength#
--     @timestampCount@ /must/ be greater than @0@
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_calibrated_timestamps VK_KHR_calibrated_timestamps>,
-- 'CalibratedTimestampInfoKHR', 'Vulkan.Core10.Handles.Device'
getCalibratedTimestampsKHR :: forall io
                            . (MonadIO io)
                           => -- | @device@ is the logical device used to perform the query.
                              Device
                           -> -- | @pTimestampInfos@ is a pointer to an array of @timestampCount@
                              -- 'CalibratedTimestampInfoKHR' structures, describing the time domains the
                              -- calibrated timestamps should be captured from.
                              ("timestampInfos" ::: Vector CalibratedTimestampInfoKHR)
                           -> io (("timestamps" ::: Vector Word64), ("maxDeviation" ::: Word64))
getCalibratedTimestampsKHR device timestampInfos = liftIO . evalContT $ do
  let vkGetCalibratedTimestampsKHRPtr = pVkGetCalibratedTimestampsKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetCalibratedTimestampsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetCalibratedTimestampsKHR is null" Nothing Nothing
  let vkGetCalibratedTimestampsKHR' = mkVkGetCalibratedTimestampsKHR vkGetCalibratedTimestampsKHRPtr
  pPTimestampInfos <- ContT $ allocaBytes @CalibratedTimestampInfoKHR ((Data.Vector.length (timestampInfos)) * 24)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPTimestampInfos `plusPtr` (24 * (i)) :: Ptr CalibratedTimestampInfoKHR) (e)) (timestampInfos)
  pPTimestamps <- ContT $ bracket (callocBytes @Word64 ((fromIntegral ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32))) * 8)) free
  pPMaxDeviation <- ContT $ bracket (callocBytes @Word64 8) free
  r <- lift $ traceAroundEvent "vkGetCalibratedTimestampsKHR" (vkGetCalibratedTimestampsKHR'
                                                                 (deviceHandle (device))
                                                                 ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32))
                                                                 (pPTimestampInfos)
                                                                 (pPTimestamps)
                                                                 (pPMaxDeviation))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pTimestamps <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32))) (\i -> peek @Word64 ((pPTimestamps `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
  pMaxDeviation <- lift $ peek @Word64 pPMaxDeviation
  pure $ (pTimestamps, pMaxDeviation)


-- | VkCalibratedTimestampInfoKHR - Structure specifying the input parameters
-- of a calibrated timestamp query
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_calibrated_timestamps VK_EXT_calibrated_timestamps>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_calibrated_timestamps VK_KHR_calibrated_timestamps>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'TimeDomainKHR',
-- 'Vulkan.Extensions.VK_EXT_calibrated_timestamps.getCalibratedTimestampsEXT',
-- 'getCalibratedTimestampsKHR'
data CalibratedTimestampInfoKHR = CalibratedTimestampInfoKHR
  { -- | @timeDomain@ is a 'TimeDomainKHR' value specifying the time domain from
    -- which the calibrated timestamp value should be returned.
    --
    -- #VUID-VkCalibratedTimestampInfoEXT-timeDomain-02354# @timeDomain@ /must/
    -- be one of the 'TimeDomainKHR' values returned by
    -- 'getPhysicalDeviceCalibrateableTimeDomainsKHR'
    --
    -- #VUID-VkCalibratedTimestampInfoKHR-timeDomain-parameter# @timeDomain@
    -- /must/ be a valid 'TimeDomainKHR' value
    timeDomain :: TimeDomainKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CalibratedTimestampInfoKHR)
#endif
deriving instance Show CalibratedTimestampInfoKHR

instance ToCStruct CalibratedTimestampInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CalibratedTimestampInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TimeDomainKHR)) (timeDomain)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TimeDomainKHR)) (zero)
    f

instance FromCStruct CalibratedTimestampInfoKHR where
  peekCStruct p = do
    timeDomain <- peek @TimeDomainKHR ((p `plusPtr` 16 :: Ptr TimeDomainKHR))
    pure $ CalibratedTimestampInfoKHR
             timeDomain

instance Storable CalibratedTimestampInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CalibratedTimestampInfoKHR where
  zero = CalibratedTimestampInfoKHR
           zero


-- | VkTimeDomainKHR - Supported time domains
--
-- = Description
--
-- Note
--
-- An implementation supporting @VK_KHR_calibrated_timestamps@ or
-- @VK_EXT_calibrated_timestamps@ will use the same time domain for all its
-- 'Vulkan.Core10.Handles.Queue' so that timestamp values reported for
-- 'TIME_DOMAIN_DEVICE_KHR' can be matched to any timestamp captured
-- through 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp' or
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_calibrated_timestamps VK_KHR_calibrated_timestamps>,
-- 'CalibratedTimestampInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_calibrated_timestamps.getPhysicalDeviceCalibrateableTimeDomainsEXT',
-- 'getPhysicalDeviceCalibrateableTimeDomainsKHR'
newtype TimeDomainKHR = TimeDomainKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'TIME_DOMAIN_DEVICE_KHR' specifies the device time domain. Timestamp
-- values in this time domain use the same units and are comparable with
-- device timestamp values captured using
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp' or
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWriteTimestamp2'
-- and are defined to be incrementing according to the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-timestampPeriod timestampPeriod>
-- of the device.
pattern TIME_DOMAIN_DEVICE_KHR = TimeDomainKHR 0

-- | 'TIME_DOMAIN_CLOCK_MONOTONIC_KHR' specifies the CLOCK_MONOTONIC time
-- domain available on POSIX platforms. Timestamp values in this time
-- domain are in units of nanoseconds and are comparable with platform
-- timestamp values captured using the POSIX clock_gettime API as computed
-- by this example:
pattern TIME_DOMAIN_CLOCK_MONOTONIC_KHR = TimeDomainKHR 1

-- | 'TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR' specifies the CLOCK_MONOTONIC_RAW
-- time domain available on POSIX platforms. Timestamp values in this time
-- domain are in units of nanoseconds and are comparable with platform
-- timestamp values captured using the POSIX clock_gettime API as computed
-- by this example:
pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR = TimeDomainKHR 2

-- | 'TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR' specifies the performance
-- counter (QPC) time domain available on Windows. Timestamp values in this
-- time domain are in the same units as those provided by the Windows
-- QueryPerformanceCounter API and are comparable with platform timestamp
-- values captured using that API as computed by this example:
pattern TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR = TimeDomainKHR 3

{-# COMPLETE
  TIME_DOMAIN_DEVICE_KHR
  , TIME_DOMAIN_CLOCK_MONOTONIC_KHR
  , TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR
  , TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR ::
    TimeDomainKHR
  #-}

conNameTimeDomainKHR :: String
conNameTimeDomainKHR = "TimeDomainKHR"

enumPrefixTimeDomainKHR :: String
enumPrefixTimeDomainKHR = "TIME_DOMAIN_"

showTableTimeDomainKHR :: [(TimeDomainKHR, String)]
showTableTimeDomainKHR =
  [ (TIME_DOMAIN_DEVICE_KHR, "DEVICE_KHR")
  ,
    ( TIME_DOMAIN_CLOCK_MONOTONIC_KHR
    , "CLOCK_MONOTONIC_KHR"
    )
  ,
    ( TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR
    , "CLOCK_MONOTONIC_RAW_KHR"
    )
  ,
    ( TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR
    , "QUERY_PERFORMANCE_COUNTER_KHR"
    )
  ]

instance Show TimeDomainKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixTimeDomainKHR
      showTableTimeDomainKHR
      conNameTimeDomainKHR
      (\(TimeDomainKHR x) -> x)
      (showsPrec 11)

instance Read TimeDomainKHR where
  readPrec =
    enumReadPrec
      enumPrefixTimeDomainKHR
      showTableTimeDomainKHR
      conNameTimeDomainKHR
      TimeDomainKHR

type KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION"
pattern KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 1


type KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_KHR_calibrated_timestamps"

-- No documentation found for TopLevel "VK_KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME"
pattern KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_KHR_calibrated_timestamps"

