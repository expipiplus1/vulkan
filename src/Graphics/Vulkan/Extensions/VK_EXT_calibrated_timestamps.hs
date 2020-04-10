{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_calibrated_timestamps  ( getPhysicalDeviceCalibrateableTimeDomainsEXT
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

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetCalibratedTimestampsEXT))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceCalibrateableTimeDomainsEXT))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice_T)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr TimeDomainEXT -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr TimeDomainEXT -> IO Result

-- | vkGetPhysicalDeviceCalibrateableTimeDomainsEXT - Query calibrateable
-- time domains
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the set
--     of calibrateable time domains.
--
-- -   @pTimeDomainCount@ is a pointer to an integer related to the number
--     of calibrateable time domains available or queried, as described
--     below.
--
-- -   @pTimeDomains@ is either @NULL@ or a pointer to an array of
--     'TimeDomainEXT' values, indicating the supported calibrateable time
--     domains.
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
-- @pTimeDomains@. If @pTimeDomainCount@ is smaller than the number of
-- calibrateable time domains supported for the given @physicalDevice@,
-- 'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned
-- instead of 'Graphics.Vulkan.Core10.Enums.Result.SUCCESS' to indicate
-- that not all the available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   @pTimeDomainCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pTimeDomainCount@ is not @0@, and
--     @pTimeDomains@ is not @NULL@, @pTimeDomains@ /must/ be a valid
--     pointer to an array of @pTimeDomainCount@ 'TimeDomainEXT' values
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice', 'TimeDomainEXT'
getPhysicalDeviceCalibrateableTimeDomainsEXT :: forall io . MonadIO io => PhysicalDevice -> io (Result, ("timeDomains" ::: Vector TimeDomainEXT))
getPhysicalDeviceCalibrateableTimeDomainsEXT physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceCalibrateableTimeDomainsEXT' = mkVkGetPhysicalDeviceCalibrateableTimeDomainsEXT (pVkGetPhysicalDeviceCalibrateableTimeDomainsEXT (instanceCmds (physicalDevice :: PhysicalDevice)))
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPTimeDomainCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceCalibrateableTimeDomainsEXT' physicalDevice' (pPTimeDomainCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pTimeDomainCount <- lift $ peek @Word32 pPTimeDomainCount
  pPTimeDomains <- ContT $ bracket (callocBytes @TimeDomainEXT ((fromIntegral (pTimeDomainCount)) * 4)) free
  r' <- lift $ vkGetPhysicalDeviceCalibrateableTimeDomainsEXT' physicalDevice' (pPTimeDomainCount) (pPTimeDomains)
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
-- = Parameters
--
-- -   @device@ is the logical device used to perform the query.
--
-- -   @timestampCount@ is the number of timestamps to query.
--
-- -   @pTimestampInfos@ is a pointer to an array of @timestampCount@
--     'CalibratedTimestampInfoEXT' structures, describing the time domains
--     the calibrated timestamps should be captured from.
--
-- -   @pTimestamps@ is a pointer to an array of @timestampCount@ 64-bit
--     unsigned integer values in which the requested calibrated timestamp
--     values are returned.
--
-- -   @pMaxDeviation@ is a pointer to a 64-bit unsigned integer value in
--     which the strictly positive maximum deviation, in nanoseconds, of
--     the calibrated timestamp values is returned.
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
-- devation value.
--
-- Calibrated timestamp values /can/ be extrapolated to estimate future
-- coinciding timestamp values, however, depending on the nature of the
-- time domains and other properties of the platform extrapolating values
-- over a sufficiently long period of time /may/ no longer be accurate
-- enough to fit any particular purpose so applications are expected to
-- re-calibrate the timestamps on a regular basis.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'CalibratedTimestampInfoEXT', 'Graphics.Vulkan.Core10.Handles.Device'
getCalibratedTimestampsEXT :: forall io . MonadIO io => Device -> ("timestampInfos" ::: Vector CalibratedTimestampInfoEXT) -> io (("timestamps" ::: Vector Word64), ("maxDeviation" ::: Word64))
getCalibratedTimestampsEXT device timestampInfos = liftIO . evalContT $ do
  let vkGetCalibratedTimestampsEXT' = mkVkGetCalibratedTimestampsEXT (pVkGetCalibratedTimestampsEXT (deviceCmds (device :: Device)))
  pPTimestampInfos <- ContT $ allocaBytesAligned @CalibratedTimestampInfoEXT ((Data.Vector.length (timestampInfos)) * 24) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPTimestampInfos `plusPtr` (24 * (i)) :: Ptr CalibratedTimestampInfoEXT) (e) . ($ ())) (timestampInfos)
  pPTimestamps <- ContT $ bracket (callocBytes @Word64 ((fromIntegral ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32))) * 8)) free
  pPMaxDeviation <- ContT $ bracket (callocBytes @Word64 8) free
  r <- lift $ vkGetCalibratedTimestampsEXT' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32)) (pPTimestampInfos) (pPTimestamps) (pPMaxDeviation)
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
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'TimeDomainEXT', 'getCalibratedTimestampsEXT'
data CalibratedTimestampInfoEXT = CalibratedTimestampInfoEXT
  { -- | @timeDomain@ /must/ be a valid 'TimeDomainEXT' value
    timeDomain :: TimeDomainEXT }
  deriving (Typeable)
deriving instance Show CalibratedTimestampInfoEXT

instance ToCStruct CalibratedTimestampInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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
-- 'CalibratedTimestampInfoEXT',
-- 'getPhysicalDeviceCalibrateableTimeDomainsEXT'
newtype TimeDomainEXT = TimeDomainEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'TIME_DOMAIN_DEVICE_EXT' specifies the device time domain. Timestamp
-- values in this time domain use the same units and are comparable with
-- device timestamp values captured using
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp' and are
-- defined to be incrementing according to the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-timestampPeriod timestampPeriod>
-- of the device.
pattern TIME_DOMAIN_DEVICE_EXT = TimeDomainEXT 0
-- | 'TIME_DOMAIN_CLOCK_MONOTONIC_EXT' specifies the CLOCK_MONOTONIC time
-- domain available on POSIX platforms. Timestamp values in this time
-- domain are in units of nanoseconds and are comparable with platform
-- timestamp values captured using the POSIX clock_gettime API as computed
-- by this example:
pattern TIME_DOMAIN_CLOCK_MONOTONIC_EXT = TimeDomainEXT 1
-- | 'TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT' specifies the CLOCK_MONOTONIC_RAW
-- time domain available on POSIX platforms. Timestamp values in this time
-- domain are in units of nanoseconds and are comparable with platform
-- timestamp values captured using the POSIX clock_gettime API as computed
-- by this example:
pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT = TimeDomainEXT 2
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

instance Show TimeDomainEXT where
  showsPrec p = \case
    TIME_DOMAIN_DEVICE_EXT -> showString "TIME_DOMAIN_DEVICE_EXT"
    TIME_DOMAIN_CLOCK_MONOTONIC_EXT -> showString "TIME_DOMAIN_CLOCK_MONOTONIC_EXT"
    TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT -> showString "TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT"
    TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT -> showString "TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT"
    TimeDomainEXT x -> showParen (p >= 11) (showString "TimeDomainEXT " . showsPrec 11 x)

instance Read TimeDomainEXT where
  readPrec = parens (choose [("TIME_DOMAIN_DEVICE_EXT", pure TIME_DOMAIN_DEVICE_EXT)
                            , ("TIME_DOMAIN_CLOCK_MONOTONIC_EXT", pure TIME_DOMAIN_CLOCK_MONOTONIC_EXT)
                            , ("TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT", pure TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT)
                            , ("TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT", pure TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "TimeDomainEXT")
                       v <- step readPrec
                       pure (TimeDomainEXT v)))


type EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION"
pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 1


type EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_EXT_calibrated_timestamps"

-- No documentation found for TopLevel "VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME"
pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_EXT_calibrated_timestamps"

