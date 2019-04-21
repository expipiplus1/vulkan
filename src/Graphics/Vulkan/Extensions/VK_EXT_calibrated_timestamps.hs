{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_calibrated_timestamps
  ( withCStructCalibratedTimestampInfoEXT
  , fromCStructCalibratedTimestampInfoEXT
  , CalibratedTimestampInfoEXT(..)
  , TimeDomainEXT
  , pattern TIME_DOMAIN_DEVICE_EXT
  , pattern TIME_DOMAIN_CLOCK_MONOTONIC_EXT
  , pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT
  , pattern TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT
  , getCalibratedTimestampsEXT
  , getNumPhysicalDeviceCalibrateableTimeDomainsEXT
  , getPhysicalDeviceCalibrateableTimeDomainsEXT
  , getAllPhysicalDeviceCalibrateableTimeDomainsEXT
  , pattern VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION
  , pattern VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps
  ( VkCalibratedTimestampInfoEXT(..)
  , VkTimeDomainEXT(..)
  , vkGetCalibratedTimestampsEXT
  , vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  , pattern VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT
  , pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT
  , pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT
  , pattern VK_TIME_DOMAIN_DEVICE_EXT
  , pattern VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , PhysicalDevice(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps
  ( pattern VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
  , pattern VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION
  )



-- | VkCalibratedTimestampInfoEXT - Structure specifying the input parameters
-- of a calibrated timestamp query
--
-- == Valid Usage
--
-- Unresolved directive in VkCalibratedTimestampInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkCalibratedTimestampInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data CalibratedTimestampInfoEXT = CalibratedTimestampInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "CalibratedTimestampInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CalibratedTimestampInfoEXT" "timeDomain"
  timeDomain :: TimeDomainEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkCalibratedTimestampInfoEXT' and
-- marshal a 'CalibratedTimestampInfoEXT' into it. The 'VkCalibratedTimestampInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCalibratedTimestampInfoEXT :: CalibratedTimestampInfoEXT -> (VkCalibratedTimestampInfoEXT -> IO a) -> IO a
withCStructCalibratedTimestampInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: CalibratedTimestampInfoEXT)) (\pPNext -> cont (VkCalibratedTimestampInfoEXT VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT pPNext (timeDomain (marshalled :: CalibratedTimestampInfoEXT))))

-- | A function to read a 'VkCalibratedTimestampInfoEXT' and all additional
-- structures in the pointer chain into a 'CalibratedTimestampInfoEXT'.
fromCStructCalibratedTimestampInfoEXT :: VkCalibratedTimestampInfoEXT -> IO CalibratedTimestampInfoEXT
fromCStructCalibratedTimestampInfoEXT c = CalibratedTimestampInfoEXT <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCalibratedTimestampInfoEXT)))
                                                                     <*> pure (vkTimeDomain (c :: VkCalibratedTimestampInfoEXT))

instance Zero CalibratedTimestampInfoEXT where
  zero = CalibratedTimestampInfoEXT Nothing
                                    zero


-- | VkTimeDomainEXT - Supported time domains
--
-- = See Also
--
-- No cross-references are available
type TimeDomainEXT = VkTimeDomainEXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps.VK_TIME_DOMAIN_DEVICE_EXT'
-- specifies the device time domain. Timestamp values in this time domain
-- are comparable with device timestamp values captured using
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp' and
-- are defined to be incrementing according to the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-timestampPeriod timestampPeriod>
-- of the device.
pattern TIME_DOMAIN_DEVICE_EXT :: (a ~ TimeDomainEXT) => a
pattern TIME_DOMAIN_DEVICE_EXT = VK_TIME_DOMAIN_DEVICE_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps.VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT'
-- specifies the CLOCK_MONOTONIC time domain available on POSIX platforms.
pattern TIME_DOMAIN_CLOCK_MONOTONIC_EXT :: (a ~ TimeDomainEXT) => a
pattern TIME_DOMAIN_CLOCK_MONOTONIC_EXT = VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps.VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT'
-- specifies the CLOCK_MONOTONIC_RAW time domain available on POSIX
-- platforms.
pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT :: (a ~ TimeDomainEXT) => a
pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT = VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps.VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT'
-- specifies the performance counter (QPC) time domain available on
-- Windows.
pattern TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT :: (a ~ TimeDomainEXT) => a
pattern TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT = VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT


-- | vkGetCalibratedTimestampsEXT - Query calibrated timestamps
--
-- = Parameters
--
-- -   @device@ is the logical device used to perform the query.
--
-- -   @timestampCount@ is the number of timestamps to query.
--
-- -   @pTimestampInfos@ is a pointer to an array of @timestampCount@
--     number of structures of type
--     'Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps.VkCalibratedTimestampInfoEXT',
--     describing the time domains the calibrated timestamps should be
--     captured from.
--
-- -   @pTimestamps@ is a pointer to an array of @timestampCount@ number of
--     64-bit unsigned integer values in which the requested calibrated
--     timestamp values are returned.
--
-- -   @pMaxDeviation@ is a pointer to a 64-bit unsigned integer value in
--     which the strictly positive maximum deviation, in nanoseconds, of
--     the calibrated timestamp values is returned.
--
-- = Description
--
-- __Note__
--
-- The maximum deviation /may/ vary between calls to
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps.vkGetCalibratedTimestampsEXT'
-- even for the same set of time domains due to implementation and platform
-- specific reasons. It is the application’s responsibility to assess
-- whether the returned maximum deviation makes the timestamp values
-- suitable for any particular purpose and /can/ choose to re-issue the
-- timestamp calibration call pursuing a lower devation value.
--
-- Calibrated timestamp values /can/ be extrapolated to estimate future
-- coinciding timestamp values, however, depending on the nature of the
-- time domains and other properties of the platform extrapolating values
-- over a sufficiently long period of time /may/ no longer be accurate
-- enough to fit any particular purpose so applications are expected to
-- re-calibrate the timestamps on a regular basis.
--
-- Unresolved directive in vkGetCalibratedTimestampsEXT.txt -
-- include::{generated}\/validity\/protos\/vkGetCalibratedTimestampsEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
getCalibratedTimestampsEXT :: Device ->  Vector CalibratedTimestampInfoEXT ->  IO (Vector Word64, Word64)
getCalibratedTimestampsEXT = \(Device device' commandTable) -> \timestampInfos' -> alloca (\pMaxDeviation' -> allocaArray ((Data.Vector.length timestampInfos')) (\pTimestamps' -> withVec withCStructCalibratedTimestampInfoEXT timestampInfos' (\pTimestampInfos' -> vkGetCalibratedTimestampsEXT commandTable device' (fromIntegral $ Data.Vector.length timestampInfos') pTimestampInfos' pTimestamps' pMaxDeviation' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> (Data.Vector.generateM ((Data.Vector.length timestampInfos')) (peekElemOff pTimestamps'))<*>peek pMaxDeviation')))))


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
--     'Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps.VkTimeDomainEXT'
--     values, indicating the supported calibrateable time domains.
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
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- Unresolved directive in
-- vkGetPhysicalDeviceCalibrateableTimeDomainsEXT.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceCalibrateableTimeDomainsEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
getNumPhysicalDeviceCalibrateableTimeDomainsEXT :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceCalibrateableTimeDomainsEXT = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pTimeDomainCount' -> vkGetPhysicalDeviceCalibrateableTimeDomainsEXT commandTable physicalDevice' pTimeDomainCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pTimeDomainCount')))

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
--     'Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps.VkTimeDomainEXT'
--     values, indicating the supported calibrateable time domains.
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
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- Unresolved directive in
-- vkGetPhysicalDeviceCalibrateableTimeDomainsEXT.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceCalibrateableTimeDomainsEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
getPhysicalDeviceCalibrateableTimeDomainsEXT :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector TimeDomainEXT)
getPhysicalDeviceCalibrateableTimeDomainsEXT = \(PhysicalDevice physicalDevice' commandTable) -> \timeDomainCount' -> allocaArray (fromIntegral timeDomainCount') (\pTimeDomains' -> with timeDomainCount' (\pTimeDomainCount' -> vkGetPhysicalDeviceCalibrateableTimeDomainsEXT commandTable physicalDevice' pTimeDomainCount' pTimeDomains' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM (peekElemOff pTimeDomains') =<< (fromIntegral <$> (peek pTimeDomainCount')))))))
-- | Returns all the values available from 'getPhysicalDeviceCalibrateableTimeDomainsEXT'.
getAllPhysicalDeviceCalibrateableTimeDomainsEXT :: PhysicalDevice ->  IO (Vector TimeDomainEXT)
getAllPhysicalDeviceCalibrateableTimeDomainsEXT physicalDevice' =
  snd <$> getNumPhysicalDeviceCalibrateableTimeDomainsEXT physicalDevice'
    >>= \num -> snd <$> getPhysicalDeviceCalibrateableTimeDomainsEXT physicalDevice' num

