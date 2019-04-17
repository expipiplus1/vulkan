{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps
  ( VkCalibratedTimestampInfoEXT(..)
  , VkTimeDomainEXT(..)
  , pattern VK_TIME_DOMAIN_DEVICE_EXT
  , pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT
  , pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT
  , pattern VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetCalibratedTimestampsEXT
#endif
  , FN_vkGetCalibratedTimestampsEXT
  , PFN_vkGetCalibratedTimestampsEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
#endif
  , FN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  , PFN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  , pattern VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
  , pattern VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  , VkPhysicalDevice
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkCalibratedTimestampInfoEXT - Structure specifying the input parameters
-- of a calibrated timestamp query
--
-- == Valid Usage
--
-- Unresolved directive in VkCalibratedTimestampInfoEXT.txt -
-- include::..\/validity\/structs\/VkCalibratedTimestampInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkCalibratedTimestampInfoEXT = VkCalibratedTimestampInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @timeDomain@ /must/ be one of the 'VkTimeDomainEXT' values returned by
  -- 'vkGetPhysicalDeviceCalibrateableTimeDomainsEXT'
  vkTimeDomain :: VkTimeDomainEXT
  }
  deriving (Eq, Show)

instance Storable VkCalibratedTimestampInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkCalibratedTimestampInfoEXT <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCalibratedTimestampInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCalibratedTimestampInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkTimeDomain (poked :: VkCalibratedTimestampInfoEXT))

instance Zero VkCalibratedTimestampInfoEXT where
  zero = VkCalibratedTimestampInfoEXT zero
                                      zero
                                      zero
-- ** VkTimeDomainEXT

-- | VkTimeDomainEXT - Supported time domains
--
-- = See Also
--
-- No cross-references are available
newtype VkTimeDomainEXT = VkTimeDomainEXT Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkTimeDomainEXT where
  showsPrec _ VK_TIME_DOMAIN_DEVICE_EXT = showString "VK_TIME_DOMAIN_DEVICE_EXT"
  showsPrec _ VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT = showString "VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT"
  showsPrec _ VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT = showString "VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT"
  showsPrec _ VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT = showString "VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT"
  showsPrec p (VkTimeDomainEXT x) = showParen (p >= 11) (showString "VkTimeDomainEXT " . showsPrec 11 x)

instance Read VkTimeDomainEXT where
  readPrec = parens ( choose [ ("VK_TIME_DOMAIN_DEVICE_EXT",                    pure VK_TIME_DOMAIN_DEVICE_EXT)
                             , ("VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT",           pure VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT)
                             , ("VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT",       pure VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT)
                             , ("VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT", pure VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkTimeDomainEXT")
                        v <- step readPrec
                        pure (VkTimeDomainEXT v)
                        )
                    )

-- | @VK_TIME_DOMAIN_DEVICE_EXT@ specifies the device time domain. Timestamp
-- values in this time domain are comparable with device timestamp values
-- captured using
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp' and
-- are defined to be incrementing according to the
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#limits-timestampPeriod timestampPeriod>
-- of the device.
pattern VK_TIME_DOMAIN_DEVICE_EXT :: VkTimeDomainEXT
pattern VK_TIME_DOMAIN_DEVICE_EXT = VkTimeDomainEXT 0

-- | @VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT@ specifies the CLOCK_MONOTONIC time
-- domain available on POSIX platforms.
pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT :: VkTimeDomainEXT
pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT = VkTimeDomainEXT 1

-- | @VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT@ specifies the
-- CLOCK_MONOTONIC_RAW time domain available on POSIX platforms.
pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT :: VkTimeDomainEXT
pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT = VkTimeDomainEXT 2

-- | @VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT@ specifies the performance
-- counter (QPC) time domain available on Windows.
pattern VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT :: VkTimeDomainEXT
pattern VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT = VkTimeDomainEXT 3
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkGetCalibratedTimestampsEXT - Query calibrated timestamps
--
-- = Parameters
--
-- -   @device@ is the logical device used to perform the query.
--
-- -   @timestampCount@ is the number of timestamps to query.
--
-- -   @pTimestampInfos@ is a pointer to an array of @timestampCount@
--     number of structures of type 'VkCalibratedTimestampInfoEXT',
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
-- @vkGetCalibratedTimestampsEXT@ even for the same set of time domains due
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
-- Unresolved directive in vkGetCalibratedTimestampsEXT.txt -
-- include::..\/validity\/protos\/vkGetCalibratedTimestampsEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetCalibratedTimestampsEXT" vkGetCalibratedTimestampsEXT :: ("device" ::: VkDevice) -> ("timestampCount" ::: Word32) -> ("pTimestampInfos" ::: Ptr VkCalibratedTimestampInfoEXT) -> ("pTimestamps" ::: Ptr Word64) -> ("pMaxDeviation" ::: Ptr Word64) -> IO VkResult

#endif
type FN_vkGetCalibratedTimestampsEXT = ("device" ::: VkDevice) -> ("timestampCount" ::: Word32) -> ("pTimestampInfos" ::: Ptr VkCalibratedTimestampInfoEXT) -> ("pTimestamps" ::: Ptr Word64) -> ("pMaxDeviation" ::: Ptr Word64) -> IO VkResult
type PFN_vkGetCalibratedTimestampsEXT = FunPtr FN_vkGetCalibratedTimestampsEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
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
--     'VkTimeDomainEXT' values, indicating the supported calibrateable
--     time domains.
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
-- @VK_INCOMPLETE@ will be returned instead of @VK_SUCCESS@ to indicate
-- that not all the available values were returned.
--
-- Unresolved directive in
-- vkGetPhysicalDeviceCalibrateableTimeDomainsEXT.txt -
-- include::..\/validity\/protos\/vkGetPhysicalDeviceCalibrateableTimeDomainsEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT" vkGetPhysicalDeviceCalibrateableTimeDomainsEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pTimeDomainCount" ::: Ptr Word32) -> ("pTimeDomains" ::: Ptr VkTimeDomainEXT) -> IO VkResult

#endif
type FN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("pTimeDomainCount" ::: Ptr Word32) -> ("pTimeDomains" ::: Ptr VkTimeDomainEXT) -> IO VkResult
type PFN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT = FunPtr FN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
-- No documentation found for TopLevel "VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME"
pattern VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_EXT_calibrated_timestamps"
-- No documentation found for TopLevel "VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION"
pattern VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION :: Integral a => a
pattern VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT"
pattern VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT = VkStructureType 1000184000
