{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_calibrated_timestamps
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  CalibratedTimestampInfoEXT(..)
  , 
#endif
  TimeDomainEXT
  , pattern TIME_DOMAIN_DEVICE_EXT
  , pattern TIME_DOMAIN_CLOCK_MONOTONIC_EXT
  , pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT
  , pattern TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT
  , getCalibratedTimestampsEXT
  , getNumPhysicalDeviceCalibrateableTimeDomainsEXT
  , getPhysicalDeviceCalibrateableTimeDomainsEXT
  , getAllPhysicalDeviceCalibrateableTimeDomainsEXT
  , pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
  , pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT
  ) where

import Data.String
  ( IsString
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
  ( with
  )
import Foreign.Ptr
  ( nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps
  ( VkTimeDomainEXT(..)
  , vkGetCalibratedTimestampsEXT
  , vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  , pattern VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
  , pattern VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION
  , pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT
  , pattern VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT
  , pattern VK_TIME_DOMAIN_DEVICE_EXT
  , pattern VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , PhysicalDevice(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkCalibratedTimestampInfoEXT"
data CalibratedTimestampInfoEXT = CalibratedTimestampInfoEXT
  { -- No documentation found for Nested "CalibratedTimestampInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CalibratedTimestampInfoEXT" "timeDomain"
  timeDomain :: TimeDomainEXT
  }
  deriving (Show, Eq)

instance Zero CalibratedTimestampInfoEXT where
  zero = CalibratedTimestampInfoEXT Nothing
                                    zero

#endif

-- No documentation found for TopLevel "TimeDomainEXT"
type TimeDomainEXT = VkTimeDomainEXT


{-# complete TIME_DOMAIN_DEVICE_EXT, TIME_DOMAIN_CLOCK_MONOTONIC_EXT, TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT, TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT :: TimeDomainEXT #-}


-- No documentation found for Nested "TimeDomainEXT" "TIME_DOMAIN_DEVICE_EXT"
pattern TIME_DOMAIN_DEVICE_EXT :: (a ~ TimeDomainEXT) => a
pattern TIME_DOMAIN_DEVICE_EXT = VK_TIME_DOMAIN_DEVICE_EXT


-- No documentation found for Nested "TimeDomainEXT" "TIME_DOMAIN_CLOCK_MONOTONIC_EXT"
pattern TIME_DOMAIN_CLOCK_MONOTONIC_EXT :: (a ~ TimeDomainEXT) => a
pattern TIME_DOMAIN_CLOCK_MONOTONIC_EXT = VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT


-- No documentation found for Nested "TimeDomainEXT" "TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT"
pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT :: (a ~ TimeDomainEXT) => a
pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT = VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT


-- No documentation found for Nested "TimeDomainEXT" "TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT"
pattern TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT :: (a ~ TimeDomainEXT) => a
pattern TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT = VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT


-- No documentation found for TopLevel "vkGetCalibratedTimestampsEXT"
getCalibratedTimestampsEXT :: Device ->  Vector CalibratedTimestampInfoEXT ->  IO (Vector Word64, Word64)
getCalibratedTimestampsEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT"
getNumPhysicalDeviceCalibrateableTimeDomainsEXT :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceCalibrateableTimeDomainsEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT"
getPhysicalDeviceCalibrateableTimeDomainsEXT :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector TimeDomainEXT)
getPhysicalDeviceCalibrateableTimeDomainsEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceCalibrateableTimeDomainsEXT'.
getAllPhysicalDeviceCalibrateableTimeDomainsEXT :: PhysicalDevice ->  IO (Vector TimeDomainEXT)
getAllPhysicalDeviceCalibrateableTimeDomainsEXT physicalDevice' =
  snd <$> getNumPhysicalDeviceCalibrateableTimeDomainsEXT physicalDevice'
    >>= \num -> snd <$> getPhysicalDeviceCalibrateableTimeDomainsEXT physicalDevice' num


-- No documentation found for TopLevel "VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME"
pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION"
pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION :: Integral a => a
pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION
