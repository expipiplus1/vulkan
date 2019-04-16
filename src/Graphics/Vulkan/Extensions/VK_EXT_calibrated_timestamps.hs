{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_calibrated_timestamps
  ( withCStructCalibratedTimestampInfoEXT
  , fromCStructCalibratedTimestampInfoEXT
  , CalibratedTimestampInfoEXT(..)
  , TimeDomainEXT
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
import qualified Graphics.Vulkan.C.Dynamic
  ( getCalibratedTimestampsEXT
  , getPhysicalDeviceCalibrateableTimeDomainsEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps
  ( VkCalibratedTimestampInfoEXT(..)
  , VkTimeDomainEXT(..)
  , pattern VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT
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


-- No documentation found for TopLevel "CalibratedTimestampInfoEXT"
data CalibratedTimestampInfoEXT = CalibratedTimestampInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "CalibratedTimestampInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CalibratedTimestampInfoEXT" "timeDomain"
  vkTimeDomain :: TimeDomainEXT
  }
  deriving (Show, Eq)
withCStructCalibratedTimestampInfoEXT :: CalibratedTimestampInfoEXT -> (VkCalibratedTimestampInfoEXT -> IO a) -> IO a
withCStructCalibratedTimestampInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: CalibratedTimestampInfoEXT)) (\pPNext -> cont (VkCalibratedTimestampInfoEXT VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT pPNext (vkTimeDomain (from :: CalibratedTimestampInfoEXT))))
fromCStructCalibratedTimestampInfoEXT :: VkCalibratedTimestampInfoEXT -> IO CalibratedTimestampInfoEXT
fromCStructCalibratedTimestampInfoEXT c = CalibratedTimestampInfoEXT <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCalibratedTimestampInfoEXT)))
                                                                     <*> pure (vkTimeDomain (c :: VkCalibratedTimestampInfoEXT))
-- No documentation found for TopLevel "TimeDomainEXT"
type TimeDomainEXT = VkTimeDomainEXT

-- | Wrapper for 'vkGetCalibratedTimestampsEXT'
getCalibratedTimestampsEXT :: Device ->  Vector CalibratedTimestampInfoEXT ->  IO (Vector Word64, Word64)
getCalibratedTimestampsEXT = \(Device device commandTable) -> \timestampInfos -> alloca (\pMaxDeviation -> allocaArray ((Data.Vector.length timestampInfos)) (\pTimestamps -> withVec withCStructCalibratedTimestampInfoEXT timestampInfos (\pTimestampInfos -> Graphics.Vulkan.C.Dynamic.getCalibratedTimestampsEXT commandTable device (fromIntegral $ Data.Vector.length timestampInfos) pTimestampInfos pTimestamps pMaxDeviation >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> (Data.Vector.generateM ((Data.Vector.length timestampInfos)) (peekElemOff pTimestamps))<*>peek pMaxDeviation)))))

-- | Wrapper for 'vkGetPhysicalDeviceCalibrateableTimeDomainsEXT'
getNumPhysicalDeviceCalibrateableTimeDomainsEXT :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceCalibrateableTimeDomainsEXT = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pTimeDomainCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceCalibrateableTimeDomainsEXT commandTable physicalDevice pTimeDomainCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pTimeDomainCount)))

-- | Wrapper for 'vkGetPhysicalDeviceCalibrateableTimeDomainsEXT'
getPhysicalDeviceCalibrateableTimeDomainsEXT :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector TimeDomainEXT)
getPhysicalDeviceCalibrateableTimeDomainsEXT = \(PhysicalDevice physicalDevice commandTable) -> \timeDomainCount -> allocaArray (fromIntegral timeDomainCount) (\pTimeDomains -> with timeDomainCount (\pTimeDomainCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceCalibrateableTimeDomainsEXT commandTable physicalDevice pTimeDomainCount pTimeDomains >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM (peekElemOff pTimeDomains) =<< (fromIntegral <$> (peek pTimeDomainCount)))))))
-- | Call 'getNumPhysicalDeviceCalibrateableTimeDomainsEXT' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceCalibrateableTimeDomainsEXT' to get all the values.
getAllPhysicalDeviceCalibrateableTimeDomainsEXT :: PhysicalDevice ->  IO (Vector TimeDomainEXT)
getAllPhysicalDeviceCalibrateableTimeDomainsEXT physicalDevice =
  snd <$> getNumPhysicalDeviceCalibrateableTimeDomainsEXT physicalDevice
    >>= \num -> snd <$> getPhysicalDeviceCalibrateableTimeDomainsEXT physicalDevice num

