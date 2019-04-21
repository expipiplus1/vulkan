{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps
  ( VkCalibratedTimestampInfoEXT
  , VkTimeDomainEXT
  , FN_vkGetCalibratedTimestampsEXT
  , PFN_vkGetCalibratedTimestampsEXT
  , FN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  , PFN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  ) where

import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  , VkPhysicalDevice
  )


data VkCalibratedTimestampInfoEXT

data VkTimeDomainEXT

type FN_vkGetCalibratedTimestampsEXT = ("device" ::: VkDevice) -> ("timestampCount" ::: Word32) -> ("pTimestampInfos" ::: Ptr VkCalibratedTimestampInfoEXT) -> ("pTimestamps" ::: Ptr Word64) -> ("pMaxDeviation" ::: Ptr Word64) -> IO VkResult
type PFN_vkGetCalibratedTimestampsEXT = FunPtr FN_vkGetCalibratedTimestampsEXT

type FN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("pTimeDomainCount" ::: Ptr Word32) -> ("pTimeDomains" ::: Ptr VkTimeDomainEXT) -> IO VkResult
type PFN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT = FunPtr FN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
