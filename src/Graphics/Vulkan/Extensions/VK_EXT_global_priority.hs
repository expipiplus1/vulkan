{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_global_priority
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DeviceQueueGlobalPriorityCreateInfoEXT(..)
  , 
#endif
  QueueGlobalPriorityEXT
  , pattern QUEUE_GLOBAL_PRIORITY_LOW_EXT
  , pattern QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT
  , pattern QUEUE_GLOBAL_PRIORITY_HIGH_EXT
  , pattern QUEUE_GLOBAL_PRIORITY_REALTIME_EXT
  , pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME
  , pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
  , pattern ERROR_NOT_PERMITTED_EXT
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_global_priority
  ( VkQueueGlobalPriorityEXT(..)
  , pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME
  , pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION
  , pattern VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT
  , pattern VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT
  , pattern VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT
  , pattern VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_NOT_PERMITTED_EXT
  , pattern STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceQueueGlobalPriorityCreateInfoEXT"
data DeviceQueueGlobalPriorityCreateInfoEXT = DeviceQueueGlobalPriorityCreateInfoEXT
  { -- No documentation found for Nested "DeviceQueueGlobalPriorityCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceQueueGlobalPriorityCreateInfoEXT" "globalPriority"
  globalPriority :: QueueGlobalPriorityEXT
  }
  deriving (Show, Eq)

instance Zero DeviceQueueGlobalPriorityCreateInfoEXT where
  zero = DeviceQueueGlobalPriorityCreateInfoEXT Nothing
                                                zero

#endif

-- No documentation found for TopLevel "QueueGlobalPriorityEXT"
type QueueGlobalPriorityEXT = VkQueueGlobalPriorityEXT


{-# complete QUEUE_GLOBAL_PRIORITY_LOW_EXT, QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT, QUEUE_GLOBAL_PRIORITY_HIGH_EXT, QUEUE_GLOBAL_PRIORITY_REALTIME_EXT :: QueueGlobalPriorityEXT #-}


-- No documentation found for Nested "QueueGlobalPriorityEXT" "QUEUE_GLOBAL_PRIORITY_LOW_EXT"
pattern QUEUE_GLOBAL_PRIORITY_LOW_EXT :: (a ~ QueueGlobalPriorityEXT) => a
pattern QUEUE_GLOBAL_PRIORITY_LOW_EXT = VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT


-- No documentation found for Nested "QueueGlobalPriorityEXT" "QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT"
pattern QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT :: (a ~ QueueGlobalPriorityEXT) => a
pattern QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT = VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT


-- No documentation found for Nested "QueueGlobalPriorityEXT" "QUEUE_GLOBAL_PRIORITY_HIGH_EXT"
pattern QUEUE_GLOBAL_PRIORITY_HIGH_EXT :: (a ~ QueueGlobalPriorityEXT) => a
pattern QUEUE_GLOBAL_PRIORITY_HIGH_EXT = VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT


-- No documentation found for Nested "QueueGlobalPriorityEXT" "QUEUE_GLOBAL_PRIORITY_REALTIME_EXT"
pattern QUEUE_GLOBAL_PRIORITY_REALTIME_EXT :: (a ~ QueueGlobalPriorityEXT) => a
pattern QUEUE_GLOBAL_PRIORITY_REALTIME_EXT = VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME"
pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME = VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION"
pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION :: Integral a => a
pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION = VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION
