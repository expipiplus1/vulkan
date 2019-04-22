{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_EXT_global_priority
  ( withCStructDeviceQueueGlobalPriorityCreateInfoEXT
  , fromCStructDeviceQueueGlobalPriorityCreateInfoEXT
  , DeviceQueueGlobalPriorityCreateInfoEXT(..)
  , QueueGlobalPriorityEXT
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
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_global_priority
  ( VkDeviceQueueGlobalPriorityCreateInfoEXT(..)
  , VkQueueGlobalPriorityEXT(..)
  , pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME
  , pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION
  , pattern VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT
  , pattern VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT
  , pattern VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT
  , pattern VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_NOT_PERMITTED_EXT
  , pattern STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
  )



-- | VkDeviceQueueGlobalPriorityCreateInfoEXT - Specify a system wide
-- priority
--
-- = Description
--
-- A queue created without specifying
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_global_priority.VkDeviceQueueGlobalPriorityCreateInfoEXT'
-- will default to
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_global_priority.VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_global_priority.VkQueueGlobalPriorityEXT',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data DeviceQueueGlobalPriorityCreateInfoEXT = DeviceQueueGlobalPriorityCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceQueueGlobalPriorityCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceQueueGlobalPriorityCreateInfoEXT" "globalPriority"
  globalPriority :: QueueGlobalPriorityEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDeviceQueueGlobalPriorityCreateInfoEXT' and
-- marshal a 'DeviceQueueGlobalPriorityCreateInfoEXT' into it. The 'VkDeviceQueueGlobalPriorityCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceQueueGlobalPriorityCreateInfoEXT :: DeviceQueueGlobalPriorityCreateInfoEXT -> (VkDeviceQueueGlobalPriorityCreateInfoEXT -> IO a) -> IO a
withCStructDeviceQueueGlobalPriorityCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DeviceQueueGlobalPriorityCreateInfoEXT)) (\pPNext -> cont (VkDeviceQueueGlobalPriorityCreateInfoEXT VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT pPNext (globalPriority (marshalled :: DeviceQueueGlobalPriorityCreateInfoEXT))))

-- | A function to read a 'VkDeviceQueueGlobalPriorityCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'DeviceQueueGlobalPriorityCreateInfoEXT'.
fromCStructDeviceQueueGlobalPriorityCreateInfoEXT :: VkDeviceQueueGlobalPriorityCreateInfoEXT -> IO DeviceQueueGlobalPriorityCreateInfoEXT
fromCStructDeviceQueueGlobalPriorityCreateInfoEXT c = DeviceQueueGlobalPriorityCreateInfoEXT <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceQueueGlobalPriorityCreateInfoEXT)))
                                                                                             <*> pure (vkGlobalPriority (c :: VkDeviceQueueGlobalPriorityCreateInfoEXT))

instance Zero DeviceQueueGlobalPriorityCreateInfoEXT where
  zero = DeviceQueueGlobalPriorityCreateInfoEXT Nothing
                                                zero


-- | VkQueueGlobalPriorityEXT - Values specifying a system-wide queue
-- priority
--
-- = Description
--
-- Priority values are sorted in ascending order. A comparison operation on
-- the enum values can be used to determine the priority order.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_global_priority.VkDeviceQueueGlobalPriorityCreateInfoEXT'
type QueueGlobalPriorityEXT = VkQueueGlobalPriorityEXT


{-# complete QUEUE_GLOBAL_PRIORITY_LOW_EXT, QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT, QUEUE_GLOBAL_PRIORITY_HIGH_EXT, QUEUE_GLOBAL_PRIORITY_REALTIME_EXT :: QueueGlobalPriorityEXT #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_global_priority.VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT'
-- is below the system default. Useful for non-interactive tasks.
pattern QUEUE_GLOBAL_PRIORITY_LOW_EXT :: (a ~ QueueGlobalPriorityEXT) => a
pattern QUEUE_GLOBAL_PRIORITY_LOW_EXT = VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_global_priority.VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT'
-- is the system default priority.
pattern QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT :: (a ~ QueueGlobalPriorityEXT) => a
pattern QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT = VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_global_priority.VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT'
-- is above the system default.
pattern QUEUE_GLOBAL_PRIORITY_HIGH_EXT :: (a ~ QueueGlobalPriorityEXT) => a
pattern QUEUE_GLOBAL_PRIORITY_HIGH_EXT = VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_global_priority.VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT'
-- is the highest priority. Useful for critical tasks.
pattern QUEUE_GLOBAL_PRIORITY_REALTIME_EXT :: (a ~ QueueGlobalPriorityEXT) => a
pattern QUEUE_GLOBAL_PRIORITY_REALTIME_EXT = VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME"
pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME = VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION"
pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION :: Integral a => a
pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION = VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION
