{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_global_priority
  ( withCStructDeviceQueueGlobalPriorityCreateInfoEXT
  , fromCStructDeviceQueueGlobalPriorityCreateInfoEXT
  , DeviceQueueGlobalPriorityCreateInfoEXT(..)
  , QueueGlobalPriorityEXT
  , pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION
  , pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
  , pattern VK_ERROR_NOT_PERMITTED_EXT
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_global_priority
  ( VkDeviceQueueGlobalPriorityCreateInfoEXT(..)
  , VkQueueGlobalPriorityEXT(..)
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_global_priority
  ( pattern VK_ERROR_NOT_PERMITTED_EXT
  , pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME
  , pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION
  )


-- No documentation found for TopLevel "DeviceQueueGlobalPriorityCreateInfoEXT"
data DeviceQueueGlobalPriorityCreateInfoEXT = DeviceQueueGlobalPriorityCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceQueueGlobalPriorityCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceQueueGlobalPriorityCreateInfoEXT" "globalPriority"
  vkGlobalPriority :: QueueGlobalPriorityEXT
  }
  deriving (Show, Eq)
withCStructDeviceQueueGlobalPriorityCreateInfoEXT :: DeviceQueueGlobalPriorityCreateInfoEXT -> (VkDeviceQueueGlobalPriorityCreateInfoEXT -> IO a) -> IO a
withCStructDeviceQueueGlobalPriorityCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: DeviceQueueGlobalPriorityCreateInfoEXT)) (\pPNext -> cont (VkDeviceQueueGlobalPriorityCreateInfoEXT VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT pPNext (vkGlobalPriority (from :: DeviceQueueGlobalPriorityCreateInfoEXT))))
fromCStructDeviceQueueGlobalPriorityCreateInfoEXT :: VkDeviceQueueGlobalPriorityCreateInfoEXT -> IO DeviceQueueGlobalPriorityCreateInfoEXT
fromCStructDeviceQueueGlobalPriorityCreateInfoEXT c = DeviceQueueGlobalPriorityCreateInfoEXT <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceQueueGlobalPriorityCreateInfoEXT)))
                                                                                             <*> pure (vkGlobalPriority (c :: VkDeviceQueueGlobalPriorityCreateInfoEXT))
-- No documentation found for TopLevel "QueueGlobalPriorityEXT"
type QueueGlobalPriorityEXT = VkQueueGlobalPriorityEXT
