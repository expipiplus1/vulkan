{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Event
  ( Event
  , EventCreateFlags
  , withCStructEventCreateInfo
  , fromCStructEventCreateInfo
  , EventCreateInfo(..)
  , createEvent
  , destroyEvent
  , getEventStatus
  , resetEvent
  , setEvent
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createEvent
  , destroyEvent
  , getEventStatus
  , resetEvent
  , setEvent
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Event
  ( VkEventCreateFlags(..)
  , VkEventCreateInfo(..)
  , VkEvent
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "Event"
type Event = VkEvent
-- No documentation found for TopLevel "EventCreateFlags"
type EventCreateFlags = VkEventCreateFlags
-- No documentation found for TopLevel "EventCreateInfo"
data EventCreateInfo = EventCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "EventCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "EventCreateInfo" "flags"
  vkFlags :: EventCreateFlags
  }
  deriving (Show, Eq)
withCStructEventCreateInfo :: EventCreateInfo -> (VkEventCreateInfo -> IO a) -> IO a
withCStructEventCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: EventCreateInfo)) (\pPNext -> cont (VkEventCreateInfo VK_STRUCTURE_TYPE_EVENT_CREATE_INFO pPNext (vkFlags (from :: EventCreateInfo))))
fromCStructEventCreateInfo :: VkEventCreateInfo -> IO EventCreateInfo
fromCStructEventCreateInfo c = EventCreateInfo <$> -- Univalued Member elided
                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkEventCreateInfo)))
                                               <*> pure (vkFlags (c :: VkEventCreateInfo))

-- | Wrapper for vkCreateEvent
createEvent :: Device ->  EventCreateInfo ->  Maybe AllocationCallbacks ->  IO (Event)
createEvent = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pEvent -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructEventCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createEvent commandTable device pCreateInfo pAllocator pEvent >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pEvent)))))

-- | Wrapper for vkDestroyEvent
destroyEvent :: Device ->  Event ->  Maybe AllocationCallbacks ->  IO ()
destroyEvent = \(Device device commandTable) -> \event -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyEvent commandTable device event pAllocator *> (pure ()))

-- | Wrapper for vkGetEventStatus
getEventStatus :: Device ->  Event ->  IO (VkResult)
getEventStatus = \(Device device commandTable) -> \event -> Graphics.Vulkan.C.Dynamic.getEventStatus commandTable device event >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure r))

-- | Wrapper for vkResetEvent
resetEvent :: Device ->  Event ->  IO ()
resetEvent = \(Device device commandTable) -> \event -> Graphics.Vulkan.C.Dynamic.resetEvent commandTable device event >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))

-- | Wrapper for vkSetEvent
setEvent :: Device ->  Event ->  IO ()
setEvent = \(Device device commandTable) -> \event -> Graphics.Vulkan.C.Dynamic.setEvent commandTable device event >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))
