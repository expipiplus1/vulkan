{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Event
  ( VkEvent
  , VkEventCreateFlags
  , VkEventCreateInfo
  , FN_vkCreateEvent
  , PFN_vkCreateEvent
  , FN_vkDestroyEvent
  , PFN_vkDestroyEvent
  , FN_vkGetEventStatus
  , PFN_vkGetEventStatus
  , FN_vkResetEvent
  , PFN_vkResetEvent
  , FN_vkSetEvent
  , PFN_vkSetEvent
  ) where

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
  ( VkAllocationCallbacks
  , VkDevice
  )


-- | Dummy data to tag the 'Ptr' with
data VkEvent_T
-- No documentation found for TopLevel "VkEvent"
type VkEvent = Ptr VkEvent_T

data VkEventCreateFlags

data VkEventCreateInfo

type FN_vkCreateEvent = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult
type PFN_vkCreateEvent = FunPtr FN_vkCreateEvent

type FN_vkDestroyEvent = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyEvent = FunPtr FN_vkDestroyEvent

type FN_vkGetEventStatus = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
type PFN_vkGetEventStatus = FunPtr FN_vkGetEventStatus

type FN_vkResetEvent = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
type PFN_vkResetEvent = FunPtr FN_vkResetEvent

type FN_vkSetEvent = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
type PFN_vkSetEvent = FunPtr FN_vkSetEvent
