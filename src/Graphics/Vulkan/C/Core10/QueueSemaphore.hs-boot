{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.QueueSemaphore
  ( VkSemaphoreCreateFlags
  , VkSemaphoreCreateInfo
  , FN_vkCreateSemaphore
  , PFN_vkCreateSemaphore
  , FN_vkDestroySemaphore
  , PFN_vkDestroySemaphore
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
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkSemaphore
  )


data VkSemaphoreCreateFlags

data VkSemaphoreCreateInfo

type FN_vkCreateSemaphore = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult
type PFN_vkCreateSemaphore = FunPtr FN_vkCreateSemaphore

type FN_vkDestroySemaphore = ("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroySemaphore = FunPtr FN_vkDestroySemaphore
