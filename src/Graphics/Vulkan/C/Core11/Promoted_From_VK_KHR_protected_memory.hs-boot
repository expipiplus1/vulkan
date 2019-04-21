{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( VkDeviceQueueInfo2
  , VkPhysicalDeviceProtectedMemoryFeatures
  , VkPhysicalDeviceProtectedMemoryProperties
  , VkProtectedSubmitInfo
  , FN_vkGetDeviceQueue2
  , PFN_vkGetDeviceQueue2
  ) where

import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkQueue
  )


data VkDeviceQueueInfo2

data VkPhysicalDeviceProtectedMemoryFeatures

data VkPhysicalDeviceProtectedMemoryProperties

data VkProtectedSubmitInfo

type FN_vkGetDeviceQueue2 = ("device" ::: VkDevice) -> ("pQueueInfo" ::: Ptr VkDeviceQueueInfo2) -> ("pQueue" ::: Ptr VkQueue) -> IO ()
type PFN_vkGetDeviceQueue2 = FunPtr FN_vkGetDeviceQueue2
