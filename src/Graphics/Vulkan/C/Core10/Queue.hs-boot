{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  , VkFence
  , VkPipelineStageFlagBits
  , VkPipelineStageFlags
  , VkQueue
  , VkSemaphore
  , VkSubmitInfo
  , FN_vkDeviceWaitIdle
  , PFN_vkDeviceWaitIdle
  , FN_vkGetDeviceQueue
  , PFN_vkGetDeviceQueue
  , FN_vkQueueSubmit
  , PFN_vkQueueSubmit
  , FN_vkQueueWaitIdle
  , PFN_vkQueueWaitIdle
  ) where

import Data.Word
  ( Word32
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
  )


-- | Dummy data to tag the 'Ptr' with
data VkCommandBuffer_T
-- No documentation found for TopLevel "VkCommandBuffer"
type VkCommandBuffer = Ptr VkCommandBuffer_T

-- | Dummy data to tag the 'Ptr' with
data VkFence_T
-- No documentation found for TopLevel "VkFence"
type VkFence = Ptr VkFence_T

data VkPipelineStageFlagBits

-- No documentation found for TopLevel "VkPipelineStageFlags"
type VkPipelineStageFlags = VkPipelineStageFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkQueue_T
-- No documentation found for TopLevel "VkQueue"
type VkQueue = Ptr VkQueue_T

-- | Dummy data to tag the 'Ptr' with
data VkSemaphore_T
-- No documentation found for TopLevel "VkSemaphore"
type VkSemaphore = Ptr VkSemaphore_T

data VkSubmitInfo

type FN_vkDeviceWaitIdle = ("device" ::: VkDevice) -> IO VkResult
type PFN_vkDeviceWaitIdle = FunPtr FN_vkDeviceWaitIdle

type FN_vkGetDeviceQueue = ("device" ::: VkDevice) -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr VkQueue) -> IO ()
type PFN_vkGetDeviceQueue = FunPtr FN_vkGetDeviceQueue

type FN_vkQueueSubmit = ("queue" ::: VkQueue) -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr VkSubmitInfo) -> ("fence" ::: VkFence) -> IO VkResult
type PFN_vkQueueSubmit = FunPtr FN_vkQueueSubmit

type FN_vkQueueWaitIdle = ("queue" ::: VkQueue) -> IO VkResult
type PFN_vkQueueWaitIdle = FunPtr FN_vkQueueWaitIdle
