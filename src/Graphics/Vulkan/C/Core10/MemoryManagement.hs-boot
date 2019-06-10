{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  , VkMemoryRequirements
  , FN_vkBindBufferMemory
  , PFN_vkBindBufferMemory
  , FN_vkBindImageMemory
  , PFN_vkBindImageMemory
  , FN_vkGetBufferMemoryRequirements
  , PFN_vkGetBufferMemoryRequirements
  , FN_vkGetImageMemoryRequirements
  , PFN_vkGetImageMemoryRequirements
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
  ( VkDevice
  , VkDeviceSize
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  )


-- | Dummy data to tag the 'Ptr' with
data VkBuffer_T
-- No documentation found for TopLevel "VkBuffer"
type VkBuffer = Ptr VkBuffer_T

-- | Dummy data to tag the 'Ptr' with
data VkImage_T
-- No documentation found for TopLevel "VkImage"
type VkImage = Ptr VkImage_T

data VkMemoryRequirements

type FN_vkBindBufferMemory = ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult
type PFN_vkBindBufferMemory = FunPtr FN_vkBindBufferMemory

type FN_vkBindImageMemory = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult
type PFN_vkBindImageMemory = FunPtr FN_vkBindImageMemory

type FN_vkGetBufferMemoryRequirements = ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ()
type PFN_vkGetBufferMemoryRequirements = FunPtr FN_vkGetBufferMemoryRequirements

type FN_vkGetImageMemoryRequirements = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ()
type PFN_vkGetImageMemoryRequirements = FunPtr FN_vkGetImageMemoryRequirements
