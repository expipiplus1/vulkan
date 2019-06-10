{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferView
  , VkBufferViewCreateFlags
  , VkBufferViewCreateInfo
  , FN_vkCreateBufferView
  , PFN_vkCreateBufferView
  , FN_vkDestroyBufferView
  , PFN_vkDestroyBufferView
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
data VkBufferView_T
-- No documentation found for TopLevel "VkBufferView"
type VkBufferView = Ptr VkBufferView_T

data VkBufferViewCreateFlags

data VkBufferViewCreateInfo

type FN_vkCreateBufferView = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkBufferView) -> IO VkResult
type PFN_vkCreateBufferView = FunPtr FN_vkCreateBufferView

type FN_vkDestroyBufferView = ("device" ::: VkDevice) -> ("bufferView" ::: VkBufferView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyBufferView = FunPtr FN_vkDestroyBufferView
