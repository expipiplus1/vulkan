{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateFlagBits
  , VkFenceCreateFlags
  , VkFenceCreateInfo
  , FN_vkCreateFence
  , PFN_vkCreateFence
  , FN_vkDestroyFence
  , PFN_vkDestroyFence
  , FN_vkGetFenceStatus
  , PFN_vkGetFenceStatus
  , FN_vkResetFences
  , PFN_vkResetFences
  , FN_vkWaitForFences
  , PFN_vkWaitForFences
  ) where

import Data.Word
  ( Word32
  , Word64
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkBool32
  , VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkFence
  )


data VkFenceCreateFlagBits

-- No documentation found for TopLevel "VkFenceCreateFlags"
type VkFenceCreateFlags = VkFenceCreateFlagBits

data VkFenceCreateInfo

type FN_vkCreateFence = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
type PFN_vkCreateFence = FunPtr FN_vkCreateFence

type FN_vkDestroyFence = ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyFence = FunPtr FN_vkDestroyFence

type FN_vkGetFenceStatus = ("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult
type PFN_vkGetFenceStatus = FunPtr FN_vkGetFenceStatus

type FN_vkResetFences = ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult
type PFN_vkResetFences = FunPtr FN_vkResetFences

type FN_vkWaitForFences = ("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult
type PFN_vkWaitForFences = FunPtr FN_vkWaitForFences
