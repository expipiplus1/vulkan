{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Image
  ( VkImageCreateInfo
  , VkImageLayout
  , VkSubresourceLayout
  , FN_vkCreateImage
  , PFN_vkCreateImage
  , FN_vkDestroyImage
  , PFN_vkDestroyImage
  , FN_vkGetImageSubresourceLayout
  , PFN_vkGetImageSubresourceLayout
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
import {-# source #-} Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkImage
  )
import {-# source #-} Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkImageSubresource
  )


data VkImageCreateInfo

data VkImageLayout

data VkSubresourceLayout

type FN_vkCreateImage = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pImage" ::: Ptr VkImage) -> IO VkResult
type PFN_vkCreateImage = FunPtr FN_vkCreateImage

type FN_vkDestroyImage = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyImage = FunPtr FN_vkDestroyImage

type FN_vkGetImageSubresourceLayout = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSubresource" ::: Ptr VkImageSubresource) -> ("pLayout" ::: Ptr VkSubresourceLayout) -> IO ()
type PFN_vkGetImageSubresourceLayout = FunPtr FN_vkGetImageSubresourceLayout
