{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkBindSparseInfo
  , VkImageAspectFlagBits
  , VkImageAspectFlags
  , VkImageSubresource
  , VkOffset3D
  , VkSparseBufferMemoryBindInfo
  , VkSparseImageFormatFlagBits
  , VkSparseImageFormatFlags
  , VkSparseImageFormatProperties
  , VkSparseImageMemoryBind
  , VkSparseImageMemoryBindInfo
  , VkSparseImageMemoryRequirements
  , VkSparseImageOpaqueMemoryBindInfo
  , VkSparseMemoryBind
  , VkSparseMemoryBindFlagBits
  , VkSparseMemoryBindFlags
  , FN_vkGetImageSparseMemoryRequirements
  , PFN_vkGetImageSparseMemoryRequirements
  , FN_vkGetPhysicalDeviceSparseImageFormatProperties
  , PFN_vkGetPhysicalDeviceSparseImageFormatProperties
  , FN_vkQueueBindSparse
  , PFN_vkQueueBindSparse
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
  ( VkFormat
  , VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  , VkImageTiling
  , VkImageType
  , VkImageUsageFlags
  , VkPhysicalDevice
  , VkSampleCountFlagBits
  )
import {-# source #-} Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkImage
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkFence
  , VkQueue
  )


data VkBindSparseInfo

data VkImageAspectFlagBits

-- No documentation found for TopLevel "VkImageAspectFlags"
type VkImageAspectFlags = VkImageAspectFlagBits

data VkImageSubresource

data VkOffset3D

data VkSparseBufferMemoryBindInfo

data VkSparseImageFormatFlagBits

-- No documentation found for TopLevel "VkSparseImageFormatFlags"
type VkSparseImageFormatFlags = VkSparseImageFormatFlagBits

data VkSparseImageFormatProperties

data VkSparseImageMemoryBind

data VkSparseImageMemoryBindInfo

data VkSparseImageMemoryRequirements

data VkSparseImageOpaqueMemoryBindInfo

data VkSparseMemoryBind

data VkSparseMemoryBindFlagBits

-- No documentation found for TopLevel "VkSparseMemoryBindFlags"
type VkSparseMemoryBindFlags = VkSparseMemoryBindFlagBits

type FN_vkGetImageSparseMemoryRequirements = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements) -> IO ()
type PFN_vkGetImageSparseMemoryRequirements = FunPtr FN_vkGetImageSparseMemoryRequirements

type FN_vkGetPhysicalDeviceSparseImageFormatProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties) -> IO ()
type PFN_vkGetPhysicalDeviceSparseImageFormatProperties = FunPtr FN_vkGetPhysicalDeviceSparseImageFormatProperties

type FN_vkQueueBindSparse = ("queue" ::: VkQueue) -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr VkBindSparseInfo) -> ("fence" ::: VkFence) -> IO VkResult
type PFN_vkQueueBindSparse = FunPtr FN_vkQueueBindSparse
