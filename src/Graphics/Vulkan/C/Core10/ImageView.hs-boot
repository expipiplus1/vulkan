{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.ImageView
  ( VkComponentMapping
  , VkComponentSwizzle
  , VkImageSubresourceRange
  , VkImageView
  , VkImageViewCreateFlagBits
  , VkImageViewCreateFlags
  , VkImageViewCreateInfo
  , VkImageViewType
  , FN_vkCreateImageView
  , PFN_vkCreateImageView
  , FN_vkDestroyImageView
  , PFN_vkDestroyImageView
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


data VkComponentMapping

data VkComponentSwizzle

data VkImageSubresourceRange

-- | Dummy data to tag the 'Ptr' with
data VkImageView_T
-- | VkImageView - Opaque handle to an image view object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle.VkImageViewHandleInfoNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.vkCmdBindShadingRateImageNV',
-- 'vkCreateImageView', 'vkDestroyImageView'
type VkImageView = Ptr VkImageView_T

data VkImageViewCreateFlagBits

-- | VkImageViewCreateFlags - Reserved for future use
--
-- = Description
--
-- 'VkImageViewCreateFlags' is a bitmask type for setting a mask of zero or
-- more 'VkImageViewCreateFlagBits'.
--
-- = See Also
--
-- 'VkImageViewCreateFlagBits', 'VkImageViewCreateInfo'
type VkImageViewCreateFlags = VkImageViewCreateFlagBits

data VkImageViewCreateInfo

data VkImageViewType

type FN_vkCreateImageView = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult
type PFN_vkCreateImageView = FunPtr FN_vkCreateImageView

type FN_vkDestroyImageView = ("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyImageView = FunPtr FN_vkDestroyImageView
