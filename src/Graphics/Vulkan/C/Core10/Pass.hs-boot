{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits
  , VkAccessFlags
  , VkAttachmentDescription
  , VkAttachmentDescriptionFlagBits
  , VkAttachmentDescriptionFlags
  , VkAttachmentLoadOp
  , VkAttachmentReference
  , VkAttachmentStoreOp
  , VkDependencyFlagBits
  , VkDependencyFlags
  , VkFramebuffer
  , VkFramebufferCreateFlags
  , VkFramebufferCreateInfo
  , VkPipelineBindPoint
  , VkRenderPassCreateFlags
  , VkRenderPassCreateInfo
  , VkSubpassDependency
  , VkSubpassDescription
  , VkSubpassDescriptionFlagBits
  , VkSubpassDescriptionFlags
  , FN_vkCreateFramebuffer
  , PFN_vkCreateFramebuffer
  , FN_vkCreateRenderPass
  , PFN_vkCreateRenderPass
  , FN_vkDestroyFramebuffer
  , PFN_vkDestroyFramebuffer
  , FN_vkDestroyRenderPass
  , PFN_vkDestroyRenderPass
  , FN_vkGetRenderAreaGranularity
  , PFN_vkGetRenderAreaGranularity
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
import {-# source #-} Graphics.Vulkan.C.Core10.Pipeline
  ( VkExtent2D
  , VkRenderPass
  )


data VkAccessFlagBits

-- | VkAccessFlags - Bitmask of VkAccessFlagBits
--
-- = Description
--
-- 'VkAccessFlags' is a bitmask type for setting a mask of zero or more
-- 'VkAccessFlagBits'.
--
-- = See Also
--
-- 'VkAccessFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkMemoryBarrier',
-- 'VkSubpassDependency'
type VkAccessFlags = VkAccessFlagBits

data VkAttachmentDescription

data VkAttachmentDescriptionFlagBits

-- | VkAttachmentDescriptionFlags - Bitmask of
-- VkAttachmentDescriptionFlagBits
--
-- = Description
--
-- 'VkAttachmentDescriptionFlags' is a bitmask type for setting a mask of
-- zero or more 'VkAttachmentDescriptionFlagBits'.
--
-- = See Also
--
-- 'VkAttachmentDescription', 'VkAttachmentDescriptionFlagBits'
type VkAttachmentDescriptionFlags = VkAttachmentDescriptionFlagBits

data VkAttachmentLoadOp

data VkAttachmentReference

data VkAttachmentStoreOp

data VkDependencyFlagBits

-- | VkDependencyFlags - Bitmask of VkDependencyFlagBits
--
-- = Description
--
-- 'VkDependencyFlags' is a bitmask type for setting a mask of zero or more
-- 'VkDependencyFlagBits'.
--
-- = See Also
--
-- 'VkDependencyFlagBits', 'VkSubpassDependency',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
type VkDependencyFlags = VkDependencyFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkFramebuffer_T
-- | VkFramebuffer - Opaque handle to a framebuffer object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo',
-- 'vkCreateFramebuffer', 'vkDestroyFramebuffer'
type VkFramebuffer = Ptr VkFramebuffer_T

data VkFramebufferCreateFlags

data VkFramebufferCreateInfo

data VkPipelineBindPoint

data VkRenderPassCreateFlags

data VkRenderPassCreateInfo

data VkSubpassDependency

data VkSubpassDescription

data VkSubpassDescriptionFlagBits

-- | VkSubpassDescriptionFlags - Bitmask of VkSubpassDescriptionFlagBits
--
-- = Description
--
-- 'VkSubpassDescriptionFlags' is a bitmask type for setting a mask of zero
-- or more 'VkSubpassDescriptionFlagBits'.
--
-- = See Also
--
-- 'VkSubpassDescription', 'VkSubpassDescriptionFlagBits'
type VkSubpassDescriptionFlags = VkSubpassDescriptionFlagBits

type FN_vkCreateFramebuffer = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult
type PFN_vkCreateFramebuffer = FunPtr FN_vkCreateFramebuffer

type FN_vkCreateRenderPass = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
type PFN_vkCreateRenderPass = FunPtr FN_vkCreateRenderPass

type FN_vkDestroyFramebuffer = ("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyFramebuffer = FunPtr FN_vkDestroyFramebuffer

type FN_vkDestroyRenderPass = ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyRenderPass = FunPtr FN_vkDestroyRenderPass

type FN_vkGetRenderAreaGranularity = ("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ()
type PFN_vkGetRenderAreaGranularity = FunPtr FN_vkGetRenderAreaGranularity
