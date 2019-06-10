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

-- No documentation found for TopLevel "VkAccessFlags"
type VkAccessFlags = VkAccessFlagBits

data VkAttachmentDescription

data VkAttachmentDescriptionFlagBits

-- No documentation found for TopLevel "VkAttachmentDescriptionFlags"
type VkAttachmentDescriptionFlags = VkAttachmentDescriptionFlagBits

data VkAttachmentLoadOp

data VkAttachmentReference

data VkAttachmentStoreOp

data VkDependencyFlagBits

-- No documentation found for TopLevel "VkDependencyFlags"
type VkDependencyFlags = VkDependencyFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkFramebuffer_T
-- No documentation found for TopLevel "VkFramebuffer"
type VkFramebuffer = Ptr VkFramebuffer_T

data VkFramebufferCreateFlags

data VkFramebufferCreateInfo

data VkPipelineBindPoint

data VkRenderPassCreateFlags

data VkRenderPassCreateInfo

data VkSubpassDependency

data VkSubpassDescription

data VkSubpassDescriptionFlagBits

-- No documentation found for TopLevel "VkSubpassDescriptionFlags"
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
