{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2
  ( VkAttachmentDescription2KHR
  , VkAttachmentReference2KHR
  , VkRenderPassCreateInfo2KHR
  , VkSubpassBeginInfoKHR
  , VkSubpassDependency2KHR
  , VkSubpassDescription2KHR
  , VkSubpassEndInfoKHR
  , FN_vkCmdBeginRenderPass2KHR
  , PFN_vkCmdBeginRenderPass2KHR
  , FN_vkCmdEndRenderPass2KHR
  , PFN_vkCmdEndRenderPass2KHR
  , FN_vkCmdNextSubpass2KHR
  , PFN_vkCmdNextSubpass2KHR
  , FN_vkCreateRenderPass2KHR
  , PFN_vkCreateRenderPass2KHR
  ) where

import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.CommandBufferBuilding
  ( VkRenderPassBeginInfo
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Pipeline
  ( VkRenderPass
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )


data VkAttachmentDescription2KHR

data VkAttachmentReference2KHR

data VkRenderPassCreateInfo2KHR

data VkSubpassBeginInfoKHR

data VkSubpassDependency2KHR

data VkSubpassDescription2KHR

data VkSubpassEndInfoKHR

type FN_vkCmdBeginRenderPass2KHR = ("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> IO ()
type PFN_vkCmdBeginRenderPass2KHR = FunPtr FN_vkCmdBeginRenderPass2KHR

type FN_vkCmdEndRenderPass2KHR = ("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()
type PFN_vkCmdEndRenderPass2KHR = FunPtr FN_vkCmdEndRenderPass2KHR

type FN_vkCmdNextSubpass2KHR = ("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()
type PFN_vkCmdNextSubpass2KHR = FunPtr FN_vkCmdNextSubpass2KHR

type FN_vkCreateRenderPass2KHR = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo2KHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult
type PFN_vkCreateRenderPass2KHR = FunPtr FN_vkCreateRenderPass2KHR
