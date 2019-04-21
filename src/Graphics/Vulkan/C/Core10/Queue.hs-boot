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
-- | VkCommandBuffer - Opaque handle to a command buffer object
--
-- = See Also
--
-- 'VkSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkAllocateCommandBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkBeginCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindPipeline',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearAttachments',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearColorImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatch',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.vkCmdDispatchBase',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDraw',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexed',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndQuery',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndRenderPass',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdExecuteCommands',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdFillBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdNextSubpass',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPushConstants',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetQueryPool',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResolveImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetBlendConstants',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetDepthBias',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetDepthBounds',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.vkCmdSetDeviceMask',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetLineWidth',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetScissor',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilCompareMask',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilReference',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilWriteMask',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetViewport',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdUpdateBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkEndCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkFreeCommandBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkResetCommandBuffer'
type VkCommandBuffer = Ptr VkCommandBuffer_T

-- | Dummy data to tag the 'Ptr' with
data VkFence_T
-- | VkFence - Opaque handle to a fence object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Fence.vkCreateFence',
-- 'Graphics.Vulkan.C.Core10.Fence.vkDestroyFence',
-- 'Graphics.Vulkan.C.Core10.Fence.vkGetFenceStatus',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkQueueBindSparse',
-- 'vkQueueSubmit', 'Graphics.Vulkan.C.Core10.Fence.vkResetFences',
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences'
type VkFence = Ptr VkFence_T

data VkPipelineStageFlagBits

-- | VkPipelineStageFlags - Bitmask of VkPipelineStageFlagBits
--
-- = Description
--
-- 'VkPipelineStageFlags' is a bitmask type for setting a mask of zero or
-- more 'VkPipelineStageFlagBits'.
--
-- = See Also
--
-- 'VkPipelineStageFlagBits', 'VkSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
type VkPipelineStageFlags = VkPipelineStageFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkQueue_T
-- | VkQueue - Opaque handle to a queue object
--
-- = See Also
--
-- 'vkGetDeviceQueue',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.vkGetDeviceQueue2',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkQueueBindSparse',
-- 'vkQueueSubmit', 'vkQueueWaitIdle'
type VkQueue = Ptr VkQueue_T

-- | Dummy data to tag the 'Ptr' with
data VkSemaphore_T
-- | VkSemaphore - Opaque handle to a semaphore object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo',
-- 'VkSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkCreateSemaphore',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkDestroySemaphore'
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
