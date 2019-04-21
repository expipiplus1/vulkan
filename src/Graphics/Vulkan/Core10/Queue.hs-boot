{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Queue
  ( Fence
  , PipelineStageFlagBits
  , PipelineStageFlags
  , Semaphore
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits
  , VkFence
  , VkSemaphore
  )


-- | VkFence - Opaque handle to a fence object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Fence.vkCreateFence',
-- 'Graphics.Vulkan.C.Core10.Fence.vkDestroyFence',
-- 'Graphics.Vulkan.C.Core10.Fence.vkGetFenceStatus',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkQueueBindSparse',
-- 'Graphics.Vulkan.C.Core10.Queue.vkQueueSubmit',
-- 'Graphics.Vulkan.C.Core10.Fence.vkResetFences',
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences'
type Fence = VkFence

-- | VkPipelineStageFlagBits - Bitmask specifying pipeline stages
--
-- = Description
--
-- __Note__
--
-- An execution dependency with only
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT' in
-- the destination stage mask will only prevent that stage from executing
-- in subsequently submitted commands. As this stage does not perform any
-- actual execution, this is not observable - in effect, it does not delay
-- processing of subsequent commands. Similarly an execution dependency
-- with only
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT' in
-- the source stage mask will effectively not wait for any prior commands
-- to complete.
--
-- When defining a memory dependency, using only
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT' or
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT' would
-- never make any accesses available and\/or visible because these stages
-- do not access memory.
--
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT'
-- and 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT'
-- are useful for accomplishing layout transitions and queue ownership
-- operations when the required execution dependency is satisfied by other
-- means - for example, semaphore operations between queues.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlags',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp'
type PipelineStageFlagBits = VkPipelineStageFlagBits

-- | VkPipelineStageFlags - Bitmask of VkPipelineStageFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlags' is a bitmask type
-- for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
type PipelineStageFlags = PipelineStageFlagBits

-- | VkSemaphore - Opaque handle to a semaphore object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkCreateSemaphore',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkDestroySemaphore'
type Semaphore = VkSemaphore
