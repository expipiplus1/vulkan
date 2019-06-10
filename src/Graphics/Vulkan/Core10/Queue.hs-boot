{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Queue
  ( Fence
  , PipelineStageFlagBits
  , PipelineStageFlags
  , Semaphore
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkFence
  , VkPipelineStageFlagBits
  , VkSemaphore
  )


-- No documentation found for TopLevel "Fence"
type Fence = VkFence

-- No documentation found for TopLevel "PipelineStageFlagBits"
type PipelineStageFlagBits = VkPipelineStageFlagBits

-- No documentation found for TopLevel "PipelineStageFlags"
type PipelineStageFlags = PipelineStageFlagBits

-- No documentation found for TopLevel "Semaphore"
type Semaphore = VkSemaphore
