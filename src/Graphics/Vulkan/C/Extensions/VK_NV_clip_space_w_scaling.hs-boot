{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( VkPipelineViewportWScalingStateCreateInfoNV
  , VkViewportWScalingNV
  , FN_vkCmdSetViewportWScalingNV
  , PFN_vkCmdSetViewportWScalingNV
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
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )


data VkPipelineViewportWScalingStateCreateInfoNV

data VkViewportWScalingNV

type FN_vkCmdSetViewportWScalingNV = ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr VkViewportWScalingNV) -> IO ()
type PFN_vkCmdSetViewportWScalingNV = FunPtr FN_vkCmdSetViewportWScalingNV
