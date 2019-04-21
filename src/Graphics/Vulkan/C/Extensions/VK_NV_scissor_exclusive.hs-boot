{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( VkPhysicalDeviceExclusiveScissorFeaturesNV
  , VkPipelineViewportExclusiveScissorStateCreateInfoNV
  , FN_vkCmdSetExclusiveScissorNV
  , PFN_vkCmdSetExclusiveScissorNV
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
import {-# source #-} Graphics.Vulkan.C.Core10.Pipeline
  ( VkRect2D
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )


data VkPhysicalDeviceExclusiveScissorFeaturesNV

data VkPipelineViewportExclusiveScissorStateCreateInfoNV

type FN_vkCmdSetExclusiveScissorNV = ("commandBuffer" ::: VkCommandBuffer) -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr VkRect2D) -> IO ()
type PFN_vkCmdSetExclusiveScissorNV = FunPtr FN_vkCmdSetExclusiveScissorNV
