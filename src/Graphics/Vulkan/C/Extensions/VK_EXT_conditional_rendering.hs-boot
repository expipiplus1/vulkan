{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkCommandBufferInheritanceConditionalRenderingInfoEXT
  , VkConditionalRenderingBeginInfoEXT
  , VkConditionalRenderingFlagBitsEXT
  , VkConditionalRenderingFlagsEXT
  , VkPhysicalDeviceConditionalRenderingFeaturesEXT
  , FN_vkCmdBeginConditionalRenderingEXT
  , PFN_vkCmdBeginConditionalRenderingEXT
  , FN_vkCmdEndConditionalRenderingEXT
  , PFN_vkCmdEndConditionalRenderingEXT
  ) where

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


data VkCommandBufferInheritanceConditionalRenderingInfoEXT

data VkConditionalRenderingBeginInfoEXT

data VkConditionalRenderingFlagBitsEXT

-- No documentation found for TopLevel "VkConditionalRenderingFlagsEXT"
type VkConditionalRenderingFlagsEXT = VkConditionalRenderingFlagBitsEXT

data VkPhysicalDeviceConditionalRenderingFeaturesEXT

type FN_vkCmdBeginConditionalRenderingEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ()
type PFN_vkCmdBeginConditionalRenderingEXT = FunPtr FN_vkCmdBeginConditionalRenderingEXT

type FN_vkCmdEndConditionalRenderingEXT = ("commandBuffer" ::: VkCommandBuffer) -> IO ()
type PFN_vkCmdEndConditionalRenderingEXT = FunPtr FN_vkCmdEndConditionalRenderingEXT
