{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( VkDiscardRectangleModeEXT
  , VkPhysicalDeviceDiscardRectanglePropertiesEXT
  , VkPipelineDiscardRectangleStateCreateFlagsEXT
  , VkPipelineDiscardRectangleStateCreateInfoEXT
  , FN_vkCmdSetDiscardRectangleEXT
  , PFN_vkCmdSetDiscardRectangleEXT
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


data VkDiscardRectangleModeEXT

data VkPhysicalDeviceDiscardRectanglePropertiesEXT

data VkPipelineDiscardRectangleStateCreateFlagsEXT

data VkPipelineDiscardRectangleStateCreateInfoEXT

type FN_vkCmdSetDiscardRectangleEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("firstDiscardRectangle" ::: Word32) -> ("discardRectangleCount" ::: Word32) -> ("pDiscardRectangles" ::: Ptr VkRect2D) -> IO ()
type PFN_vkCmdSetDiscardRectangleEXT = FunPtr FN_vkCmdSetDiscardRectangleEXT
