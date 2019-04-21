{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker
  ( FN_vkCmdWriteBufferMarkerAMD
  , PFN_vkCmdWriteBufferMarkerAMD
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import {-# source #-} Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits
  , VkCommandBuffer
  )


type FN_vkCmdWriteBufferMarkerAMD = ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ()
type PFN_vkCmdWriteBufferMarkerAMD = FunPtr FN_vkCmdWriteBufferMarkerAMD
