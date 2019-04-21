{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count
  ( FN_vkCmdDrawIndexedIndirectCountKHR
  , PFN_vkCmdDrawIndexedIndirectCountKHR
  , FN_vkCmdDrawIndirectCountKHR
  , PFN_vkCmdDrawIndirectCountKHR
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
  ( VkCommandBuffer
  )


type FN_vkCmdDrawIndexedIndirectCountKHR = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndexedIndirectCountKHR = FunPtr FN_vkCmdDrawIndexedIndirectCountKHR

type FN_vkCmdDrawIndirectCountKHR = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndirectCountKHR = FunPtr FN_vkCmdDrawIndirectCountKHR
