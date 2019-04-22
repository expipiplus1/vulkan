{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_draw_indirect_count
  ( vkCmdDrawIndexedIndirectCountAMD
  , vkCmdDrawIndirectCountAMD
  , pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count
  ( vkCmdDrawIndexedIndirectCountKHR
  , vkCmdDrawIndirectCountKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "vkCmdDrawIndexedIndirectCountAMD"
vkCmdDrawIndexedIndirectCountAMD :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
vkCmdDrawIndexedIndirectCountAMD = vkCmdDrawIndexedIndirectCountKHR

-- No documentation found for TopLevel "vkCmdDrawIndirectCountAMD"
vkCmdDrawIndirectCountAMD :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
vkCmdDrawIndirectCountAMD = vkCmdDrawIndirectCountKHR

-- No documentation found for TopLevel "VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME"
pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_AMD_draw_indirect_count"

-- No documentation found for TopLevel "VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION"
pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION :: Integral a => a
pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1
