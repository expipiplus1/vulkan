{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count
  ( pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
  , pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , vkCmdDrawIndirectCountAMD
  , vkCmdDrawIndexedIndirectCountAMD
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.Core10.Queue
  ( VkCommandBuffer
  )


pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION :: Integral a => a
pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1
pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_AMD_draw_indirect_count"
-- | 
foreign import ccall "vkCmdDrawIndirectCountAMD" vkCmdDrawIndirectCountAMD :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
-- | 
foreign import ccall "vkCmdDrawIndexedIndirectCountAMD" vkCmdDrawIndexedIndirectCountAMD :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
