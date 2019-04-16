{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_draw_indirect_count
  ( cmdDrawIndexedIndirectCountKHR
  , cmdDrawIndirectCountKHR
  , pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
  , pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  ) where

import Data.Word
  ( Word32
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdDrawIndexedIndirectCountKHR
  , cmdDrawIndirectCountKHR
  )


import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count
  ( pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
  )



-- | Wrapper for vkCmdDrawIndexedIndirectCountKHR
cmdDrawIndexedIndirectCountKHR :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndexedIndirectCountKHR = \(CommandBuffer commandBuffer commandTable) -> \buffer -> \offset -> \countBuffer -> \countBufferOffset -> \maxDrawCount -> \stride -> Graphics.Vulkan.C.Dynamic.cmdDrawIndexedIndirectCountKHR commandTable commandBuffer buffer offset countBuffer countBufferOffset maxDrawCount stride *> (pure ())

-- | Wrapper for vkCmdDrawIndirectCountKHR
cmdDrawIndirectCountKHR :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndirectCountKHR = \(CommandBuffer commandBuffer commandTable) -> \buffer -> \offset -> \countBuffer -> \countBufferOffset -> \maxDrawCount -> \stride -> Graphics.Vulkan.C.Dynamic.cmdDrawIndirectCountKHR commandTable commandBuffer buffer offset countBuffer countBufferOffset maxDrawCount stride *> (pure ())
