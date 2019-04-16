{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count
  ( cmdDrawIndexedIndirectCountAMD
  , cmdDrawIndirectCountAMD
  , pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
  , pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  ) where

import Data.Word
  ( Word32
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdDrawIndexedIndirectCountAMD
  , cmdDrawIndirectCountAMD
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
import Graphics.Vulkan.C.Extensions.VK_AMD_draw_indirect_count
  ( pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
  )



-- | Wrapper for 'vkCmdDrawIndexedIndirectCountAMD'
cmdDrawIndexedIndirectCountAMD :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO (  )
cmdDrawIndexedIndirectCountAMD = \(CommandBuffer commandBuffer commandTable) -> \buffer -> \offset -> \countBuffer -> \countBufferOffset -> \maxDrawCount -> \stride -> Graphics.Vulkan.C.Dynamic.cmdDrawIndexedIndirectCountAMD commandTable commandBuffer buffer offset countBuffer countBufferOffset maxDrawCount stride *> (pure ())

-- | Wrapper for 'vkCmdDrawIndirectCountAMD'
cmdDrawIndirectCountAMD :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO (  )
cmdDrawIndirectCountAMD = \(CommandBuffer commandBuffer commandTable) -> \buffer -> \offset -> \countBuffer -> \countBufferOffset -> \maxDrawCount -> \stride -> Graphics.Vulkan.C.Dynamic.cmdDrawIndirectCountAMD commandTable commandBuffer buffer offset countBuffer countBufferOffset maxDrawCount stride *> (pure ())
