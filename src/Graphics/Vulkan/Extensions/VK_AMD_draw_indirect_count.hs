{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count
  ( cmdDrawIndexedIndirectCountAMD
  , cmdDrawIndirectCountAMD
  , pattern AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , pattern AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Extensions.VK_AMD_draw_indirect_count
  ( pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
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
import Graphics.Vulkan.Extensions.VK_KHR_draw_indirect_count
  ( cmdDrawIndexedIndirectCountKHR
  , cmdDrawIndirectCountKHR
  )


cmdDrawIndexedIndirectCountAMD :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndexedIndirectCountAMD = cmdDrawIndexedIndirectCountKHR

cmdDrawIndirectCountAMD :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndirectCountAMD = cmdDrawIndirectCountKHR

-- No documentation found for TopLevel "VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME"
pattern AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME = VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION"
pattern AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION :: Integral a => a
pattern AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
