{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_draw_indirect_count
  ( cmdDrawIndexedIndirectCountKHR
  , cmdDrawIndirectCountKHR
  , pattern KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , pattern KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count
  ( vkCmdDrawIndexedIndirectCountKHR
  , vkCmdDrawIndirectCountKHR
  , pattern VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , pattern VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
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



-- No documentation found for TopLevel "vkCmdDrawIndexedIndirectCountKHR"
cmdDrawIndexedIndirectCountKHR :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndexedIndirectCountKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdDrawIndirectCountKHR"
cmdDrawIndirectCountKHR :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndirectCountKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME"
pattern KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME = VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION"
pattern KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION :: Integral a => a
pattern KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION = VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
