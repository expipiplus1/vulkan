{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_draw_indirect_count
  ( 
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  vkCmdDrawIndexedIndirectCountAMD
  , 
#endif
  FN_vkCmdDrawIndexedIndirectCountAMD
  , PFN_vkCmdDrawIndexedIndirectCountAMD
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdDrawIndirectCountAMD
#endif
  , FN_vkCmdDrawIndirectCountAMD
  , PFN_vkCmdDrawIndirectCountAMD
  , pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
  , pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
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
import Graphics.Vulkan.NamedType
  ( (:::)
  )


#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdDrawIndexedIndirectCountAMD"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndexedIndirectCountAMD" vkCmdDrawIndexedIndirectCountAMD :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()

#endif
type FN_vkCmdDrawIndexedIndirectCountAMD = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndexedIndirectCountAMD = FunPtr FN_vkCmdDrawIndexedIndirectCountAMD
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdDrawIndirectCountAMD"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndirectCountAMD" vkCmdDrawIndirectCountAMD :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()

#endif
type FN_vkCmdDrawIndirectCountAMD = ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
type PFN_vkCmdDrawIndirectCountAMD = FunPtr FN_vkCmdDrawIndirectCountAMD
-- No documentation found for TopLevel "VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME"
pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_AMD_draw_indirect_count"
-- No documentation found for TopLevel "VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION"
pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION :: Integral a => a
pattern VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1
