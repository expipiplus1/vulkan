{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker
  ( 
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  vkCmdWriteBufferMarkerAMD
  , 
#endif
  FN_vkCmdWriteBufferMarkerAMD
  , PFN_vkCmdWriteBufferMarkerAMD
  , pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME
  , pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION
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
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdWriteBufferMarkerAMD"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdWriteBufferMarkerAMD" vkCmdWriteBufferMarkerAMD :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ()

#endif
type FN_vkCmdWriteBufferMarkerAMD = ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ()
type PFN_vkCmdWriteBufferMarkerAMD = FunPtr FN_vkCmdWriteBufferMarkerAMD
-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_EXTENSION_NAME"
pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"
-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_SPEC_VERSION"
pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION :: Integral a => a
pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION = 1
