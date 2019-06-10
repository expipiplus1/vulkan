{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker
  ( FN_vkCmdWriteBufferMarkerAMD
  , PFN_vkCmdWriteBufferMarkerAMD
  , vkCmdWriteBufferMarkerAMD
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
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "vkCmdWriteBufferMarkerAMD"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdWriteBufferMarkerAMD" vkCmdWriteBufferMarkerAMD :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ()
#else
vkCmdWriteBufferMarkerAMD :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ()
vkCmdWriteBufferMarkerAMD deviceCmds = mkVkCmdWriteBufferMarkerAMD (pVkCmdWriteBufferMarkerAMD deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteBufferMarkerAMD
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ())
#endif

type FN_vkCmdWriteBufferMarkerAMD = ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ()
type PFN_vkCmdWriteBufferMarkerAMD = FunPtr FN_vkCmdWriteBufferMarkerAMD

-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_EXTENSION_NAME"
pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"

-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_SPEC_VERSION"
pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION :: Integral a => a
pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION = 1
