{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_buffer_marker
  ( cmdWriteBufferMarkerAMD
  , pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION
  , pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME
  ) where

import Data.Word
  ( Word32
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdWriteBufferMarkerAMD
  )


import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  , PipelineStageFlagBits
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker
  ( pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME
  , pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION
  )



-- | Wrapper for 'vkCmdWriteBufferMarkerAMD'
cmdWriteBufferMarkerAMD :: CommandBuffer ->  PipelineStageFlagBits ->  Buffer ->  DeviceSize ->  Word32 ->  IO ()
cmdWriteBufferMarkerAMD = \(CommandBuffer commandBuffer commandTable) -> \pipelineStage -> \dstBuffer -> \dstOffset -> \marker -> Graphics.Vulkan.C.Dynamic.cmdWriteBufferMarkerAMD commandTable commandBuffer pipelineStage dstBuffer dstOffset marker *> (pure ())
