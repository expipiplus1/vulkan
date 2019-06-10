{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_buffer_marker
  ( cmdWriteBufferMarkerAMD
  , pattern AMD_BUFFER_MARKER_EXTENSION_NAME
  , pattern AMD_BUFFER_MARKER_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker
  ( vkCmdWriteBufferMarkerAMD
  , pattern VK_AMD_BUFFER_MARKER_EXTENSION_NAME
  , pattern VK_AMD_BUFFER_MARKER_SPEC_VERSION
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



-- No documentation found for TopLevel "vkCmdWriteBufferMarkerAMD"
cmdWriteBufferMarkerAMD :: CommandBuffer ->  PipelineStageFlagBits ->  Buffer ->  DeviceSize ->  Word32 ->  IO ()
cmdWriteBufferMarkerAMD = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_EXTENSION_NAME"
pattern AMD_BUFFER_MARKER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_BUFFER_MARKER_EXTENSION_NAME = VK_AMD_BUFFER_MARKER_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_SPEC_VERSION"
pattern AMD_BUFFER_MARKER_SPEC_VERSION :: Integral a => a
pattern AMD_BUFFER_MARKER_SPEC_VERSION = VK_AMD_BUFFER_MARKER_SPEC_VERSION
