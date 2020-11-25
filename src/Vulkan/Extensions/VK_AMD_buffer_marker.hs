{-# language CPP #-}
-- No documentation found for Chapter "VK_AMD_buffer_marker"
module Vulkan.Extensions.VK_AMD_buffer_marker  ( cmdWriteBufferMarkerAMD
                                               , AMD_BUFFER_MARKER_SPEC_VERSION
                                               , pattern AMD_BUFFER_MARKER_SPEC_VERSION
                                               , AMD_BUFFER_MARKER_EXTENSION_NAME
                                               , pattern AMD_BUFFER_MARKER_EXTENSION_NAME
                                               ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteBufferMarkerAMD))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteBufferMarkerAMD
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlagBits -> Buffer -> DeviceSize -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlagBits -> Buffer -> DeviceSize -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdWriteBufferMarkerAMD"
cmdWriteBufferMarkerAMD :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkCmdWriteBufferMarkerAMD" "commandBuffer"
                           CommandBuffer
                        -> -- No documentation found for Nested "vkCmdWriteBufferMarkerAMD" "pipelineStage"
                           PipelineStageFlagBits
                        -> -- No documentation found for Nested "vkCmdWriteBufferMarkerAMD" "dstBuffer"
                           ("dstBuffer" ::: Buffer)
                        -> -- No documentation found for Nested "vkCmdWriteBufferMarkerAMD" "dstOffset"
                           ("dstOffset" ::: DeviceSize)
                        -> -- No documentation found for Nested "vkCmdWriteBufferMarkerAMD" "marker"
                           ("marker" ::: Word32)
                        -> io ()
cmdWriteBufferMarkerAMD commandBuffer pipelineStage dstBuffer dstOffset marker = liftIO $ do
  let vkCmdWriteBufferMarkerAMDPtr = pVkCmdWriteBufferMarkerAMD (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdWriteBufferMarkerAMDPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteBufferMarkerAMD is null" Nothing Nothing
  let vkCmdWriteBufferMarkerAMD' = mkVkCmdWriteBufferMarkerAMD vkCmdWriteBufferMarkerAMDPtr
  vkCmdWriteBufferMarkerAMD' (commandBufferHandle (commandBuffer)) (pipelineStage) (dstBuffer) (dstOffset) (marker)
  pure $ ()


type AMD_BUFFER_MARKER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_SPEC_VERSION"
pattern AMD_BUFFER_MARKER_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_BUFFER_MARKER_SPEC_VERSION = 1


type AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"

-- No documentation found for TopLevel "VK_AMD_BUFFER_MARKER_EXTENSION_NAME"
pattern AMD_BUFFER_MARKER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_BUFFER_MARKER_EXTENSION_NAME = "VK_AMD_buffer_marker"

