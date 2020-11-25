{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_draw_indirect_count"
module Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count  ( cmdDrawIndirectCount
                                                               , cmdDrawIndexedIndirectCount
                                                               ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.IO.Class (MonadIO)
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
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndexedIndirectCount))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndirectCount))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirectCount
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdDrawIndirectCount"
cmdDrawIndirectCount :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkCmdDrawIndirectCount" "commandBuffer"
                        CommandBuffer
                     -> -- No documentation found for Nested "vkCmdDrawIndirectCount" "buffer"
                        Buffer
                     -> -- No documentation found for Nested "vkCmdDrawIndirectCount" "offset"
                        ("offset" ::: DeviceSize)
                     -> -- No documentation found for Nested "vkCmdDrawIndirectCount" "countBuffer"
                        ("countBuffer" ::: Buffer)
                     -> -- No documentation found for Nested "vkCmdDrawIndirectCount" "countBufferOffset"
                        ("countBufferOffset" ::: DeviceSize)
                     -> -- No documentation found for Nested "vkCmdDrawIndirectCount" "maxDrawCount"
                        ("maxDrawCount" ::: Word32)
                     -> -- No documentation found for Nested "vkCmdDrawIndirectCount" "stride"
                        ("stride" ::: Word32)
                     -> io ()
cmdDrawIndirectCount commandBuffer buffer offset countBuffer countBufferOffset maxDrawCount stride = liftIO $ do
  let vkCmdDrawIndirectCountPtr = pVkCmdDrawIndirectCount (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawIndirectCountPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawIndirectCount is null" Nothing Nothing
  let vkCmdDrawIndirectCount' = mkVkCmdDrawIndirectCount vkCmdDrawIndirectCountPtr
  vkCmdDrawIndirectCount' (commandBufferHandle (commandBuffer)) (buffer) (offset) (countBuffer) (countBufferOffset) (maxDrawCount) (stride)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndexedIndirectCount
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdDrawIndexedIndirectCount"
cmdDrawIndexedIndirectCount :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "vkCmdDrawIndexedIndirectCount" "commandBuffer"
                               CommandBuffer
                            -> -- No documentation found for Nested "vkCmdDrawIndexedIndirectCount" "buffer"
                               Buffer
                            -> -- No documentation found for Nested "vkCmdDrawIndexedIndirectCount" "offset"
                               ("offset" ::: DeviceSize)
                            -> -- No documentation found for Nested "vkCmdDrawIndexedIndirectCount" "countBuffer"
                               ("countBuffer" ::: Buffer)
                            -> -- No documentation found for Nested "vkCmdDrawIndexedIndirectCount" "countBufferOffset"
                               ("countBufferOffset" ::: DeviceSize)
                            -> -- No documentation found for Nested "vkCmdDrawIndexedIndirectCount" "maxDrawCount"
                               ("maxDrawCount" ::: Word32)
                            -> -- No documentation found for Nested "vkCmdDrawIndexedIndirectCount" "stride"
                               ("stride" ::: Word32)
                            -> io ()
cmdDrawIndexedIndirectCount commandBuffer buffer offset countBuffer countBufferOffset maxDrawCount stride = liftIO $ do
  let vkCmdDrawIndexedIndirectCountPtr = pVkCmdDrawIndexedIndirectCount (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawIndexedIndirectCountPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawIndexedIndirectCount is null" Nothing Nothing
  let vkCmdDrawIndexedIndirectCount' = mkVkCmdDrawIndexedIndirectCount vkCmdDrawIndexedIndirectCountPtr
  vkCmdDrawIndexedIndirectCount' (commandBufferHandle (commandBuffer)) (buffer) (offset) (countBuffer) (countBufferOffset) (maxDrawCount) (stride)
  pure $ ()

