{-# language CPP #-}
-- No documentation found for Chapter "CommandBufferBuilding"
module Vulkan.Core10.CommandBufferBuilding  ( cmdBindPipeline
                                            , cmdSetViewport
                                            , cmdSetScissor
                                            , cmdSetLineWidth
                                            , cmdSetDepthBias
                                            , cmdSetBlendConstants
                                            , cmdSetDepthBounds
                                            , cmdSetStencilCompareMask
                                            , cmdSetStencilWriteMask
                                            , cmdSetStencilReference
                                            , cmdBindDescriptorSets
                                            , cmdBindIndexBuffer
                                            , cmdBindVertexBuffers
                                            , cmdDraw
                                            , cmdDrawIndexed
                                            , cmdDrawIndirect
                                            , cmdDrawIndexedIndirect
                                            , cmdDispatch
                                            , cmdDispatchIndirect
                                            , cmdCopyBuffer
                                            , cmdCopyImage
                                            , cmdBlitImage
                                            , cmdCopyBufferToImage
                                            , cmdCopyImageToBuffer
                                            , cmdUpdateBuffer
                                            , cmdFillBuffer
                                            , cmdClearColorImage
                                            , cmdClearDepthStencilImage
                                            , cmdClearAttachments
                                            , cmdResolveImage
                                            , cmdSetEvent
                                            , cmdResetEvent
                                            , cmdWaitEvents
                                            , cmdWaitEventsSafe
                                            , cmdPipelineBarrier
                                            , cmdBeginQuery
                                            , cmdUseQuery
                                            , cmdEndQuery
                                            , cmdResetQueryPool
                                            , cmdWriteTimestamp
                                            , cmdCopyQueryPoolResults
                                            , cmdPushConstants
                                            , cmdBeginRenderPass
                                            , cmdUseRenderPass
                                            , cmdNextSubpass
                                            , cmdEndRenderPass
                                            , cmdExecuteCommands
                                            , ClearRect(..)
                                            , ImageSubresourceLayers(..)
                                            , BufferCopy(..)
                                            , ImageCopy(..)
                                            , ImageBlit(..)
                                            , BufferImageCopy(..)
                                            , ImageResolve(..)
                                            , RenderPassBeginInfo(..)
                                            , ClearDepthStencilValue(..)
                                            , ClearAttachment(..)
                                            , ClearColorValue(..)
                                            , ClearValue(..)
                                            , IndexType(..)
                                            , SubpassContents(..)
                                            , StencilFaceFlagBits(..)
                                            , StencilFaceFlags
                                            ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CFloat(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.OtherTypes (BufferMemoryBarrier)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlagBits(..))
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core10.Handles (DescriptorSet)
import Vulkan.Core10.Handles (DescriptorSet(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginQuery))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginRenderPass))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindDescriptorSets))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindIndexBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindPipeline))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindVertexBuffers))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBlitImage))
import Vulkan.Dynamic (DeviceCmds(pVkCmdClearAttachments))
import Vulkan.Dynamic (DeviceCmds(pVkCmdClearColorImage))
import Vulkan.Dynamic (DeviceCmds(pVkCmdClearDepthStencilImage))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyBufferToImage))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyImage))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyImageToBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyQueryPoolResults))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDispatch))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDispatchIndirect))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDraw))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndexed))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndexedIndirect))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawIndirect))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndQuery))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndRenderPass))
import Vulkan.Dynamic (DeviceCmds(pVkCmdExecuteCommands))
import Vulkan.Dynamic (DeviceCmds(pVkCmdFillBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkCmdNextSubpass))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPipelineBarrier))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushConstants))
import Vulkan.Dynamic (DeviceCmds(pVkCmdResetEvent))
import Vulkan.Dynamic (DeviceCmds(pVkCmdResetQueryPool))
import Vulkan.Dynamic (DeviceCmds(pVkCmdResolveImage))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetBlendConstants))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthBias))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthBounds))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetEvent))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetLineWidth))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetScissor))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilCompareMask))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilReference))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetStencilWriteMask))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewport))
import Vulkan.Dynamic (DeviceCmds(pVkCmdUpdateBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWaitEvents))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteTimestamp))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupRenderPassBeginInfo)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Event)
import Vulkan.Core10.Handles (Event(..))
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent3D)
import Vulkan.Core10.Enums.Filter (Filter)
import Vulkan.Core10.Enums.Filter (Filter(..))
import Vulkan.Core10.Handles (Framebuffer)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(..))
import Vulkan.Core10.OtherTypes (ImageMemoryBarrier)
import Vulkan.Core10.ImageView (ImageSubresourceRange)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Core10.Enums.IndexType (IndexType(..))
import Vulkan.Core10.OtherTypes (MemoryBarrier)
import Vulkan.Core10.FundamentalTypes (Offset3D)
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Handles (PipelineLayout(..))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(..))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlagBits(..))
import Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlags)
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlagBits(..))
import Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlags)
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Handles (RenderPass)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (RenderPassAttachmentBeginInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (RenderPassSampleLocationsBeginInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_render_pass_transform (RenderPassTransformBeginInfoQCOM)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlagBits(..))
import Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.SubpassContents (SubpassContents)
import Vulkan.Core10.Enums.SubpassContents (SubpassContents(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Core10.Pipeline (Viewport)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO))
import Vulkan.Core10.Enums.IndexType (IndexType(..))
import Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlagBits(..))
import Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlags)
import Vulkan.Core10.Enums.SubpassContents (SubpassContents(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindPipeline
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> IO ()

-- No documentation found for TopLevel "vkCmdBindPipeline"
cmdBindPipeline :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkCmdBindPipeline" "commandBuffer"
                   CommandBuffer
                -> -- No documentation found for Nested "vkCmdBindPipeline" "pipelineBindPoint"
                   PipelineBindPoint
                -> -- No documentation found for Nested "vkCmdBindPipeline" "pipeline"
                   Pipeline
                -> io ()
cmdBindPipeline commandBuffer pipelineBindPoint pipeline = liftIO $ do
  let vkCmdBindPipelinePtr = pVkCmdBindPipeline (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBindPipelinePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindPipeline is null" Nothing Nothing
  let vkCmdBindPipeline' = mkVkCmdBindPipeline vkCmdBindPipelinePtr
  vkCmdBindPipeline' (commandBufferHandle (commandBuffer)) (pipelineBindPoint) (pipeline)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewport
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Viewport -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Viewport -> IO ()

-- No documentation found for TopLevel "vkCmdSetViewport"
cmdSetViewport :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vkCmdSetViewport" "commandBuffer"
                  CommandBuffer
               -> -- No documentation found for Nested "vkCmdSetViewport" "firstViewport"
                  ("firstViewport" ::: Word32)
               -> -- No documentation found for Nested "vkCmdSetViewport" "pViewports"
                  ("viewports" ::: Vector Viewport)
               -> io ()
cmdSetViewport commandBuffer firstViewport viewports = liftIO . evalContT $ do
  let vkCmdSetViewportPtr = pVkCmdSetViewport (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetViewportPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetViewport is null" Nothing Nothing
  let vkCmdSetViewport' = mkVkCmdSetViewport vkCmdSetViewportPtr
  pPViewports <- ContT $ allocaBytesAligned @Viewport ((Data.Vector.length (viewports)) * 24) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPViewports `plusPtr` (24 * (i)) :: Ptr Viewport) (e)) (viewports)
  lift $ vkCmdSetViewport' (commandBufferHandle (commandBuffer)) (firstViewport) ((fromIntegral (Data.Vector.length $ (viewports)) :: Word32)) (pPViewports)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetScissor
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()

-- No documentation found for TopLevel "vkCmdSetScissor"
cmdSetScissor :: forall io
               . (MonadIO io)
              => -- No documentation found for Nested "vkCmdSetScissor" "commandBuffer"
                 CommandBuffer
              -> -- No documentation found for Nested "vkCmdSetScissor" "firstScissor"
                 ("firstScissor" ::: Word32)
              -> -- No documentation found for Nested "vkCmdSetScissor" "pScissors"
                 ("scissors" ::: Vector Rect2D)
              -> io ()
cmdSetScissor commandBuffer firstScissor scissors = liftIO . evalContT $ do
  let vkCmdSetScissorPtr = pVkCmdSetScissor (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetScissorPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetScissor is null" Nothing Nothing
  let vkCmdSetScissor' = mkVkCmdSetScissor vkCmdSetScissorPtr
  pPScissors <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (scissors)) * 16) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (scissors)
  lift $ vkCmdSetScissor' (commandBufferHandle (commandBuffer)) (firstScissor) ((fromIntegral (Data.Vector.length $ (scissors)) :: Word32)) (pPScissors)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetLineWidth
  :: FunPtr (Ptr CommandBuffer_T -> CFloat -> IO ()) -> Ptr CommandBuffer_T -> CFloat -> IO ()

-- No documentation found for TopLevel "vkCmdSetLineWidth"
cmdSetLineWidth :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkCmdSetLineWidth" "commandBuffer"
                   CommandBuffer
                -> -- No documentation found for Nested "vkCmdSetLineWidth" "lineWidth"
                   ("lineWidth" ::: Float)
                -> io ()
cmdSetLineWidth commandBuffer lineWidth = liftIO $ do
  let vkCmdSetLineWidthPtr = pVkCmdSetLineWidth (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetLineWidthPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetLineWidth is null" Nothing Nothing
  let vkCmdSetLineWidth' = mkVkCmdSetLineWidth vkCmdSetLineWidthPtr
  vkCmdSetLineWidth' (commandBufferHandle (commandBuffer)) (CFloat (lineWidth))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBias
  :: FunPtr (Ptr CommandBuffer_T -> CFloat -> CFloat -> CFloat -> IO ()) -> Ptr CommandBuffer_T -> CFloat -> CFloat -> CFloat -> IO ()

-- No documentation found for TopLevel "vkCmdSetDepthBias"
cmdSetDepthBias :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkCmdSetDepthBias" "commandBuffer"
                   CommandBuffer
                -> -- No documentation found for Nested "vkCmdSetDepthBias" "depthBiasConstantFactor"
                   ("depthBiasConstantFactor" ::: Float)
                -> -- No documentation found for Nested "vkCmdSetDepthBias" "depthBiasClamp"
                   ("depthBiasClamp" ::: Float)
                -> -- No documentation found for Nested "vkCmdSetDepthBias" "depthBiasSlopeFactor"
                   ("depthBiasSlopeFactor" ::: Float)
                -> io ()
cmdSetDepthBias commandBuffer depthBiasConstantFactor depthBiasClamp depthBiasSlopeFactor = liftIO $ do
  let vkCmdSetDepthBiasPtr = pVkCmdSetDepthBias (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDepthBiasPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthBias is null" Nothing Nothing
  let vkCmdSetDepthBias' = mkVkCmdSetDepthBias vkCmdSetDepthBiasPtr
  vkCmdSetDepthBias' (commandBufferHandle (commandBuffer)) (CFloat (depthBiasConstantFactor)) (CFloat (depthBiasClamp)) (CFloat (depthBiasSlopeFactor))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetBlendConstants
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (FixedArray 4 CFloat) -> IO ()) -> Ptr CommandBuffer_T -> Ptr (FixedArray 4 CFloat) -> IO ()

-- No documentation found for TopLevel "vkCmdSetBlendConstants"
cmdSetBlendConstants :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkCmdSetBlendConstants" "commandBuffer"
                        CommandBuffer
                     -> -- No documentation found for Nested "vkCmdSetBlendConstants" "blendConstants"
                        ("blendConstants" ::: (Float, Float, Float, Float))
                     -> io ()
cmdSetBlendConstants commandBuffer blendConstants = liftIO . evalContT $ do
  let vkCmdSetBlendConstantsPtr = pVkCmdSetBlendConstants (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetBlendConstantsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetBlendConstants is null" Nothing Nothing
  let vkCmdSetBlendConstants' = mkVkCmdSetBlendConstants vkCmdSetBlendConstantsPtr
  pBlendConstants <- ContT $ allocaBytesAligned @(FixedArray 4 CFloat) 16 4
  let pBlendConstants' = lowerArrayPtr pBlendConstants
  lift $ case (blendConstants) of
    (e0, e1, e2, e3) -> do
      poke (pBlendConstants' :: Ptr CFloat) (CFloat (e0))
      poke (pBlendConstants' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
      poke (pBlendConstants' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
      poke (pBlendConstants' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
  lift $ vkCmdSetBlendConstants' (commandBufferHandle (commandBuffer)) (pBlendConstants)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBounds
  :: FunPtr (Ptr CommandBuffer_T -> CFloat -> CFloat -> IO ()) -> Ptr CommandBuffer_T -> CFloat -> CFloat -> IO ()

-- No documentation found for TopLevel "vkCmdSetDepthBounds"
cmdSetDepthBounds :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkCmdSetDepthBounds" "commandBuffer"
                     CommandBuffer
                  -> -- No documentation found for Nested "vkCmdSetDepthBounds" "minDepthBounds"
                     ("minDepthBounds" ::: Float)
                  -> -- No documentation found for Nested "vkCmdSetDepthBounds" "maxDepthBounds"
                     ("maxDepthBounds" ::: Float)
                  -> io ()
cmdSetDepthBounds commandBuffer minDepthBounds maxDepthBounds = liftIO $ do
  let vkCmdSetDepthBoundsPtr = pVkCmdSetDepthBounds (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetDepthBoundsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthBounds is null" Nothing Nothing
  let vkCmdSetDepthBounds' = mkVkCmdSetDepthBounds vkCmdSetDepthBoundsPtr
  vkCmdSetDepthBounds' (commandBufferHandle (commandBuffer)) (CFloat (minDepthBounds)) (CFloat (maxDepthBounds))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilCompareMask
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdSetStencilCompareMask"
cmdSetStencilCompareMask :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkCmdSetStencilCompareMask" "commandBuffer"
                            CommandBuffer
                         -> -- No documentation found for Nested "vkCmdSetStencilCompareMask" "faceMask"
                            ("faceMask" ::: StencilFaceFlags)
                         -> -- No documentation found for Nested "vkCmdSetStencilCompareMask" "compareMask"
                            ("compareMask" ::: Word32)
                         -> io ()
cmdSetStencilCompareMask commandBuffer faceMask compareMask = liftIO $ do
  let vkCmdSetStencilCompareMaskPtr = pVkCmdSetStencilCompareMask (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetStencilCompareMaskPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetStencilCompareMask is null" Nothing Nothing
  let vkCmdSetStencilCompareMask' = mkVkCmdSetStencilCompareMask vkCmdSetStencilCompareMaskPtr
  vkCmdSetStencilCompareMask' (commandBufferHandle (commandBuffer)) (faceMask) (compareMask)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilWriteMask
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdSetStencilWriteMask"
cmdSetStencilWriteMask :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkCmdSetStencilWriteMask" "commandBuffer"
                          CommandBuffer
                       -> -- No documentation found for Nested "vkCmdSetStencilWriteMask" "faceMask"
                          ("faceMask" ::: StencilFaceFlags)
                       -> -- No documentation found for Nested "vkCmdSetStencilWriteMask" "writeMask"
                          ("writeMask" ::: Word32)
                       -> io ()
cmdSetStencilWriteMask commandBuffer faceMask writeMask = liftIO $ do
  let vkCmdSetStencilWriteMaskPtr = pVkCmdSetStencilWriteMask (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetStencilWriteMaskPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetStencilWriteMask is null" Nothing Nothing
  let vkCmdSetStencilWriteMask' = mkVkCmdSetStencilWriteMask vkCmdSetStencilWriteMaskPtr
  vkCmdSetStencilWriteMask' (commandBufferHandle (commandBuffer)) (faceMask) (writeMask)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilReference
  :: FunPtr (Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> StencilFaceFlags -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdSetStencilReference"
cmdSetStencilReference :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkCmdSetStencilReference" "commandBuffer"
                          CommandBuffer
                       -> -- No documentation found for Nested "vkCmdSetStencilReference" "faceMask"
                          ("faceMask" ::: StencilFaceFlags)
                       -> -- No documentation found for Nested "vkCmdSetStencilReference" "reference"
                          ("reference" ::: Word32)
                       -> io ()
cmdSetStencilReference commandBuffer faceMask reference = liftIO $ do
  let vkCmdSetStencilReferencePtr = pVkCmdSetStencilReference (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetStencilReferencePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetStencilReference is null" Nothing Nothing
  let vkCmdSetStencilReference' = mkVkCmdSetStencilReference vkCmdSetStencilReferencePtr
  vkCmdSetStencilReference' (commandBufferHandle (commandBuffer)) (faceMask) (reference)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindDescriptorSets
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr DescriptorSet -> Word32 -> Ptr Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr DescriptorSet -> Word32 -> Ptr Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdBindDescriptorSets"
cmdBindDescriptorSets :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vkCmdBindDescriptorSets" "commandBuffer"
                         CommandBuffer
                      -> -- No documentation found for Nested "vkCmdBindDescriptorSets" "pipelineBindPoint"
                         PipelineBindPoint
                      -> -- No documentation found for Nested "vkCmdBindDescriptorSets" "layout"
                         PipelineLayout
                      -> -- No documentation found for Nested "vkCmdBindDescriptorSets" "firstSet"
                         ("firstSet" ::: Word32)
                      -> -- No documentation found for Nested "vkCmdBindDescriptorSets" "pDescriptorSets"
                         ("descriptorSets" ::: Vector DescriptorSet)
                      -> -- No documentation found for Nested "vkCmdBindDescriptorSets" "pDynamicOffsets"
                         ("dynamicOffsets" ::: Vector Word32)
                      -> io ()
cmdBindDescriptorSets commandBuffer pipelineBindPoint layout firstSet descriptorSets dynamicOffsets = liftIO . evalContT $ do
  let vkCmdBindDescriptorSetsPtr = pVkCmdBindDescriptorSets (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBindDescriptorSetsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindDescriptorSets is null" Nothing Nothing
  let vkCmdBindDescriptorSets' = mkVkCmdBindDescriptorSets vkCmdBindDescriptorSetsPtr
  pPDescriptorSets <- ContT $ allocaBytesAligned @DescriptorSet ((Data.Vector.length (descriptorSets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorSets `plusPtr` (8 * (i)) :: Ptr DescriptorSet) (e)) (descriptorSets)
  pPDynamicOffsets <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (dynamicOffsets)) * 4) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPDynamicOffsets `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (dynamicOffsets)
  lift $ vkCmdBindDescriptorSets' (commandBufferHandle (commandBuffer)) (pipelineBindPoint) (layout) (firstSet) ((fromIntegral (Data.Vector.length $ (descriptorSets)) :: Word32)) (pPDescriptorSets) ((fromIntegral (Data.Vector.length $ (dynamicOffsets)) :: Word32)) (pPDynamicOffsets)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindIndexBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> IndexType -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> IndexType -> IO ()

-- No documentation found for TopLevel "vkCmdBindIndexBuffer"
cmdBindIndexBuffer :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkCmdBindIndexBuffer" "commandBuffer"
                      CommandBuffer
                   -> -- No documentation found for Nested "vkCmdBindIndexBuffer" "buffer"
                      Buffer
                   -> -- No documentation found for Nested "vkCmdBindIndexBuffer" "offset"
                      ("offset" ::: DeviceSize)
                   -> -- No documentation found for Nested "vkCmdBindIndexBuffer" "indexType"
                      IndexType
                   -> io ()
cmdBindIndexBuffer commandBuffer buffer offset indexType = liftIO $ do
  let vkCmdBindIndexBufferPtr = pVkCmdBindIndexBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBindIndexBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindIndexBuffer is null" Nothing Nothing
  let vkCmdBindIndexBuffer' = mkVkCmdBindIndexBuffer vkCmdBindIndexBufferPtr
  vkCmdBindIndexBuffer' (commandBufferHandle (commandBuffer)) (buffer) (offset) (indexType)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindVertexBuffers
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Buffer -> Ptr DeviceSize -> IO ()

-- No documentation found for TopLevel "vkCmdBindVertexBuffers"
cmdBindVertexBuffers :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkCmdBindVertexBuffers" "commandBuffer"
                        CommandBuffer
                     -> -- No documentation found for Nested "vkCmdBindVertexBuffers" "firstBinding"
                        ("firstBinding" ::: Word32)
                     -> -- No documentation found for Nested "vkCmdBindVertexBuffers" "pBuffers"
                        ("buffers" ::: Vector Buffer)
                     -> -- No documentation found for Nested "vkCmdBindVertexBuffers" "pOffsets"
                        ("offsets" ::: Vector DeviceSize)
                     -> io ()
cmdBindVertexBuffers commandBuffer firstBinding buffers offsets = liftIO . evalContT $ do
  let vkCmdBindVertexBuffersPtr = pVkCmdBindVertexBuffers (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBindVertexBuffersPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindVertexBuffers is null" Nothing Nothing
  let vkCmdBindVertexBuffers' = mkVkCmdBindVertexBuffers vkCmdBindVertexBuffersPtr
  let pBuffersLength = Data.Vector.length $ (buffers)
  lift $ unless ((Data.Vector.length $ (offsets)) == pBuffersLength) $
    throwIO $ IOError Nothing InvalidArgument "" "pOffsets and pBuffers must have the same length" Nothing Nothing
  pPBuffers <- ContT $ allocaBytesAligned @Buffer ((Data.Vector.length (buffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBuffers `plusPtr` (8 * (i)) :: Ptr Buffer) (e)) (buffers)
  pPOffsets <- ContT $ allocaBytesAligned @DeviceSize ((Data.Vector.length (offsets)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPOffsets `plusPtr` (8 * (i)) :: Ptr DeviceSize) (e)) (offsets)
  lift $ vkCmdBindVertexBuffers' (commandBufferHandle (commandBuffer)) (firstBinding) ((fromIntegral pBuffersLength :: Word32)) (pPBuffers) (pPOffsets)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDraw
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdDraw"
cmdDraw :: forall io
         . (MonadIO io)
        => -- No documentation found for Nested "vkCmdDraw" "commandBuffer"
           CommandBuffer
        -> -- No documentation found for Nested "vkCmdDraw" "vertexCount"
           ("vertexCount" ::: Word32)
        -> -- No documentation found for Nested "vkCmdDraw" "instanceCount"
           ("instanceCount" ::: Word32)
        -> -- No documentation found for Nested "vkCmdDraw" "firstVertex"
           ("firstVertex" ::: Word32)
        -> -- No documentation found for Nested "vkCmdDraw" "firstInstance"
           ("firstInstance" ::: Word32)
        -> io ()
cmdDraw commandBuffer vertexCount instanceCount firstVertex firstInstance = liftIO $ do
  let vkCmdDrawPtr = pVkCmdDraw (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDraw is null" Nothing Nothing
  let vkCmdDraw' = mkVkCmdDraw vkCmdDrawPtr
  vkCmdDraw' (commandBufferHandle (commandBuffer)) (vertexCount) (instanceCount) (firstVertex) (firstInstance)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndexed
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> Int32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdDrawIndexed"
cmdDrawIndexed :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vkCmdDrawIndexed" "commandBuffer"
                  CommandBuffer
               -> -- No documentation found for Nested "vkCmdDrawIndexed" "indexCount"
                  ("indexCount" ::: Word32)
               -> -- No documentation found for Nested "vkCmdDrawIndexed" "instanceCount"
                  ("instanceCount" ::: Word32)
               -> -- No documentation found for Nested "vkCmdDrawIndexed" "firstIndex"
                  ("firstIndex" ::: Word32)
               -> -- No documentation found for Nested "vkCmdDrawIndexed" "vertexOffset"
                  ("vertexOffset" ::: Int32)
               -> -- No documentation found for Nested "vkCmdDrawIndexed" "firstInstance"
                  ("firstInstance" ::: Word32)
               -> io ()
cmdDrawIndexed commandBuffer indexCount instanceCount firstIndex vertexOffset firstInstance = liftIO $ do
  let vkCmdDrawIndexedPtr = pVkCmdDrawIndexed (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawIndexedPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawIndexed is null" Nothing Nothing
  let vkCmdDrawIndexed' = mkVkCmdDrawIndexed vkCmdDrawIndexedPtr
  vkCmdDrawIndexed' (commandBufferHandle (commandBuffer)) (indexCount) (instanceCount) (firstIndex) (vertexOffset) (firstInstance)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirect
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdDrawIndirect"
cmdDrawIndirect :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkCmdDrawIndirect" "commandBuffer"
                   CommandBuffer
                -> -- No documentation found for Nested "vkCmdDrawIndirect" "buffer"
                   Buffer
                -> -- No documentation found for Nested "vkCmdDrawIndirect" "offset"
                   ("offset" ::: DeviceSize)
                -> -- No documentation found for Nested "vkCmdDrawIndirect" "drawCount"
                   ("drawCount" ::: Word32)
                -> -- No documentation found for Nested "vkCmdDrawIndirect" "stride"
                   ("stride" ::: Word32)
                -> io ()
cmdDrawIndirect commandBuffer buffer offset drawCount stride = liftIO $ do
  let vkCmdDrawIndirectPtr = pVkCmdDrawIndirect (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawIndirectPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawIndirect is null" Nothing Nothing
  let vkCmdDrawIndirect' = mkVkCmdDrawIndirect vkCmdDrawIndirectPtr
  vkCmdDrawIndirect' (commandBufferHandle (commandBuffer)) (buffer) (offset) (drawCount) (stride)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndexedIndirect
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdDrawIndexedIndirect"
cmdDrawIndexedIndirect :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkCmdDrawIndexedIndirect" "commandBuffer"
                          CommandBuffer
                       -> -- No documentation found for Nested "vkCmdDrawIndexedIndirect" "buffer"
                          Buffer
                       -> -- No documentation found for Nested "vkCmdDrawIndexedIndirect" "offset"
                          ("offset" ::: DeviceSize)
                       -> -- No documentation found for Nested "vkCmdDrawIndexedIndirect" "drawCount"
                          ("drawCount" ::: Word32)
                       -> -- No documentation found for Nested "vkCmdDrawIndexedIndirect" "stride"
                          ("stride" ::: Word32)
                       -> io ()
cmdDrawIndexedIndirect commandBuffer buffer offset drawCount stride = liftIO $ do
  let vkCmdDrawIndexedIndirectPtr = pVkCmdDrawIndexedIndirect (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawIndexedIndirectPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawIndexedIndirect is null" Nothing Nothing
  let vkCmdDrawIndexedIndirect' = mkVkCmdDrawIndexedIndirect vkCmdDrawIndexedIndirectPtr
  vkCmdDrawIndexedIndirect' (commandBufferHandle (commandBuffer)) (buffer) (offset) (drawCount) (stride)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatch
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdDispatch"
cmdDispatch :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "vkCmdDispatch" "commandBuffer"
               CommandBuffer
            -> -- No documentation found for Nested "vkCmdDispatch" "groupCountX"
               ("groupCountX" ::: Word32)
            -> -- No documentation found for Nested "vkCmdDispatch" "groupCountY"
               ("groupCountY" ::: Word32)
            -> -- No documentation found for Nested "vkCmdDispatch" "groupCountZ"
               ("groupCountZ" ::: Word32)
            -> io ()
cmdDispatch commandBuffer groupCountX groupCountY groupCountZ = liftIO $ do
  let vkCmdDispatchPtr = pVkCmdDispatch (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDispatchPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDispatch is null" Nothing Nothing
  let vkCmdDispatch' = mkVkCmdDispatch vkCmdDispatchPtr
  vkCmdDispatch' (commandBufferHandle (commandBuffer)) (groupCountX) (groupCountY) (groupCountZ)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchIndirect
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> IO ()

-- No documentation found for TopLevel "vkCmdDispatchIndirect"
cmdDispatchIndirect :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkCmdDispatchIndirect" "commandBuffer"
                       CommandBuffer
                    -> -- No documentation found for Nested "vkCmdDispatchIndirect" "buffer"
                       Buffer
                    -> -- No documentation found for Nested "vkCmdDispatchIndirect" "offset"
                       ("offset" ::: DeviceSize)
                    -> io ()
cmdDispatchIndirect commandBuffer buffer offset = liftIO $ do
  let vkCmdDispatchIndirectPtr = pVkCmdDispatchIndirect (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDispatchIndirectPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDispatchIndirect is null" Nothing Nothing
  let vkCmdDispatchIndirect' = mkVkCmdDispatchIndirect vkCmdDispatchIndirectPtr
  vkCmdDispatchIndirect' (commandBufferHandle (commandBuffer)) (buffer) (offset)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> Buffer -> Word32 -> Ptr BufferCopy -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> Buffer -> Word32 -> Ptr BufferCopy -> IO ()

-- No documentation found for TopLevel "vkCmdCopyBuffer"
cmdCopyBuffer :: forall io
               . (MonadIO io)
              => -- No documentation found for Nested "vkCmdCopyBuffer" "commandBuffer"
                 CommandBuffer
              -> -- No documentation found for Nested "vkCmdCopyBuffer" "srcBuffer"
                 ("srcBuffer" ::: Buffer)
              -> -- No documentation found for Nested "vkCmdCopyBuffer" "dstBuffer"
                 ("dstBuffer" ::: Buffer)
              -> -- No documentation found for Nested "vkCmdCopyBuffer" "pRegions"
                 ("regions" ::: Vector BufferCopy)
              -> io ()
cmdCopyBuffer commandBuffer srcBuffer dstBuffer regions = liftIO . evalContT $ do
  let vkCmdCopyBufferPtr = pVkCmdCopyBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyBuffer is null" Nothing Nothing
  let vkCmdCopyBuffer' = mkVkCmdCopyBuffer vkCmdCopyBufferPtr
  pPRegions <- ContT $ allocaBytesAligned @BufferCopy ((Data.Vector.length (regions)) * 24) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions `plusPtr` (24 * (i)) :: Ptr BufferCopy) (e)) (regions)
  lift $ vkCmdCopyBuffer' (commandBufferHandle (commandBuffer)) (srcBuffer) (dstBuffer) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageCopy -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageCopy -> IO ()

-- No documentation found for TopLevel "vkCmdCopyImage"
cmdCopyImage :: forall io
              . (MonadIO io)
             => -- No documentation found for Nested "vkCmdCopyImage" "commandBuffer"
                CommandBuffer
             -> -- No documentation found for Nested "vkCmdCopyImage" "srcImage"
                ("srcImage" ::: Image)
             -> -- No documentation found for Nested "vkCmdCopyImage" "srcImageLayout"
                ("srcImageLayout" ::: ImageLayout)
             -> -- No documentation found for Nested "vkCmdCopyImage" "dstImage"
                ("dstImage" ::: Image)
             -> -- No documentation found for Nested "vkCmdCopyImage" "dstImageLayout"
                ("dstImageLayout" ::: ImageLayout)
             -> -- No documentation found for Nested "vkCmdCopyImage" "pRegions"
                ("regions" ::: Vector ImageCopy)
             -> io ()
cmdCopyImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout regions = liftIO . evalContT $ do
  let vkCmdCopyImagePtr = pVkCmdCopyImage (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyImage is null" Nothing Nothing
  let vkCmdCopyImage' = mkVkCmdCopyImage vkCmdCopyImagePtr
  pPRegions <- ContT $ allocaBytesAligned @ImageCopy ((Data.Vector.length (regions)) * 68) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions `plusPtr` (68 * (i)) :: Ptr ImageCopy) (e)) (regions)
  lift $ vkCmdCopyImage' (commandBufferHandle (commandBuffer)) (srcImage) (srcImageLayout) (dstImage) (dstImageLayout) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBlitImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageBlit -> Filter -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageBlit -> Filter -> IO ()

-- No documentation found for TopLevel "vkCmdBlitImage"
cmdBlitImage :: forall io
              . (MonadIO io)
             => -- No documentation found for Nested "vkCmdBlitImage" "commandBuffer"
                CommandBuffer
             -> -- No documentation found for Nested "vkCmdBlitImage" "srcImage"
                ("srcImage" ::: Image)
             -> -- No documentation found for Nested "vkCmdBlitImage" "srcImageLayout"
                ("srcImageLayout" ::: ImageLayout)
             -> -- No documentation found for Nested "vkCmdBlitImage" "dstImage"
                ("dstImage" ::: Image)
             -> -- No documentation found for Nested "vkCmdBlitImage" "dstImageLayout"
                ("dstImageLayout" ::: ImageLayout)
             -> -- No documentation found for Nested "vkCmdBlitImage" "pRegions"
                ("regions" ::: Vector ImageBlit)
             -> -- No documentation found for Nested "vkCmdBlitImage" "filter"
                Filter
             -> io ()
cmdBlitImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout regions filter' = liftIO . evalContT $ do
  let vkCmdBlitImagePtr = pVkCmdBlitImage (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBlitImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBlitImage is null" Nothing Nothing
  let vkCmdBlitImage' = mkVkCmdBlitImage vkCmdBlitImagePtr
  pPRegions <- ContT $ allocaBytesAligned @ImageBlit ((Data.Vector.length (regions)) * 80) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions `plusPtr` (80 * (i)) :: Ptr ImageBlit) (e)) (regions)
  lift $ vkCmdBlitImage' (commandBufferHandle (commandBuffer)) (srcImage) (srcImageLayout) (dstImage) (dstImageLayout) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions) (filter')
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBufferToImage
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> Image -> ImageLayout -> Word32 -> Ptr BufferImageCopy -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> Image -> ImageLayout -> Word32 -> Ptr BufferImageCopy -> IO ()

-- No documentation found for TopLevel "vkCmdCopyBufferToImage"
cmdCopyBufferToImage :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkCmdCopyBufferToImage" "commandBuffer"
                        CommandBuffer
                     -> -- No documentation found for Nested "vkCmdCopyBufferToImage" "srcBuffer"
                        ("srcBuffer" ::: Buffer)
                     -> -- No documentation found for Nested "vkCmdCopyBufferToImage" "dstImage"
                        ("dstImage" ::: Image)
                     -> -- No documentation found for Nested "vkCmdCopyBufferToImage" "dstImageLayout"
                        ("dstImageLayout" ::: ImageLayout)
                     -> -- No documentation found for Nested "vkCmdCopyBufferToImage" "pRegions"
                        ("regions" ::: Vector BufferImageCopy)
                     -> io ()
cmdCopyBufferToImage commandBuffer srcBuffer dstImage dstImageLayout regions = liftIO . evalContT $ do
  let vkCmdCopyBufferToImagePtr = pVkCmdCopyBufferToImage (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyBufferToImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyBufferToImage is null" Nothing Nothing
  let vkCmdCopyBufferToImage' = mkVkCmdCopyBufferToImage vkCmdCopyBufferToImagePtr
  pPRegions <- ContT $ allocaBytesAligned @BufferImageCopy ((Data.Vector.length (regions)) * 56) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions `plusPtr` (56 * (i)) :: Ptr BufferImageCopy) (e)) (regions)
  lift $ vkCmdCopyBufferToImage' (commandBufferHandle (commandBuffer)) (srcBuffer) (dstImage) (dstImageLayout) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImageToBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Buffer -> Word32 -> Ptr BufferImageCopy -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Buffer -> Word32 -> Ptr BufferImageCopy -> IO ()

-- No documentation found for TopLevel "vkCmdCopyImageToBuffer"
cmdCopyImageToBuffer :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkCmdCopyImageToBuffer" "commandBuffer"
                        CommandBuffer
                     -> -- No documentation found for Nested "vkCmdCopyImageToBuffer" "srcImage"
                        ("srcImage" ::: Image)
                     -> -- No documentation found for Nested "vkCmdCopyImageToBuffer" "srcImageLayout"
                        ("srcImageLayout" ::: ImageLayout)
                     -> -- No documentation found for Nested "vkCmdCopyImageToBuffer" "dstBuffer"
                        ("dstBuffer" ::: Buffer)
                     -> -- No documentation found for Nested "vkCmdCopyImageToBuffer" "pRegions"
                        ("regions" ::: Vector BufferImageCopy)
                     -> io ()
cmdCopyImageToBuffer commandBuffer srcImage srcImageLayout dstBuffer regions = liftIO . evalContT $ do
  let vkCmdCopyImageToBufferPtr = pVkCmdCopyImageToBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdCopyImageToBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyImageToBuffer is null" Nothing Nothing
  let vkCmdCopyImageToBuffer' = mkVkCmdCopyImageToBuffer vkCmdCopyImageToBufferPtr
  pPRegions <- ContT $ allocaBytesAligned @BufferImageCopy ((Data.Vector.length (regions)) * 56) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions `plusPtr` (56 * (i)) :: Ptr BufferImageCopy) (e)) (regions)
  lift $ vkCmdCopyImageToBuffer' (commandBufferHandle (commandBuffer)) (srcImage) (srcImageLayout) (dstBuffer) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdUpdateBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> Ptr () -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> Ptr () -> IO ()

-- No documentation found for TopLevel "vkCmdUpdateBuffer"
cmdUpdateBuffer :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkCmdUpdateBuffer" "commandBuffer"
                   CommandBuffer
                -> -- No documentation found for Nested "vkCmdUpdateBuffer" "dstBuffer"
                   ("dstBuffer" ::: Buffer)
                -> -- No documentation found for Nested "vkCmdUpdateBuffer" "dstOffset"
                   ("dstOffset" ::: DeviceSize)
                -> -- No documentation found for Nested "vkCmdUpdateBuffer" "dataSize"
                   ("dataSize" ::: DeviceSize)
                -> -- No documentation found for Nested "vkCmdUpdateBuffer" "pData"
                   ("data" ::: Ptr ())
                -> io ()
cmdUpdateBuffer commandBuffer dstBuffer dstOffset dataSize data' = liftIO $ do
  let vkCmdUpdateBufferPtr = pVkCmdUpdateBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdUpdateBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdUpdateBuffer is null" Nothing Nothing
  let vkCmdUpdateBuffer' = mkVkCmdUpdateBuffer vkCmdUpdateBufferPtr
  vkCmdUpdateBuffer' (commandBufferHandle (commandBuffer)) (dstBuffer) (dstOffset) (dataSize) (data')
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdFillBuffer
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdFillBuffer"
cmdFillBuffer :: forall io
               . (MonadIO io)
              => -- No documentation found for Nested "vkCmdFillBuffer" "commandBuffer"
                 CommandBuffer
              -> -- No documentation found for Nested "vkCmdFillBuffer" "dstBuffer"
                 ("dstBuffer" ::: Buffer)
              -> -- No documentation found for Nested "vkCmdFillBuffer" "dstOffset"
                 ("dstOffset" ::: DeviceSize)
              -> -- No documentation found for Nested "vkCmdFillBuffer" "size"
                 DeviceSize
              -> -- No documentation found for Nested "vkCmdFillBuffer" "data"
                 ("data" ::: Word32)
              -> io ()
cmdFillBuffer commandBuffer dstBuffer dstOffset size data' = liftIO $ do
  let vkCmdFillBufferPtr = pVkCmdFillBuffer (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdFillBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdFillBuffer is null" Nothing Nothing
  let vkCmdFillBuffer' = mkVkCmdFillBuffer vkCmdFillBufferPtr
  vkCmdFillBuffer' (commandBufferHandle (commandBuffer)) (dstBuffer) (dstOffset) (size) (data')
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdClearColorImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Ptr ClearColorValue -> Word32 -> Ptr ImageSubresourceRange -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Ptr ClearColorValue -> Word32 -> Ptr ImageSubresourceRange -> IO ()

-- No documentation found for TopLevel "vkCmdClearColorImage"
cmdClearColorImage :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkCmdClearColorImage" "commandBuffer"
                      CommandBuffer
                   -> -- No documentation found for Nested "vkCmdClearColorImage" "image"
                      Image
                   -> -- No documentation found for Nested "vkCmdClearColorImage" "imageLayout"
                      ImageLayout
                   -> -- No documentation found for Nested "vkCmdClearColorImage" "pColor"
                      ClearColorValue
                   -> -- No documentation found for Nested "vkCmdClearColorImage" "pRanges"
                      ("ranges" ::: Vector ImageSubresourceRange)
                   -> io ()
cmdClearColorImage commandBuffer image imageLayout color ranges = liftIO . evalContT $ do
  let vkCmdClearColorImagePtr = pVkCmdClearColorImage (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdClearColorImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdClearColorImage is null" Nothing Nothing
  let vkCmdClearColorImage' = mkVkCmdClearColorImage vkCmdClearColorImagePtr
  pColor <- ContT $ withCStruct (color)
  pPRanges <- ContT $ allocaBytesAligned @ImageSubresourceRange ((Data.Vector.length (ranges)) * 20) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRanges `plusPtr` (20 * (i)) :: Ptr ImageSubresourceRange) (e)) (ranges)
  lift $ vkCmdClearColorImage' (commandBufferHandle (commandBuffer)) (image) (imageLayout) pColor ((fromIntegral (Data.Vector.length $ (ranges)) :: Word32)) (pPRanges)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdClearDepthStencilImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Ptr ClearDepthStencilValue -> Word32 -> Ptr ImageSubresourceRange -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Ptr ClearDepthStencilValue -> Word32 -> Ptr ImageSubresourceRange -> IO ()

-- No documentation found for TopLevel "vkCmdClearDepthStencilImage"
cmdClearDepthStencilImage :: forall io
                           . (MonadIO io)
                          => -- No documentation found for Nested "vkCmdClearDepthStencilImage" "commandBuffer"
                             CommandBuffer
                          -> -- No documentation found for Nested "vkCmdClearDepthStencilImage" "image"
                             Image
                          -> -- No documentation found for Nested "vkCmdClearDepthStencilImage" "imageLayout"
                             ImageLayout
                          -> -- No documentation found for Nested "vkCmdClearDepthStencilImage" "pDepthStencil"
                             ClearDepthStencilValue
                          -> -- No documentation found for Nested "vkCmdClearDepthStencilImage" "pRanges"
                             ("ranges" ::: Vector ImageSubresourceRange)
                          -> io ()
cmdClearDepthStencilImage commandBuffer image imageLayout depthStencil ranges = liftIO . evalContT $ do
  let vkCmdClearDepthStencilImagePtr = pVkCmdClearDepthStencilImage (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdClearDepthStencilImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdClearDepthStencilImage is null" Nothing Nothing
  let vkCmdClearDepthStencilImage' = mkVkCmdClearDepthStencilImage vkCmdClearDepthStencilImagePtr
  pDepthStencil <- ContT $ withCStruct (depthStencil)
  pPRanges <- ContT $ allocaBytesAligned @ImageSubresourceRange ((Data.Vector.length (ranges)) * 20) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRanges `plusPtr` (20 * (i)) :: Ptr ImageSubresourceRange) (e)) (ranges)
  lift $ vkCmdClearDepthStencilImage' (commandBufferHandle (commandBuffer)) (image) (imageLayout) pDepthStencil ((fromIntegral (Data.Vector.length $ (ranges)) :: Word32)) (pPRanges)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdClearAttachments
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr ClearAttachment -> Word32 -> Ptr ClearRect -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr ClearAttachment -> Word32 -> Ptr ClearRect -> IO ()

-- No documentation found for TopLevel "vkCmdClearAttachments"
cmdClearAttachments :: forall io
                     . (MonadIO io)
                    => -- No documentation found for Nested "vkCmdClearAttachments" "commandBuffer"
                       CommandBuffer
                    -> -- No documentation found for Nested "vkCmdClearAttachments" "pAttachments"
                       ("attachments" ::: Vector ClearAttachment)
                    -> -- No documentation found for Nested "vkCmdClearAttachments" "pRects"
                       ("rects" ::: Vector ClearRect)
                    -> io ()
cmdClearAttachments commandBuffer attachments rects = liftIO . evalContT $ do
  let vkCmdClearAttachmentsPtr = pVkCmdClearAttachments (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdClearAttachmentsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdClearAttachments is null" Nothing Nothing
  let vkCmdClearAttachments' = mkVkCmdClearAttachments vkCmdClearAttachmentsPtr
  pPAttachments <- ContT $ allocaBytesAligned @ClearAttachment ((Data.Vector.length (attachments)) * 24) 4
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachments `plusPtr` (24 * (i)) :: Ptr ClearAttachment) (e) . ($ ())) (attachments)
  pPRects <- ContT $ allocaBytesAligned @ClearRect ((Data.Vector.length (rects)) * 24) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRects `plusPtr` (24 * (i)) :: Ptr ClearRect) (e)) (rects)
  lift $ vkCmdClearAttachments' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (attachments)) :: Word32)) (pPAttachments) ((fromIntegral (Data.Vector.length $ (rects)) :: Word32)) (pPRects)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResolveImage
  :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageResolve -> IO ()) -> Ptr CommandBuffer_T -> Image -> ImageLayout -> Image -> ImageLayout -> Word32 -> Ptr ImageResolve -> IO ()

-- No documentation found for TopLevel "vkCmdResolveImage"
cmdResolveImage :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkCmdResolveImage" "commandBuffer"
                   CommandBuffer
                -> -- No documentation found for Nested "vkCmdResolveImage" "srcImage"
                   ("srcImage" ::: Image)
                -> -- No documentation found for Nested "vkCmdResolveImage" "srcImageLayout"
                   ("srcImageLayout" ::: ImageLayout)
                -> -- No documentation found for Nested "vkCmdResolveImage" "dstImage"
                   ("dstImage" ::: Image)
                -> -- No documentation found for Nested "vkCmdResolveImage" "dstImageLayout"
                   ("dstImageLayout" ::: ImageLayout)
                -> -- No documentation found for Nested "vkCmdResolveImage" "pRegions"
                   ("regions" ::: Vector ImageResolve)
                -> io ()
cmdResolveImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout regions = liftIO . evalContT $ do
  let vkCmdResolveImagePtr = pVkCmdResolveImage (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdResolveImagePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdResolveImage is null" Nothing Nothing
  let vkCmdResolveImage' = mkVkCmdResolveImage vkCmdResolveImagePtr
  pPRegions <- ContT $ allocaBytesAligned @ImageResolve ((Data.Vector.length (regions)) * 68) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPRegions `plusPtr` (68 * (i)) :: Ptr ImageResolve) (e)) (regions)
  lift $ vkCmdResolveImage' (commandBufferHandle (commandBuffer)) (srcImage) (srcImageLayout) (dstImage) (dstImageLayout) ((fromIntegral (Data.Vector.length $ (regions)) :: Word32)) (pPRegions)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetEvent
  :: FunPtr (Ptr CommandBuffer_T -> Event -> PipelineStageFlags -> IO ()) -> Ptr CommandBuffer_T -> Event -> PipelineStageFlags -> IO ()

-- No documentation found for TopLevel "vkCmdSetEvent"
cmdSetEvent :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "vkCmdSetEvent" "commandBuffer"
               CommandBuffer
            -> -- No documentation found for Nested "vkCmdSetEvent" "event"
               Event
            -> -- No documentation found for Nested "vkCmdSetEvent" "stageMask"
               ("stageMask" ::: PipelineStageFlags)
            -> io ()
cmdSetEvent commandBuffer event stageMask = liftIO $ do
  let vkCmdSetEventPtr = pVkCmdSetEvent (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdSetEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetEvent is null" Nothing Nothing
  let vkCmdSetEvent' = mkVkCmdSetEvent vkCmdSetEventPtr
  vkCmdSetEvent' (commandBufferHandle (commandBuffer)) (event) (stageMask)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResetEvent
  :: FunPtr (Ptr CommandBuffer_T -> Event -> PipelineStageFlags -> IO ()) -> Ptr CommandBuffer_T -> Event -> PipelineStageFlags -> IO ()

-- No documentation found for TopLevel "vkCmdResetEvent"
cmdResetEvent :: forall io
               . (MonadIO io)
              => -- No documentation found for Nested "vkCmdResetEvent" "commandBuffer"
                 CommandBuffer
              -> -- No documentation found for Nested "vkCmdResetEvent" "event"
                 Event
              -> -- No documentation found for Nested "vkCmdResetEvent" "stageMask"
                 ("stageMask" ::: PipelineStageFlags)
              -> io ()
cmdResetEvent commandBuffer event stageMask = liftIO $ do
  let vkCmdResetEventPtr = pVkCmdResetEvent (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdResetEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdResetEvent is null" Nothing Nothing
  let vkCmdResetEvent' = mkVkCmdResetEvent vkCmdResetEventPtr
  vkCmdResetEvent' (commandBufferHandle (commandBuffer)) (event) (stageMask)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWaitEventsUnsafe
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()

foreign import ccall
  "dynamic" mkVkCmdWaitEventsSafe
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()

-- | cmdWaitEvents with selectable safeness
cmdWaitEventsSafeOrUnsafe :: forall io
                           . (MonadIO io)
                          => -- No documentation found for TopLevel ""
                             (FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr Event -> PipelineStageFlags -> PipelineStageFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ())
                          -> -- No documentation found for Nested "vkCmdWaitEvents" "commandBuffer"
                             CommandBuffer
                          -> -- No documentation found for Nested "vkCmdWaitEvents" "pEvents"
                             ("events" ::: Vector Event)
                          -> -- No documentation found for Nested "vkCmdWaitEvents" "srcStageMask"
                             ("srcStageMask" ::: PipelineStageFlags)
                          -> -- No documentation found for Nested "vkCmdWaitEvents" "dstStageMask"
                             ("dstStageMask" ::: PipelineStageFlags)
                          -> -- No documentation found for Nested "vkCmdWaitEvents" "pMemoryBarriers"
                             ("memoryBarriers" ::: Vector MemoryBarrier)
                          -> -- No documentation found for Nested "vkCmdWaitEvents" "pBufferMemoryBarriers"
                             ("bufferMemoryBarriers" ::: Vector BufferMemoryBarrier)
                          -> -- No documentation found for Nested "vkCmdWaitEvents" "pImageMemoryBarriers"
                             ("imageMemoryBarriers" ::: Vector (SomeStruct ImageMemoryBarrier))
                          -> io ()
cmdWaitEventsSafeOrUnsafe mkVkCmdWaitEvents commandBuffer events srcStageMask dstStageMask memoryBarriers bufferMemoryBarriers imageMemoryBarriers = liftIO . evalContT $ do
  let vkCmdWaitEventsPtr = pVkCmdWaitEvents (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdWaitEventsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWaitEvents is null" Nothing Nothing
  let vkCmdWaitEvents' = mkVkCmdWaitEvents vkCmdWaitEventsPtr
  pPEvents <- ContT $ allocaBytesAligned @Event ((Data.Vector.length (events)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPEvents `plusPtr` (8 * (i)) :: Ptr Event) (e)) (events)
  pPMemoryBarriers <- ContT $ allocaBytesAligned @MemoryBarrier ((Data.Vector.length (memoryBarriers)) * 24) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMemoryBarriers `plusPtr` (24 * (i)) :: Ptr MemoryBarrier) (e)) (memoryBarriers)
  pPBufferMemoryBarriers <- ContT $ allocaBytesAligned @BufferMemoryBarrier ((Data.Vector.length (bufferMemoryBarriers)) * 56) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBufferMemoryBarriers `plusPtr` (56 * (i)) :: Ptr BufferMemoryBarrier) (e)) (bufferMemoryBarriers)
  pPImageMemoryBarriers <- ContT $ allocaBytesAligned @(ImageMemoryBarrier _) ((Data.Vector.length (imageMemoryBarriers)) * 72) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPImageMemoryBarriers `plusPtr` (72 * (i)) :: Ptr (ImageMemoryBarrier _))) (e) . ($ ())) (imageMemoryBarriers)
  lift $ vkCmdWaitEvents' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (events)) :: Word32)) (pPEvents) (srcStageMask) (dstStageMask) ((fromIntegral (Data.Vector.length $ (memoryBarriers)) :: Word32)) (pPMemoryBarriers) ((fromIntegral (Data.Vector.length $ (bufferMemoryBarriers)) :: Word32)) (pPBufferMemoryBarriers) ((fromIntegral (Data.Vector.length $ (imageMemoryBarriers)) :: Word32)) (forgetExtensions (pPImageMemoryBarriers))
  pure $ ()

-- No documentation found for TopLevel "vkCmdWaitEvents"
cmdWaitEvents :: forall io
               . (MonadIO io)
              => -- No documentation found for Nested "vkCmdWaitEvents" "commandBuffer"
                 CommandBuffer
              -> -- No documentation found for Nested "vkCmdWaitEvents" "pEvents"
                 ("events" ::: Vector Event)
              -> -- No documentation found for Nested "vkCmdWaitEvents" "srcStageMask"
                 ("srcStageMask" ::: PipelineStageFlags)
              -> -- No documentation found for Nested "vkCmdWaitEvents" "dstStageMask"
                 ("dstStageMask" ::: PipelineStageFlags)
              -> -- No documentation found for Nested "vkCmdWaitEvents" "pMemoryBarriers"
                 ("memoryBarriers" ::: Vector MemoryBarrier)
              -> -- No documentation found for Nested "vkCmdWaitEvents" "pBufferMemoryBarriers"
                 ("bufferMemoryBarriers" ::: Vector BufferMemoryBarrier)
              -> -- No documentation found for Nested "vkCmdWaitEvents" "pImageMemoryBarriers"
                 ("imageMemoryBarriers" ::: Vector (SomeStruct ImageMemoryBarrier))
              -> io ()
cmdWaitEvents = cmdWaitEventsSafeOrUnsafe mkVkCmdWaitEventsUnsafe

-- | A variant of 'cmdWaitEvents' which makes a *safe* FFI call
cmdWaitEventsSafe :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkCmdWaitEvents" "commandBuffer"
                     CommandBuffer
                  -> -- No documentation found for Nested "vkCmdWaitEvents" "pEvents"
                     ("events" ::: Vector Event)
                  -> -- No documentation found for Nested "vkCmdWaitEvents" "srcStageMask"
                     ("srcStageMask" ::: PipelineStageFlags)
                  -> -- No documentation found for Nested "vkCmdWaitEvents" "dstStageMask"
                     ("dstStageMask" ::: PipelineStageFlags)
                  -> -- No documentation found for Nested "vkCmdWaitEvents" "pMemoryBarriers"
                     ("memoryBarriers" ::: Vector MemoryBarrier)
                  -> -- No documentation found for Nested "vkCmdWaitEvents" "pBufferMemoryBarriers"
                     ("bufferMemoryBarriers" ::: Vector BufferMemoryBarrier)
                  -> -- No documentation found for Nested "vkCmdWaitEvents" "pImageMemoryBarriers"
                     ("imageMemoryBarriers" ::: Vector (SomeStruct ImageMemoryBarrier))
                  -> io ()
cmdWaitEventsSafe = cmdWaitEventsSafeOrUnsafe mkVkCmdWaitEventsSafe


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPipelineBarrier
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlags -> PipelineStageFlags -> DependencyFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlags -> PipelineStageFlags -> DependencyFlags -> Word32 -> Ptr MemoryBarrier -> Word32 -> Ptr BufferMemoryBarrier -> Word32 -> Ptr (SomeStruct ImageMemoryBarrier) -> IO ()

-- No documentation found for TopLevel "vkCmdPipelineBarrier"
cmdPipelineBarrier :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkCmdPipelineBarrier" "commandBuffer"
                      CommandBuffer
                   -> -- No documentation found for Nested "vkCmdPipelineBarrier" "srcStageMask"
                      ("srcStageMask" ::: PipelineStageFlags)
                   -> -- No documentation found for Nested "vkCmdPipelineBarrier" "dstStageMask"
                      ("dstStageMask" ::: PipelineStageFlags)
                   -> -- No documentation found for Nested "vkCmdPipelineBarrier" "dependencyFlags"
                      DependencyFlags
                   -> -- No documentation found for Nested "vkCmdPipelineBarrier" "pMemoryBarriers"
                      ("memoryBarriers" ::: Vector MemoryBarrier)
                   -> -- No documentation found for Nested "vkCmdPipelineBarrier" "pBufferMemoryBarriers"
                      ("bufferMemoryBarriers" ::: Vector BufferMemoryBarrier)
                   -> -- No documentation found for Nested "vkCmdPipelineBarrier" "pImageMemoryBarriers"
                      ("imageMemoryBarriers" ::: Vector (SomeStruct ImageMemoryBarrier))
                   -> io ()
cmdPipelineBarrier commandBuffer srcStageMask dstStageMask dependencyFlags memoryBarriers bufferMemoryBarriers imageMemoryBarriers = liftIO . evalContT $ do
  let vkCmdPipelineBarrierPtr = pVkCmdPipelineBarrier (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdPipelineBarrierPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPipelineBarrier is null" Nothing Nothing
  let vkCmdPipelineBarrier' = mkVkCmdPipelineBarrier vkCmdPipelineBarrierPtr
  pPMemoryBarriers <- ContT $ allocaBytesAligned @MemoryBarrier ((Data.Vector.length (memoryBarriers)) * 24) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPMemoryBarriers `plusPtr` (24 * (i)) :: Ptr MemoryBarrier) (e)) (memoryBarriers)
  pPBufferMemoryBarriers <- ContT $ allocaBytesAligned @BufferMemoryBarrier ((Data.Vector.length (bufferMemoryBarriers)) * 56) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPBufferMemoryBarriers `plusPtr` (56 * (i)) :: Ptr BufferMemoryBarrier) (e)) (bufferMemoryBarriers)
  pPImageMemoryBarriers <- ContT $ allocaBytesAligned @(ImageMemoryBarrier _) ((Data.Vector.length (imageMemoryBarriers)) * 72) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPImageMemoryBarriers `plusPtr` (72 * (i)) :: Ptr (ImageMemoryBarrier _))) (e) . ($ ())) (imageMemoryBarriers)
  lift $ vkCmdPipelineBarrier' (commandBufferHandle (commandBuffer)) (srcStageMask) (dstStageMask) (dependencyFlags) ((fromIntegral (Data.Vector.length $ (memoryBarriers)) :: Word32)) (pPMemoryBarriers) ((fromIntegral (Data.Vector.length $ (bufferMemoryBarriers)) :: Word32)) (pPBufferMemoryBarriers) ((fromIntegral (Data.Vector.length $ (imageMemoryBarriers)) :: Word32)) (forgetExtensions (pPImageMemoryBarriers))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginQuery
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> QueryControlFlags -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> QueryControlFlags -> IO ()

-- No documentation found for TopLevel "vkCmdBeginQuery"
cmdBeginQuery :: forall io
               . (MonadIO io)
              => -- No documentation found for Nested "vkCmdBeginQuery" "commandBuffer"
                 CommandBuffer
              -> -- No documentation found for Nested "vkCmdBeginQuery" "queryPool"
                 QueryPool
              -> -- No documentation found for Nested "vkCmdBeginQuery" "query"
                 ("query" ::: Word32)
              -> -- No documentation found for Nested "vkCmdBeginQuery" "flags"
                 QueryControlFlags
              -> io ()
cmdBeginQuery commandBuffer queryPool query flags = liftIO $ do
  let vkCmdBeginQueryPtr = pVkCmdBeginQuery (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBeginQueryPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginQuery is null" Nothing Nothing
  let vkCmdBeginQuery' = mkVkCmdBeginQuery vkCmdBeginQueryPtr
  vkCmdBeginQuery' (commandBufferHandle (commandBuffer)) (queryPool) (query) (flags)
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginQuery' and 'cmdEndQuery'
--
-- Note that 'cmdEndQuery' is *not* called if an exception is thrown by the
-- inner action.
cmdUseQuery :: forall io r . MonadIO io => CommandBuffer -> QueryPool -> Word32 -> QueryControlFlags -> io r -> io r
cmdUseQuery commandBuffer queryPool query flags a =
  (cmdBeginQuery commandBuffer queryPool query flags) *> a <* (cmdEndQuery commandBuffer queryPool query)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndQuery
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdEndQuery"
cmdEndQuery :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "vkCmdEndQuery" "commandBuffer"
               CommandBuffer
            -> -- No documentation found for Nested "vkCmdEndQuery" "queryPool"
               QueryPool
            -> -- No documentation found for Nested "vkCmdEndQuery" "query"
               ("query" ::: Word32)
            -> io ()
cmdEndQuery commandBuffer queryPool query = liftIO $ do
  let vkCmdEndQueryPtr = pVkCmdEndQuery (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdEndQueryPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndQuery is null" Nothing Nothing
  let vkCmdEndQuery' = mkVkCmdEndQuery vkCmdEndQueryPtr
  vkCmdEndQuery' (commandBufferHandle (commandBuffer)) (queryPool) (query)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResetQueryPool
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdResetQueryPool"
cmdResetQueryPool :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkCmdResetQueryPool" "commandBuffer"
                     CommandBuffer
                  -> -- No documentation found for Nested "vkCmdResetQueryPool" "queryPool"
                     QueryPool
                  -> -- No documentation found for Nested "vkCmdResetQueryPool" "firstQuery"
                     ("firstQuery" ::: Word32)
                  -> -- No documentation found for Nested "vkCmdResetQueryPool" "queryCount"
                     ("queryCount" ::: Word32)
                  -> io ()
cmdResetQueryPool commandBuffer queryPool firstQuery queryCount = liftIO $ do
  let vkCmdResetQueryPoolPtr = pVkCmdResetQueryPool (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdResetQueryPoolPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdResetQueryPool is null" Nothing Nothing
  let vkCmdResetQueryPool' = mkVkCmdResetQueryPool vkCmdResetQueryPoolPtr
  vkCmdResetQueryPool' (commandBufferHandle (commandBuffer)) (queryPool) (firstQuery) (queryCount)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteTimestamp
  :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlagBits -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineStageFlagBits -> QueryPool -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdWriteTimestamp"
cmdWriteTimestamp :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkCmdWriteTimestamp" "commandBuffer"
                     CommandBuffer
                  -> -- No documentation found for Nested "vkCmdWriteTimestamp" "pipelineStage"
                     PipelineStageFlagBits
                  -> -- No documentation found for Nested "vkCmdWriteTimestamp" "queryPool"
                     QueryPool
                  -> -- No documentation found for Nested "vkCmdWriteTimestamp" "query"
                     ("query" ::: Word32)
                  -> io ()
cmdWriteTimestamp commandBuffer pipelineStage queryPool query = liftIO $ do
  let vkCmdWriteTimestampPtr = pVkCmdWriteTimestamp (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdWriteTimestampPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteTimestamp is null" Nothing Nothing
  let vkCmdWriteTimestamp' = mkVkCmdWriteTimestamp vkCmdWriteTimestampPtr
  vkCmdWriteTimestamp' (commandBufferHandle (commandBuffer)) (pipelineStage) (queryPool) (query)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyQueryPoolResults
  :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> Buffer -> DeviceSize -> DeviceSize -> QueryResultFlags -> IO ()) -> Ptr CommandBuffer_T -> QueryPool -> Word32 -> Word32 -> Buffer -> DeviceSize -> DeviceSize -> QueryResultFlags -> IO ()

-- No documentation found for TopLevel "vkCmdCopyQueryPoolResults"
cmdCopyQueryPoolResults :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkCmdCopyQueryPoolResults" "commandBuffer"
                           CommandBuffer
                        -> -- No documentation found for Nested "vkCmdCopyQueryPoolResults" "queryPool"
                           QueryPool
                        -> -- No documentation found for Nested "vkCmdCopyQueryPoolResults" "firstQuery"
                           ("firstQuery" ::: Word32)
                        -> -- No documentation found for Nested "vkCmdCopyQueryPoolResults" "queryCount"
                           ("queryCount" ::: Word32)
                        -> -- No documentation found for Nested "vkCmdCopyQueryPoolResults" "dstBuffer"
                           ("dstBuffer" ::: Buffer)
                        -> -- No documentation found for Nested "vkCmdCopyQueryPoolResults" "dstOffset"
                           ("dstOffset" ::: DeviceSize)
                        -> -- No documentation found for Nested "vkCmdCopyQueryPoolResults" "stride"
                           ("stride" ::: DeviceSize)
                        -> -- No documentation found for Nested "vkCmdCopyQueryPoolResults" "flags"
                           QueryResultFlags
                        -> io ()
cmdCopyQueryPoolResults commandBuffer queryPool firstQuery queryCount dstBuffer dstOffset stride flags = liftIO $ do
  let vkCmdCopyQueryPoolResultsPtr = pVkCmdCopyQueryPoolResults (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdCopyQueryPoolResultsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyQueryPoolResults is null" Nothing Nothing
  let vkCmdCopyQueryPoolResults' = mkVkCmdCopyQueryPoolResults vkCmdCopyQueryPoolResultsPtr
  vkCmdCopyQueryPoolResults' (commandBufferHandle (commandBuffer)) (queryPool) (firstQuery) (queryCount) (dstBuffer) (dstOffset) (stride) (flags)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushConstants
  :: FunPtr (Ptr CommandBuffer_T -> PipelineLayout -> ShaderStageFlags -> Word32 -> Word32 -> Ptr () -> IO ()) -> Ptr CommandBuffer_T -> PipelineLayout -> ShaderStageFlags -> Word32 -> Word32 -> Ptr () -> IO ()

-- No documentation found for TopLevel "vkCmdPushConstants"
cmdPushConstants :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkCmdPushConstants" "commandBuffer"
                    CommandBuffer
                 -> -- No documentation found for Nested "vkCmdPushConstants" "layout"
                    PipelineLayout
                 -> -- No documentation found for Nested "vkCmdPushConstants" "stageFlags"
                    ShaderStageFlags
                 -> -- No documentation found for Nested "vkCmdPushConstants" "offset"
                    ("offset" ::: Word32)
                 -> -- No documentation found for Nested "vkCmdPushConstants" "size"
                    ("size" ::: Word32)
                 -> -- No documentation found for Nested "vkCmdPushConstants" "pValues"
                    ("values" ::: Ptr ())
                 -> io ()
cmdPushConstants commandBuffer layout stageFlags offset size values = liftIO $ do
  let vkCmdPushConstantsPtr = pVkCmdPushConstants (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdPushConstantsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushConstants is null" Nothing Nothing
  let vkCmdPushConstants' = mkVkCmdPushConstants vkCmdPushConstantsPtr
  vkCmdPushConstants' (commandBufferHandle (commandBuffer)) (layout) (stageFlags) (offset) (size) (values)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginRenderPass
  :: FunPtr (Ptr CommandBuffer_T -> Ptr (SomeStruct RenderPassBeginInfo) -> SubpassContents -> IO ()) -> Ptr CommandBuffer_T -> Ptr (SomeStruct RenderPassBeginInfo) -> SubpassContents -> IO ()

-- No documentation found for TopLevel "vkCmdBeginRenderPass"
cmdBeginRenderPass :: forall a io
                    . (Extendss RenderPassBeginInfo a, PokeChain a, MonadIO io)
                   => -- No documentation found for Nested "vkCmdBeginRenderPass" "commandBuffer"
                      CommandBuffer
                   -> -- No documentation found for Nested "vkCmdBeginRenderPass" "pRenderPassBegin"
                      (RenderPassBeginInfo a)
                   -> -- No documentation found for Nested "vkCmdBeginRenderPass" "contents"
                      SubpassContents
                   -> io ()
cmdBeginRenderPass commandBuffer renderPassBegin contents = liftIO . evalContT $ do
  let vkCmdBeginRenderPassPtr = pVkCmdBeginRenderPass (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBeginRenderPassPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginRenderPass is null" Nothing Nothing
  let vkCmdBeginRenderPass' = mkVkCmdBeginRenderPass vkCmdBeginRenderPassPtr
  pRenderPassBegin <- ContT $ withCStruct (renderPassBegin)
  lift $ vkCmdBeginRenderPass' (commandBufferHandle (commandBuffer)) (forgetExtensions pRenderPassBegin) (contents)
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginRenderPass' and 'cmdEndRenderPass'
--
-- Note that 'cmdEndRenderPass' is *not* called if an exception is thrown
-- by the inner action.
cmdUseRenderPass :: forall a io r . (Extendss RenderPassBeginInfo a, PokeChain a, MonadIO io) => CommandBuffer -> RenderPassBeginInfo a -> SubpassContents -> io r -> io r
cmdUseRenderPass commandBuffer pRenderPassBegin contents a =
  (cmdBeginRenderPass commandBuffer pRenderPassBegin contents) *> a <* (cmdEndRenderPass commandBuffer)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdNextSubpass
  :: FunPtr (Ptr CommandBuffer_T -> SubpassContents -> IO ()) -> Ptr CommandBuffer_T -> SubpassContents -> IO ()

-- No documentation found for TopLevel "vkCmdNextSubpass"
cmdNextSubpass :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vkCmdNextSubpass" "commandBuffer"
                  CommandBuffer
               -> -- No documentation found for Nested "vkCmdNextSubpass" "contents"
                  SubpassContents
               -> io ()
cmdNextSubpass commandBuffer contents = liftIO $ do
  let vkCmdNextSubpassPtr = pVkCmdNextSubpass (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdNextSubpassPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdNextSubpass is null" Nothing Nothing
  let vkCmdNextSubpass' = mkVkCmdNextSubpass vkCmdNextSubpassPtr
  vkCmdNextSubpass' (commandBufferHandle (commandBuffer)) (contents)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndRenderPass
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- No documentation found for TopLevel "vkCmdEndRenderPass"
cmdEndRenderPass :: forall io
                  . (MonadIO io)
                 => -- No documentation found for Nested "vkCmdEndRenderPass" "commandBuffer"
                    CommandBuffer
                 -> io ()
cmdEndRenderPass commandBuffer = liftIO $ do
  let vkCmdEndRenderPassPtr = pVkCmdEndRenderPass (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdEndRenderPassPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndRenderPass is null" Nothing Nothing
  let vkCmdEndRenderPass' = mkVkCmdEndRenderPass vkCmdEndRenderPassPtr
  vkCmdEndRenderPass' (commandBufferHandle (commandBuffer))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdExecuteCommands
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr (Ptr CommandBuffer_T) -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr (Ptr CommandBuffer_T) -> IO ()

-- No documentation found for TopLevel "vkCmdExecuteCommands"
cmdExecuteCommands :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkCmdExecuteCommands" "commandBuffer"
                      CommandBuffer
                   -> -- No documentation found for Nested "vkCmdExecuteCommands" "pCommandBuffers"
                      ("commandBuffers" ::: Vector CommandBuffer)
                   -> io ()
cmdExecuteCommands commandBuffer commandBuffers = liftIO . evalContT $ do
  let vkCmdExecuteCommandsPtr = pVkCmdExecuteCommands (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdExecuteCommandsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdExecuteCommands is null" Nothing Nothing
  let vkCmdExecuteCommands' = mkVkCmdExecuteCommands vkCmdExecuteCommandsPtr
  pPCommandBuffers <- ContT $ allocaBytesAligned @(Ptr CommandBuffer_T) ((Data.Vector.length (commandBuffers)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCommandBuffers `plusPtr` (8 * (i)) :: Ptr (Ptr CommandBuffer_T)) (commandBufferHandle (e))) (commandBuffers)
  lift $ vkCmdExecuteCommands' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (commandBuffers)) :: Word32)) (pPCommandBuffers)
  pure $ ()



-- No documentation found for TopLevel "VkClearRect"
data ClearRect = ClearRect
  { -- No documentation found for Nested "VkClearRect" "rect"
    rect :: Rect2D
  , -- No documentation found for Nested "VkClearRect" "baseArrayLayer"
    baseArrayLayer :: Word32
  , -- No documentation found for Nested "VkClearRect" "layerCount"
    layerCount :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ClearRect)
#endif
deriving instance Show ClearRect

instance ToCStruct ClearRect where
  withCStruct x f = allocaBytesAligned 24 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ClearRect{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Rect2D)) (rect)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (baseArrayLayer)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (layerCount)
    f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Rect2D)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct ClearRect where
  peekCStruct p = do
    rect <- peekCStruct @Rect2D ((p `plusPtr` 0 :: Ptr Rect2D))
    baseArrayLayer <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    layerCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ ClearRect
             rect baseArrayLayer layerCount


instance Storable ClearRect where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ClearRect where
  zero = ClearRect
           zero
           zero
           zero



-- No documentation found for TopLevel "VkImageSubresourceLayers"
data ImageSubresourceLayers = ImageSubresourceLayers
  { -- No documentation found for Nested "VkImageSubresourceLayers" "aspectMask"
    aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "VkImageSubresourceLayers" "mipLevel"
    mipLevel :: Word32
  , -- No documentation found for Nested "VkImageSubresourceLayers" "baseArrayLayer"
    baseArrayLayer :: Word32
  , -- No documentation found for Nested "VkImageSubresourceLayers" "layerCount"
    layerCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageSubresourceLayers)
#endif
deriving instance Show ImageSubresourceLayers

instance ToCStruct ImageSubresourceLayers where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSubresourceLayers{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (aspectMask)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (mipLevel)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (baseArrayLayer)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (layerCount)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct ImageSubresourceLayers where
  peekCStruct p = do
    aspectMask <- peek @ImageAspectFlags ((p `plusPtr` 0 :: Ptr ImageAspectFlags))
    mipLevel <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    baseArrayLayer <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    layerCount <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ ImageSubresourceLayers
             aspectMask mipLevel baseArrayLayer layerCount


instance Storable ImageSubresourceLayers where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSubresourceLayers where
  zero = ImageSubresourceLayers
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkBufferCopy"
data BufferCopy = BufferCopy
  { -- No documentation found for Nested "VkBufferCopy" "srcOffset"
    srcOffset :: DeviceSize
  , -- No documentation found for Nested "VkBufferCopy" "dstOffset"
    dstOffset :: DeviceSize
  , -- No documentation found for Nested "VkBufferCopy" "size"
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferCopy)
#endif
deriving instance Show BufferCopy

instance ToCStruct BufferCopy where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCopy{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (srcOffset)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (dstOffset)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BufferCopy where
  peekCStruct p = do
    srcOffset <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    dstOffset <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ BufferCopy
             srcOffset dstOffset size


instance Storable BufferCopy where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferCopy where
  zero = BufferCopy
           zero
           zero
           zero



-- No documentation found for TopLevel "VkImageCopy"
data ImageCopy = ImageCopy
  { -- No documentation found for Nested "VkImageCopy" "srcSubresource"
    srcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageCopy" "srcOffset"
    srcOffset :: Offset3D
  , -- No documentation found for Nested "VkImageCopy" "dstSubresource"
    dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageCopy" "dstOffset"
    dstOffset :: Offset3D
  , -- No documentation found for Nested "VkImageCopy" "extent"
    extent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageCopy)
#endif
deriving instance Show ImageCopy

instance ToCStruct ImageCopy where
  withCStruct x f = allocaBytesAligned 68 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageCopy{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (srcSubresource)
    poke ((p `plusPtr` 16 :: Ptr Offset3D)) (srcOffset)
    poke ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers)) (dstSubresource)
    poke ((p `plusPtr` 44 :: Ptr Offset3D)) (dstOffset)
    poke ((p `plusPtr` 56 :: Ptr Extent3D)) (extent)
    f
  cStructSize = 68
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct ImageCopy where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers))
    srcOffset <- peekCStruct @Offset3D ((p `plusPtr` 16 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers))
    dstOffset <- peekCStruct @Offset3D ((p `plusPtr` 44 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 56 :: Ptr Extent3D))
    pure $ ImageCopy
             srcSubresource srcOffset dstSubresource dstOffset extent


instance Storable ImageCopy where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageCopy where
  zero = ImageCopy
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkImageBlit"
data ImageBlit = ImageBlit
  { -- No documentation found for Nested "VkImageBlit" "srcSubresource"
    srcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageBlit" "srcOffsets"
    srcOffsets :: (Offset3D, Offset3D)
  , -- No documentation found for Nested "VkImageBlit" "dstSubresource"
    dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageBlit" "dstOffsets"
    dstOffsets :: (Offset3D, Offset3D)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageBlit)
#endif
deriving instance Show ImageBlit

instance ToCStruct ImageBlit where
  withCStruct x f = allocaBytesAligned 80 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageBlit{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (srcSubresource)
    let pSrcOffsets' = lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray 2 Offset3D)))
    case (srcOffsets) of
      (e0, e1) -> do
        poke (pSrcOffsets' :: Ptr Offset3D) (e0)
        poke (pSrcOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    poke ((p `plusPtr` 40 :: Ptr ImageSubresourceLayers)) (dstSubresource)
    let pDstOffsets' = lowerArrayPtr ((p `plusPtr` 56 :: Ptr (FixedArray 2 Offset3D)))
    case (dstOffsets) of
      (e0, e1) -> do
        poke (pDstOffsets' :: Ptr Offset3D) (e0)
        poke (pDstOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    f
  cStructSize = 80
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (zero)
    let pSrcOffsets' = lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray 2 Offset3D)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pSrcOffsets' :: Ptr Offset3D) (e0)
        poke (pSrcOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    poke ((p `plusPtr` 40 :: Ptr ImageSubresourceLayers)) (zero)
    let pDstOffsets' = lowerArrayPtr ((p `plusPtr` 56 :: Ptr (FixedArray 2 Offset3D)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pDstOffsets' :: Ptr Offset3D) (e0)
        poke (pDstOffsets' `plusPtr` 12 :: Ptr Offset3D) (e1)
    f

instance FromCStruct ImageBlit where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers))
    let psrcOffsets = lowerArrayPtr @Offset3D ((p `plusPtr` 16 :: Ptr (FixedArray 2 Offset3D)))
    srcOffsets0 <- peekCStruct @Offset3D ((psrcOffsets `advancePtrBytes` 0 :: Ptr Offset3D))
    srcOffsets1 <- peekCStruct @Offset3D ((psrcOffsets `advancePtrBytes` 12 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 40 :: Ptr ImageSubresourceLayers))
    let pdstOffsets = lowerArrayPtr @Offset3D ((p `plusPtr` 56 :: Ptr (FixedArray 2 Offset3D)))
    dstOffsets0 <- peekCStruct @Offset3D ((pdstOffsets `advancePtrBytes` 0 :: Ptr Offset3D))
    dstOffsets1 <- peekCStruct @Offset3D ((pdstOffsets `advancePtrBytes` 12 :: Ptr Offset3D))
    pure $ ImageBlit
             srcSubresource ((srcOffsets0, srcOffsets1)) dstSubresource ((dstOffsets0, dstOffsets1))


instance Storable ImageBlit where
  sizeOf ~_ = 80
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageBlit where
  zero = ImageBlit
           zero
           (zero, zero)
           zero
           (zero, zero)



-- No documentation found for TopLevel "VkBufferImageCopy"
data BufferImageCopy = BufferImageCopy
  { -- No documentation found for Nested "VkBufferImageCopy" "bufferOffset"
    bufferOffset :: DeviceSize
  , -- No documentation found for Nested "VkBufferImageCopy" "bufferRowLength"
    bufferRowLength :: Word32
  , -- No documentation found for Nested "VkBufferImageCopy" "bufferImageHeight"
    bufferImageHeight :: Word32
  , -- No documentation found for Nested "VkBufferImageCopy" "imageSubresource"
    imageSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkBufferImageCopy" "imageOffset"
    imageOffset :: Offset3D
  , -- No documentation found for Nested "VkBufferImageCopy" "imageExtent"
    imageExtent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferImageCopy)
#endif
deriving instance Show BufferImageCopy

instance ToCStruct BufferImageCopy where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferImageCopy{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (bufferOffset)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (bufferRowLength)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (bufferImageHeight)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (imageSubresource)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (imageOffset)
    poke ((p `plusPtr` 44 :: Ptr Extent3D)) (imageExtent)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct BufferImageCopy where
  peekCStruct p = do
    bufferOffset <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    bufferRowLength <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    bufferImageHeight <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    imageSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 16 :: Ptr ImageSubresourceLayers))
    imageOffset <- peekCStruct @Offset3D ((p `plusPtr` 32 :: Ptr Offset3D))
    imageExtent <- peekCStruct @Extent3D ((p `plusPtr` 44 :: Ptr Extent3D))
    pure $ BufferImageCopy
             bufferOffset bufferRowLength bufferImageHeight imageSubresource imageOffset imageExtent


instance Storable BufferImageCopy where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferImageCopy where
  zero = BufferImageCopy
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkImageResolve"
data ImageResolve = ImageResolve
  { -- No documentation found for Nested "VkImageResolve" "srcSubresource"
    srcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageResolve" "srcOffset"
    srcOffset :: Offset3D
  , -- No documentation found for Nested "VkImageResolve" "dstSubresource"
    dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "VkImageResolve" "dstOffset"
    dstOffset :: Offset3D
  , -- No documentation found for Nested "VkImageResolve" "extent"
    extent :: Extent3D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageResolve)
#endif
deriving instance Show ImageResolve

instance ToCStruct ImageResolve where
  withCStruct x f = allocaBytesAligned 68 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageResolve{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (srcSubresource)
    poke ((p `plusPtr` 16 :: Ptr Offset3D)) (srcOffset)
    poke ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers)) (dstSubresource)
    poke ((p `plusPtr` 44 :: Ptr Offset3D)) (dstOffset)
    poke ((p `plusPtr` 56 :: Ptr Extent3D)) (extent)
    f
  cStructSize = 68
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Offset3D)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Extent3D)) (zero)
    f

instance FromCStruct ImageResolve where
  peekCStruct p = do
    srcSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 0 :: Ptr ImageSubresourceLayers))
    srcOffset <- peekCStruct @Offset3D ((p `plusPtr` 16 :: Ptr Offset3D))
    dstSubresource <- peekCStruct @ImageSubresourceLayers ((p `plusPtr` 28 :: Ptr ImageSubresourceLayers))
    dstOffset <- peekCStruct @Offset3D ((p `plusPtr` 44 :: Ptr Offset3D))
    extent <- peekCStruct @Extent3D ((p `plusPtr` 56 :: Ptr Extent3D))
    pure $ ImageResolve
             srcSubresource srcOffset dstSubresource dstOffset extent


instance Storable ImageResolve where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageResolve where
  zero = ImageResolve
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkRenderPassBeginInfo"
data RenderPassBeginInfo (es :: [Type]) = RenderPassBeginInfo
  { -- No documentation found for Nested "VkRenderPassBeginInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "renderPass"
    renderPass :: RenderPass
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "framebuffer"
    framebuffer :: Framebuffer
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "renderArea"
    renderArea :: Rect2D
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "pClearValues"
    clearValues :: Vector ClearValue
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassBeginInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RenderPassBeginInfo es)

instance Extensible RenderPassBeginInfo where
  extensibleType = STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
  setNext x next = x{next = next}
  getNext RenderPassBeginInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RenderPassBeginInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @RenderPassTransformBeginInfoQCOM = Just f
    | Just Refl <- eqT @e @RenderPassAttachmentBeginInfo = Just f
    | Just Refl <- eqT @e @RenderPassSampleLocationsBeginInfoEXT = Just f
    | Just Refl <- eqT @e @DeviceGroupRenderPassBeginInfo = Just f
    | otherwise = Nothing

instance (Extendss RenderPassBeginInfo es, PokeChain es) => ToCStruct (RenderPassBeginInfo es) where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassBeginInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderPass)) (renderPass)
    lift $ poke ((p `plusPtr` 24 :: Ptr Framebuffer)) (framebuffer)
    lift $ poke ((p `plusPtr` 32 :: Ptr Rect2D)) (renderArea)
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (clearValues)) :: Word32))
    pPClearValues' <- ContT $ allocaBytesAligned @ClearValue ((Data.Vector.length (clearValues)) * 16) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPClearValues' `plusPtr` (16 * (i)) :: Ptr ClearValue) (e) . ($ ())) (clearValues)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr ClearValue))) (pPClearValues')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr RenderPass)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Framebuffer)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Rect2D)) (zero)
    pPClearValues' <- ContT $ allocaBytesAligned @ClearValue ((Data.Vector.length (mempty)) * 16) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPClearValues' `plusPtr` (16 * (i)) :: Ptr ClearValue) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr ClearValue))) (pPClearValues')
    lift $ f

instance es ~ '[] => Zero (RenderPassBeginInfo es) where
  zero = RenderPassBeginInfo
           ()
           zero
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkClearDepthStencilValue"
data ClearDepthStencilValue = ClearDepthStencilValue
  { -- No documentation found for Nested "VkClearDepthStencilValue" "depth"
    depth :: Float
  , -- No documentation found for Nested "VkClearDepthStencilValue" "stencil"
    stencil :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ClearDepthStencilValue)
#endif
deriving instance Show ClearDepthStencilValue

instance ToCStruct ClearDepthStencilValue where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ClearDepthStencilValue{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (depth))
    poke ((p `plusPtr` 4 :: Ptr Word32)) (stencil)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct ClearDepthStencilValue where
  peekCStruct p = do
    depth <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    stencil <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ ClearDepthStencilValue
             ((\(CFloat a) -> a) depth) stencil


instance Storable ClearDepthStencilValue where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ClearDepthStencilValue where
  zero = ClearDepthStencilValue
           zero
           zero



-- No documentation found for TopLevel "VkClearAttachment"
data ClearAttachment = ClearAttachment
  { -- No documentation found for Nested "VkClearAttachment" "aspectMask"
    aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "VkClearAttachment" "colorAttachment"
    colorAttachment :: Word32
  , -- No documentation found for Nested "VkClearAttachment" "clearValue"
    clearValue :: ClearValue
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ClearAttachment)
#endif
deriving instance Show ClearAttachment

instance ToCStruct ClearAttachment where
  withCStruct x f = allocaBytesAligned 24 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ClearAttachment{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (aspectMask)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (colorAttachment)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr ClearValue)) (clearValue) . ($ ())
    lift $ f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr ImageAspectFlags)) (zero)
    lift $ poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr ClearValue)) (zero) . ($ ())
    lift $ f

instance Zero ClearAttachment where
  zero = ClearAttachment
           zero
           zero
           zero


data ClearColorValue
  = Float32 Float Float Float Float
  | Int32 Int32 Int32 Int32 Int32
  | Uint32 Word32 Word32 Word32 Word32
  deriving (Show)

instance ToCStruct ClearColorValue where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr ClearColorValue -> ClearColorValue -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    Float32 v0 v1 v2 v3 -> lift $ do
      let pFloat32 = lowerArrayPtr (castPtr @_ @(FixedArray 4 CFloat) p)
      case ((v0, v1, v2, v3)) of
        (e0, e1, e2, e3) -> do
          poke (pFloat32 :: Ptr CFloat) (CFloat (e0))
          poke (pFloat32 `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
          poke (pFloat32 `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
          poke (pFloat32 `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    Int32 v0 v1 v2 v3 -> lift $ do
      let pInt32 = lowerArrayPtr (castPtr @_ @(FixedArray 4 Int32) p)
      case ((v0, v1, v2, v3)) of
        (e0, e1, e2, e3) -> do
          poke (pInt32 :: Ptr Int32) (e0)
          poke (pInt32 `plusPtr` 4 :: Ptr Int32) (e1)
          poke (pInt32 `plusPtr` 8 :: Ptr Int32) (e2)
          poke (pInt32 `plusPtr` 12 :: Ptr Int32) (e3)
    Uint32 v0 v1 v2 v3 -> lift $ do
      let pUint32 = lowerArrayPtr (castPtr @_ @(FixedArray 4 Word32) p)
      case ((v0, v1, v2, v3)) of
        (e0, e1, e2, e3) -> do
          poke (pUint32 :: Ptr Word32) (e0)
          poke (pUint32 `plusPtr` 4 :: Ptr Word32) (e1)
          poke (pUint32 `plusPtr` 8 :: Ptr Word32) (e2)
          poke (pUint32 `plusPtr` 12 :: Ptr Word32) (e3)
  pokeZeroCStruct :: Ptr ClearColorValue -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 16
  cStructAlignment = 4

instance Zero ClearColorValue where
  zero = Float32 zero zero zero zero


data ClearValue
  = Color ClearColorValue
  | DepthStencil ClearDepthStencilValue
  deriving (Show)

instance ToCStruct ClearValue where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr ClearValue -> ClearValue -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    Color v -> ContT $ pokeCStruct (castPtr @_ @ClearColorValue p) (v) . ($ ())
    DepthStencil v -> lift $ poke (castPtr @_ @ClearDepthStencilValue p) (v)
  pokeZeroCStruct :: Ptr ClearValue -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 16
  cStructAlignment = 4

instance Zero ClearValue where
  zero = Color zero

