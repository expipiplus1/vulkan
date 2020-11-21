{-# LANGUAGE OverloadedLists #-}

module Render
  ( renderFrame
  ) where

import           Control.Exception              ( throwIO )
import           Control.Monad.IO.Class
import           Data.Vector                    ( (!) )
import           Data.Word
import           Frame
import           GHC.IO.Exception               ( IOErrorType(TimeExpired)
                                                , IOException(IOError)
                                                )
import           MonadFrame
import           MonadVulkan
import           Say
import           Swapchain
import           UnliftIO.Exception             ( throwString )
import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Core10
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import           Vulkan.Extensions.VK_KHR_swapchain
                                               as Swap
import           Vulkan.Zero

renderFrame :: F ()
renderFrame = do
  f@Frame {..} <- askFrame
  let RecycledResources {..} = fRecycledResources
  let oneSecond               = 1e9
      SwapchainResources {..} = fSwapchainResources
      SwapchainInfo {..}      = srInfo

  -- Ensure that the swapchain survives for the duration of this frame
  frameRefCount srRelease

  -- Make sure we'll have an image to render to
  imageIndex <-
    acquireNextImageKHR' siSwapchain
                         oneSecond
                         fImageAvailableSemaphore
                         NULL_HANDLE
      >>= \case
            (SUCCESS, imageIndex) -> pure imageIndex
            (TIMEOUT, _) ->
              timeoutError "Timed out (1s) trying to acquire next image"
            _ -> throwString "Unexpected Result from acquireNextImageKHR"

  -- Allocate a command buffer and populate it
  let commandBufferAllocateInfo = zero { commandPool = fCommandPool
                                       , level = COMMAND_BUFFER_LEVEL_PRIMARY
                                       , commandBufferCount = 1
                                       }
  ~[commandBuffer] <- allocateCommandBuffers' commandBufferAllocateInfo
  useCommandBuffer' commandBuffer zero $ myRecordCommandBuffer f imageIndex

  -- Submit the work
  let -- Wait for the 'imageAvailableSemaphore' before outputting to the color
      -- attachment
    submitInfo =
      zero
          { Core10.waitSemaphores = [fImageAvailableSemaphore]
          , waitDstStageMask      = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
          , commandBuffers        = [commandBufferHandle commandBuffer]
          , signalSemaphores      = [ fRenderFinishedSemaphore
                                    , fRenderFinishedHostSemaphore
                                    ]
          }
        ::& zero { waitSemaphoreValues   = [1]
                 , signalSemaphoreValues = [1, fIndex]
                 }
        :&  ()
  graphicsQueue <- getGraphicsQueue
  queueSubmitFrame graphicsQueue
                   [SomeStruct submitInfo]
                   fRenderFinishedHostSemaphore
                   fIndex

  -- Present the frame when the render is finished
  -- The return code here could be SUBOPTIMAL_KHR
  -- TODO, check for that
  _ <- queuePresentKHR
    graphicsQueue
    zero { Swap.waitSemaphores = [fRenderFinishedSemaphore]
         , swapchains          = [siSwapchain]
         , imageIndices        = [imageIndex]
         }
  sayErrString ("submitted " <> show fIndex)

-- | Clear and render a triangle
myRecordCommandBuffer :: MonadIO m => Frame -> Word32 -> CmdT m ()
myRecordCommandBuffer Frame {..} imageIndex = do
  let SwapchainResources {..} = fSwapchainResources
      SwapchainInfo {..}      = srInfo
      renderPassBeginInfo     = zero
        { renderPass  = srRenderPass
        , framebuffer = srFramebuffers ! fromIntegral imageIndex
        , renderArea  = Rect2D { offset = zero, extent = siImageExtent }
        , clearValues = [Color (Float32 (0.3, 0.4, 0.8, 1))]
        }
  cmdUseRenderPass' renderPassBeginInfo SUBPASS_CONTENTS_INLINE $ do
    cmdSetViewport'
      0
      [ Viewport { x        = 0
                 , y        = 0
                 , width    = realToFrac (width (siImageExtent :: Extent2D))
                 , height   = realToFrac (height (siImageExtent :: Extent2D))
                 , minDepth = 0
                 , maxDepth = 1
                 }
      ]
    cmdSetScissor' 0 [Rect2D { offset = Offset2D 0 0, extent = siImageExtent }]
    cmdBindPipeline' PIPELINE_BIND_POINT_GRAPHICS fPipeline
    cmdDraw' 3 1 0 0

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

timeoutError :: MonadIO m => String -> m a
timeoutError message =
  liftIO . throwIO $ IOError Nothing TimeExpired "" message Nothing Nothing
