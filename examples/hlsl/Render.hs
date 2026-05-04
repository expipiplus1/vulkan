{-# LANGUAGE OverloadedLists #-}

module Render
  ( renderFrame
  ) where

import           Control.Exception              ( throwIO )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource   ( ResourceT
                                                , allocate
                                                )
import           Data.Vector                    ( (!), Vector )
import           Frame                          ( Frame(..)
                                                , queueSubmitFrame
                                                )
import           GHC.IO.Exception               ( IOErrorType(TimeExpired)
                                                , IOException(IOError)
                                                )
import           RefCounted                     ( resourceTRefCount )
import           Swapchain                      ( Swapchain(..) )
import           UnliftIO.Exception             ( throwString )
import           VkResources                    ( Queues(..)
                                                , RecycledResources(..)
                                                , VkResources(..)
                                                )
import           Vulkan.CStruct.Extends
import           Vulkan.Exception               ( VulkanException(..) )
import           Vulkan.Core10                 as Core10
import qualified Vulkan.Core10                 as CommandBufferBeginInfo (CommandBufferBeginInfo(..))
import qualified Vulkan.Core10                 as Extent2D (Extent2D(..))
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import           Vulkan.Extensions.VK_KHR_swapchain
                                               as Swap
import           Vulkan.Zero

-- | Acquire an image, record a clear+draw, submit, and present.
renderFrame
  :: VkResources
  -> RenderPass
  -> Pipeline
  -> Vector Framebuffer
  -> Frame
  -> ResourceT IO ()
renderFrame vr renderPass pipeline framebuffers f = do
  let RecycledResources {..} = fRecycled f
      sc                     = fSwapchain f
      dev                    = vrDevice vr
      gQ                     = snd (qGraphics (vrQueues vr))
      oneSecond              = 1e9

  -- Hold a refcount on the swapchain release group so it survives this frame
  -- if the window resizes mid-flight.
  resourceTRefCount (sRelease sc)

  -- Acquire next image.
  (acquireResult, imageIndex) <-
    acquireNextImageKHRSafe dev (sSwapchain sc) oneSecond rrImageAvailable NULL_HANDLE
      >>= \case
            r@(SUCCESS,        _) -> pure r
            r@(SUBOPTIMAL_KHR, _) -> pure r
            (TIMEOUT, _) -> timeoutError "Timed out (1s) acquiring next image"
            _            -> throwString "Unexpected Result from acquireNextImageKHR"

  -- Allocate a per-frame command buffer from the recycled pool.
  (_, ~[commandBuffer]) <- withCommandBuffers
    dev
    zero { commandPool        = rrCommandPool
         , level              = COMMAND_BUFFER_LEVEL_PRIMARY
         , commandBufferCount = 1
         }
    allocate

  let renderPassBeginInfo = zero
        { renderPass  = renderPass
        , framebuffer = framebuffers ! fromIntegral imageIndex
        , renderArea  = Rect2D { offset = zero, extent = sExtent sc }
        , clearValues = [Color (Float32 0.3 0.4 0.8 1)]
        }

  useCommandBuffer
      commandBuffer
      zero { CommandBufferBeginInfo.flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    $ do
        cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE
          $ do
              cmdSetViewport commandBuffer
                             0
                             [ Viewport
                                 { x        = 0
                                 , y        = 0
                                 , width    = realToFrac (Extent2D.width  (sExtent sc))
                                 , height   = realToFrac (Extent2D.height (sExtent sc))
                                 , minDepth = 0
                                 , maxDepth = 1
                                 }
                             ]
              cmdSetScissor commandBuffer
                            0
                            [Rect2D { offset = Offset2D 0 0, extent = sExtent sc }]
              cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS pipeline
              cmdDraw commandBuffer 3 1 0 0

  let submitInfo =
        zero { Core10.waitSemaphores = [rrImageAvailable]
             , waitDstStageMask      = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
             , commandBuffers        = [commandBufferHandle commandBuffer]
             , signalSemaphores      = [rrRenderFinished, fHostTimeline f]
             }
          ::& zero { waitSemaphoreValues   = [1]
                   , signalSemaphoreValues = [1, fIndex f]
                   }
          :&  ()
  liftIO $ queueSubmitFrame gQ
                            f
                            [SomeStruct submitInfo]
                            (fHostTimeline f)
                            (fIndex f)

  presentResult <- queuePresentKHR
    gQ
    zero { Swap.waitSemaphores = [rrRenderFinished]
         , swapchains          = [sSwapchain sc]
         , imageIndices        = [imageIndex]
         }

  -- Surface either reported SUBOPTIMAL on acquire or present — bubble it up
  -- as an OUT_OF_DATE so the main loop will recreate the swapchain.
  case (acquireResult, presentResult) of
    (SUBOPTIMAL_KHR, _) -> liftIO . throwIO $ VulkanException ERROR_OUT_OF_DATE_KHR
    (_, SUBOPTIMAL_KHR) -> liftIO . throwIO $ VulkanException ERROR_OUT_OF_DATE_KHR
    _                   -> pure ()

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

timeoutError :: MonadIO m => String -> m a
timeoutError message =
  liftIO . throwIO $ IOError Nothing TimeExpired "" message Nothing Nothing
