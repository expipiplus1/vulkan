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
import           Vulkan.Extensions.VK_KHR_ray_tracing
import           Vulkan.Extensions.VK_KHR_swapchain
                                               as Swap
import           Vulkan.Zero
import Control.Monad.Trans.Class (MonadTrans(lift))

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
myRecordCommandBuffer :: Frame -> Word32 -> CmdT F ()
myRecordCommandBuffer Frame {..} imageIndex = do
  -- TODO: neaten
  RTInfo {..} <- CmdT . lift . liftV $ getRTInfo
  let SwapchainResources {..} = fSwapchainResources
      SwapchainInfo {..}      = srInfo
      image                   = srImages ! fromIntegral imageIndex
      imageWidth              = width (siImageExtent :: Extent2D)
      imageHeight             = height (siImageExtent :: Extent2D)
      imageSubresourceRange   = ImageSubresourceRange
        { aspectMask     = IMAGE_ASPECT_COLOR_BIT
        , baseMipLevel   = 0
        , levelCount     = 1
        , baseArrayLayer = 0
        , layerCount     = 1
        }
      sbtRegion = StridedBufferRegionKHR
        { buffer = fShaderBindingTable
        , offset = 0
        , stride = fromIntegral rtiShaderGroupBaseAlignment
        , size   = fromIntegral rtiShaderGroupBaseAlignment --  * 1
        }
  do
    -- Transition image to general, to write from the compute shader
    cmdPipelineBarrier'
      PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
      zero
      []
      []
      [ SomeStruct zero { srcAccessMask    = zero
                        , dstAccessMask    = ACCESS_SHADER_WRITE_BIT
                        , oldLayout        = IMAGE_LAYOUT_UNDEFINED
                        , newLayout        = IMAGE_LAYOUT_GENERAL
                        , image            = image
                        , subresourceRange = imageSubresourceRange
                        }
      ]

    -- Bind descriptor sets
    cmdBindPipeline' PIPELINE_BIND_POINT_RAY_TRACING_KHR fPipeline
    cmdBindDescriptorSets' PIPELINE_BIND_POINT_RAY_TRACING_KHR
                           fPipelineLayout
                           0
                           [fDescriptorSets ! fromIntegral imageIndex]
                           []

    --
    -- The actual ray tracing
    --
    cmdTraceRaysKHR' sbtRegion zero zero zero imageWidth imageHeight 1

    -- Transition image back to present
    cmdPipelineBarrier'
      PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
      -- No need to get anything to wait because we're synchronizing with
      -- the semaphore
      PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
      zero
      []
      []
      [ SomeStruct zero { srcAccessMask    = ACCESS_SHADER_WRITE_BIT
                        , dstAccessMask    = zero
                        , oldLayout        = IMAGE_LAYOUT_GENERAL
                        , newLayout        = IMAGE_LAYOUT_PRESENT_SRC_KHR
                        , image            = image
                        , subresourceRange = imageSubresourceRange
                        }
      ]

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

timeoutError :: MonadIO m => String -> m a
timeoutError message =
  liftIO . throwIO $ IOError Nothing TimeExpired "" message Nothing Nothing
