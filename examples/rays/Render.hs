{-# LANGUAGE OverloadedLists #-}

module Render
  ( renderFrame
  ) where

import           Control.Exception              ( throwIO )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Data.Vector                    ( (!) )
import           Data.Word
import           Frame
import           GHC.IO.Exception               ( IOErrorType(TimeExpired)
                                                , IOException(IOError)
                                                )
import           MonadFrame
import           MonadVulkan
import           Swapchain
import           UnliftIO.Exception             ( throwString )
import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Core10
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import           Vulkan.Extensions.VK_KHR_ray_tracing
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

  -- Update the necessary descriptor sets
  updateDescriptorSets'
    [ SomeStruct zero
        { dstSet          = fDescriptorSet
        , dstBinding      = 1
        , descriptorType  = DESCRIPTOR_TYPE_STORAGE_IMAGE
        , descriptorCount = 1
        , imageInfo       = [ DescriptorImageInfo
                                { sampler = NULL_HANDLE
                                , imageView = srImageViews ! fromIntegral imageIndex
                                , imageLayout = IMAGE_LAYOUT_GENERAL
                                }
                            ]
        }
    ]
    []

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
  pure ()

----------------------------------------------------------------
-- Command buffer recording
----------------------------------------------------------------

-- | Clear and render a triangle
myRecordCommandBuffer :: Frame -> Word32 -> CmdT F ()
myRecordCommandBuffer Frame {..} imageIndex = do
  -- TODO: neaten
  RTInfo {..} <- CmdT . lift . liftV $ getRTInfo
  let
    RecycledResources {..}  = fRecycledResources
    SwapchainResources {..} = fSwapchainResources
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
    numRayGenShaderGroups = 1
    rayGenRegion          = StridedBufferRegionKHR
      { buffer = fShaderBindingTable
      , offset = 0
      , stride = fromIntegral rtiShaderGroupBaseAlignment
      , size = fromIntegral rtiShaderGroupBaseAlignment * numRayGenShaderGroups
      }
    numHitShaderGroups = 1
    hitRegion          = StridedBufferRegionKHR
      { buffer = fShaderBindingTable
      , offset = 1 * fromIntegral rtiShaderGroupBaseAlignment
      , stride = fromIntegral rtiShaderGroupBaseAlignment
      , size   = fromIntegral rtiShaderGroupBaseAlignment * numHitShaderGroups
      }
    numMissShaderGroups = 1
    missRegion          = StridedBufferRegionKHR
      { buffer = fShaderBindingTable
      , offset = 2 * fromIntegral rtiShaderGroupBaseAlignment
      , stride = fromIntegral rtiShaderGroupBaseAlignment
      , size   = fromIntegral rtiShaderGroupBaseAlignment * numMissShaderGroups
      }
    callableRegion = zero
  do
    -- Transition image to general, to write from the ray tracing shader
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
                           [fDescriptorSet]
                           []

    --
    -- The actual ray tracing
    --
    cmdTraceRaysKHR' rayGenRegion
                     missRegion
                     hitRegion
                     callableRegion
                     imageWidth
                     imageHeight
                     1

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
