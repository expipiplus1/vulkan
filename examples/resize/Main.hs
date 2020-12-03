{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main
  ( main
  ) where

import           Control.Exception              ( handle )
import           Control.Lens.Getter
import           Control.Monad.Extra            ( unlessM
                                                , when
                                                )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bool                      ( bool )
import qualified Data.Vector                   as V
import           GHC.Clock                      ( getMonotonicTimeNSec )
import           Linear.Affine                  ( Point(..) )
import           Linear.Metric                  ( norm )
import           Linear.V2
import qualified SDL
import           Say
import           UnliftIO.Exception             ( displayException
                                                , throwString
                                                )
import           UnliftIO.Foreign               ( allocaBytes
                                                , plusPtr
                                                , poke
                                                )
import           UnliftIO.IORef
import           UnliftIO.MVar
import           Utils

import           Vulkan.CStruct.Extends         ( SomeStruct(..) )
import           Vulkan.Core10                 as Vk
                                         hiding ( createDevice
                                                , createFramebuffer
                                                , createImageView
                                                , createInstance
                                                , withBuffer
                                                , withImage
                                                )
import           Vulkan.Exception
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.Zero

import qualified Data.ByteString               as BS
import           Frame
import           HasVulkan
import           Init
import           Julia
import           MonadVulkan
import           Pipeline
import qualified SDL.Video.Vulkan              as SDL
import           Swapchain
import           Window

----------------------------------------------------------------
-- Main performs some one time initialization of the windowing system and
-- Vulkan, then it loops generating frames
--
-- It's bound to an OS thread so SDL.pumpEvents can work properly.
----------------------------------------------------------------
main :: IO ()
main = prettyError . runResourceT $ do
  -- Start SDL
  _ <- allocate_ (SDL.initialize @[] [SDL.InitEvents]) SDL.quit

  let initWidth  = 1280
      initHeight = 720

  -- Create everything up to the device
  sdlWindow  <- createWindow "Haskell ❤️ Vulkan" initWidth initHeight
  windowExts <-
    liftIO $ traverse BS.packCString =<< SDL.vkGetInstanceExtensions sdlWindow
  inst    <- createInstance windowExts
  surface <- createSurface inst sdlWindow
  DeviceParams devName phys dev graphicsQueue graphicsQueueFamilyIndex <-
    createDevice inst (snd surface)
  let commandPoolCreateInfo :: CommandPoolCreateInfo
      commandPoolCreateInfo =
        zero { queueFamilyIndex = graphicsQueueFamilyIndex }
  commandPools <- V.replicateM
    numConcurrentFrames
    (snd <$> withCommandPool dev commandPoolCreateInfo Nothing allocate)

  allocator <- createVMA inst phys dev

  sayErr $ "Using device: " <> devName

  -- Now all the globals are initialized
  runV inst
       phys
       dev
       graphicsQueue
       graphicsQueueFamilyIndex
       commandPools
       allocator
    $ do
        i <- initialFrame sdlWindow
                          (Just surface)
                          (Extent2D initWidth initHeight)

        SDL.showWindow sdlWindow
        loopJust frame i

prettyError :: IO () -> IO ()
prettyError =
  handle (\e@(VulkanException _) -> sayErrString (displayException e))

initialFrame
  :: SDL.Window
  -> Maybe (ReleaseKey, SurfaceKHR)
  -- ^ existing surface for window
  -> Extent2D
  -> V Frame
initialFrame window surfaceM windowSize = do
  inst                     <- getInstance
  (_, surface)             <- maybe (createSurface inst window) pure surfaceM

  graphicsQueueFamilyIndex <- getGraphicsQueueFamilyIndex
  phys                     <- getPhysicalDevice
  unlessM
      (getPhysicalDeviceSurfaceSupportKHR phys graphicsQueueFamilyIndex surface)
    $ throwString "Device isn't able to present to the new surface"

  (swapchain, imageExtent, framebuffers, imageViews, images, swapchainFormat, releaseSwapchain) <-
    allocSwapchainResources windowSize NULL_HANDLE surface

  renderPass <- snd <$> Pipeline.createRenderPass swapchainFormat
  pipeline                     <- snd <$> createPipeline renderPass
  (juliaPipeline, juliaPipelineLayout, juliaDSets) <- juliaPipeline imageViews

  (_, imageAvailableSemaphore) <- withSemaphore' zero
  (_, renderFinishedSemaphore) <- withSemaphore' zero

  currentPresented             <- newEmptyMVar
  lastPresented                <- newMVar ()
  secondLastPresented          <- newMVar ()
  thirdLastPresented           <- newMVar ()

  start                        <- liftIO getMonotonicTimeNSec

  frameResources <- allocate createInternalState closeInternalState
  fences                       <- newIORef mempty

  pure
    (Frame 0
           window
           surface
           swapchain
           swapchainFormat
           renderPass
           imageExtent
           imageAvailableSemaphore
           renderFinishedSemaphore
           pipeline
           juliaPipeline
           juliaPipelineLayout
           ((juliaDSets V.!) . fromIntegral)
           ((images V.!) . fromIntegral)
           ((imageViews V.!) . fromIntegral)
           ((framebuffers V.!) . fromIntegral)
           releaseSwapchain
           currentPresented
           lastPresented
           secondLastPresented
           thirdLastPresented
           start
           frameResources
           fences
    )

-- | Process a single frame, returning Nothing if we should exit.
frame :: Frame -> V (Maybe Frame)
frame f = shouldQuit (TimeLimit 6) >>= \case
  True  -> pure Nothing
  False -> do
    -- Wait for the second previous frame to have finished presenting so the
    -- CPU doesn't get too far ahead.
    readMVar (fSecondLastPresented f)

    f                 <- startFrame f

    -- Render this frame
    needsNewSwapchain <- threwSwapchainError $ runFrame f draw

    -- Advance the frame, recreating the swapchain if necessary
    f' <- advanceFrame =<< bool pure recreateSwapchain needsNewSwapchain f

      -- Print out frame timing info
    endTime           <- liftIO getMonotonicTimeNSec
    let
      frameTimeNSec       = realToFrac (endTime - fStartTime f) :: Double
      targetHz            = 60
      frameTimeBudgetMSec = recip targetHz * 1e3
      frameTimeMSec       = frameTimeNSec / 1e6
      frameBudgetPercent =
        ceiling (100 * frameTimeMSec / frameTimeBudgetMSec) :: Int
    when (frameBudgetPercent > 50) $ sayErrString
      (show frameTimeMSec <> "ms \t" <> show frameBudgetPercent <> "%")

    pure $ Just f'

-- | Set the frame start time
startFrame :: Frame -> V Frame
startFrame f = do
  start <- liftIO getMonotonicTimeNSec
  pure f { fStartTime = start }

-- | Shuffle along previous frames's info and make per-frame resources
advanceFrame :: Frame -> V Frame
advanceFrame f = do
  nextPresented <- newEmptyMVar
  resources     <- allocate createInternalState closeInternalState
  fences        <- newIORef mempty
  pure f { fIndex               = succ (fIndex f)
         , fCurrentPresented    = nextPresented
         , fLastPresented       = fCurrentPresented f
         , fSecondLastPresented = fLastPresented f
         , fThirdLastPresented  = fSecondLastPresented f
         , fResources           = resources
         , fGPUWork             = fences
         }

-- | Submit GPU commands for a frame
draw :: F (Fence, ())
draw = do
  Frame {..} <- askFrame

  imageIndex <-
    acquireNextImageKHR' fSwapchain 1e9 fImageAvailableSemaphore zero >>= \case
      (SUCCESS, imageIndex) -> pure imageIndex
      (TIMEOUT, _) -> throwString "Couldn't acquire next image after 1 second"
      _ -> throwString "Unexpected Result from acquireNextImageKHR"

  let image = fImages imageIndex
  let imageSubresourceRange = ImageSubresourceRange
        { aspectMask     = IMAGE_ASPECT_COLOR_BIT
        , baseMipLevel   = 0
        , levelCount     = 1
        , baseArrayLayer = 0
        , layerCount     = 1
        }
  let Extent2D imageWidth imageHeight = fImageExtent

  -- Make sure we don't destroy the swapchain until at least this frame has
  -- finished GPU execution.
  frameRefCount fReleaseSwapchain

  commandPool <- frameCommandPool
  let commandBufferAllocateInfo = zero { commandPool = commandPool
                                       , level = COMMAND_BUFFER_LEVEL_PRIMARY
                                       , commandBufferCount = 1
                                       }

  -- The command buffer will be freed when the frame is retired
  [commandBuffer] <- allocateCommandBuffers' commandBufferAllocateInfo

  updateDescriptorSets'
    [ SomeStruct zero
        { dstSet          = fJuliaDescriptorSets imageIndex
        , dstBinding      = 0
        , descriptorType  = DESCRIPTOR_TYPE_STORAGE_IMAGE
        , descriptorCount = 1
        , imageInfo = [ DescriptorImageInfo { sampler = NULL_HANDLE
                                            , imageView = fImageViews imageIndex
                                            , imageLayout = IMAGE_LAYOUT_GENERAL
                                            }
                      ]
        }
    ]
    []

  let julia = True
  useCommandBuffer' commandBuffer
                    zero { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    $ if julia
        then do
          -- Transition image to general, to write from the compute shader
          cmdPipelineBarrier
            commandBuffer
            PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
            PIPELINE_STAGE_COMPUTE_SHADER_BIT
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

          cmdBindPipeline' PIPELINE_BIND_POINT_COMPUTE fJuliaPipeline

          -- Get the mouse position in the window (in [-1..1]) and send it as a
          -- push constant.
          P m <- SDL.getAbsoluteMouseLocation
          let m' :: V2 Float
              m' = fmap realToFrac m
                / fmap realToFrac (V2 imageWidth imageHeight)
              c :: V2 Float
              c             = (m' * 2) - 1
              r             = 0.5 * (1 + sqrt (4 * norm c + 1))
              imageSizeF    = realToFrac <$> V2 imageWidth imageHeight
              aspect = pure (recip (min (imageSizeF ^. _x) (imageSizeF ^. _y)))
              frameScale    = aspect * 2 * pure r
              frameOffset   = negate (imageSizeF * aspect) * pure r
              constantBytes = 4 * (2 + 2 + 2 + 1)
              escapeRadius  = 12 :: Float
          allocaBytes constantBytes $ \p -> do
            liftIO $ poke (p `plusPtr` 0) frameScale
            liftIO $ poke (p `plusPtr` 8) frameOffset
            liftIO $ poke (p `plusPtr` 16) c
            liftIO $ poke (p `plusPtr` 24) escapeRadius
            cmdPushConstants' fJuliaPipelineLayout
                              SHADER_STAGE_COMPUTE_BIT
                              0
                              constantBytes
                              p
          cmdBindDescriptorSets' PIPELINE_BIND_POINT_COMPUTE
                                 fJuliaPipelineLayout
                                 0
                                 [fJuliaDescriptorSets imageIndex]
                                 []
          cmdDispatch'
            ((imageWidth + juliaWorkgroupX - 1) `quot` juliaWorkgroupX)
            ((imageHeight + juliaWorkgroupY - 1) `quot` juliaWorkgroupY)
            1

          -- Transition image back to present
          cmdPipelineBarrier
            commandBuffer
            PIPELINE_STAGE_COMPUTE_SHADER_BIT
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
        else do
          let renderPassBeginInfo = zero
                { renderPass  = fRenderPass
                , framebuffer = fFramebuffers imageIndex
                , renderArea  = Rect2D zero fImageExtent
                , clearValues = [Color (Float32 0.1 0.1 0.1 1)]
                }
          cmdSetViewport'
            0
            [ Viewport { x        = 0
                       , y        = 0
                       , width    = realToFrac imageWidth
                       , height   = realToFrac imageHeight
                       , minDepth = 0
                       , maxDepth = 1
                       }
            ]
          cmdSetScissor'
            0
            [Rect2D { offset = Offset2D 0 0, extent = fImageExtent }]
          cmdUseRenderPass' renderPassBeginInfo SUBPASS_CONTENTS_INLINE $ do
            cmdBindPipeline' PIPELINE_BIND_POINT_GRAPHICS fPipeline
            cmdDraw' 3 1 0 0

  let submitInfo = zero
        { waitSemaphores   = [fImageAvailableSemaphore]
        , waitDstStageMask = [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
        , commandBuffers   = [commandBufferHandle commandBuffer]
        , signalSemaphores = [fRenderFinishedSemaphore]
        }
  graphicsQueue    <- getGraphicsQueue
  (_, renderFence) <- withFence' zero
  queueSubmitFrame graphicsQueue [SomeStruct submitInfo] renderFence

  let presentInfo = zero { waitSemaphores = [fRenderFinishedSemaphore]
                         , swapchains     = [fSwapchain]
                         , imageIndices   = [imageIndex]
                         }
  _ <- queuePresentKHR graphicsQueue presentInfo

  pure (renderFence, ())

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- | Print a string if something is slow
_time :: MonadIO m => String -> m a -> m a
_time n a = do
  t1 <- liftIO getMonotonicTimeNSec
  r  <- a
  t2 <- liftIO getMonotonicTimeNSec
  let d = t2 - t1
      t = 3e6
  when (d >= t) $ sayErrString (n <> ": " <> show (realToFrac d / 1e6 :: Float))
  pure r
