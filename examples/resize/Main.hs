{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main
  ( main
  ) where

import           Control.Exception              ( handle )
import           Control.Lens.Getter
import           Control.Monad                  ( when )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.IORef
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Frame                          ( Frame(..)
                                                , advanceFrame
                                                , frameInstanceRequirements
                                                , initialFrame
                                                , queueSubmitFrame
                                                , runFrame
                                                )
import qualified Framebuffer
import           GHC.Clock                      ( getMonotonicTimeNSec )
import           Init                           ( createVMA
                                                , deviceRequirements
                                                , myApiVersion
                                                )
import           InitDevice                     ( withGraphicsPresentDevice )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Julia                          ( JuliaPipeline(..)
                                                , createJuliaDescriptorSets
                                                , createJuliaPipeline
                                                , juliaWorkgroupX
                                                , juliaWorkgroupY
                                                )
import           Linear.Affine                  ( Point(..) )
import           Linear.Metric                  ( norm )
import           Linear.V2
import qualified Pipeline
import           RefCounted                     ( RefCounted
                                                , newRefCounted
                                                , releaseRefCounted
                                                )
import qualified SDL
import           Say
import           Data.Word                      ( Word64 )
import           Swapchain                      ( Swapchain(..)
                                                , allocSwapchain
                                                , recreateSwapchain
                                                , threwSwapchainError
                                                )
import           UnliftIO.Exception             ( displayException
                                                , throwIO
                                                , throwString
                                                )
import           UnliftIO.Foreign               ( allocaBytes
                                                , plusPtr
                                                , poke
                                                )
import           Utils                          ( loopJust )
import           VkResources                    ( Queues(..)
                                                , RecycledResources(..)
                                                , VkResources(..)
                                                , mkVkResources
                                                )

import           Vulkan.CStruct.Extends         ( SomeStruct(..)
                                                , pattern (:&)
                                                , pattern (::&)
                                                )
import           Vulkan.Core10                 as Vk
                                         hiding ( createDevice
                                                , createFramebuffer
                                                , createImageView
                                                , createInstance
                                                , withBuffer
                                                , withImage
                                                )
import qualified Vulkan.Core10                 as CommandBufferBeginInfo (CommandBufferBeginInfo(..))
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import           Vulkan.Exception
import           Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR(..))
import           Vulkan.Extensions.VK_KHR_swapchain
                                               as Swap
import           Vulkan.Zero
import qualified Vulkan.Utils.Init.SDL2        as Init
import           Window.SDL2                    ( RefreshLimit(..)
                                                , createSurface
                                                , createWindow
                                                , drawableSize
                                                , shouldQuit
                                                , withSDL
                                                )

----------------------------------------------------------------
-- Main
----------------------------------------------------------------
main :: IO ()
main = prettyError . runResourceT $ do
  withSDL

  let initWidth  = 1280
      initHeight = 720

  sdlWindow <- createWindow "Haskell ❤️ Vulkan" initWidth initHeight
  inst      <- Init.withInstance
    sdlWindow
    (Just zero { applicationName = Nothing, apiVersion = myApiVersion })
    frameInstanceRequirements
    []
  (_, surface) <- createSurface inst sdlWindow
  (phys, dev, qfi, graphicsQueue) <-
    withGraphicsPresentDevice inst surface deviceRequirements
  vma   <- createVMA inst phys dev
  props <- getPhysicalDeviceProperties phys
  sayErr $ "Using device: " <> decodeUtf8 (deviceName props)

  let qs = Queues (qfi, graphicsQueue)
  vr <- liftIO $ mkVkResources inst phys dev vma qs

  -- Initial swapchain at the requested size.
  let initialSize = Extent2D initWidth initHeight
  initialSC <- allocSwapchain vr NULL_HANDLE initialSize surface

  -- Long-lived render setup. Both the graphics pipeline (currently dormant)
  -- and the Julia compute pipeline are created up front.
  (_, renderPass) <- Pipeline.createRenderPass dev (SurfaceFormatKHR.format (sFormat initialSC))
  -- (_, pipeline)   <- Pipeline.createPipeline dev renderPass
  juliaPL         <- createJuliaPipeline dev

  -- Per-swapchain bindings: framebuffers + Julia descriptor sets, both pinned
  -- to the current swapchain images.
  initialBindings <- createBindings dev renderPass juliaPL initialSC

  scRef       <- liftIO $ newIORef initialSC
  bindingsRef <- liftIO $ newIORef initialBindings

  initial <- initialFrame vr initialSC
  SDL.showWindow sdlWindow

  let
    perFrame f = do
      currentSC <- liftIO $ readIORef scRef
      bindings  <- liftIO $ readIORef bindingsRef
      let f' = f { fSwapchain = currentSC }
      startNs <- liftIO getMonotonicTimeNSec
      needsNew <- threwSwapchainError $ liftIO $ runFrame vr f' $
        renderJulia vr juliaPL bindings f'
      sc' <- if needsNew
        then do
          newSize <- liftIO $ drawableSize sdlWindow
          sc'     <- recreateSwapchain vr newSize currentSC
          newBindings <- createBindings dev renderPass juliaPL sc'
          liftIO $ writeIORef scRef sc'
          dropBindings =<< liftIO (readIORef bindingsRef)
          liftIO $ writeIORef bindingsRef newBindings
          pure sc'
        else pure currentSC
      endNs <- liftIO getMonotonicTimeNSec
      reportFrameTime (endNs - startNs)
      advanceFrame vr sc' f'

    loop f = shouldQuit (TimeLimit 6) >>= \case
      True  -> pure Nothing
      False -> Just <$> perFrame f

  loopJust loop initial

prettyError :: IO () -> IO ()
prettyError =
  handle (\e@(VulkanException _) -> sayErrString (displayException e))

----------------------------------------------------------------
-- Per-swapchain bindings
----------------------------------------------------------------

data Bindings = Bindings
  { bFramebuffers         :: Vector Framebuffer
  , bReleaseFramebuffers  :: RefCounted
  , bJuliaDescriptorSets  :: Vector DescriptorSet
  , bReleaseJuliaDescSets :: RefCounted
  }

createBindings
  :: MonadResource m
  => Device
  -> RenderPass
  -> JuliaPipeline
  -> Swapchain
  -> m Bindings
createBindings dev renderPass jp sc = do
  -- Framebuffers (one per swapchain image) for the dormant graphics pipeline.
  (framebuffers, fbRel) <-
    Framebuffer.createFramebuffers dev renderPass (sImageViews sc) (sExtent sc)

  -- Julia descriptor sets (one per swapchain image).
  juliaSets <- createJuliaDescriptorSets
    dev
    (jpDescriptorSetLayout jp)
    (sImageViews sc)
  -- The whole pool is freed when its allocate-frame closes; mirror that with
  -- a dummy refcount so swapping bindings releases the previous pool.
  (poolKey, _) <- allocate (pure ()) (\_ -> pure ())
  poolRel      <- newRefCounted (release poolKey)

  pure Bindings
    { bFramebuffers         = framebuffers
    , bReleaseFramebuffers  = fbRel
    , bJuliaDescriptorSets  = juliaSets
    , bReleaseJuliaDescSets = poolRel
    }

dropBindings :: MonadIO m => Bindings -> m ()
dropBindings b = do
  releaseRefCounted (bReleaseFramebuffers b)
  releaseRefCounted (bReleaseJuliaDescSets b)

----------------------------------------------------------------
-- Per-frame rendering
----------------------------------------------------------------

renderJulia
  :: VkResources
  -> JuliaPipeline
  -> Bindings
  -> Frame
  -> ResourceT IO ()
renderJulia vr jp bindings f = do
  let RecycledResources {..} = fRecycled f
      sc                     = fSwapchain f
      Queues (_, gQ)         = vrQueues vr
      dev                    = vrDevice vr
      oneSecond              = 1e9
      Extent2D imageWidth imageHeight = sExtent sc
      imageSubresourceRange  = ImageSubresourceRange
        { aspectMask     = IMAGE_ASPECT_COLOR_BIT
        , baseMipLevel   = 0
        , levelCount     = 1
        , baseArrayLayer = 0
        , layerCount     = 1
        }

  (acquireResult, imageIndex) <-
    acquireNextImageKHRSafe dev (sSwapchain sc) oneSecond rrImageAvailable NULL_HANDLE
      >>= \case
            r@(SUCCESS,        _) -> pure r
            r@(SUBOPTIMAL_KHR, _) -> pure r
            (TIMEOUT, _) -> throwString "Couldn't acquire next image after 1 second"
            _ -> throwString "Unexpected Result from acquireNextImageKHR"

  let image = sImages sc V.! fromIntegral imageIndex
      descriptorSet = bJuliaDescriptorSets bindings V.! fromIntegral imageIndex

  -- Allocate a per-frame command buffer from the recycled pool.
  (_, ~[commandBuffer]) <- withCommandBuffers
    dev
    zero { commandPool        = rrCommandPool
         , level              = COMMAND_BUFFER_LEVEL_PRIMARY
         , commandBufferCount = 1
         }
    allocate

  let julia = True
  useCommandBuffer
      commandBuffer
      zero { CommandBufferBeginInfo.flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    $ if julia
        then do
          -- Transition image to general (compute write target).
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

          cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_COMPUTE (jpPipeline jp)

          -- Mouse-driven push constants.
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
            cmdPushConstants commandBuffer
                             (jpPipelineLayout jp)
                             SHADER_STAGE_COMPUTE_BIT
                             0
                             (fromIntegral constantBytes)
                             p
          cmdBindDescriptorSets commandBuffer
                                PIPELINE_BIND_POINT_COMPUTE
                                (jpPipelineLayout jp)
                                0
                                [descriptorSet]
                                []
          cmdDispatch
            commandBuffer
            ((imageWidth + juliaWorkgroupX - 1) `quot` juliaWorkgroupX)
            ((imageHeight + juliaWorkgroupY - 1) `quot` juliaWorkgroupY)
            1

          -- Transition image back to present.
          cmdPipelineBarrier
            commandBuffer
            PIPELINE_STAGE_COMPUTE_SHADER_BIT
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
          -- Dormant graphics pipeline path; preserved for reference.
          let renderPassBeginInfo = zero
                { renderPass  = NULL_HANDLE -- intentionally invalid; see note
                , framebuffer = bFramebuffers bindings V.! fromIntegral imageIndex
                , renderArea  = Rect2D zero (sExtent sc)
                , clearValues = [Color (Float32 0.1 0.1 0.1 1)]
                }
          cmdSetViewport commandBuffer 0
            [ Viewport { x        = 0
                       , y        = 0
                       , width    = realToFrac imageWidth
                       , height   = realToFrac imageHeight
                       , minDepth = 0
                       , maxDepth = 1
                       }
            ]
          cmdSetScissor commandBuffer 0
            [Rect2D { offset = Offset2D 0 0, extent = sExtent sc }]
          cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE $ do
            cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS NULL_HANDLE
            cmdDraw commandBuffer 3 1 0 0

  -- Submit (and record GPU work for the wait thread).
  let submitInfo =
        zero { Vk.waitSemaphores     = [rrImageAvailable]
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

  case (acquireResult, presentResult) of
    (SUBOPTIMAL_KHR, _) -> liftIO . throwIO $ VulkanException ERROR_OUT_OF_DATE_KHR
    (_, SUBOPTIMAL_KHR) -> liftIO . throwIO $ VulkanException ERROR_OUT_OF_DATE_KHR
    _                   -> pure ()

----------------------------------------------------------------
-- Frame timing
----------------------------------------------------------------

reportFrameTime :: MonadIO m => Word64 -> m ()
reportFrameTime nsec = do
  let frameTimeNSec       = realToFrac nsec :: Double
      targetHz            = 60
      frameTimeBudgetMSec = recip targetHz * 1e3
      frameTimeMSec       = frameTimeNSec / 1e6
      frameBudgetPercent  = ceiling (100 * frameTimeMSec / frameTimeBudgetMSec) :: Int
  when (frameBudgetPercent > 50) $
    sayErrString (show frameTimeMSec <> "ms \t" <> show frameBudgetPercent <> "%")
