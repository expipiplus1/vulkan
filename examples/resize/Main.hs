{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main
  ( main
  ) where

import Control.Exception (handle)
import Control.Lens.Getter ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.IORef
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import Frame (Frame (..), advanceFrame, initialFrame, queueSubmitFrame, runFrame)
import qualified Framebuffer
import GHC.Clock (getMonotonicTimeNSec)
import Julia (JuliaPipeline (..), createJuliaDescriptorSets, createJuliaPipeline, juliaWorkgroupX, juliaWorkgroupY)
import Linear.Affine (Point (..))
import Linear.Metric (norm)
import Linear.V2

-- import qualified Pipeline
import RefCounted (RefCounted, newRefCounted, releaseRefCounted)
import qualified RenderPass
import qualified SDL
import Say
import Swapchain (Swapchain (..), recreateSwapchain, threwSwapchainError)
import UnliftIO.Exception (displayException, throwIO, throwString)
import UnliftIO.Foreign (allocaBytes, plusPtr, poke)
import Utils (loopJust)
import VkResources (Queues (..), RecycledResources (..), VkResources (..))
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import Vulkan.Exception
import Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))
import Vulkan.Extensions.VK_KHR_swapchain as KHR
import Vulkan.Zero (zero)
import Window.SDL2 (createWindow, drawableSize, sdl2Adapter, shouldQuit, withSDL)
import WindowedBoot (WindowedConfig (..), withWindowedVk)

main :: IO ()
main = prettyError . runResourceT $ do
  withSDL

  let
    initWidth = 1280
    initHeight = 720

  sdlWindow <- createWindow "Haskell ❤️ Vulkan" initWidth initHeight
  (vr, initialSC) <-
    withWindowedVk
      WindowedConfig
        { wcAppName = "Haskell ❤️ Vulkan"
        , wcInstanceReqs = []
        , wcDeviceReqs = []
        , wcVmaFlags = zero
        }
      (sdl2Adapter sdlWindow)
  let dev = vrDevice vr

  (_, renderPass) <-
    RenderPass.createColorRenderPass
      dev
      (SurfaceFormatKHR.format (sFormat initialSC))
      Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
  juliaPL <- createJuliaPipeline dev

  -- Per-swapchain bindings: framebuffers + Julia descriptor sets, both pinned
  -- to the current swapchain images.
  initialBindings <- createBindings dev renderPass juliaPL initialSC

  scRef <- liftIO $ newIORef initialSC
  bindingsRef <- liftIO $ newIORef initialBindings

  initial <- initialFrame vr initialSC
  SDL.showWindow sdlWindow

  let
    perFrame f = do
      currentSC <- liftIO $ readIORef scRef
      bindings <- liftIO $ readIORef bindingsRef
      let f' = f{fSwapchain = currentSC}
      startNs <- liftIO getMonotonicTimeNSec
      needsNew <-
        threwSwapchainError $
          liftIO $
            runFrame vr f' $
              renderJulia vr juliaPL bindings f'
      sc' <-
        if needsNew
          then do
            newSize <- liftIO $ drawableSize sdlWindow
            sc' <- recreateSwapchain vr newSize currentSC
            newBindings <- createBindings dev renderPass juliaPL sc'
            liftIO $ writeIORef scRef sc'
            dropBindings =<< liftIO (readIORef bindingsRef)
            liftIO $ writeIORef bindingsRef newBindings
            pure sc'
          else pure currentSC
      endNs <- liftIO getMonotonicTimeNSec
      reportFrameTime (endNs - startNs)
      advanceFrame vr sc' f'

    loop f =
      shouldQuit sdlWindow >>= \case
        True -> pure Nothing
        False -> Just <$> perFrame f

  loopJust loop initial

prettyError :: IO () -> IO ()
prettyError =
  handle (\e@(VulkanException _) -> sayErrString (displayException e))

----------------------------------------------------------------
-- Per-swapchain bindings
----------------------------------------------------------------

data Bindings = Bindings
  { bFramebuffers :: Vector Vk.Framebuffer
  , bReleaseFramebuffers :: RefCounted
  , bJuliaDescriptorSets :: Vector Vk.DescriptorSet
  , bReleaseJuliaDescSets :: RefCounted
  }

createBindings
  :: (MonadResource m)
  => Vk.Device
  -> Vk.RenderPass
  -> JuliaPipeline
  -> Swapchain
  -> m Bindings
createBindings dev renderPass jp sc = do
  -- Framebuffers (one per swapchain image) for the dormant graphics pipeline.
  (framebuffers, fbRel) <-
    Framebuffer.createFramebuffers dev renderPass (sImageViews sc) (sExtent sc)

  -- Julia descriptor sets (one per swapchain image).
  juliaSets <-
    createJuliaDescriptorSets
      dev
      (jpDescriptorSetLayout jp)
      (sImageViews sc)
  -- The whole pool is freed when its allocate-frame closes; mirror that with
  -- a dummy refcount so swapping bindings releases the previous pool.
  (poolKey, _) <- allocate (pure ()) (\_ -> pure ())
  poolRel <- newRefCounted (release poolKey)

  pure
    Bindings
      { bFramebuffers = framebuffers
      , bReleaseFramebuffers = fbRel
      , bJuliaDescriptorSets = juliaSets
      , bReleaseJuliaDescSets = poolRel
      }

dropBindings :: (MonadIO m) => Bindings -> m ()
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
  let
    RecycledResources{..} = fRecycled f
    sc = fSwapchain f
    gQ = snd (qGraphics (vrQueues vr))
    dev = vrDevice vr
    oneSecond = 1e9
    Vk.Extent2D imageWidth imageHeight = sExtent sc
    imageSubresourceRange =
      Vk.ImageSubresourceRange
        { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
        , Vk.baseMipLevel = 0
        , Vk.levelCount = 1
        , Vk.baseArrayLayer = 0
        , Vk.layerCount = 1
        }

  (acquireResult, imageIndex) <-
    acquireNextImageKHRSafe dev (sSwapchain sc) oneSecond rrImageAvailable Vk.NULL_HANDLE >>= \case
      r@(Vk.SUCCESS, _) -> pure r
      r@(Vk.SUBOPTIMAL_KHR, _) -> pure r
      (Vk.TIMEOUT, _) -> throwString "Couldn't acquire next image after 1 second"
      _ -> throwString "Unexpected Result from acquireNextImageKHR"

  let
    image = sImages sc V.! fromIntegral imageIndex
    descriptorSet = bJuliaDescriptorSets bindings V.! fromIntegral imageIndex

  -- Allocate a per-frame command buffer from the recycled pool.
  (_, [commandBuffer]) <-
    Vk.withCommandBuffers
      dev
      zero
        { Vk.commandPool = rrCommandPool
        , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
      allocate

  Vk.useCommandBuffer commandBuffer zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} do
    -- Transition image to general (compute write target).
    Vk.cmdPipelineBarrier
      commandBuffer
      Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
      zero
      []
      []
      [ SomeStruct
          zero
            { Vk.srcAccessMask = zero
            , Vk.dstAccessMask = Vk.ACCESS_SHADER_WRITE_BIT
            , Vk.oldLayout = Vk.IMAGE_LAYOUT_UNDEFINED
            , Vk.newLayout = Vk.IMAGE_LAYOUT_GENERAL
            , Vk.image = image
            , Vk.subresourceRange = imageSubresourceRange
            }
      ]

    Vk.cmdBindPipeline commandBuffer Vk.PIPELINE_BIND_POINT_COMPUTE (jpPipeline jp)

    -- Mouse-driven push constants.
    P m <- SDL.getAbsoluteMouseLocation
    let
      m' :: V2 Float
      m' = fmap realToFrac m / imageSizeF
      c :: V2 Float
      c = (m' * 2) - 1
      r = 0.5 * (1 + sqrt (4 * norm c + 1))
      imageSizeF = realToFrac <$> V2 imageWidth imageHeight
      aspect = pure (recip (min (imageSizeF ^. _x) (imageSizeF ^. _y)))
      frameScale = aspect * 2 * pure r
      frameOffset = negate (imageSizeF * aspect) * pure r
      constantBytes = 4 * (2 + 2 + 2 + 1)
      escapeRadius = 12 :: Float
    allocaBytes constantBytes $ \p -> do
      liftIO $ poke (p `plusPtr` 0) frameScale
      liftIO $ poke (p `plusPtr` 8) frameOffset
      liftIO $ poke (p `plusPtr` 16) c
      liftIO $ poke (p `plusPtr` 24) escapeRadius
      Vk.cmdPushConstants
        commandBuffer
        (jpPipelineLayout jp)
        Vk.SHADER_STAGE_COMPUTE_BIT
        0
        constantBytes
        p
    Vk.cmdBindDescriptorSets
      commandBuffer
      Vk.PIPELINE_BIND_POINT_COMPUTE
      (jpPipelineLayout jp)
      0
      [descriptorSet]
      []
    Vk.cmdDispatch
      commandBuffer
      ((imageWidth + juliaWorkgroupX - 1) `quot` juliaWorkgroupX)
      ((imageHeight + juliaWorkgroupY - 1) `quot` juliaWorkgroupY)
      1

    -- Transition image back to present.
    Vk.cmdPipelineBarrier
      commandBuffer
      Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
      Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
      zero
      []
      []
      [ SomeStruct
          zero
            { Vk.srcAccessMask = Vk.ACCESS_SHADER_WRITE_BIT
            , Vk.dstAccessMask = zero
            , Vk.oldLayout = Vk.IMAGE_LAYOUT_GENERAL
            , Vk.newLayout = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
            , Vk.image = image
            , Vk.subresourceRange = imageSubresourceRange
            }
      ]

  -- Submit (and record GPU work for the wait thread).
  let submitInfo =
        zero
          { Vk.waitSemaphores = [rrImageAvailable]
          , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
          , Vk.commandBuffers = [Vk.commandBufferHandle commandBuffer]
          , Vk.signalSemaphores = [rrRenderFinished, fHostTimeline f]
          }
          ::& zero
            { waitSemaphoreValues = [1]
            , signalSemaphoreValues = [1, fIndex f]
            }
            :& ()
  liftIO $
    queueSubmitFrame
      gQ
      f
      [SomeStruct submitInfo]
      (fHostTimeline f)
      (fIndex f)

  presentResult <-
    queuePresentKHR
      gQ
      zero
        { KHR.waitSemaphores = [rrRenderFinished]
        , KHR.swapchains = [sSwapchain sc]
        , KHR.imageIndices = [imageIndex]
        }

  case (acquireResult, presentResult) of
    (Vk.SUBOPTIMAL_KHR, _) -> liftIO . throwIO $ VulkanException Vk.ERROR_OUT_OF_DATE_KHR
    (_, Vk.SUBOPTIMAL_KHR) -> liftIO . throwIO $ VulkanException Vk.ERROR_OUT_OF_DATE_KHR
    _ -> pure ()

----------------------------------------------------------------
-- Frame timing
----------------------------------------------------------------

reportFrameTime :: (MonadIO m) => Word64 -> m ()
reportFrameTime nsec = do
  let
    frameTimeNSec = realToFrac nsec :: Double
    targetHz = 60
    frameTimeBudgetMSec = recip targetHz * 1e3
    frameTimeMSec = frameTimeNSec / 1e6
    frameBudgetPercent = ceiling (100 * frameTimeMSec / frameTimeBudgetMSec) :: Int
  when (frameBudgetPercent > 50) $
    sayErrString (show frameTimeMSec <> "ms \t" <> show frameBudgetPercent <> "%")
