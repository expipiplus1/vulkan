{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import Control.Exception (handle)
import Control.Lens.Getter ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word64)
import Julia (JuliaPipeline (..), createJuliaDescriptorSets, createJuliaPipeline, juliaWorkgroupX, juliaWorkgroupY)
import Linear.Affine (Point (..))
import Linear.Metric (norm)
import Linear.V2
import qualified SDL
import Say (sayErrString)
import UnliftIO.Exception (displayException)
import UnliftIO.Foreign (allocaBytes, plusPtr, poke)
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Exception
import Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame, recordCommands)
import qualified Vulkan.Utils.Framebuffer as Framebuffer
import qualified Vulkan.Utils.RenderPass as RenderPass
import Vulkan.Utils.Swapchain (Swapchain (..), SwapchainConfig (..), defaultSwapchainConfig)
import Vulkan.Utils.VulkanContext (VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, runWindowLoop)
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
  SDL.showWindow sdlWindow

  (vc, _vma, initialSC) <-
    withWindowedVk
      WindowedConfig
        { wcAppName = "Haskell ❤️ Vulkan"
        , wcInstanceReqs = []
        , wcDeviceReqs = []
        , wcVmaFlags = zero
        , wcSwapchainConfig =
            defaultSwapchainConfig
              { scRequiredUsageFlags =
                  [Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT, Vk.IMAGE_USAGE_STORAGE_BIT]
              , scRequiredFormatFeatures = [Vk.FORMAT_FEATURE_STORAGE_IMAGE_BIT]
              }
        }
      (sdl2Adapter sdlWindow)
  let dev = vcDevice vc

  (_, renderPass) <-
    RenderPass.createColorRenderPass
      dev
      (SurfaceFormatKHR.format (sFormat initialSC))
      Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
  juliaPL <- createJuliaPipeline dev

  runWindowLoop
    vc
    initialSC
    (drawableSize sdlWindow)
    (shouldQuit sdlWindow)
    WindowLoop
      { wlMkState = createBindings dev renderPass juliaPL
      , wlRender = \bindings f -> renderJulia vc juliaPL bindings f
      , wlOnFrame = \start end -> reportFrameTime (end - start)
      , wlOnExit = noOnExit
      }

prettyError :: IO () -> IO ()
prettyError =
  handle (\e@(VulkanException _) -> sayErrString (displayException e))

----------------------------------------------------------------
-- Per-swapchain bindings
----------------------------------------------------------------

data Bindings = Bindings
  { bFramebuffers :: Vector Vk.Framebuffer
  , bJuliaDescriptorSets :: Vector Vk.DescriptorSet
  }

createBindings
  :: Vk.Device
  -> Vk.RenderPass
  -> JuliaPipeline
  -> Swapchain
  -> ResourceT IO (Bindings, ReleaseKey)
createBindings dev renderPass jp sc = do
  -- Framebuffers (one per swapchain image) for the dormant graphics pipeline.
  (framebuffers, fbKey) <-
    Framebuffer.createFramebuffers dev renderPass (sImageViews sc) (sExtent sc)

  -- Julia descriptor sets (one per swapchain image).
  juliaSets <-
    createJuliaDescriptorSets
      dev
      (jpDescriptorSetLayout jp)
      (sImageViews sc)

  pure
    ( Bindings
        { bFramebuffers = framebuffers
        , bJuliaDescriptorSets = juliaSets
        }
    , fbKey
    )

----------------------------------------------------------------
-- Per-frame rendering
----------------------------------------------------------------

renderJulia
  :: VulkanContext
  -> JuliaPipeline
  -> Bindings
  -> Frame
  -> ResourceT IO ()
renderJulia vc jp bindings f = do
  (acquireResult, imageIndex) <- acquireFrameImage vc f
  let
    image = sImages sc V.! fromIntegral imageIndex
    descriptorSet = bJuliaDescriptorSets bindings V.! fromIntegral imageIndex

  commands <- recordCommands vc f \cb -> do
    -- Transition image to general (compute write target).
    Vk.cmdPipelineBarrier
      cb
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

    Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE (jpPipeline jp)

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
        cb
        (jpPipelineLayout jp)
        Vk.SHADER_STAGE_COMPUTE_BIT
        0
        constantBytes
        p
    Vk.cmdBindDescriptorSets
      cb
      Vk.PIPELINE_BIND_POINT_COMPUTE
      (jpPipelineLayout jp)
      0
      [descriptorSet]
      []
    Vk.cmdDispatch
      cb
      ((imageWidth + juliaWorkgroupX - 1) `quot` juliaWorkgroupX)
      ((imageHeight + juliaWorkgroupY - 1) `quot` juliaWorkgroupY)
      1

    -- Transition image back to present.
    Vk.cmdPipelineBarrier
      cb
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

  queueSubmitFrame vc f [commands]
  presentFrameImage vc f acquireResult imageIndex
  where
    sc = fSwapchain f
    Vk.Extent2D imageWidth imageHeight = sExtent sc
    imageSubresourceRange =
      Vk.ImageSubresourceRange
        { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
        , Vk.baseMipLevel = 0
        , Vk.levelCount = 1
        , Vk.baseArrayLayer = 0
        , Vk.layerCount = 1
        }

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
