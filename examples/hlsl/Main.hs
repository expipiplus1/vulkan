{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, release, runResourceT)
import Render (renderFrame)
import qualified SDL
import VkResources (VkResources (..), vrContext)
import qualified Vulkan.Core10 as Vk
import Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))
import Vulkan.Utils.Frame (Frame (..))
import qualified Vulkan.Utils.Framebuffer as Framebuffer
import qualified Vulkan.Utils.Pipeline as Pipeline
import qualified Vulkan.Utils.RenderPass as RenderPass
import Vulkan.Utils.Shader (shaderStage)
import Vulkan.Utils.ShaderQQ.HLSL.Shaderc (frag, vert)
import Vulkan.Utils.Swapchain (Swapchain (..), defaultSwapchainConfig)
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnFrame, runWindowLoop)
import Vulkan.Zero (zero)
import Window.SDL2 (createWindow, drawableSize, sdl2Adapter, shouldQuit, withSDL)
import WindowedBoot (WindowedConfig (..), withWindowedVk)

main :: IO ()
main = runResourceT $ do
  withSDL
  win <- createWindow "Vulkan 🚀 Haskell" 1280 720
  (vr, initialSC) <-
    withWindowedVk
      WindowedConfig
        { wcAppName = "Vulkan 🚀 Haskell"
        , wcInstanceReqs = []
        , wcDeviceReqs = []
        , wcVmaFlags = zero
        , wcSwapchainConfig = defaultSwapchainConfig
        }
      (sdl2Adapter win)
  let dev = vrDevice vr
  (_, renderPass) <-
    RenderPass.createColorRenderPass
      dev
      (SurfaceFormatKHR.format (sFormat initialSC))
      Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
  (_, pipeline) <- createPipeline dev renderPass

  SDL.showWindow win
  start <- SDL.time @Double

  runWindowLoop
    (vrContext vr)
    initialSC
    (drawableSize win)
    (shouldQuit win)
    WindowLoop
      { wlMkState = \sc ->
          Framebuffer.createFramebuffers dev renderPass (sImageViews sc) (sExtent sc)
      , wlRender = \fbs f -> renderFrame vr renderPass pipeline fbs f
      , wlOnFrame = noOnFrame
      , wlOnExit = \f -> liftIO $ do
          end <- SDL.time
          let fps = realToFrac (fIndex f) / (end - start) :: Double
          putStrLn $ "Average: " <> show fps
      }

----------------------------------------------------------------
-- HLSL pipeline
----------------------------------------------------------------

createPipeline
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> Vk.RenderPass
  -> m (ReleaseKey, Vk.Pipeline)
createPipeline dev renderPass = do
  (vertKey, vertStage) <- shaderStage dev Vk.SHADER_STAGE_VERTEX_BIT vertCode
  (fragKey, fragStage) <- shaderStage dev Vk.SHADER_STAGE_FRAGMENT_BIT fragCode
  (key, pipeline) <- Pipeline.createColorPipeline dev renderPass [vertStage, fragStage]
  release vertKey
  release fragKey
  pure (key, pipeline)
  where
    vertCode =
      [vert|
        const static float2 positions[3] = {
          {0.0, -0.5},
          {0.5, 0.5},
          {-0.5, 0.5}
        };

        const static float3 colors[3] = {
          {1.0, 1.0, 0.0},
          {0.0, 1.0, 1.0},
          {1.0, 0.0, 1.0}
        };

        struct VSOutput
        {
          float4 pos : SV_POSITION;
          [[vk::location(0)]] float3 col;
        };

        VSOutput main(const uint i : SV_VertexID)
        {
          VSOutput output;
          output.pos = float4(positions[i], 0, 1.0);
          output.col = colors[i];
          return output;
        }
      |]
    fragCode =
      [frag|
        float4 main([[vk::location(0)]] const float3 col) : SV_TARGET
        {
            return float4(col, 1);
        }
      |]
