{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, register, release, runResourceT)
import Data.Foldable (traverse_)
import Render (renderFrame)
import qualified SDL
import UnliftIO (MonadUnliftIO)
import qualified Vulkan.Core10 as Vk
import Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))
import Vulkan.Utils.DynamicState (minimalDynamicStates)
import Vulkan.Utils.Frame (Frame (..))
import qualified Vulkan.Utils.Framebuffer as Framebuffer
import Vulkan.Utils.Init.SDL2.Window (createWindow, drawableSize, sdl2Adapter, shouldQuit, withSDL)
import qualified Vulkan.Utils.RenderPass as RenderPass
import Vulkan.Utils.ShaderQQ.HLSL.Shaderc (frag, vert)
import Vulkan.Utils.Swapchain (Swapchain (..), defaultSwapchainConfig)
import Vulkan.Utils.VulkanContext (VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnFrame, runWindowLoop)
import Vulkan.Zero (zero)
import WindowedBoot (WindowedConfig (..), withWindowedVk)

main :: IO ()
main = runResourceT $ do
  withSDL
  win <- createWindow "Vulkan 🚀 Haskell" 1280 720
  (vc, _vma, initialSC) <-
    withWindowedVk
      WindowedConfig
        { appName = "Vulkan 🚀 Haskell"
        , instanceReqs = []
        , deviceReqs = []
        , vmaFlags = zero
        , swapchainConfig = defaultSwapchainConfig
        }
      (sdl2Adapter win)
  let dev = vcDevice vc
  let colorFormat = SurfaceFormatKHR.format (sFormat initialSC)
  (_, renderPass) <- RenderPass.allocateColorRenderPass dev colorFormat Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
  (_, pipeline) <- createPipeline dev renderPass colorFormat

  SDL.showWindow win
  start <- SDL.time @Double

  runWindowLoop
    vc
    initialSC
    (drawableSize win)
    (shouldQuit win)
    WindowLoop
      { wlMkState = \sc -> do
          framebuffers <-
            traverse (\iv -> Framebuffer.allocateFramebuffer dev renderPass iv (sExtent sc)) (sImageViews sc)
          groupKey <- register (traverse_ (release . fst) framebuffers)
          pure (fmap snd framebuffers, groupKey)
      , wlRender = \fbs f ->
          renderFrame vc renderPass pipeline fbs f
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
  :: (MonadResource m, MonadUnliftIO m, MonadFail m)
  => Vk.Device
  -> Vk.RenderPass
  -> Vk.Format
  -> m (ReleaseKey, Vk.Pipeline)
createPipeline dev renderPass colorFormat =
  RenderPass.allocatePipelineFromShaders
    dev
    renderPass
    zero
      { RenderPass.colorFormats = [colorFormat]
      , RenderPass.dynamicStates = Just minimalDynamicStates
      }
    ()
    [ (Vk.SHADER_STAGE_VERTEX_BIT, vertCode)
    , (Vk.SHADER_STAGE_FRAGMENT_BIT, fragCode)
    ]
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
