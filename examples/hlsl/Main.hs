{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.IORef
import Frame (Frame (..), advanceFrame, initialFrame, runFrame)
import qualified Framebuffer
import qualified Pipeline
import RefCounted (releaseRefCounted)
import Render (renderFrame)
import qualified RenderPass
import SDL (showWindow, time)
import Swapchain (Swapchain (..), recreateSwapchain, threwSwapchainError)
import Utils (loopJust)
import VkResources (VkResources (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))
import Vulkan.Zero (zero)
import Window.SDL2 (createWindow, drawableSize, sdl2Adapter, shouldQuit, withSDL)
import WindowedBoot (WindowedConfig (..), withWindowedVk)

main :: IO ()
main = runResourceT $ do
  --
  -- Initialization
  --
  withSDL
  win <- createWindow "Vulkan 🚀 Haskell" 1280 720
  (vr, initialSC) <-
    withWindowedVk
      WindowedConfig
        { wcAppName = "Vulkan 🚀 Haskell"
        , wcInstanceReqs = []
        , wcDeviceReqs = []
        , wcVmaFlags = zero
        }
      (sdl2Adapter win)
  let dev = vrDevice vr
  (_, renderPass) <-
    RenderPass.createColorRenderPass
      dev
      (SurfaceFormatKHR.format (sFormat initialSC))
      Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
  (_, pipeline) <- Pipeline.createPipeline dev renderPass
  initialFBs <- Framebuffer.createFramebuffers dev renderPass (sImageViews initialSC) (sExtent initialSC)

  scRef <- liftIO $ newIORef initialSC
  fbsRef <- liftIO $ newIORef initialFBs

  initial <- initialFrame vr initialSC

  showWindow win
  start <- SDL.time @Double

  let
    perFrame f = do
      currentSC <- liftIO $ readIORef scRef
      (currentFBs, _rel) <- liftIO $ readIORef fbsRef
      let f' = f{fSwapchain = currentSC}
      needsNew <-
        threwSwapchainError $
          liftIO $
            runFrame vr f' $
              renderFrame vr renderPass pipeline currentFBs f'
      sc' <-
        if needsNew
          then do
            newSize <- liftIO $ drawableSize win
            sc' <- recreateSwapchain vr newSize currentSC
            newFBs <- Framebuffer.createFramebuffers dev renderPass (sImageViews sc') (sExtent sc')
            (_oldFbs, oldRel) <- liftIO $ readIORef fbsRef
            releaseRefCounted oldRel
            liftIO $ writeIORef scRef sc'
            liftIO $ writeIORef fbsRef newFBs
            pure sc'
          else pure currentSC
      advanceFrame vr sc' f'

    loop f =
      shouldQuit win >>= \case
        True -> do
          end <- SDL.time
          let fps = realToFrac (fIndex f) / (end - start) :: Double
          liftIO $ putStrLn $ "Average: " <> show fps
          pure Nothing
        False -> Just <$> perFrame f

  loopJust loop initial
