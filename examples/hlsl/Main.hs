{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (runResourceT)
import Data.IORef
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import Frame (Frame (..), advanceFrame, frameDeviceRequirements, frameInstanceRequirements, initialFrame, runFrame)
import qualified Framebuffer
import InitDevice (withDevice)
import qualified Pipeline
import RefCounted (releaseRefCounted)
import Render (renderFrame)
import qualified RenderPass
import SDL (showWindow, time)
import Say (sayErr)
import Swapchain (Swapchain (..), allocSwapchain, recreateSwapchain, threwSwapchainError)
import Utils (loopJust)
import VkResources (mkVkResources)
import qualified Vma
import qualified Vulkan.Core10 as Vk
import Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR (SurfaceFormatKHR (..))
import qualified Vulkan.Utils.Init.SDL2 as VkInit
import Vulkan.Zero (zero)
import Window.SDL2 (RefreshLimit (..), createSurface, createWindow, drawableSize, shouldQuit, withSDL)

main :: IO ()
main = runResourceT $ do
  --
  -- Initialization
  --
  withSDL
  win <- createWindow "Vulkan 🚀 Haskell" 1280 720
  inst <-
    VkInit.withInstance
      win
      (Just zero{Vk.applicationName = Nothing, Vk.apiVersion = myApiVersion})
      frameInstanceRequirements
      []
  (_, surf) <- createSurface inst win
  (phys, dev, qs) <- withDevice inst surf frameDeviceRequirements
  vma <- Vma.createVMA zero myApiVersion inst phys dev
  props <- Vk.getPhysicalDeviceProperties phys
  sayErr $ "Using device: " <> decodeUtf8 (Vk.deviceName props)
  vr <- liftIO $ mkVkResources inst phys dev vma qs

  -- Initial swapchain
  initialSize <- liftIO $ drawableSize win
  initialSC <- allocSwapchain vr Vk.NULL_HANDLE initialSize surf
  (_, renderPass) <- RenderPass.createRenderPass dev (SurfaceFormatKHR.format (sFormat initialSC))
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
      shouldQuit (TimeLimit 6) >>= \case
        True -> do
          end <- SDL.time
          let fps = realToFrac (fIndex f) / (end - start) :: Double
          liftIO $ putStrLn $ "Average: " <> show fps
          pure Nothing
        False -> Just <$> perFrame f

  loopJust loop initial

myApiVersion :: Word32
myApiVersion = Vk.API_VERSION_1_0
