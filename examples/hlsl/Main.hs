{-# LANGUAGE TypeApplications #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Foldable                  ( traverse_ )
import           Data.IORef
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Frame                          ( Frame(..)
                                                , advanceFrame
                                                , initialFrame
                                                , runFrame
                                                )
import qualified Framebuffer
import           Init                           ( createDevice
                                                , createInstance
                                                , createVMA
                                                )
import           RefCounted                     ( RefCounted
                                                , newRefCounted
                                                , releaseRefCounted
                                                )
import           Render                         ( renderFrame )
import qualified RenderPass
import           SDL                            ( showWindow
                                                , time
                                                )
import qualified SDL
import qualified SDL.Video.Vulkan              as SDL
import           Swapchain                      ( Swapchain(..)
                                                , allocSwapchain
                                                , recreateSwapchain
                                                , threwSwapchainError
                                                )
import           Utils                          ( loopJust )
import           VkResources                    ( mkVkResources )
import qualified Pipeline
import           Vulkan.Core10                  ( Device
                                                , Extent2D(..)
                                                , Framebuffer
                                                , RenderPass
                                                , pattern NULL_HANDLE
                                                )
import           Vulkan.Extensions.VK_KHR_surface as SurfaceFormatKHR
                                                ( SurfaceFormatKHR(..) )
import           Window.SDL2                    ( RefreshLimit(..)
                                                , createWindow
                                                , shouldQuit
                                                , withSDL
                                                )

main :: IO ()
main = runResourceT $ do
  --
  -- Initialization
  --
  withSDL
  win <- createWindow "Vulkan 🚀 Haskell" 1280 720
  inst                  <- Init.createInstance win
  (phys, dev, qs, surf) <- Init.createDevice inst win
  vma                   <- createVMA inst phys dev
  vr                    <- liftIO $ mkVkResources inst phys dev vma qs

  -- Initial swapchain
  initialSize          <- liftIO $ drawableSize win
  initialSC            <- allocSwapchain vr NULL_HANDLE initialSize surf
  (_, renderPass)      <- RenderPass.createRenderPass dev (SurfaceFormatKHR.format (sFormat initialSC))
  (_, pipeline)        <- Pipeline.createPipeline dev renderPass
  initialFBs           <- createFramebuffers dev renderPass initialSC

  scRef                <- liftIO $ newIORef initialSC
  fbsRef               <- liftIO $ newIORef initialFBs

  initial              <- initialFrame vr initialSC

  showWindow win
  start <- SDL.time @Double

  let
    perFrame f = do
      currentSC          <- liftIO $ readIORef scRef
      (currentFBs, _rel) <- liftIO $ readIORef fbsRef
      let f' = f { fSwapchain = currentSC }
      needsNew <- threwSwapchainError $ liftIO $ runFrame vr f' $
        renderFrame vr renderPass pipeline currentFBs f'
      sc' <- if needsNew
        then do
          newSize           <- liftIO $ drawableSize win
          sc'               <- recreateSwapchain vr newSize currentSC
          newFBs            <- createFramebuffers dev renderPass sc'
          (_oldFbs, oldRel) <- liftIO $ readIORef fbsRef
          releaseRefCounted oldRel
          liftIO $ writeIORef scRef  sc'
          liftIO $ writeIORef fbsRef newFBs
          pure sc'
        else pure currentSC
      advanceFrame vr sc' f'

    loop f = shouldQuit (TimeLimit 6) >>= \case
      True -> do
        end <- SDL.time
        let fps = realToFrac (fIndex f) / (end - start) :: Double
        liftIO $ putStrLn $ "Average: " <> show fps
        pure Nothing
      False -> Just <$> perFrame f

  loopJust loop initial

drawableSize :: SDL.Window -> IO Extent2D
drawableSize win = do
  SDL.V2 w h <- SDL.vkGetDrawableSize win
  pure $ Extent2D (fromIntegral w) (fromIntegral h)

-- | Build a framebuffer per swapchain image; bundle a 'RefCounted' that
-- frees them all when no in-flight frame still uses them.
createFramebuffers
  :: MonadResource m
  => Device
  -> RenderPass
  -> Swapchain
  -> m (Vector Framebuffer, RefCounted)
createFramebuffers dev rp sc = do
  (keys, fbs) <- fmap V.unzip . V.forM (sImageViews sc) $ \iv ->
    Framebuffer.createFramebuffer dev rp iv (sExtent sc)
  rel <- newRefCounted (traverse_ release keys)
  pure (fbs, rel)
