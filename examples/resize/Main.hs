{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main
  ( main
  )
where

import           Control.Exception              ( handle )
import           Control.Monad.Extra            ( unlessM
                                                , when
                                                )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified SDL
import           Say
import           UnliftIO.Exception             ( displayException
                                                , throwString
                                                )
import           UnliftIO.IORef
import           UnliftIO.MVar

import           Data.Bool                      ( bool )
import qualified Data.Vector                   as V
import           GHC.Clock                      ( getMonotonicTimeNSec )

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

import           Frame
import           Init
import           MonadVulkan
import           Swapchain
import           Window

----------------------------------------------------------------
-- Main performs some one time initialization of the windowing system and
-- Vulkan, then it loops generating frames
----------------------------------------------------------------
main :: IO ()
main = prettyError . runResourceT $ do
  -- Start SDL
  _ <- allocate_ (SDL.initialize @[] [SDL.InitEvents]) SDL.quit

  let initWidth  = 1280
      initHeight = 720

  -- Create everything up to the device
  (windowExts, sdlWindow) <- createWindow "Haskell ❤️ Vulkan"
                                          initWidth
                                          initHeight
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


  (swapchain, imageExtent, framebuffers, pipeline, renderPass, releaseSwapchain) <-
    allocSwapchainResources windowSize NULL_HANDLE surface

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
           renderPass
           imageExtent
           imageAvailableSemaphore
           renderFinishedSemaphore
           pipeline
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
frame f = shouldQuit >>= \case
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
  Frame {..}            <- askFrame

  (SUCCESS, imageIndex) <- acquireNextImageKHR' fSwapchain
                                                0
                                                fImageAvailableSemaphore
                                                zero

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

  useCommandBuffer' commandBuffer
                    zero { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
    $ do
        let renderPassBeginInfo = zero
              { renderPass  = fRenderPass
              , framebuffer = fFramebuffers imageIndex
              , renderArea  = Rect2D zero fImageExtent
              , clearValues = [Color (Float32 (0.1, 0.1, 0.1, 1))]
              }
        cmdUseRenderPass commandBuffer
                         renderPassBeginInfo
                         SUBPASS_CONTENTS_INLINE
          $ do
              cmdBindPipeline commandBuffer
                              PIPELINE_BIND_POINT_GRAPHICS
                              fPipeline
              cmdDraw commandBuffer 3 1 0 0

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
-- SDL helpers
----------------------------------------------------------------

shouldQuit :: MonadIO m => m Bool
shouldQuit = maybe False isQuitEvent <$> SDL.pollEvent
 where
  isQuitEvent :: SDL.Event -> Bool
  isQuitEvent = \case
    (SDL.Event _ SDL.QuitEvent) -> True
    SDL.Event _ (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released False (SDL.Keysym _ code _)))
      | code == SDL.KeycodeQ || code == SDL.KeycodeEscape
      -> True
    _ -> False

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

loopJust :: Monad m => (a -> m (Maybe a)) -> a -> m ()
loopJust f x = f x >>= \case
  Nothing -> pure ()
  Just x' -> loopJust f x'

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
