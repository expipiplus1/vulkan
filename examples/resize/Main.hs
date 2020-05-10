{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main
  ( main
  )
where

import           Control.Exception              ( handle
                                                , mask
                                                )
import           Control.Exception.Safe         ( catchJust
                                                , displayException
                                                , finally
                                                , throwString
                                                )
import           Control.Monad.Extra            ( unlessM )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified SDL
import qualified SDL.Video.Vulkan              as SDL
import           Say
import           UnliftIO                       ( toIO )
import           UnliftIO.Async                 ( async
                                                , uninterruptibleCancel
                                                )

import           Data.Foldable                  ( traverse_ )
import           Data.IORef
import qualified Data.Vector                   as V
import           Data.Word

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

import           Framebuffer
import           Init
import           MonadVulkan
import           Pipeline
import           Swapchain
import           Window

----------------------------------------------------------------
-- The program
--
-- Main does the one-time initialization at the start
----------------------------------------------------------------

prettyError :: IO () -> IO ()
prettyError =
  handle (\e@(VulkanException _) -> sayErrString (displayException e))

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
  (_, commandPool) <- withCommandPool dev commandPoolCreateInfo Nothing allocate
  allocator        <- createVMA inst phys dev

  sayErr $ "Using device: " <> devName

  -- Now all the globals are initialized
  runV inst
       phys
       dev
       graphicsQueue
       graphicsQueueFamilyIndex
       commandPool
       allocator
    . (`finally` deviceWaitIdleSafe')
    $ do

        frame <- initialFrame sdlWindow
                              (Just surface)
                              (Extent2D initWidth initHeight)

        SDL.showWindow sdlWindow

        render frame

data Frame = Frame
  { fWindow                  :: SDL.Window
    -- Vulkan items
  , fSurface                 :: SurfaceKHR
  , fSwapchain               :: SwapchainKHR
  , fRenderPass              :: RenderPass
  , fImageExtent             :: Extent2D
  , fImageAvailableSemaphore :: Semaphore
  , fRenderFinishedSemaphore :: Semaphore
  , fPipeline                :: Pipeline
  , fFramebuffers            :: Word32 -> Framebuffer
  , fReleaseSwapchain        :: RefCounted
  }

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
  pure
    (Frame window
           surface
           swapchain
           renderPass
           imageExtent
           imageAvailableSemaphore
           renderFinishedSemaphore
           pipeline
           ((framebuffers V.!) . fromIntegral)
           releaseSwapchain
    )

allocSwapchainResources
  :: Extent2D
  -> SwapchainKHR
  -- ^ Previous swapchain, can be NULL_HANDLE
  -> SurfaceKHR
  -> V
       ( SwapchainKHR
       , Extent2D
       , V.Vector Framebuffer
       , Pipeline
       , RenderPass
       , RefCounted
       )
allocSwapchainResources windowSize oldSwapchain surface = do
  (swapchainKey, swapchain, surfaceFormat, imageExtent) <- createSwapchain
    oldSwapchain
    windowSize
    surface

  (renderPassKey, renderPass) <- Pipeline.createRenderPass
    (format (surfaceFormat :: SurfaceFormatKHR))
  (pipelineKey  , pipeline       ) <- createPipeline imageExtent renderPass

  (_            , swapchainImages) <- getSwapchainImagesKHR' swapchain
  (imageViewKeys, imageViews     ) <-
    fmap V.unzip . V.forM swapchainImages $ \image ->
      createImageView (format (surfaceFormat :: SurfaceFormatKHR)) image

  (framebufferKeys, framebuffers) <-
    fmap V.unzip . V.forM imageViews $ \imageView ->
      createFramebuffer renderPass imageView imageExtent

  releaseSwapchain <- newRefCounted $ do
    sayErr "Releasing swapchain resources"
    traverse_ release framebufferKeys
    traverse_ release imageViewKeys
    release pipelineKey
    release renderPassKey
    release swapchainKey

  pure
    ( swapchain
    , imageExtent
    , framebuffers
    , pipeline
    , renderPass
    , releaseSwapchain
    )

render :: Frame -> V ()
render Frame {..} =
  do
      (SUCCESS, imageIndex) <- acquireNextImageKHR' fSwapchain
                                                    0
                                                    fImageAvailableSemaphore
                                                    zero

      commandPool <- getCommandPool
      let commandBufferAllocateInfo = zero
            { commandPool        = commandPool
            , level              = COMMAND_BUFFER_LEVEL_PRIMARY
            , commandBufferCount = 1
            }
      (_, [commandBuffer]) <- withCommandBuffers' commandBufferAllocateInfo

      useCommandBuffer'
          commandBuffer
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
      graphicsQueue           <- getGraphicsQueue
      (fenceKey, renderFence) <- withFence' zero
      queueSubmit graphicsQueue [SomeStruct submitInfo] renderFence

      useRefCounted fReleaseSwapchain
      waitAndRelease <- toIO $ do
        waitForFencesSafe' [renderFence] True 1e9 >>= \case
          TIMEOUT -> throwString "AAA"
          _       -> pure ()
        release fenceKey
        releaseRefCounted fReleaseSwapchain
      _ <- allocate (async waitAndRelease) uninterruptibleCancel

      let presentInfo = zero { waitSemaphores = [fRenderFinishedSemaphore]
                             , swapchains     = [fSwapchain]
                             , imageIndices   = [imageIndex]
                             }
      _ <- queuePresentKHR graphicsQueue presentInfo

      unlessM shouldQuit $ render Frame { .. }
    `catchSwapchainError` \_ -> do
                            SDL.V2 width height <- SDL.vkGetDrawableSize fWindow
                            (swapchain, imageExtent, framebuffers, pipeline, renderPass, releaseSwapchain) <-
                              allocSwapchainResources
                                (Extent2D (fromIntegral width)
                                          (fromIntegral height)
                                )
                                fSwapchain
                                fSurface

                            releaseRefCounted fReleaseSwapchain
                            render $ Frame fWindow
                                           fSurface
                                           swapchain
                                           renderPass
                                           imageExtent
                                           fImageAvailableSemaphore
                                           fRenderFinishedSemaphore
                                           pipeline
                                           ((framebuffers V.!) . fromIntegral)
                                           releaseSwapchain


catchSwapchainError :: V a -> (Result -> V a) -> V a
catchSwapchainError = catchJust $ \case
  VulkanException e@ERROR_OUT_OF_DATE_KHR  -> Just e
  -- TODO handle this case
  -- VulkanException e@ERROR_SURFACE_LOST_KHR -> Just e
  VulkanException _                        -> Nothing


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Create a pretty vanilla ImageView covering the whole image
createImageView :: Format -> Image -> V (ReleaseKey, ImageView)
createImageView format = \image ->
  withImageView' imageViewCreateInfo { image = image }
 where
  imageViewCreateInfo = zero
    { viewType         = IMAGE_VIEW_TYPE_2D
    , format           = format
    , components       = zero { r = COMPONENT_SWIZZLE_IDENTITY
                              , g = COMPONENT_SWIZZLE_IDENTITY
                              , b = COMPONENT_SWIZZLE_IDENTITY
                              , a = COMPONENT_SWIZZLE_IDENTITY
                              }
    , subresourceRange = zero { aspectMask     = IMAGE_ASPECT_COLOR_BIT
                              , baseMipLevel   = 0
                              , levelCount     = 1
                              , baseArrayLayer = 0
                              , layerCount     = 1
                              }
    }

----------------------------------------------------------------
-- Ref counting helper
----------------------------------------------------------------

-- A 'RefCounted' will perform the specified action when the count reaches 0
data RefCounted = RefCounted
  { rcCount  :: IORef Word
  , rcAction :: IO ()
  }

newRefCounted :: MonadIO m => IO () -> m RefCounted
newRefCounted rcAction = do
  rcCount <- liftIO $ newIORef 1
  pure RefCounted { .. }

releaseRefCounted :: MonadIO m => RefCounted -> m ()
releaseRefCounted RefCounted {..} = liftIO $ mask $ \_ ->
  atomicModifyIORef' rcCount (\c -> (pred c, pred c)) >>= \case
    0 -> rcAction
    _ -> pure ()

useRefCounted :: MonadIO m => RefCounted -> m ()
useRefCounted RefCounted {..} =
  liftIO $ atomicModifyIORef' rcCount (\c -> (succ c, ()))

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
