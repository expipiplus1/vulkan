{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Main
  ( main
  )
where

import           Control.Exception.Safe         ( finally
                                                , displayException
                                                )
import           Control.Exception              ( handle )
import           Control.Monad.Trans.Resource
import qualified SDL
import           Say

import qualified Data.Vector                   as V
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
    createDevice inst surface
  let commandPoolCreateInfo :: CommandPoolCreateInfo
      commandPoolCreateInfo =
        zero { queueFamilyIndex = graphicsQueueFamilyIndex }
  (_, commandPool) <- withCommandPool dev commandPoolCreateInfo Nothing allocate
  allocator        <- createVMA inst phys dev

  sayErr $ "Using device: " <> devName

  -- Now all the globals are initialized
  runV inst phys dev graphicsQueue commandPool allocator
    . (`finally` deviceWaitIdle')
    $ do
    -- The first swapchain initialization
        (swapchain, surfaceFormat, imageExtent) <- createSwapchain
          (Extent2D initWidth initHeight)
          surface

        renderPass <- Pipeline.createRenderPass
          (format (surfaceFormat :: SurfaceFormatKHR))
        pipeline             <- createPipeline imageExtent renderPass

        (_, swapchainImages) <- getSwapchainImagesKHR' swapchain
        imageViews           <- V.forM swapchainImages $ \image ->
          createImageView (format (surfaceFormat :: SurfaceFormatKHR)) image

        framebuffers <- V.forM imageViews
          $ \imageView -> createFramebuffer renderPass imageView imageExtent

        (_, imageAvailableSemaphore) <- withSemaphore' zero
        (_, renderFinishedSemaphore) <- withSemaphore' zero

        SDL.showWindow sdlWindow

        let
          go = do
            (_, imageIndex) <- acquireNextImageKHR' swapchain
                                                    maxBound
                                                    imageAvailableSemaphore
                                                    zero
            commandPool <- getCommandPool
            let commandBufferAllocateInfo = zero
                  { commandPool        = commandPool
                  , level              = COMMAND_BUFFER_LEVEL_PRIMARY
                  , commandBufferCount = 1
                  }
            (_, [commandBuffer]) <- withCommandBuffers'
              commandBufferAllocateInfo

            useCommandBuffer'
                commandBuffer
                zero { flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT }
              $ do
                  let renderPassBeginInfo = zero
                        { renderPass  = renderPass
                        , framebuffer = framebuffers V.! fromIntegral imageIndex
                        , renderArea  = Rect2D zero imageExtent
                        , clearValues = [Color (Float32 (0.1, 0.1, 0.1, 1))]
                        }
                  cmdUseRenderPass commandBuffer
                                   renderPassBeginInfo
                                   SUBPASS_CONTENTS_INLINE
                    $ do
                        cmdBindPipeline commandBuffer
                                        PIPELINE_BIND_POINT_GRAPHICS
                                        pipeline
                        cmdDraw commandBuffer 3 1 0 0

            let submitInfo = zero
                  { waitSemaphores   = [imageAvailableSemaphore]
                  , waitDstStageMask =
                    [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
                  , commandBuffers   = [commandBufferHandle commandBuffer]
                  , signalSemaphores = [renderFinishedSemaphore]
                  }
            graphicsQueue <- getGraphicsQueue
            queueSubmit graphicsQueue [SomeStruct submitInfo] zero

            let presentInfo = zero { waitSemaphores = [renderFinishedSemaphore]
                                   , swapchains     = [swapchain]
                                   , imageIndices   = [imageIndex]
                                   }
            _ <- queuePresentKHR graphicsQueue presentInfo
            sayErr "frame"
            pure ()

        sequence_ $ replicate 400 go

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- | Create a pretty vanilla ImageView covering the whole image
createImageView :: Format -> Image -> V ImageView
createImageView format = \image ->
  snd <$> withImageView' imageViewCreateInfo { image = image }
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
