{-# LANGUAGE OverloadedLists #-}

module Render
  ( renderFrame
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.Vector (Vector, (!))
import VkResources (VkResources (..), vrContext)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as Extent2D (Extent2D (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame)
import Vulkan.Utils.Swapchain (Swapchain (..))
import Vulkan.Utils.VulkanContext (RecycledResources (..))
import Vulkan.Zero (zero)

-- | Acquire an image, record a clear+draw, submit, and present.
renderFrame
  :: VkResources
  -> Vk.RenderPass
  -> Vk.Pipeline
  -> Vector Vk.Framebuffer
  -> Frame
  -> ResourceT IO ()
renderFrame vr renderPass pipeline framebuffers f = do
  let
    sc = fSwapchain f
    dev = vrDevice vr

  (acquireResult, imageIndex) <- acquireFrameImage (vrContext vr) f

  -- Allocate a per-frame command buffer from the recycled pool.
  (_, [commandBuffer]) <-
    Vk.withCommandBuffers
      dev
      zero
        { Vk.commandPool = rrCommandPool (fRecycled f)
        , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
      allocate

  let renderPassBeginInfo =
        zero
          { Vk.renderPass = renderPass
          , Vk.framebuffer = framebuffers ! fromIntegral imageIndex
          , Vk.renderArea = Vk.Rect2D{offset = zero, extent = sExtent sc}
          , Vk.clearValues = [Vk.Color (Vk.Float32 0.3 0.4 0.8 1)]
          }

  Vk.useCommandBuffer
    commandBuffer
    zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
    do
      Vk.cmdUseRenderPass commandBuffer renderPassBeginInfo Vk.SUBPASS_CONTENTS_INLINE do
        Vk.cmdSetViewport
          commandBuffer
          0
          [ Vk.Viewport
              { x = 0
              , y = 0
              , width = realToFrac (Extent2D.width (sExtent sc))
              , height = realToFrac (Extent2D.height (sExtent sc))
              , minDepth = 0
              , maxDepth = 1
              }
          ]
        Vk.cmdSetScissor commandBuffer 0 [Vk.Rect2D{offset = Vk.Offset2D 0 0, extent = sExtent sc}]
        Vk.cmdBindPipeline commandBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
        Vk.cmdDraw commandBuffer 3 1 0 0

  queueSubmitFrame (vrContext vr) f [commandBuffer]
  presentFrameImage (vrContext vr) f acquireResult imageIndex
