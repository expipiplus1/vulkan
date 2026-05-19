{-# LANGUAGE OverloadedLists #-}

module Render
  ( renderFrame
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import Data.Vector (Vector, (!))
import qualified Vulkan.Core10 as Extent2D (Extent2D (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame, recordCommands)
import Vulkan.Utils.Swapchain (Swapchain (..))
import Vulkan.Utils.VulkanContext (VulkanContext)
import Vulkan.Zero (zero)

-- | Acquire an image, record a clear+draw, submit, and present.
renderFrame
  :: VulkanContext
  -> Vk.RenderPass
  -> Vk.Pipeline
  -> Vector Vk.Framebuffer
  -> Frame
  -> ResourceT IO ()
renderFrame vc renderPass pipeline framebuffers f = do
  let sc = fSwapchain f

  (acquireResult, imageIndex) <- acquireFrameImage vc f

  let renderPassBeginInfo =
        zero
          { Vk.renderPass = renderPass
          , Vk.framebuffer = framebuffers ! fromIntegral imageIndex
          , Vk.renderArea = Vk.Rect2D{offset = zero, extent = sExtent sc}
          , Vk.clearValues = [Vk.Color (Vk.Float32 0.3 0.4 0.8 1)]
          }

  commands <- recordCommands vc f \cb ->
    Vk.cmdUseRenderPass cb renderPassBeginInfo Vk.SUBPASS_CONTENTS_INLINE do
      Vk.cmdSetViewport
        cb
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
      Vk.cmdSetScissor cb 0 [Vk.Rect2D{offset = Vk.Offset2D 0 0, extent = sExtent sc}]
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
      Vk.cmdDraw cb 3 1 0 0

  queueSubmitFrame vc f [commands]
  presentFrameImage vc f acquireResult imageIndex
