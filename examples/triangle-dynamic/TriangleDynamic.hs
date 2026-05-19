{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-| Dynamic-rendering version of "Triangle". Same colored triangle, same
recycling-Frame loop, but with no 'Vk.RenderPass' and no 'Vk.Framebuffer':
the swapchain image's layout transitions are handled by explicit pipeline
barriers from "Vulkan.Utils.ImageBarrier", and the rendering region is
opened with 'Vk.cmdUseRendering' against a 'Vk.RenderingInfo' that points
straight at the swapchain image view.

The graphics pipeline is built with 'createColorPipelineDynamicFromShaders',
which omits the render pass and instead carries a
'Vk.PipelineRenderingCreateInfo' in its pNext chain.
-}
module TriangleDynamic
  ( runTriangle
  ) where

import Control.Monad.Trans.Resource (ResourceT, register)
import qualified Data.Vector as V
import Data.Word (Word32)
import qualified Triangle
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Rect2D (Rect2D (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as KHR
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame, recordCommands)
import Vulkan.Utils.ImageBarrier (cmdTransitionForColorAttachment, cmdTransitionForPresent)
import Vulkan.Utils.Pipeline (createColorPipelineDynamicFromShaders)
import Vulkan.Utils.Swapchain (Swapchain (..))
import Vulkan.Utils.VulkanContext (VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, noOnFrame, runWindowLoop)
import Vulkan.Zero (zero)

{- | Drive a recycling-Frame render loop drawing the colored triangle with
'VK_KHR_dynamic_rendering'.
-}
runTriangle
  :: VulkanContext
  -> Swapchain
  -- ^ Initial swapchain
  -> IO Vk.Extent2D
  -- ^ Get current drawable size (for resize)
  -> IO Bool
  -- ^ Per-frame poller; 'True' means quit
  -> ResourceT IO ()
runTriangle vc initialSC getDrawableSize shouldQuit = do
  (_, pipeline) <-
    createColorPipelineDynamicFromShaders
      (vcDevice vc)
      [KHR.format (sFormat initialSC)]
      [ (Vk.SHADER_STAGE_VERTEX_BIT, Triangle.vertCode)
      , (Vk.SHADER_STAGE_FRAGMENT_BIT, Triangle.fragCode)
      ]

  runWindowLoop
    vc
    initialSC
    getDrawableSize
    shouldQuit
    WindowLoop
      { wlMkState = \_sc -> do
          k <- register (pure ())
          pure ((), k)
      , wlRender = \() -> drawTriangle vc pipeline
      , wlOnFrame = noOnFrame
      , wlOnExit = noOnExit
      }

----------------------------------------------------------------
-- Per-frame draw
----------------------------------------------------------------

drawTriangle :: VulkanContext -> Vk.Pipeline -> Frame -> ResourceT IO ()
drawTriangle vc pipeline f = do
  (acquireResult, imageIndex) <- acquireFrameImage vc f
  commands <- recordCommands vc f \cb -> do
    let image = sImages V.! fromIntegral imageIndex
    cmdTransitionForColorAttachment cb image
    Vk.cmdUseRendering cb (renderingInfo imageIndex) do
      Vk.cmdSetViewport cb 0 [vp]
      Vk.cmdSetScissor cb 0 [area]
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
      Vk.cmdDraw cb 3 1 0 0
    cmdTransitionForPresent cb image
  queueSubmitFrame vc f [commands]
  presentFrameImage vc f acquireResult imageIndex
  where
    Swapchain{sExtent, sImages, sImageViews} = fSwapchain f
    area = zero{Rect2D.extent = sExtent}
    renderingInfo :: Word32 -> Vk.RenderingInfo '[]
    renderingInfo imageIndex =
      zero
        { Vk.renderArea = area
        , Vk.layerCount = 1
        , Vk.colorAttachments = [SomeStruct colorAttachment]
        }
      where
        colorAttachment :: Vk.RenderingAttachmentInfo '[]
        colorAttachment =
          zero
            { Vk.imageView = sImageViews V.! fromIntegral imageIndex
            , Vk.imageLayout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
            , Vk.loadOp = Vk.ATTACHMENT_LOAD_OP_CLEAR
            , Vk.storeOp = Vk.ATTACHMENT_STORE_OP_STORE
            , Vk.clearValue = Vk.Color (Vk.Float32 0.1 0.1 0.1 0)
            }
    vp :: Vk.Viewport
    vp = zero{Vk.width = realToFrac w, Vk.height = realToFrac h, Vk.maxDepth = 1}
      where
        Vk.Extent2D w h = sExtent
