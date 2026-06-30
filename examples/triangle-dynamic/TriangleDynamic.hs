{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-| Dynamic-rendering version of "Triangle". Same colored triangle, same
recycling-Frame loop, but with no 'Vk.RenderPass' and no 'Vk.Framebuffer':
the swapchain image's layout transitions are handled by explicit pipeline
barriers, and the rendering region is opened with 'Vk.cmdUseRendering'
against a 'Vk.RenderingInfo' that points straight at the swapchain image
view. Everything comes from the single "Vulkan.Utils.DynamicRendering" path
module.

The graphics pipeline is built with
'Vulkan.Utils.DynamicRendering.allocatePipelineFromShaders', which omits the render
pass and instead carries a 'Vk.PipelineRenderingCreateInfo' in its pNext chain.

It also passes 'Nothing' for the dynamic-state set, so the pipeline declares the
full Vulkan-1.3 always-on set dynamic; every frame re-applies it in one call via
'applyDynamicStates' 'allDynamicStates' over a 'dynamicStateFor' (the whole-extent
viewport+scissor plus safe defaults). One pipeline object then covers any
cull/depth/topology/size.
-}
module TriangleDynamic
  ( runTriangle
  ) where

import Control.Monad.Trans.Resource (ResourceT, register)
import qualified Data.Vector as V
import qualified Triangle
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as KHR
import Vulkan.Utils.Barrier (transitionColorAttachment, transitionPresent)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (allDynamicStates, applyDynamicStates, dynamicStateFor, fullScissor)
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame, recordCommands)
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
    Dynamic.allocatePipelineFromShaders
      (vcDevice vc)
      zero{Dynamic.colorFormats = [KHR.format (sFormat initialSC)]}
      () -- no specialization constants
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
    let
      image = sImages V.! fromIntegral imageIndex
      imageView = sImageViews V.! fromIntegral imageIndex
    transitionColorAttachment cb image
    Vk.cmdUseRendering cb (renderingInfo imageView) do
      applyDynamicStates allDynamicStates cb (dynamicStateFor sExtent)
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
      Vk.cmdDraw cb 3 1 0 0
    transitionPresent cb image
  queueSubmitFrame vc f imageIndex [commands]
  presentFrameImage vc f acquireResult imageIndex
  where
    Swapchain{sExtent, sImages, sImageViews} = fSwapchain f
    renderingInfo :: Vk.ImageView -> Vk.RenderingInfo '[]
    renderingInfo imageView =
      Dynamic.colorAttachmentRenderingInfo (fullScissor sExtent) imageView (Vk.Float32 0.1 0.1 0.1 0)
