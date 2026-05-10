{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-| Backend-independent triangle renderer using the recycling 'Frame' loop
from "Frame". Each backend (SDL2, GLFW) builds 'VkResources' + an initial
'Swapchain', supplies callbacks for "current drawable size" and "should
quit", and hands off to 'runTriangle'.
-}
module Triangle
  ( runTriangle
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, ResourceT, allocate, release)
import Data.Vector (Vector)
import qualified Data.Vector as V
import VkResources (Queues (..), VkResources (..), vrContext)
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core12 as Vk12
import Vulkan.Exception (VulkanException (..))
import qualified Vulkan.Extensions.VK_KHR_surface as KHR
import qualified Vulkan.Extensions.VK_KHR_swapchain as KHR
import Vulkan.Utils.Frame (Frame (..), queueSubmitFrame)
import qualified Vulkan.Utils.Framebuffer as Framebuffer
import Vulkan.Utils.Pipeline (createColorPipeline)
import qualified Vulkan.Utils.RenderPass as RenderPass
import Vulkan.Utils.Shader (shaderStage)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Vulkan.Utils.Swapchain (Swapchain (..))
import Vulkan.Utils.VulkanContext (RecycledResources (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnFrame, runWindowLoop)
import Vulkan.Zero (zero)

-- | Drive a recycling-Frame render loop drawing the colored triangle.
runTriangle
  :: VkResources
  -> Swapchain
  -- ^ Initial swapchain
  -> IO Vk.Extent2D
  -- ^ Get current drawable size (for resize)
  -> IO Bool
  -- ^ Per-frame poller; 'True' means quit
  -> ResourceT IO ()
runTriangle vr initialSC getDrawableSize shouldQuit = do
  let dev = vrDevice vr
  (_, renderPass) <-
    RenderPass.createColorRenderPass
      dev
      (KHR.format (sFormat initialSC))
      Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
  (_, pipeline) <- createGraphicsPipeline dev renderPass

  runWindowLoop
    (vrContext vr)
    initialSC
    getDrawableSize
    shouldQuit
    WindowLoop
      { wlMkState = \sc ->
          Framebuffer.createFramebuffers dev renderPass (sImageViews sc) (sExtent sc)
      , wlRender = drawTriangle vr renderPass pipeline
      , wlOnFrame = noOnFrame
      , wlOnExit = \_ -> Vk.deviceWaitIdle dev
      }

----------------------------------------------------------------
-- Per-frame draw
----------------------------------------------------------------

drawTriangle
  :: VkResources
  -> Vk.RenderPass
  -> Vk.Pipeline
  -> Vector Vk.Framebuffer
  -> Frame
  -> ResourceT IO ()
drawTriangle vr renderPass pipeline framebuffers f = do
  let
    RecycledResources{..} = fRecycled f
    sc = fSwapchain f
    dev = vrDevice vr
    gQ = snd (qGraphics (vrQueues vr))
    oneSecond = 1e9

  (acquireResult, imageIndex) <-
    KHR.acquireNextImageKHRSafe dev (sSwapchain sc) oneSecond rrImageAvailable Vk.NULL_HANDLE >>= \case
      r@(Vk.SUCCESS, _) -> pure r
      r@(Vk.SUBOPTIMAL_KHR, _) -> pure r
      (Vk.TIMEOUT, _) -> liftIO . throwIO $ VulkanException Vk.ERROR_OUT_OF_DATE_KHR
      _ -> liftIO . throwIO $ VulkanException Vk.ERROR_OUT_OF_DATE_KHR

  (_, [commandBuffer]) <-
    Vk.withCommandBuffers
      dev
      zero
        { Vk.commandPool = rrCommandPool
        , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
      allocate

  let renderPassBeginInfo =
        zero
          { Vk.renderPass = renderPass
          , Vk.framebuffer = framebuffers V.! fromIntegral imageIndex
          , Vk.renderArea = Vk.Rect2D{Vk.offset = zero, Vk.extent = sExtent sc}
          , Vk.clearValues = [Vk.Color (Vk.Float32 0.1 0.1 0.1 0)]
          }

  Vk.useCommandBuffer
    commandBuffer
    zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
    do
      Vk.cmdUseRenderPass commandBuffer renderPassBeginInfo Vk.SUBPASS_CONTENTS_INLINE do
        let Vk.Extent2D w h = sExtent sc
        Vk.cmdSetViewport
          commandBuffer
          0
          [ Vk.Viewport
              { Vk.x = 0
              , Vk.y = 0
              , Vk.width = realToFrac w
              , Vk.height = realToFrac h
              , Vk.minDepth = 0
              , Vk.maxDepth = 1
              }
          ]
        Vk.cmdSetScissor
          commandBuffer
          0
          [Vk.Rect2D{Vk.offset = Vk.Offset2D 0 0, Vk.extent = sExtent sc}]
        Vk.cmdBindPipeline commandBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
        Vk.cmdDraw commandBuffer 3 1 0 0

  let submitInfo =
        zero
          { Vk.waitSemaphores = [rrImageAvailable]
          , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
          , Vk.commandBuffers = [Vk.commandBufferHandle commandBuffer]
          , Vk.signalSemaphores = [rrRenderFinished, fHostTimeline f]
          }
          ::& zero
            { Vk12.waitSemaphoreValues = [1]
            , Vk12.signalSemaphoreValues = [1, fIndex f]
            }
            :& ()
  liftIO $
    queueSubmitFrame
      gQ
      f
      [SomeStruct submitInfo]
      (fHostTimeline f)
      (fIndex f)

  presentResult <-
    KHR.queuePresentKHR
      gQ
      zero
        { KHR.waitSemaphores = [rrRenderFinished]
        , KHR.swapchains = [sSwapchain sc]
        , KHR.imageIndices = [imageIndex]
        }

  case (acquireResult, presentResult) of
    (Vk.SUBOPTIMAL_KHR, _) -> liftIO . throwIO $ VulkanException Vk.ERROR_OUT_OF_DATE_KHR
    (_, Vk.SUBOPTIMAL_KHR) -> liftIO . throwIO $ VulkanException Vk.ERROR_OUT_OF_DATE_KHR
    _ -> pure ()

----------------------------------------------------------------
-- Pipeline (long-lived)
----------------------------------------------------------------

createGraphicsPipeline
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> Vk.RenderPass
  -> m (ReleaseKey, Vk.Pipeline)
createGraphicsPipeline dev renderPass = do
  (vertKey, vertStage) <- shaderStage dev Vk.SHADER_STAGE_VERTEX_BIT vertCode
  (fragKey, fragStage) <- shaderStage dev Vk.SHADER_STAGE_FRAGMENT_BIT fragCode
  (key, pipeline) <- createColorPipeline dev renderPass [vertStage, fragStage]
  release vertKey
  release fragKey
  pure (key, pipeline)
  where
    vertCode =
      [vert|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        layout(location = 0) out vec3 fragColor;

        vec2 positions[3] = vec2[](
          vec2(0.0, -0.5),
          vec2(0.5, 0.5),
          vec2(-0.5, 0.5)
        );
        vec3 colors[3] = vec3[](
          vec3(1.0, 1.0, 0.0),
          vec3(0.0, 1.0, 1.0),
          vec3(1.0, 0.0, 1.0)
        );

        void main() {
          gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
          fragColor   = colors[gl_VertexIndex];
        }
      |]
    fragCode =
      [frag|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        layout(location = 0) in vec3 fragColor;
        layout(location = 0) out vec4 outColor;

        void main() {
            outColor = vec4(fragColor, 1.0);
        }
      |]
