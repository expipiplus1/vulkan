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

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, ResourceT)
import Data.Vector (Vector)
import qualified Data.Vector as V
import VkResources (VkResources (..), vrContext)
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as KHR
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame, recordCommands)
import qualified Vulkan.Utils.Framebuffer as Framebuffer
import Vulkan.Utils.Pipeline (createColorPipelineFromShaders)
import qualified Vulkan.Utils.RenderPass as RenderPass
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Vulkan.Utils.Swapchain (Swapchain (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, noOnFrame, runWindowLoop)
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
      , wlOnExit = noOnExit
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
  let sc = fSwapchain f

  (acquireResult, imageIndex) <- acquireFrameImage (vrContext vr) f

  commands <- recordCommands (vrContext vr) f \cb -> do
    let renderPassBeginInfo =
          zero
            { Vk.renderPass = renderPass
            , Vk.framebuffer = framebuffers V.! fromIntegral imageIndex
            , Vk.renderArea = Vk.Rect2D{Vk.offset = zero, Vk.extent = sExtent sc}
            , Vk.clearValues = [Vk.Color (Vk.Float32 0.1 0.1 0.1 0)]
            }
    Vk.cmdUseRenderPass cb renderPassBeginInfo Vk.SUBPASS_CONTENTS_INLINE do
      let Vk.Extent2D w h = sExtent sc
      Vk.cmdSetViewport
        cb
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
        cb
        0
        [Vk.Rect2D{Vk.offset = Vk.Offset2D 0 0, Vk.extent = sExtent sc}]
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
      Vk.cmdDraw cb 3 1 0 0

  queueSubmitFrame (vrContext vr) f [commands]
  presentFrameImage (vrContext vr) f acquireResult imageIndex

----------------------------------------------------------------
-- Pipeline (long-lived)
----------------------------------------------------------------

createGraphicsPipeline
  :: (MonadResource m, MonadFail m)
  => Vk.Device
  -> Vk.RenderPass
  -> m (ReleaseKey, Vk.Pipeline)
createGraphicsPipeline dev renderPass =
  createColorPipelineFromShaders
    dev
    renderPass
    [ (Vk.SHADER_STAGE_VERTEX_BIT, vertCode)
    , (Vk.SHADER_STAGE_FRAGMENT_BIT, fragCode)
    ]
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
