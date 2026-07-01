{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-| Backend-independent triangle renderer using the recycling 'Frame' loop
from "Frame". Each backend (SDL2, GLFW) builds a 'VulkanContext' + an initial
'Swapchain', supplies callbacks for "current drawable size" and "should
quit", and hands off to 'runTriangle'.
-}
module Triangle
  ( runTriangle
  , vertCode
  , fragCode
  ) where

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, ResourceT, register, release)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Vector (Vector)
import qualified Data.Vector as V
import UnliftIO (MonadUnliftIO)
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as KHR
import Vulkan.Utils.DynamicState (fullScissor, fullViewport, minimalDynamicStates)
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame, recordCommands)
import qualified Vulkan.Utils.Framebuffer as Framebuffer
import qualified Vulkan.Utils.RenderPass as RenderPass
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Vulkan.Utils.Swapchain (Swapchain (..))
import Vulkan.Utils.VulkanContext (VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, noOnFrame, runWindowLoop)
import Vulkan.Zero (zero)

-- | Drive a recycling-Frame render loop drawing the colored triangle.
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
  let dev = vcDevice vc
  let colorFormat = KHR.format (sFormat initialSC)
  (_, renderPass) <-
    RenderPass.allocateColorRenderPass
      dev
      colorFormat
      Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
  (_, pipeline) <- createGraphicsPipeline dev renderPass colorFormat

  runWindowLoop
    vc
    initialSC
    getDrawableSize
    shouldQuit
    WindowLoop
      { wlMkState = \sc -> do
          framebuffers <-
            traverse (\iv -> Framebuffer.allocateFramebuffer dev renderPass iv (sExtent sc)) (sImageViews sc)
          groupKey <- register (traverse_ (release . fst) framebuffers)
          pure (fmap snd framebuffers, groupKey)
      , wlRender = drawTriangle vc renderPass pipeline
      , wlOnFrame = noOnFrame
      , wlOnExit = noOnExit
      }

----------------------------------------------------------------
-- Per-frame draw
----------------------------------------------------------------

drawTriangle
  :: VulkanContext
  -> Vk.RenderPass
  -> Vk.Pipeline
  -> Vector Vk.Framebuffer
  -> Frame
  -> ResourceT IO ()
drawTriangle vc renderPass pipeline framebuffers f = do
  let sc = fSwapchain f

  (acquireResult, imageIndex) <- acquireFrameImage vc f

  commands <- recordCommands vc f \cb -> do
    let renderPassBeginInfo =
          zero
            { Vk.renderPass = renderPass
            , Vk.framebuffer = framebuffers V.! fromIntegral imageIndex
            , Vk.renderArea = Vk.Rect2D{Vk.offset = zero, Vk.extent = sExtent sc}
            , Vk.clearValues = [Vk.Color (Vk.Float32 0.1 0.1 0.1 0)]
            }
    Vk.cmdUseRenderPass cb renderPassBeginInfo Vk.SUBPASS_CONTENTS_INLINE do
      Vk.cmdSetViewport cb 0 [fullViewport (sExtent sc)]
      Vk.cmdSetScissor cb 0 [fullScissor (sExtent sc)]
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
      Vk.cmdDraw cb 3 1 0 0

  queueSubmitFrame vc f imageIndex [commands]
  presentFrameImage vc f acquireResult imageIndex

----------------------------------------------------------------
-- Pipeline (long-lived)
----------------------------------------------------------------

createGraphicsPipeline
  :: (MonadResource m, MonadUnliftIO m, MonadFail m)
  => Vk.Device
  -> Vk.RenderPass
  -> Vk.Format
  -- ^ Colour attachment format (matches the render pass).
  -> m (ReleaseKey, Vk.Pipeline)
createGraphicsPipeline dev renderPass colorFormat =
  RenderPass.allocatePipelineFromShaders
    dev
    renderPass
    zero
      { RenderPass.colorFormats = [colorFormat]
      , RenderPass.dynamicStates = Just minimalDynamicStates -- just viewport+scissor; set below with cmdSetViewport/Scissor
      }
    () -- no specialization constants
    [ (Vk.SHADER_STAGE_VERTEX_BIT, vertCode)
    , (Vk.SHADER_STAGE_FRAGMENT_BIT, fragCode)
    ]

vertCode :: ByteString
vertCode =
  [vert|
    #version 450

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

fragCode :: ByteString
fragCode =
  [frag|
    #version 450

    layout(location = 0) in vec3 fragColor;
    layout(location = 0) out vec4 outColor;

    void main() {
        outColor = vec4(fragColor, 1.0);
    }
  |]
