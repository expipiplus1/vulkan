{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-| Headless test for the always-on dynamic state (Vulkan 1.3 core, no
extensions). Builds ONE dynamic-rendering pipeline (full dynamic set) and
renders the colored triangle into an offscreen image several times, each with a
different 'DynamicState' — default, back-face cull, front-face cull, and
rasterizer-discard. Every render goes through the same 'Vk.Pipeline'; only the
per-frame 'applyDynamicStates' differs.

Each result is read back to the CPU, saved as a @.png@, and its centre pixel
printed. The program then asserts that the dynamic state was actually consumed
(the default render shows the triangle, rasterizer-discard shows nothing, and
exactly one of the two cull modes hides it), exiting non-zero on mismatch.
-}
module Main where

import qualified Codec.Picture as JP
import Control.Monad (forM, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.Word
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), submitAndWait, withHeadlessVk)
import ImageReadback (copyImageToHost, makeReadbackImage, savePng)
import RenderTarget (createColorTarget)
import System.Exit (exitFailure)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PhysicalDeviceDynamicRenderingFeatures)
import Vulkan.Utils.Barrier (transitionColorAttachment)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (DynamicState (..), allDynamicStates, applyDynamicStates, dynamicStateFor, fullScissor)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import qualified Vulkan.Utils.Requirements.TH as U
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

width, height :: Word32
width = 256
height = 256

main :: IO ()
main = runResourceT $ do
  HeadlessVk{..} <-
    withHeadlessVk
      HeadlessConfig
        { appName = "Haskell Vulkan dynamic-state (headless) test"
        , instanceReqs = []
        , deviceReqs =
            [U.reqs|
              VK_KHR_dynamic_rendering
              PhysicalDeviceDynamicRenderingFeatures.dynamicRendering
            |]
        , vmaFlags = zero
        }
  let QueueFamilyIndex graphicsQueueFamilyIndex = fst (qGraphics queues)
  results <- render allocator device graphicsQueueFamilyIndex
  Vk.deviceWaitIdle device

  centres <- forM results $ \(name, image) -> do
    savePng ("headless-dynamic-" <> name <> ".png") image
    let
      JP.PixelRGBA8 r g b _ = JP.pixelAt image (fromIntegral width `div` 2) (fromIntegral height `div` 2)
      vis = visible r g b
    liftIO $ putStrLn $ name <> " centre=" <> show (r, g, b) <> "  " <> (if vis then "triangle" else "clear")
    pure (name, vis)

  -- The clear colour is ~(26,26,26); a covered centre pixel is bright. The
  -- triangle's winding decides which cull mode hides it, so we only require
  -- that exactly one of the two does — that, plus discard hiding it and the
  -- default showing it, proves one pipeline honoured four different states.
  let
    isVis n = maybe False id (lookup n centres)
    checks :: [(String, Bool)]
    checks =
      [ ("default shows the triangle", isVis "default")
      , ("rasterizer-discard hides it", not (isVis "rasterizer-discard"))
      , ("exactly one cull mode hides it", isVis "cull-back" /= isVis "cull-front")
      ]
  liftIO $ do
    mapM_ (\(label, ok) -> putStrLn $ "[" <> (if ok then "PASS" else "FAIL") <> "] " <> label) checks
    unless (all snd checks) exitFailure
    putStrLn "All dynamic-state checks passed."
  where
    visible r g b = fromIntegral r + fromIntegral g + (fromIntegral b :: Int) > 180

{- | Render the triangle once per dynamic-state variant through a single
full-dynamic pipeline, reading each result back to a JuicyPixels image.
-}
render
  :: VMA.Allocator
  -> Vk.Device
  -> Word32
  -> ResourceT IO [(String, JP.Image JP.PixelRGBA8)]
render allocator dev graphicsQueueFamilyIndex = do
  let
    imageFormat = Vk.FORMAT_R8G8B8A8_UNORM
    extent = Vk.Extent2D width height

  (_, (image, imageView)) <- createColorTarget allocator dev imageFormat extent

  (cpuImage, readback) <- makeReadbackImage allocator dev imageFormat extent

  -- One pipeline; dynamic states default to full always-on.
  (_, pipeline) <-
    Dynamic.allocatePipelineFromShaders
      dev
      zero{Dynamic.colorFormats = [imageFormat]}
      ()
      [(Vk.SHADER_STAGE_VERTEX_BIT, vertCode), (Vk.SHADER_STAGE_FRAGMENT_BIT, fragCode)]

  let poolCreateInfo = zero{CommandPoolCreateInfo.queueFamilyIndex = graphicsQueueFamilyIndex}
  (_, commandPool) <- Vk.withCommandPool dev poolCreateInfo Nothing allocate
  graphicsQueue <- Vk.getDeviceQueue dev graphicsQueueFamilyIndex 0

  let
    base = dynamicStateFor extent
    variants =
      [ ("default", base)
      , ("cull-back", base{cullMode = Vk.CULL_MODE_BACK_BIT})
      , ("cull-front", base{cullMode = Vk.CULL_MODE_FRONT_BIT})
      , ("rasterizer-discard", base{rasterizerDiscard = True})
      ]

  forM variants $ \(name, dynState) -> do
    (_, [cb]) <-
      Vk.withCommandBuffers
        dev
        zero
          { Vk.commandPool = commandPool
          , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
          , Vk.commandBufferCount = 1
          }
        allocate
    Vk.useCommandBuffer cb zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} do
      -- UNDEFINED -> COLOR_ATTACHMENT_OPTIMAL (discards the previous render).
      transitionColorAttachment cb image
      let renderingInfo =
            Dynamic.colorAttachmentRenderingInfo
              (fullScissor extent)
              imageView
              (Vk.Float32 0.1 0.1 0.1 1)
      Vk.cmdUseRendering cb renderingInfo do
        Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
        applyDynamicStates allDynamicStates cb dynState
        Vk.cmdDraw cb 3 1 0 0
      copyImageToHost cb extent image cpuImage
    submitAndWait dev graphicsQueue cb ("Timed out rendering variant: " <> name)
    img <- readback
    pure (name, img)

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
      fragColor = colors[gl_VertexIndex];
    }
  |]

fragCode =
  [frag|
    #version 450
    layout(location = 0) in vec3 fragColor;
    layout(location = 0) out vec4 outColor;
    void main() {
      outColor = vec4(fragColor, 1.0);
    }
  |]
