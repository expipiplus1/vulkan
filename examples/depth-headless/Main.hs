{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-| Headless test for depth attachments and vertex-buffer input together. Two
fully-overlapping triangles — a near red one and a far cyan one — are fed from a
single vertex buffer (position + colour per vertex) and rendered twice through
ONE dynamic-rendering pipeline that carries a depth attachment. Only the per-frame
depth compare op differs: @LESS@ (with the depth cleared to the far plane) lets the
near triangle win, @GREATER@ (cleared to the near plane) lets the far one win.

Each result is read back to the CPU, saved as a @.png@, and its centre pixel
checked: red under @LESS@, cyan under @GREATER@. The opposite winners from
identical geometry and draw order prove the depth test is consumed; that the
expected colours appear at all proves the vertex buffer fed correct positions and
colours. Exits non-zero on mismatch.
-}
module Main where

import qualified Codec.Picture as JP
import Control.Monad (forM, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.Word
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (sizeOf)
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), submitAndWait, withHeadlessVk)
import ImageReadback (copyImageToHost, makeReadbackImage, savePng)
import RenderTarget (createColorTarget, createDepthTarget)
import System.Exit (exitFailure)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PhysicalDeviceDynamicRenderingFeatures)
import Vulkan.Utils.Barrier (transitionColorAttachment, transitionDepthAttachment)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (DynamicState (..), allDynamicStates, applyDynamicStates, dynamicStateFor, fullScissor)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import qualified Vulkan.Utils.Requirements.TH as U
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (frag, vert)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA

width, height :: Word32
width = 256
height = 256

imageFormat :: Vk.Format
imageFormat = Vk.FORMAT_R8G8B8A8_UNORM

depthFormat :: Vk.Format
depthFormat = Vk.FORMAT_D32_SFLOAT

floatSize :: Int
floatSize = sizeOf (undefined :: Float)

{- | 6 vertices (2 triangles), each @(pos :: vec3, colour :: vec3)@. Both triangles
cover the centre pixel; the first is nearer (smaller z) and red, the second
farther and cyan.
-}
vertices :: [Float]
vertices =
  [ -0.7
  , -0.7
  , 0.3
  , 0.9
  , 0.2
  , 0.2
  , 0.7
  , -0.7
  , 0.3
  , 0.9
  , 0.2
  , 0.2
  , 0.0
  , 0.7
  , 0.3
  , 0.9
  , 0.2
  , 0.2
  , -0.7
  , -0.7
  , 0.7
  , 0.2
  , 0.8
  , 0.9
  , 0.7
  , -0.7
  , 0.7
  , 0.2
  , 0.8
  , 0.9
  , 0.0
  , 0.7
  , 0.7
  , 0.2
  , 0.8
  , 0.9
  ]

main :: IO ()
main = runResourceT $ do
  HeadlessVk{..} <-
    withHeadlessVk
      HeadlessConfig
        { hcAppName = "Haskell Vulkan depth + vertex-buffer (headless) test"
        , hcInstanceReqs = []
        , hcDeviceReqs =
            [U.reqs|
              VK_KHR_dynamic_rendering
              PhysicalDeviceDynamicRenderingFeatures.dynamicRendering
            |]
        }
  let QueueFamilyIndex graphicsQueueFamilyIndex = fst (qGraphics hvQueues)
  results <- render hvAllocator hvDevice graphicsQueueFamilyIndex
  Vk.deviceWaitIdle hvDevice

  centres <- forM results $ \(name, image) -> do
    savePng ("depth-headless-" <> name <> ".png") image
    let JP.PixelRGBA8 r g b _ = JP.pixelAt image (fromIntegral width `div` 2) (fromIntegral height `div` 2)
    liftIO $ putStrLn $ name <> " centre=" <> show (r, g, b)
    pure (name, (r, g, b))

  let
    centre n = maybe (0, 0, 0) id (lookup n centres)
    isRed (r, g, b) = r > 150 && g < 120 && b < 120
    isCyan (r, g, b) = r < 120 && g > 150 && b > 150
    checks :: [(String, Bool)]
    checks =
      [ ("LESS shows near (red)", isRed (centre "less"))
      , ("GREATER shows far (cyan)", isCyan (centre "greater"))
      ]
  liftIO $ do
    mapM_ (\(label, ok) -> putStrLn $ "[" <> (if ok then "PASS" else "FAIL") <> "] " <> label) checks
    unless (all snd checks) exitFailure
    putStrLn "All depth + vertex-buffer checks passed."

{- | Render the two overlapping triangles once per depth compare op through a
single pipeline (depth attachment + vertex buffer), reading each result back.
-}
render
  :: VMA.Allocator
  -> Vk.Device
  -> Word32
  -> ResourceT IO [(String, JP.Image JP.PixelRGBA8)]
render allocator dev graphicsQueueFamilyIndex = do
  let extent = Vk.Extent2D width height

  -- GPU colour render target plus the depth attachment (reused across
  -- variants; headless is serialized), each with its view.
  (_, (image, imageView)) <- createColorTarget allocator dev imageFormat extent
  (_, (depthImage, depthView)) <- createDepthTarget allocator dev depthFormat extent

  -- CPU readback image + its reader.
  (cpuImage, readback) <- makeReadbackImage allocator dev imageFormat extent

  -- Vertex buffer: host-visible, mapped, filled with the 6 vertices.
  let
    vertexBufferCreateInfo =
      zero
        { Vk.size = fromIntegral (length vertices * floatSize)
        , Vk.usage = Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT
        }
    vertexBufferAlloc =
      zero
        { AllocationCreateInfo.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , AllocationCreateInfo.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        , AllocationCreateInfo.requiredFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
        }
  (_, (vertexBuffer, _, vertexBufferAllocInfo)) <-
    VMA.withBuffer allocator vertexBufferCreateInfo vertexBufferAlloc allocate
  liftIO $ pokeArray (castPtr (VMA.mappedData vertexBufferAllocInfo)) vertices

  -- One pipeline: colour + depth attachment, geometry from the vertex buffer.
  let vertexInput =
        zero
          { Vk.vertexBindingDescriptions =
              [Vk.VertexInputBindingDescription 0 (fromIntegral (6 * floatSize)) Vk.VERTEX_INPUT_RATE_VERTEX]
          , Vk.vertexAttributeDescriptions =
              [ Vk.VertexInputAttributeDescription 0 0 Vk.FORMAT_R32G32B32_SFLOAT 0
              , Vk.VertexInputAttributeDescription 1 0 Vk.FORMAT_R32G32B32_SFLOAT (fromIntegral (3 * floatSize))
              ]
          }
  (_, pipeline) <-
    Dynamic.createPipelineFromShaders
      dev
      [imageFormat]
      (Just depthFormat)
      vertexInput
      Nothing -- full always-on dynamic state
      Nothing -- empty pipeline layout (no descriptor sets / push constants)
      () -- no specialization constants
      [(Vk.SHADER_STAGE_VERTEX_BIT, vertCode), (Vk.SHADER_STAGE_FRAGMENT_BIT, fragCode)]

  let poolCreateInfo = zero{CommandPoolCreateInfo.queueFamilyIndex = graphicsQueueFamilyIndex}
  (_, commandPool) <- Vk.withCommandPool dev poolCreateInfo Nothing allocate
  graphicsQueue <- Vk.getDeviceQueue dev graphicsQueueFamilyIndex 0

  let -- (name, compare op, clear depth). LESS clears to the far plane so the near
      -- triangle passes; GREATER clears to the near plane so the far one does.
      variants =
        [ ("less", Vk.COMPARE_OP_LESS, 1)
        , ("greater", Vk.COMPARE_OP_GREATER, 0)
        ]

  forM variants $ \(name, compareOp, clearDepth) -> do
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
      transitionColorAttachment cb image
      transitionDepthAttachment cb depthImage
      let renderingInfo =
            Dynamic.renderingInfo
              (fullScissor extent)
              [(imageView, Vk.Float32 0.1 0.1 0.1 1)]
              (Just (depthView, clearDepth))
      Vk.cmdUseRendering cb renderingInfo do
        Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
        applyDynamicStates
          allDynamicStates
          cb
          (dynamicStateFor extent)
            { depthTest = True
            , depthWrite = True
            , depthCompareOp = compareOp
            }
        Vk.cmdBindVertexBuffers cb 0 [vertexBuffer] [0]
        Vk.cmdDraw cb 6 1 0 0
      copyImageToHost cb extent image cpuImage
    submitAndWait dev graphicsQueue cb ("Timed out rendering variant: " <> name)
    img <- readback
    pure (name, img)

vertCode =
  [vert|
    #version 450
    layout(location = 0) in vec3 inPos;
    layout(location = 1) in vec3 inColor;
    layout(location = 0) out vec3 fragColor;
    void main() {
      gl_Position = vec4(inPos, 1.0);
      fragColor = inColor;
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
