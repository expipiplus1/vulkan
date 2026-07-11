{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| The headless frame, driven by a 'FG.FrameGraph'. Two passes, each a
top-level function that creates its own targets and pipeline right next to its
'FG.addPass': 'offscreenTrianglePass' draws the RGB triangle into an offscreen
colour image, 'cubePass' draws the cube sampling it. The images are imported as
'ManagedImage's; each pass declares how it uses them (via 'usageFlags') and the
graph's 'FG.preRead' / 'FG.preWrite' hooks place every layout-transition barrier
— including the offscreen colour→sampled one that used to be hand-written.

The shared set 0 (Globals) is bound once, in the offscreen pass, and never
rebound; the cube pass only binds its sampler at set 1 (the layouts are
compatible for set 0).
-}
module Render
  ( render
  , width
  , height
  ) where

import qualified Codec.Picture as JP
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.Bits ((.|.))
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (poke, sizeOf)
import qualified Fragr as FG
import HeadlessBoot (submitAndWait)
import ImageReadback (copyImageToHost, makeReadbackImage)
import RenderTarget (createColorTarget, createDepthTarget)
import System.Exit (exitFailure)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as SamplerCreateInfo (SamplerCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import Vulkan.Utils.Descriptors (bufferWrite, combinedImageSamplerWrite)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (DynamicState (..), allDynamicStates, applyDynamicStates, dynamicStateFor, fullScissor)
import Vulkan.Utils.FrameGraph.Image (Usage (..), importManagedImage, newManagedImage, usageFlags)
import Vulkan.Utils.FrameGraph.Recorder (Recorder, newRecorder, recordingCommandBuffer)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA

import Data.SpirV.Reflect.FFI (loadBytes)
import Vulkan.Utils.SpirV.Descriptors (mergedDescriptorSetLayoutInfos)

import qualified Cube
import qualified Cube.Shader
import Tri (Globals (..))
import qualified Tri
import qualified Tri.Shader

width, height :: Word32
width = 256
height = 256

extent :: Vk.Extent2D
extent = Vk.Extent2D width height

colorFormat :: Vk.Format
colorFormat = Vk.FORMAT_R8G8B8A8_UNORM

depthFormat :: Vk.Format
depthFormat = Vk.FORMAT_D32_SFLOAT

floatSize :: Int
floatSize = sizeOf (0 :: Float)

-- | The shared uniform: a single @time@ both pipelines read.
globalsValue :: Globals
globalsValue = Globals{time = 0.7}

{- | The per-frame context shared by every pass: the allocator/device, the two
merged descriptor-set layouts, the descriptor pool the passes draw their sets
from, and the graph they register into.
-}
data Shared = Shared
  { allocator :: VMA.Allocator
  , device :: Vk.Device
  , set0Layout :: Vk.DescriptorSetLayout
  , set1Layout :: Vk.DescriptorSetLayout
  , descriptorPool :: Vk.DescriptorPool
  , graph :: FG.FrameGraph Recorder ()
  }

render
  :: VMA.Allocator
  -> Vk.Device
  -> Word32
  -> ResourceT IO (JP.Image JP.PixelRGBA8)
render allocator device graphicsQueueFamilyIndex = do
  -- Shared wiring: the merged set layouts, the pool, and the Globals UBO with
  -- its set 0 (read by both pipelines).
  (set0Layout, set1Layout) <- mergedSetLayouts device
  descriptorPool <- allocateDescriptorPool device
  globalsSet <- allocateGlobals allocator device descriptorPool set0Layout

  graph <- FG.newFrameGraph
  let shared = Shared{allocator, device, set0Layout, set1Layout, descriptorPool, graph}

  -- Each pass owns its targets and pipeline; the offscreen handle and view flow
  -- into the cube pass (which samples the drawn image and reads it back).
  (offscreenView, offscreenColored) <- offscreenTrianglePass shared globalsSet
  sceneImage <- cubePass shared offscreenView offscreenColored

  FG.compile graph

  (cpuImage, readback) <- makeReadbackImage allocator device colorFormat extent
  (_, commandPool) <-
    Vk.withCommandPool device zero{CommandPoolCreateInfo.queueFamilyIndex = graphicsQueueFamilyIndex} Nothing allocate
  graphicsQueue <- Vk.getDeviceQueue device graphicsQueueFamilyIndex 0
  cb <- oneCommandBuffer device commandPool

  recorder <- newRecorder cb
  let oneShot = zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
  Vk.useCommandBuffer cb oneShot $ do
    -- Records both passes, firing the transition hooks in between.
    FG.execute graph recorder ()
    -- Scene is left in COLOR_ATTACHMENT_OPTIMAL; the readback issues its own
    -- colour->transfer-src barrier.
    copyImageToHost cb extent sceneImage cpuImage
  submitAndWait device graphicsQueue cb "Timed out in the render-to-texture passes"
  readback

{- | Offscreen pass: create the sampled colour target, draw the RGB triangle
into it, and hand back its view (for the cube to sample) and the graph handle
naming the drawn image. Writing the imported target declares COLOR_ATTACHMENT
usage, so the hook transitions it UNDEFINED->attachment before the draw. Binds
the shared Globals set 0 once, under the (compatible) triangle layout.
-}
offscreenTrianglePass :: Shared -> Vk.DescriptorSet -> ResourceT IO (Vk.ImageView, FG.Handle)
offscreenTrianglePass shared globalsSet = do
  (offscreenImage, offscreenView) <- createSampledColorTarget shared.allocator shared.device colorFormat extent

  offscreenH <- importImage shared.graph "offscreen" offscreenImage Vk.IMAGE_ASPECT_COLOR_BIT
  let mkHandle = FG.writeWith offscreenH (usageFlags ColorAttachment)

  tri <- Tri.allocatePipeline shared.device colorFormat shared.set0Layout

  offscreenColored <- FG.addPass shared.graph "offscreen-triangle" mkHandle \_handle -> do
    cb <- recordingCommandBuffer
    let dri = Dynamic.renderingInfo (fullScissor extent) [(offscreenView, Vk.Float32 0 0 0 1)] Nothing
    Vk.cmdUseRendering cb dri do
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS tri.pipeline
      applyDynamicStates allDynamicStates cb (dynamicStateFor extent)
      Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_GRAPHICS tri.pipelineLayout 0 [globalsSet] []
      Vk.cmdDraw cb 3 1 0 0

  pure (offscreenView, offscreenColored)

{- | Cube pass: create the scene colour and depth targets, draw the cube
sampling the offscreen image, and hand back the scene image for readback. Owns
the cube pipeline, the sampler and its set 1. Reading @offscreenColored@ as a
sampled texture is what places the colour->sampled barrier (formerly the
hand-written @colorToSampled@); the scene/depth writes transition those
attachments. Set 0 (Globals) is still bound from the offscreen pass — only set 1
is bound here.
-}
cubePass :: Shared -> Vk.ImageView -> FG.Handle -> ResourceT IO Vk.Image
cubePass shared offscreenView offscreenColored = do
  (_, (sceneImage, sceneView)) <- createColorTarget shared.allocator shared.device colorFormat extent
  (_, (depthImage, depthView)) <- createDepthTarget shared.allocator shared.device depthFormat extent

  sceneH <- importImage shared.graph "scene" sceneImage Vk.IMAGE_ASPECT_COLOR_BIT
  depthH <- importImage shared.graph "depth" depthImage Vk.IMAGE_ASPECT_DEPTH_BIT
  let cubeSetup = do
        FG.readWith offscreenColored (usageFlags (Sampled Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT))
        FG.writeWith_ sceneH (usageFlags ColorAttachment)
        FG.writeWith_ depthH (usageFlags DepthAttachment)

  cube <- Cube.allocatePipeline shared.device colorFormat depthFormat shared.set0Layout shared.set1Layout
  (_, sampler) <- Vk.withSampler shared.device samplerInfo Nothing allocate
  samplerSet <- allocateSamplerSet shared.device shared.descriptorPool shared.set1Layout sampler offscreenView
  cubeBuffer <- cubeVertexBuffer shared.allocator

  FG.addPass_ shared.graph "cube" cubeSetup do
    cb <- recordingCommandBuffer
    let dri = Dynamic.renderingInfo (fullScissor extent) [(sceneView, Vk.Float32 0.30 0.32 0.38 1)] (Just (depthView, 1))
    Vk.cmdUseRendering cb dri do
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS cube.pipeline
      applyDynamicStates
        allDynamicStates
        cb
        (dynamicStateFor extent){depthTest = True, depthWrite = True, depthCompareOp = Vk.COMPARE_OP_LESS}
      Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_GRAPHICS cube.pipelineLayout 1 [samplerSet] []
      Vk.cmdBindVertexBuffers cb 0 [cubeBuffer] [0]
      Vk.cmdDraw cb cubeVertexCount 1 0 0

  pure sceneImage

-- | Import a raw image into the graph as a layout-tracked 'ManagedImage'.
importImage :: FG.FrameGraph Recorder () -> Text -> Vk.Image -> Vk.ImageAspectFlags -> ResourceT IO FG.Handle
importImage graph name image aspect = do
  managed <- newManagedImage image aspect
  importManagedImage graph name managed

{- | The set 0 (Globals UBO, all stages) and set 1 (sampler) layouts, merged
across all four shaders. One layout object per set, reused across both pipeline
layouts, makes them compatible for set 0.
-}
mergedSetLayouts :: Vk.Device -> ResourceT IO (Vk.DescriptorSetLayout, Vk.DescriptorSetLayout)
mergedSetLayouts dev = do
  modules <-
    traverse
      loadBytes
      [Tri.Shader.vertCode, Tri.Shader.fragCode, Cube.Shader.vertCode, Cube.Shader.fragCode]
  setInfos <- orDie (mergedDescriptorSetLayoutInfos modules)
  setLayouts <-
    mapM
      (\(setNo, info) -> do (_, l) <- Vk.withDescriptorSetLayout dev info Nothing allocate; pure (setNo, l))
      setInfos
  let layoutFor n = maybe (error ("missing descriptor set " <> show n)) id (lookup n setLayouts)
  pure (layoutFor 0, layoutFor 1)

-- | A pool for the two descriptor sets: the Globals UBO and the sampler.
allocateDescriptorPool :: Vk.Device -> ResourceT IO Vk.DescriptorPool
allocateDescriptorPool dev = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 2
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1
            ]
        }
      Nothing
      allocate
  pure pool

-- | The Globals UBO (host-visible, mapped) and its set 0 descriptor.
allocateGlobals :: VMA.Allocator -> Vk.Device -> Vk.DescriptorPool -> Vk.DescriptorSetLayout -> ResourceT IO Vk.DescriptorSet
allocateGlobals allocator dev pool set0Layout = do
  (_, (uboBuffer, _, uboInfo)) <-
    VMA.withBuffer
      allocator
      zero{Vk.size = fromIntegral (sizeOf globalsValue), Vk.usage = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT}
      mappedAlloc
      allocate
  liftIO $ poke (castPtr (VMA.mappedData uboInfo)) globalsValue
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [set0Layout]}
  let globalsSet = V.head sets
  Vk.updateDescriptorSets dev [bufferWrite globalsSet 0 Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER uboBuffer] []
  pure globalsSet

-- | Set 1: the offscreen image sampled through @sampler@.
allocateSamplerSet :: Vk.Device -> Vk.DescriptorPool -> Vk.DescriptorSetLayout -> Vk.Sampler -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateSamplerSet dev pool set1Layout sampler view = do
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [set1Layout]}
  let samplerSet = V.head sets
  Vk.updateDescriptorSets
    dev
    [combinedImageSamplerWrite samplerSet 0 sampler view Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL]
    []
  pure samplerSet

{- | The cube vertex buffer (host-visible, mapped) — plain floats, interpreted
by the reflected attribute descriptions.
-}
cubeVertexBuffer :: VMA.Allocator -> ResourceT IO Vk.Buffer
cubeVertexBuffer allocator = do
  (_, (cubeBuffer, _, cubeBufInfo)) <-
    VMA.withBuffer
      allocator
      zero{Vk.size = fromIntegral (length cubeVertices * floatSize), Vk.usage = Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT}
      mappedAlloc
      allocate
  liftIO $ pokeArray (castPtr (VMA.mappedData cubeBufInfo)) cubeVertices
  pure cubeBuffer

{- | A unit cube centred at the origin: 6 faces × 2 triangles, each vertex five
floats @px py pz u v@ (tightly packed, matching the reflected vertex input).
-}
cubeVertices :: [Float]
cubeVertices = concatMap faceVerts faces
  where
    h = 0.5
    add :: (Float, Float, Float) -> (Float, Float, Float) -> (Float, Float, Float)
    add (a, b, c) (d, e, f) = (a + d, b + e, c + f)
    -- Each face: an origin corner and two edge vectors (length 1).
    faces :: [((Float, Float, Float), (Float, Float, Float), (Float, Float, Float))]
    faces =
      [ ((-h, -h, h), (1, 0, 0), (0, 1, 0)) -- +Z
      , ((h, -h, -h), (-1, 0, 0), (0, 1, 0)) -- -Z
      , ((h, -h, h), (0, 0, -1), (0, 1, 0)) -- +X
      , ((-h, -h, -h), (0, 0, 1), (0, 1, 0)) -- -X
      , ((-h, h, h), (1, 0, 0), (0, 0, -1)) -- +Y
      , ((-h, -h, -h), (1, 0, 0), (0, 0, 1)) -- -Y
      ]
    faceVerts :: ((Float, Float, Float), (Float, Float, Float), (Float, Float, Float)) -> [Float]
    faceVerts (o, du, dv) =
      let
        c00 = o
        c10 = add o du
        c11 = add (add o du) dv
        c01 = add o dv
        -- V is flipped (top edge -> v = 0): Vulkan's texture origin is top-left,
        -- so v = 0 is the top row of the sampled offscreen image.
        quad :: [((Float, Float, Float), (Float, Float))]
        quad = [(c00, (0, 1)), (c10, (1, 1)), (c11, (1, 0)), (c00, (0, 1)), (c11, (1, 0)), (c01, (0, 0))]
      in
        concatMap (\((x, y, z), (u, v)) -> [x, y, z, u, v]) quad

{- | Five floats per vertex (@px py pz u v@), so the draw count is derived from
the geometry rather than asserted separately.
-}
cubeVertexCount :: Word32
cubeVertexCount = fromIntegral (length cubeVertices `div` 5)

-- | Print a merged-layout conflict and exit non-zero.
orDie :: Either String a -> ResourceT IO a
orDie = either (\e -> liftIO (putStrLn ("merged layout error: " <> e) >> exitFailure)) pure

mappedAlloc :: VMA.AllocationCreateInfo
mappedAlloc =
  zero
    { AllocationCreateInfo.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
    , AllocationCreateInfo.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
    , AllocationCreateInfo.requiredFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
    }

samplerInfo :: Vk.SamplerCreateInfo '[]
samplerInfo =
  zero
    { SamplerCreateInfo.magFilter = Vk.FILTER_LINEAR
    , SamplerCreateInfo.minFilter = Vk.FILTER_LINEAR
    , SamplerCreateInfo.addressModeU = Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
    , SamplerCreateInfo.addressModeV = Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
    }

oneCommandBuffer :: Vk.Device -> Vk.CommandPool -> ResourceT IO Vk.CommandBuffer
oneCommandBuffer dev pool = do
  (_, cbs) <-
    Vk.withCommandBuffers
      dev
      zero{Vk.commandPool = pool, Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY, Vk.commandBufferCount = 1}
      allocate
  pure (V.head cbs)

{- | A GPU-only colour target that can be a colour attachment AND be sampled (the
offscreen render-to-texture target). Like 'RenderTarget.createColorTarget' but
with @SAMPLED@ instead of @TRANSFER_SRC@.
-}
createSampledColorTarget
  :: VMA.Allocator -> Vk.Device -> Vk.Format -> Vk.Extent2D -> ResourceT IO (Vk.Image, Vk.ImageView)
createSampledColorTarget allocator dev format (Vk.Extent2D w h) = do
  (_, (image, _, _)) <- VMA.withImage allocator imageCreateInfo gpuAlloc allocate
  (_, view) <- Vk.withImageView dev (viewCreateInfo image) Nothing allocate
  pure (image, view)
  where
    gpuAlloc = zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}
    imageCreateInfo =
      zero
        { Vk.imageType = Vk.IMAGE_TYPE_2D
        , Vk.format = format
        , Vk.extent = Vk.Extent3D w h 1
        , Vk.mipLevels = 1
        , Vk.arrayLayers = 1
        , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
        , Vk.tiling = Vk.IMAGE_TILING_OPTIMAL
        , Vk.usage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
    viewCreateInfo image =
      zero
        { Vk.image = image
        , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format = format
        , Vk.subresourceRange = Vk.ImageSubresourceRange Vk.IMAGE_ASPECT_COLOR_BIT 0 1 0 1
        }
