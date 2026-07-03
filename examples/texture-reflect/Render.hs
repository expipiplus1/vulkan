{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The headless frame: the offscreen RGB triangle, a barrier to a sampleable
layout, then the cube pass sampling it — both in one command buffer. The
shared set 0 (Globals) is bound once, before the offscreen pass, and never
rebound.
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
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (poke, sizeOf)
import HeadlessBoot (submitAndWait)
import ImageReadback (copyImageToHost, makeReadbackImage)
import RenderTarget (createColorTarget, createDepthTarget)
import System.Exit (exitFailure)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as SamplerCreateInfo (SamplerCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import Vulkan.Utils.Barrier (imageBarrier, transitionColorAttachment, transitionDepthAttachment)
import Vulkan.Utils.Descriptors (bufferWrite, combinedImageSamplerWrite)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (DynamicState (..), allDynamicStates, applyDynamicStates, dynamicStateFor, fullScissor)
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

colorFormat :: Vk.Format
colorFormat = Vk.FORMAT_R8G8B8A8_UNORM

depthFormat :: Vk.Format
depthFormat = Vk.FORMAT_D32_SFLOAT

floatSize :: Int
floatSize = sizeOf (0 :: Float)

-- | The shared uniform: a single @time@ both pipelines read.
globalsValue :: Globals
globalsValue = Globals{time = 0.7}

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

render
  :: VMA.Allocator
  -> Vk.Device
  -> Word32
  -> ResourceT IO (JP.Image JP.PixelRGBA8)
render allocator dev graphicsQueueFamilyIndex = do
  let extent = Vk.Extent2D width height

  -- Offscreen target (sampled in the cube pass), final colour target (read back),
  -- depth for the cube, and the host readback image.
  (offscreenImage, offscreenView) <- createSampledColorTarget allocator dev colorFormat extent
  (_, (sceneImage, sceneView)) <- createColorTarget allocator dev colorFormat extent
  (_, (depthImage, depthView)) <- createDepthTarget allocator dev depthFormat extent
  (cpuImage, readback) <- makeReadbackImage allocator dev colorFormat extent

  -- Shared Globals UBO (host-visible, mapped).
  (_, (uboBuffer, _, uboInfo)) <-
    VMA.withBuffer
      allocator
      zero{Vk.size = fromIntegral (sizeOf globalsValue), Vk.usage = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT}
      mappedAlloc
      allocate
  liftIO $ poke (castPtr (VMA.mappedData uboInfo)) globalsValue

  -- Cube vertex buffer (host-visible, mapped) — plain floats, interpreted by the
  -- reflected attribute descriptions.
  (_, (cubeBuffer, _, cubeBufInfo)) <-
    VMA.withBuffer
      allocator
      zero{Vk.size = fromIntegral (length cubeVertices * floatSize), Vk.usage = Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT}
      mappedAlloc
      allocate
  liftIO $ pokeArray (castPtr (VMA.mappedData cubeBufInfo)) cubeVertices

  (_, sampler) <- Vk.withSampler dev samplerInfo Nothing allocate

  -- ONE set 0 layout (the Globals UBO, visible to all stages that read it) and one
  -- set 1 layout (the sampler), merged across all four shaders. Reusing the set 0
  -- layout object in both pipeline layouts makes them compatible for set 0.
  modules <-
    traverse
      loadBytes
      [Tri.Shader.vertCode, Tri.Shader.fragCode, Cube.Shader.vertCode, Cube.Shader.fragCode]
  setInfos <- orDie (mergedDescriptorSetLayoutInfos modules)
  setLayouts <-
    mapM
      (\(setNo, info) -> do (_, l) <- Vk.withDescriptorSetLayout dev info Nothing allocate; pure (setNo, l))
      setInfos
  let
    layoutFor n = maybe (error ("missing descriptor set " <> show n)) id (lookup n setLayouts)
    set0Layout = layoutFor 0
    set1Layout = layoutFor 1

  tri <- Tri.allocatePipeline dev colorFormat set0Layout
  cube <- Cube.allocatePipeline dev colorFormat depthFormat set0Layout set1Layout

  -- Descriptor sets: set 0 = Globals UBO, set 1 = the offscreen image + sampler.
  (_, descriptorPool) <-
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
  descriptorSets <-
    Vk.allocateDescriptorSets
      dev
      zero{Vk.descriptorPool = descriptorPool, Vk.setLayouts = [set0Layout, set1Layout]}
  let
    globalsSet = descriptorSets V.! 0
    samplerSet = descriptorSets V.! 1
  Vk.updateDescriptorSets
    dev
    [ bufferWrite globalsSet 0 Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER uboBuffer
    , combinedImageSamplerWrite samplerSet 0 sampler offscreenView Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    ]
    []

  (_, commandPool) <-
    Vk.withCommandPool dev zero{CommandPoolCreateInfo.queueFamilyIndex = graphicsQueueFamilyIndex} Nothing allocate
  graphicsQueue <- Vk.getDeviceQueue dev graphicsQueueFamilyIndex 0
  cb <- oneCommandBuffer dev commandPool

  let oneShot = zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
  Vk.useCommandBuffer cb oneShot $ do
    transitionColorAttachment cb offscreenImage
    transitionColorAttachment cb sceneImage
    transitionDepthAttachment cb depthImage

    -- Pass 1: draw the RGB triangle into the offscreen image. The shared Globals
    -- descriptor is bound here, ONCE, under the (compatible) triangle layout.
    Vk.cmdUseRendering cb (Dynamic.renderingInfo (fullScissor extent) [(offscreenView, Vk.Float32 0 0 0 1)] Nothing) $ do
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS tri.pipeline
      applyDynamicStates allDynamicStates cb (dynamicStateFor extent)
      Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_GRAPHICS tri.pipelineLayout 0 [globalsSet] []
      Vk.cmdDraw cb 3 1 0 0

    -- Make the offscreen colour image readable by the cube's fragment shader.
    colorToSampled cb offscreenImage

    -- Pass 2: draw the cube sampling the offscreen image. Set 0 (Globals) is still
    -- bound — only the sampler at set 1 is bound now.
    Vk.cmdUseRendering
      cb
      (Dynamic.renderingInfo (fullScissor extent) [(sceneView, Vk.Float32 0.30 0.32 0.38 1)] (Just (depthView, 1)))
      $ do
        Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS cube.pipeline
        applyDynamicStates
          allDynamicStates
          cb
          (dynamicStateFor extent){depthTest = True, depthWrite = True, depthCompareOp = Vk.COMPARE_OP_LESS}
        Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_GRAPHICS cube.pipelineLayout 1 [samplerSet] []
        Vk.cmdBindVertexBuffers cb 0 [cubeBuffer] [0]
        Vk.cmdDraw cb cubeVertexCount 1 0 0

    copyImageToHost cb extent sceneImage cpuImage
  submitAndWait dev graphicsQueue cb "Timed out in the render-to-texture passes"
  readback
  where
    samplerInfo =
      zero
        { SamplerCreateInfo.magFilter = Vk.FILTER_LINEAR
        , SamplerCreateInfo.minFilter = Vk.FILTER_LINEAR
        , SamplerCreateInfo.addressModeU = Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        , SamplerCreateInfo.addressModeV = Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
        }

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

{- | Barrier a colour image from a colour attachment (after the offscreen pass) to
a shader-readable texture for the next pass's fragment sampling.
-}
colorToSampled :: Vk.CommandBuffer -> Vk.Image -> ResourceT IO ()
colorToSampled cb img =
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    zero
    []
    []
    [ imageBarrier
        Vk.IMAGE_ASPECT_COLOR_BIT
        Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        Vk.ACCESS_SHADER_READ_BIT
        Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
        img
    ]
