{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-| Headless demonstration of __using a colour attachment as a texture__
(render-to-texture), driven by reflected SPIR-V. Two pipelines run back to back
in one command buffer:

  * __offscreen pass__: a full-screen triangle with red/green/blue corners is
    drawn into an offscreen colour image (@COLOR_ATTACHMENT | SAMPLED@). A barrier
    then moves that image from @COLOR_ATTACHMENT_OPTIMAL@ to
    @SHADER_READ_ONLY_OPTIMAL@.

  * __cube pass__: a spinning cube — its @position@ + @uv@ vertex attributes
    described from reflection ("Vulkan.Utils.SpirV.VertexInput") — samples the
    offscreen image through a @sampler2D@ at set 1, binding 0.

Both pipelines share a @Globals@ UBO at __set 0, binding 0__ (a @time@). The set 0
descriptor-set layout is a single object reused by both pipeline layouts, so the
layouts are /compatible for set 0/: the UBO is bound __once__ before the offscreen
pass and is never rebound — the cube pass only binds its sampler at set 1. (The
cross-pipeline layout match is not type-enforced here; each pipeline's own
vertex↔fragment composition still is, via 'MatchInterface' / 'CompatibleResources'.)
-}
module Main where

import qualified Codec.Picture as JP
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.List (foldl')
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Marshal.Array (pokeArray)
import Foreign.Ptr (castPtr)
import Foreign.Storable (poke, sizeOf)
import Graphics.Gl.Block (Std140 (..))
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), submitAndWait, withHeadlessVk)
import ImageReadback (copyImageToHost, makeReadbackImage, savePng)
import RenderTarget (createColorTarget, createDepthTarget)
import System.Exit (exitFailure)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as PipelineLayoutCreateInfo (PipelineLayoutCreateInfo (..))
import qualified Vulkan.Core10 as SamplerCreateInfo (SamplerCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import Vulkan.Utils.Barrier (imageBarrier, transitionColorAttachment, transitionDepthAttachment)
import Vulkan.Utils.Descriptors (bufferWrite, combinedImageSamplerWrite)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (DynamicState (..), allDynamicStates, applyDynamicStates, dynamicStateFor, fullScissor)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA

import Data.SpirV.Reflect.FFI (loadBytes)
import Vulkan.Utils.SpirV.Descriptors (mergedDescriptorSetLayoutInfos)
import Vulkan.Utils.SpirV.Stage (CompatibleResources, MatchInterface, reflectStageSig)
import Vulkan.Utils.SpirV.TH (reflectShaderTypes)
import Vulkan.Utils.SpirV.VertexInput (vertexInputState)

-- Compile-time reflection: the shared @Globals@ UBO record (from any shader that
-- declares it) and a stage signature per shader.
reflectShaderTypes "texture-reflect/tri.frag.spv"
reflectStageSig "TriVert" "texture-reflect/tri.vert.spv"
reflectStageSig "TriFrag" "texture-reflect/tri.frag.spv"
reflectStageSig "CubeVert" "texture-reflect/cube.vert.spv"
reflectStageSig "CubeFrag" "texture-reflect/cube.frag.spv"

-- Each pipeline's own vertex↔fragment composition, proved at compile time.
triComposes :: (MatchInterface TriVert TriFrag, CompatibleResources TriVert TriFrag) => Bool
triComposes = True

cubeComposes :: (MatchInterface CubeVert CubeFrag, CompatibleResources CubeVert CubeFrag) => Bool
cubeComposes = True

triVertSpv, triFragSpv, cubeVertSpv, cubeFragSpv :: ByteString
triVertSpv = $(embedFile "texture-reflect/tri.vert.spv")
triFragSpv = $(embedFile "texture-reflect/tri.frag.spv")
cubeVertSpv = $(embedFile "texture-reflect/cube.vert.spv")
cubeFragSpv = $(embedFile "texture-reflect/cube.frag.spv")

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

main :: IO ()
main = runResourceT $ do
  HeadlessVk{..} <-
    withHeadlessVk
      HeadlessConfig
        { appName = "Haskell Vulkan colour-attachment-as-texture (headless)"
        , instanceReqs = []
        , deviceReqs = Dynamic.dynamicRenderingRequirements
        , vmaFlags = zero
        }
  liftIO $ do
    putStrLn $ "offscreen pipeline composes (compile-time): " <> show triComposes
    putStrLn $ "cube pipeline composes (compile-time):      " <> show cubeComposes
  let QueueFamilyIndex graphicsQueueFamilyIndex = fst (qGraphics queues)

  colorImage <- render allocator device graphicsQueueFamilyIndex
  Vk.deviceWaitIdle device

  savePng "texture-reflect.png" colorImage
  let
    pixel x y = JP.pixelAt colorImage x y
    allPixels = [pixel x y | y <- [0 .. fromIntegral height - 1], x <- [0 .. fromIntegral width - 1]]
    -- Brightest sampled R/G/B across the image, in a single pass.
    (maxR, maxG, maxB) =
      foldl'
        (\(!r, !g, !b) (JP.PixelRGBA8 pr pg pb _) -> (max r (fromIntegral pr), max g (fromIntegral pg), max b (fromIntegral pb)))
        ((0, 0, 0) :: (Int, Int, Int))
        allPixels
    JP.PixelRGBA8 cr cg cb _ = pixel 2 2
    cornerChannels = [fromIntegral cr, fromIntegral cg, fromIntegral cb] :: [Int]
    cornerLum = sum cornerChannels
    cornerSaturation = maximum cornerChannels - minimum cornerChannels
  liftIO $
    putStrLn $
      "sampled max channels: R="
        <> show maxR
        <> " G="
        <> show maxG
        <> " B="
        <> show maxB
        <> "; background corner luminance="
        <> show cornerLum
        <> " saturation="
        <> show cornerSaturation

  let
    checks :: [(String, Bool)]
    checks =
      [ ("offscreen red was sampled onto the cube", maxR > 150)
      , ("offscreen green was sampled onto the cube", maxG > 150)
      , ("offscreen blue was sampled onto the cube", maxB > 150)
      , ("background is the neutral clear colour, not the texture", cornerLum > 80 && cornerSaturation < 40)
      ]
  liftIO $ do
    mapM_ (\(label, ok) -> putStrLn $ "[" <> (if ok then "PASS" else "FAIL") <> "] " <> label) checks
    unless (all snd checks) exitFailure
    putStrLn "All render-to-texture checks passed."

render
  :: VMA.Allocator
  -> Vk.Device
  -> Word32
  -> ResourceT IO (JP.Image JP.PixelRGBA8)
render allocator dev graphicsQueueFamilyIndex = do
  let extent = Vk.Extent2D width height

  -- Reflected modules drive the merged set layouts and the cube's vertex input.
  triVertModule <- loadBytes triVertSpv
  triFragModule <- loadBytes triFragSpv
  cubeVertModule <- loadBytes cubeVertSpv
  cubeFragModule <- loadBytes cubeFragSpv

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
  setInfos <- orDie (mergedDescriptorSetLayoutInfos [triVertModule, triFragModule, cubeVertModule, cubeFragModule])
  setLayouts <-
    mapM
      (\(setNo, info) -> do (_, l) <- Vk.withDescriptorSetLayout dev info Nothing allocate; pure (setNo, l))
      setInfos
  let
    layoutFor n = maybe (error ("missing descriptor set " <> show n)) id (lookup n setLayouts)
    set0Layout = layoutFor 0
    set1Layout = layoutFor 1
  (_, triPipelineLayout) <-
    Vk.withPipelineLayout dev zero{PipelineLayoutCreateInfo.setLayouts = [set0Layout]} Nothing allocate
  (_, cubePipelineLayout) <-
    Vk.withPipelineLayout dev zero{PipelineLayoutCreateInfo.setLayouts = [set0Layout, set1Layout]} Nothing allocate

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

  -- Offscreen triangle pipeline (colour only, no vertex input) and cube pipeline
  -- (colour + depth, vertex attributes from reflection).
  let cubeVertexInput = vertexInputState cubeVertModule
  (_, triPipeline) <-
    Dynamic.allocatePipelineFromShaders
      dev
      zero
        { Dynamic.colorFormats = [colorFormat]
        , Dynamic.layout = Just triPipelineLayout
        }
      ()
      [(Vk.SHADER_STAGE_VERTEX_BIT, triVertSpv), (Vk.SHADER_STAGE_FRAGMENT_BIT, triFragSpv)]
  (_, cubePipeline) <-
    Dynamic.allocatePipelineFromShaders
      dev
      zero
        { Dynamic.colorFormats = [colorFormat]
        , Dynamic.depthFormat = Just depthFormat
        , Dynamic.vertexInput = cubeVertexInput
        , Dynamic.layout = Just cubePipelineLayout
        }
      ()
      [(Vk.SHADER_STAGE_VERTEX_BIT, cubeVertSpv), (Vk.SHADER_STAGE_FRAGMENT_BIT, cubeFragSpv)]

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
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS triPipeline
      applyDynamicStates allDynamicStates cb (dynamicStateFor extent)
      Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_GRAPHICS triPipelineLayout 0 [globalsSet] []
      Vk.cmdDraw cb 3 1 0 0

    -- Make the offscreen colour image readable by the cube's fragment shader.
    colorToSampled cb offscreenImage

    -- Pass 2: draw the cube sampling the offscreen image. Set 0 (Globals) is still
    -- bound — only the sampler at set 1 is bound now.
    Vk.cmdUseRendering
      cb
      (Dynamic.renderingInfo (fullScissor extent) [(sceneView, Vk.Float32 0.30 0.32 0.38 1)] (Just (depthView, 1)))
      $ do
        Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS cubePipeline
        applyDynamicStates
          allDynamicStates
          cb
          (dynamicStateFor extent){depthTest = True, depthWrite = True, depthCompareOp = Vk.COMPARE_OP_LESS}
        Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_GRAPHICS cubePipelineLayout 1 [samplerSet] []
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
