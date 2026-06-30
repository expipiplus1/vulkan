{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-| Headless demonstration of type-verified pipeline assembly from reflected
SPIR-V. A single triangle's vertices live in an SSBO; ONE vertex shader pulls
them (indexed by @gl_VertexIndex@) and drives two pipelines built from the same
reflected resources:

  * a __depth-only__ pipeline (vertex stage alone, no colour attachment) — a
    z-prepass whose depth buffer is read back and checked (near depth written at
    the centre, the cleared far value at the corner).

  * a __depth+colour__ pipeline (vertex + fragment) — the fragment stage shades
    the surface with a Lambert (N·L) light from the shared @Scene@ UBO, producing
    a top-bright/bottom-dark gradient that is read back and checked.

The fragment stage's compatibility with the vertex stage — matching @out@/@in@
interface and shared use of the @Scene@ descriptor — is verified __at compile
time__ by 'MatchInterface' / 'CompatibleResources' (see 'pipelineComposes'); the
merged pipeline layout (Scene visible to both stages, the Mesh SSBO to the vertex
stage) and each pipeline come from 'allocateReflectedLayout' / 'allocateGraphicsPipeline'.
Exits non-zero on mismatch.
-}
module Main where

import qualified Codec.Picture as JP
import Control.Monad (unless, zipWithM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peekElemOff, poke, sizeOf)
import qualified Geomancy
import qualified Geomancy.Mat4 as Mat4
import Geomancy.Vec3 (vec3)
import Geomancy.Vec4 (vec4)
import Graphics.Gl.Block (Std140 (..), Std430 (..))
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), submitAndWait, withHeadlessVk)
import ImageReadback (copyImageToHost, makeReadbackImage, savePng)
import RenderTarget (createColorTarget)
import System.Exit (exitFailure)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import Vulkan.Utils.Barrier (imageBarrier, transitionColorAttachment, transitionDepthAttachment)
import Vulkan.Utils.Descriptors (bufferWrite)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (DynamicState (..), allDynamicStates, applyDynamicStates, depthOnlyDynamicStates, dynamicStateFor, fullScissor)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA

import Data.SpirV.Reflect.FFI (loadBytes)
import qualified Vulkan.Utils.SpirV.Array as Array
import Vulkan.Utils.SpirV.Buffer (Buffer, unsafeAsBufferPtr, writeBufferElem)
import Vulkan.Utils.SpirV.Pipeline (ReflectedLayout (..), allocateGraphicsPipeline, allocateReflectedLayout)
import Vulkan.Utils.SpirV.Signature (ArrayOf, Sig)
import Vulkan.Utils.SpirV.Stage (CompatibleResources, MatchInterface, reflectStageSig)
import Vulkan.Utils.SpirV.TH (reflectShaderTypes)

-- Reflection at compile time: the @Scene@ UBO record and the @Vertex@ SSBO
-- element record, plus a stage signature for each shader.
reflectShaderTypes "mesh-reflect/shader.vert.spv"
reflectStageSig "VertSig" "mesh-reflect/shader.vert.spv"
reflectStageSig "FragSig" "mesh-reflect/shader.frag.spv"

-- Compile-time proof that the fragment stage composes with the vertex stage:
-- matching interface and compatible shared resources. Evaluating it forces GHC to
-- discharge those constraints.
pipelineComposes :: (MatchInterface VertSig FragSig, CompatibleResources VertSig FragSig) => Bool
pipelineComposes = True

-- Committed SPIR-V, embedded for the runtime shader modules and reflection.
vertSpv :: ByteString
vertSpv = $(embedFile "mesh-reflect/shader.vert.spv")

fragSpv :: ByteString
fragSpv = $(embedFile "mesh-reflect/shader.frag.spv")

width, height :: Word32
width = 256
height = 256

colorFormat :: Vk.Format
colorFormat = Vk.FORMAT_R8G8B8A8_UNORM

depthFormat :: Vk.Format
depthFormat = Vk.FORMAT_D32_SFLOAT

floatSize :: Int
floatSize = sizeOf (0 :: Float)

{- | The triangle, stored in the SSBO. Apex at the top (Vulkan clip space is +Y
down), facing the light; the base faces away — so Lambert shading gives a
top-bright/bottom-dark gradient. Vertices are white; the colour comes from the
light.
-}
meshVertices :: [Vertex]
meshVertices =
  [ Vertex{position = vec3 0.0 (-0.7) 0.5, normal = vec3 0.0 (-1.0) 0.6, color = vec3 1 1 1}
  , Vertex{position = vec3 (-0.7) 0.6 0.5, normal = vec3 0.0 1.0 0.6, color = vec3 1 1 1}
  , Vertex{position = vec3 0.7 0.6 0.5, normal = vec3 0.0 1.0 0.6, color = vec3 1 1 1}
  ]

vertexCount :: Word32
vertexCount = 3

-- | Identity transform, a light from the top-front, and a warm light colour.
sceneValue :: Scene
sceneValue =
  Scene
    { transform = Mat4.identity
    , lightDir = vec4 0.0 (-1.0) 0.6 0.0
    , lightColor = vec4 1.0 0.85 0.6 1.0
    }

main :: IO ()
main = runResourceT $ do
  HeadlessVk{..} <-
    withHeadlessVk
      HeadlessConfig
        { appName = "Haskell Vulkan type-verified pipeline assembly (headless)"
        , instanceReqs = []
        , deviceReqs = Dynamic.dynamicRenderingRequirements
        , vmaFlags = zero
        }
  liftIO $ putStrLn $ "vertex+fragment pipeline composes (compile-time): " <> show pipelineComposes
  let QueueFamilyIndex graphicsQueueFamilyIndex = fst (qGraphics queues)

  (depthCentre, depthCorner, colorImage) <-
    render allocator device graphicsQueueFamilyIndex
  Vk.deviceWaitIdle device

  savePng "mesh-reflect-color.png" colorImage
  let
    pixel x y = JP.pixelAt colorImage x y
    lum (JP.PixelRGBA8 r g b _) = fromIntegral r + fromIntegral g + fromIntegral b :: Int
    cx = fromIntegral width `div` 2
    upper = lum (pixel cx (fromIntegral height `div` 4)) -- near the lit apex
    lower = lum (pixel cx (3 * fromIntegral height `div` 4)) -- near the dark base
    centre = lum (pixel cx (fromIntegral height `div` 2))
    corner = lum (pixel 4 4) -- background
  liftIO $ do
    putStrLn $ "depth-only:  centre=" <> show depthCentre <> " corner=" <> show depthCorner
    putStrLn $
      "depth+color: luminance upper="
        <> show upper
        <> " lower="
        <> show lower
        <> " centre="
        <> show centre
        <> " corner="
        <> show corner

  let
    checks :: [(String, Bool)]
    checks =
      [ ("depth-only wrote near depth at the centre", depthCentre < 0.9)
      , ("depth-only left the corner at the far plane", depthCorner > 0.99)
      , ("depth+color lit the geometry above the background", centre > corner + 60)
      , ("Lambert shading is brighter near the light (top) than away (bottom)", upper > lower + 60)
      ]
  liftIO $ do
    mapM_ (\(label, ok) -> putStrLn $ "[" <> (if ok then "PASS" else "FAIL") <> "] " <> label) checks
    unless (all snd checks) exitFailure
    putStrLn "All type-verified pipeline-assembly checks passed."

render
  :: VMA.Allocator
  -> Vk.Device
  -> Word32
  -> ResourceT IO (Float, Float, JP.Image JP.PixelRGBA8)
render allocator dev graphicsQueueFamilyIndex = do
  let extent = Vk.Extent2D width height

  -- Reflected modules drive the merged pipeline layout.
  vertModule <- loadBytes vertSpv
  fragModule <- loadBytes fragSpv

  (_, (image, imageView)) <- createColorTarget allocator dev colorFormat extent
  -- Depth target with TRANSFER_SRC so the prepass result can be read back.
  (depthImage, depthView) <- createReadableDepthTarget allocator dev depthFormat extent
  (cpuImage, readback) <- makeReadbackImage allocator dev colorFormat extent

  -- Geometry SSBO (host-visible, mapped). The mapped pointer is tagged as a typed
  -- runtime array of the reflected @Vertex@ element — @'ArrayOf' ('Sig' Vertex)@ —
  -- and each vertex is written with 'writeBufferElem', which is gated by
  -- 'Vulkan.Utils.SpirV.Signature.FitsTail': a wrong element layout (or indexing a
  -- non-array buffer) is a compile error, and the element stride comes from the
  -- signature, not a hand-computed offset.
  (_, (meshBuffer, _, meshInfo)) <-
    VMA.withBuffer
      allocator
      zero
        { Vk.size = fromIntegral (length meshVertices * Array.std430Stride (Proxy @Vertex))
        , Vk.usage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
        }
      mappedAlloc
      allocate
  meshBuf <- unsafeAsBufferPtr (VMA.mappedData meshInfo) :: ResourceT IO (Buffer (ArrayOf (Sig Vertex)))
  zipWithM_ (writeBufferElem meshBuf) [0 ..] meshVertices

  -- Scene UBO (host-visible, mapped).
  (_, (sceneBuffer, _, sceneInfo)) <-
    VMA.withBuffer
      allocator
      zero
        { Vk.size = fromIntegral (sizeOf sceneValue)
        , Vk.usage = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
        }
      mappedAlloc
      allocate
  liftIO $ poke (castPtr (VMA.mappedData sceneInfo)) sceneValue

  -- Descriptor set layouts + pipeline layout from reflection, merged across both
  -- stages: Scene visible to vertex AND fragment (stage-flag union), the Mesh SSBO
  -- to the vertex stage. One layout, shared by both pipelines below.
  (_, layout) <- allocateReflectedLayout dev [vertModule, fragModule]

  -- One descriptor set: binding 0 -> Scene UBO, binding 1 -> Mesh SSBO.
  (_, descriptorPool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 1
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 1
            ]
        }
      Nothing
      allocate
  descriptorSets <-
    Vk.allocateDescriptorSets
      dev
      zero
        { Vk.descriptorPool = descriptorPool
        , Vk.setLayouts = V.fromList (map snd layout.setLayouts)
        }
  let descriptorSet = V.head descriptorSets
  Vk.updateDescriptorSets
    dev
    [ bufferWrite descriptorSet 0 Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER sceneBuffer
    , bufferWrite descriptorSet 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER meshBuffer
    ]
    []

  -- Two pipelines from the same vertex shader and the same layout: depth-only (no
  -- colour attachment) and depth+colour. Vertex input is empty — geometry comes
  -- from the SSBO via gl_VertexIndex.
  (_, depthOnlyPipeline) <-
    allocateGraphicsPipeline
      dev
      layout
      zero{Dynamic.depthFormat = Just depthFormat}
      ()
      [(vertModule, vertSpv)]
  (_, colorPipeline) <-
    allocateGraphicsPipeline
      dev
      layout
      zero
        { Dynamic.colorFormats = [colorFormat]
        , Dynamic.depthFormat = Just depthFormat
        }
      ()
      [(vertModule, vertSpv), (fragModule, fragSpv)]

  (_, commandPool) <-
    Vk.withCommandPool dev zero{CommandPoolCreateInfo.queueFamilyIndex = graphicsQueueFamilyIndex} Nothing allocate
  graphicsQueue <- Vk.getDeviceQueue dev graphicsQueueFamilyIndex 0

  -- Depth buffer readback target (host-visible buffer of width*height floats).
  (_, (depthBuffer, depthAllocation, depthBufInfo)) <-
    VMA.withBuffer
      allocator
      zero
        { Vk.size = fromIntegral (fromIntegral width * fromIntegral height * floatSize)
        , Vk.usage = Vk.BUFFER_USAGE_TRANSFER_DST_BIT
        }
      readbackAlloc
      allocate

  let
    oneShot = zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
    -- Apply only the dynamic states the bound pipeline declares (the depth-only
    -- pipeline has no colour-blend state).
    bindCommon dynStates cb pipeline = do
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS pipeline
      applyDynamicStates
        dynStates
        cb
        (dynamicStateFor extent){depthTest = True, depthWrite = True, depthCompareOp = Vk.COMPARE_OP_LESS}
      Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_GRAPHICS layout.pipelineLayout 0 [descriptorSet] []
      Vk.cmdDraw cb vertexCount 1 0 0

  -- Pass 1: depth-only z-prepass, then copy the depth image to the host buffer.
  cb1 <- oneCommandBuffer dev commandPool
  Vk.useCommandBuffer cb1 oneShot do
    transitionDepthAttachment cb1 depthImage
    Vk.cmdUseRendering cb1 (Dynamic.renderingInfo (fullScissor extent) [] (Just (depthView, 1))) $
      bindCommon depthOnlyDynamicStates cb1 depthOnlyPipeline
    depthToTransferSrc cb1 depthImage
    Vk.cmdCopyImageToBuffer cb1 depthImage Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL depthBuffer [depthCopyRegion extent]
  submitAndWait dev graphicsQueue cb1 "Timed out in the depth-only prepass"
  VMA.invalidateAllocation allocator depthAllocation 0 Vk.WHOLE_SIZE
  let depthPtr = castPtr (VMA.mappedData depthBufInfo)
  depthCentre <- liftIO $ peekElemOff depthPtr (fromIntegral (height `div` 2) * fromIntegral width + fromIntegral (width `div` 2))
  depthCorner <- liftIO $ peekElemOff depthPtr 0

  -- Pass 2: depth+colour, then copy the colour image to the host image.
  cb2 <- oneCommandBuffer dev commandPool
  Vk.useCommandBuffer cb2 oneShot do
    transitionColorAttachment cb2 image
    transitionDepthAttachment cb2 depthImage
    Vk.cmdUseRendering
      cb2
      (Dynamic.renderingInfo (fullScissor extent) [(imageView, Vk.Float32 0.05 0.05 0.05 1)] (Just (depthView, 1)))
      $ bindCommon allDynamicStates cb2 colorPipeline
    copyImageToHost cb2 extent image cpuImage
  submitAndWait dev graphicsQueue cb2 "Timed out in the colour pass"
  colorImage <- readback

  pure (depthCentre, depthCorner, colorImage)

mappedAlloc :: VMA.AllocationCreateInfo
mappedAlloc =
  zero
    { AllocationCreateInfo.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
    , AllocationCreateInfo.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
    , AllocationCreateInfo.requiredFlags = Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT
    }

readbackAlloc :: VMA.AllocationCreateInfo
readbackAlloc =
  zero
    { AllocationCreateInfo.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
    , AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_TO_CPU
    }

oneCommandBuffer :: Vk.Device -> Vk.CommandPool -> ResourceT IO Vk.CommandBuffer
oneCommandBuffer dev pool = do
  (_, cbs) <-
    Vk.withCommandBuffers
      dev
      zero
        { Vk.commandPool = pool
        , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
      allocate
  pure (V.head cbs)

depthCopyRegion :: Vk.Extent2D -> Vk.BufferImageCopy
depthCopyRegion (Vk.Extent2D w h) =
  Vk.BufferImageCopy
    { Vk.bufferOffset = 0
    , Vk.bufferRowLength = 0
    , Vk.bufferImageHeight = 0
    , Vk.imageSubresource = Vk.ImageSubresourceLayers Vk.IMAGE_ASPECT_DEPTH_BIT 0 0 1
    , Vk.imageOffset = Vk.Offset3D 0 0 0
    , Vk.imageExtent = Vk.Extent3D w h 1
    }

{- | A GPU-only depth attachment that can also be a transfer source (so the
prepass depth can be copied back). Like 'RenderTarget.createDepthTarget' but
with the extra usage flag.
-}
createReadableDepthTarget
  :: VMA.Allocator -> Vk.Device -> Vk.Format -> Vk.Extent2D -> ResourceT IO (Vk.Image, Vk.ImageView)
createReadableDepthTarget allocator dev format (Vk.Extent2D w h) = do
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
        , Vk.usage = Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT
        , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
        }
    viewCreateInfo image =
      zero
        { Vk.image = image
        , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format = format
        , Vk.subresourceRange = Vk.ImageSubresourceRange Vk.IMAGE_ASPECT_DEPTH_BIT 0 1 0 1
        }

{- | Barrier the depth image from a depth attachment (after the prepass) to a
transfer source, so it can be copied out.
-}
depthToTransferSrc :: Vk.CommandBuffer -> Vk.Image -> ResourceT IO ()
depthToTransferSrc cb img =
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    zero
    []
    []
    [ imageBarrier
        Vk.IMAGE_ASPECT_DEPTH_BIT
        Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
        Vk.ACCESS_TRANSFER_READ_BIT
        Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
        Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
        img
    ]
