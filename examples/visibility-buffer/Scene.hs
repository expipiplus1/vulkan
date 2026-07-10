{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| The visibility-buffer scene.

A voxel cave ("Cave"), a compute-generated knot and a dynamic orb, all drawn as one
unified mesh (shared vertex/mesh/object tables) and rasterized through an
@(objectId, triangleId)@ visibility buffer, then resolved by a single DAIS path.

The knot, the orb and the six glowstones share the lit central stage; the six side
chambers are dim backrooms, reached by halls and lit only by the colour that spills down
them — so the stage meters near middle grey and a backroom pins the auto-exposure ceiling.

'allocateStatic' builds the shared SSBOs, runs the "Pipeline.Voxels" generator, and
bakes the shadows — once, surviving resize. 'allocateTargets' allocates the
extent-sized render targets over it. 'addScenePasses' assembles the frame graph.
-}
module Scene
  ( visFormat
  , hdrFormat
  , colorFormat
  , depthFormat
  , shadowRes
  , SceneTargets (..)
  , SceneStatic (..)
  , Scene (..)
  , ScenePipelines (..)
  , PassOutputs (..)
  , Tweaks (..)
  , defaultTweaks
  , allocatePipelines
  , allocateStatic
  , allocateTargets
  , addScenePasses
  , recordOrbFrame
  , readLuminance
  , debugImages
  , lumProbe
  , shadowImage
  , cameraTarget
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.Bits (shiftR, (.|.))
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, sizeOf)
import qualified Fragr as FG
import Geomancy (Vec3, vec3, vec4, withVec3)
import qualified Geomancy.Mat4 as Mat4
import Geomancy.Transform (scale3, unTransform)
import qualified Geomancy.Vulkan.Projection as Projection
import qualified Geomancy.Vulkan.View as View
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as ImageMemoryBarrier (ImageMemoryBarrier (..))
import qualified Vulkan.Core10 as MemoryBarrier (MemoryBarrier (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as RenderingInfo (RenderingInfo (..))
import qualified Vulkan.Core13 as Vk
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (DynamicState (..), allDynamicStates, applyDynamicStates, dynamicStateFor, fullScissor)
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), Usage (..), importManagedImage, newManagedImage, newManagedImageMip, transitionImageTo, usageFlags)
import Vulkan.Utils.FrameGraph.Recorder (Recorder, recorderCommandBuffer)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

import Buffer (deviceBuffer, stagingBuffer, storageBuffer)
import Driver (beginPrimary, commandPool)
import qualified Pipeline.Bloom as Bloom
import qualified Pipeline.Gamma as Gamma
import qualified Pipeline.Knot as Knot
import qualified Pipeline.Luminance as Luminance
import qualified Pipeline.Mesh as Mesh
import qualified Pipeline.Shade as Shade
import qualified Pipeline.Shadow as Shadow
import Pipeline.Shadow.Params (Params (..))
import qualified Pipeline.Tonemap as Tonemap
import qualified Pipeline.Voxels as Voxels
import RenderTarget (allocateArrayTarget, allocateCubeArray, allocateImage, allocateLinearSampler, allocateMipChain, allocateTarget)
import qualified Scene.Cave as Cave
import qualified Scene.Lights as Lights
import qualified Scene.Materials as Materials
import qualified Scene.Meshes as Meshes
import qualified Scene.Objects as Objects
import qualified Upload

visFormat :: Vk.Format
visFormat = Vk.FORMAT_R32G32_UINT

-- | Scene-linear HDR (shade output, tonemap in/out): radiance may exceed 1.
hdrFormat :: Vk.Format
hdrFormat = Vk.FORMAT_R16G16B16A16_SFLOAT

-- | The display (gamma output / readback) format — 8-bit sRGB-encoded.
colorFormat :: Vk.Format
colorFormat = Vk.FORMAT_R8G8B8A8_UNORM

depthFormat :: Vk.Format
depthFormat = Vk.FORMAT_D32_SFLOAT

-- | EVSM shadow moments (4 exponential-variance moments per texel).
shadowFormat :: Vk.Format
shadowFormat = Vk.FORMAT_R32G32B32A32_SFLOAT

-- | Shadow cube face resolution (square).
shadowRes :: Word32
shadowRes = 256

{- | The EVSM encoding, specialized into the occluder and the resolve alike.

'far' spans a stage lamp to the far side of the rock ball (@stageRadius + caveRadius@ =
68 m) with room to spare; the fp32 ceiling on the moments only bites past @1.48 * far@.
-}
shadowParams :: Params
shadowParams = Params{far = 90, warpC = 30.0}

-- | The six cube-face directions + up vectors (standard cube-map convention).
shadowFaces :: [(Vec3, Vec3)]
shadowFaces =
  [ (vec3 1 0 0, vec3 0 (-1) 0)
  , (vec3 (-1) 0 0, vec3 0 (-1) 0)
  , (vec3 0 1 0, vec3 0 0 1)
  , (vec3 0 (-1) 0, vec3 0 0 (-1))
  , (vec3 0 0 1, vec3 0 (-1) 0)
  , (vec3 0 0 (-1), vec3 0 (-1) 0)
  ]

{- | View-projections for every @(light, face)@.

Light-major, so the vertex shader indexes @light*6 + gl_ViewIndex@; reverse-Z, 90°
square per face.
-}
shadowViewProjs :: Float -> [Mat4.Mat4]
shadowViewProjs t = concatMap (lightShadowViewProjs . Lights.position) (Lights.lights t)

{- | The six shadow-cube view-projections for a light at @pos@ (reverse-Z, 90° square).

'shadowFaces' are the GL cube-face view vectors (Y-up NDC), but geomancy's Vulkan
projection is Y-down, so each face renders vertically flipped versus what the hardware
cube sampler reads. @flipY@ negates clip-space Y to realign them (cull is NONE, so the
inverted winding is harmless).
-}
lightShadowViewProjs :: Vec3 -> [Mat4.Mat4]
lightShadowViewProjs pos = do
  (dir, up) <- shadowFaces
  pure . unTransform $
    flipY <> proj <> View.lookAtRH pos (pos + dir) up
  where
    proj = Projection.reverseDepthRH (pi / 2) 0.07 (fromIntegral shadowRes) (fromIntegral shadowRes)
    flipY = scale3 1 (-1) 1

-- | Generation parameters: the cave layout ("Cave"), in metres.
genParams :: Voxels.Params
genParams =
  Voxels.Params
    { Voxels.gridN = Cave.gridN
    , Voxels.worldScale = Cave.worldScale
    , Voxels.rockThreshold = Cave.rockThreshold
    , Voxels.chamberRadius = Cave.chamberRadius
    , Voxels.sideRadius = Cave.sideRadius
    , Voxels.sideDistance = Cave.sideDistance
    , Voxels.hallRadius = Cave.hallRadius
    , Voxels.caveRadius = Cave.caveRadius
    , Voxels.carveBand = Cave.carveBand
    , Voxels.hallBand = Cave.hallBand
    , Voxels.greyCount = Materials.greyCount
    }

{- | The knot's outer radius, in metres — the scene's reference length.

Every other distance here is chosen against a knot you could pick up.
-}
knotRadius :: Float
knotRadius = 0.5

-- | The knot tube's radius, in metres.
knotTubeR :: Float
knotTubeR = 0.07

{- | Knot mesh-gen parameters.

@scale@ is the swept curve's outer radius, so the mesh reaches 'knotRadius'.
-}
knotParams :: Knot.GenParams
knotParams = Knot.GenParams{Knot.tubeR = knotTubeR, Knot.scale = knotRadius - knotTubeR}

-- | Shade reads/writes its storage images from the compute stage.
shadeStage :: Vk.PipelineStageFlags
shadeStage = Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT

{- | The scene's GPU-only images at a given extent, wrapped for layout tracking.

The chain is @vis+depth@ (raster) → @colorHDR@ (shade) → @tone@ (tonemap) →
@display@ (gamma).
-}
data SceneTargets = SceneTargets
  { vis :: ManagedImage
  , visView :: Vk.ImageView
  , depth :: ManagedImage
  , depthView :: Vk.ImageView
  , colorHDR :: ManagedImage
  , colorHDRView :: Vk.ImageView
  , tone :: ManagedImage
  , toneView :: Vk.ImageView
  , display :: ManagedImage
  , displayView :: Vk.ImageView
  }

{- | Everything that survives a resize.

The generated geometry, the shared SSBO tables, the EVSM shadow cubes, and the
descriptor sets that read only these. Built once by 'allocateStatic' (which also runs
the one-shot generation submit); the extent-dependent 'Scene' is rebuilt on top of it
per swapchain.
-}
data SceneStatic = SceneStatic
  { indirect :: Vk.Buffer
  , objectsBuffer :: Vk.Buffer
  -- ^ The shared object table (the orb slot is rewritten per frame).
  , objLayout :: Objects.Layout
  , meshSet :: Vk.DescriptorSet
  , vertexBuffer :: Vk.Buffer
  , meshTableBuffer :: Vk.Buffer
  , materialsBuffer :: Vk.Buffer
  , sampler :: Vk.Sampler
  -- ^ Shared linear sampler (resolve, tonemap, bloom).
  , lumBuffer :: Vk.Buffer
  , lumAllocation :: VMA.Allocation
  , lumMapped :: Ptr ()
  , allocator :: VMA.Allocator
  -- ^ For host-cache invalidation before the luminance readback.
  , shadowMoments :: Vk.Image
  -- ^ EVSM moments cube array: one cube (6 faces) per light.
  , shadowCubeView :: Vk.ImageView
  -- ^ @CUBE_ARRAY@ view for sampling in the resolve.
  , shadowRenderViews :: V.Vector Vk.ImageView
  -- ^ One @2D_ARRAY@ (6-face) render view per light, for the multiview shadow pass.
  , shadowDepthView :: Vk.ImageView
  -- ^ Shared 6-layer depth cube for the shadow render's depth test.
  , shadowDepthImage :: Vk.Image
  -- ^ Backing image of 'shadowDepthView' (for the per-frame orb-shadow re-render).
  , lightsBuffer :: Vk.Buffer
  -- ^ The shared lights SSBO (glowstone draw, orb draw, shadow render, resolve).
  , viewProjBuffer :: Vk.Buffer
  -- ^ Shadow view-projections SSBO (one @mat4@ per @(light, face)@).
  , shadowSet :: Vk.DescriptorSet
  -- ^ Occluder set for the shadow render (shared vertex/mesh/object tables + view-projs).
  }

{- | The extent-dependent scene.

The render targets, the bloom pyramid, and the descriptor sets that bind extent-sized
views. Rebuilt on resize by 'allocateTargets' over a shared 'SceneStatic'.
-}
data Scene = Scene
  { static :: SceneStatic
  , targets :: SceneTargets
  , shadeSet :: Vk.DescriptorSet
  , lumSet :: Vk.DescriptorSet
  , toneSet :: Vk.DescriptorSet
  , gammaSet :: Vk.DescriptorSet
  , bloomMips :: V.Vector ManagedImage
  -- ^ One tracked subresource per mip of the single bloom image.
  , bloomExtents :: V.Vector Vk.Extent2D
  , downSets :: V.Vector Vk.DescriptorSet
  , upSets :: V.Vector Vk.DescriptorSet
  , probe :: Maybe ManagedImage
  -- ^ Snapshot of the mip the luminance pass reduces ('lumProbe'); headless only.
  }

{- | The two graph outputs a driver can consume.

The tonemapped (display-linear) image and the gamma-encoded display image. Reading
@toneOut@ (windowed, blit into an sRGB swapchain) leaves @displayOut@ unread, so the
graph culls the gamma pass; reading @displayOut@ (headless PNG) keeps it.
-}
data PassOutputs = PassOutputs
  { toneOut :: FG.Handle
  , displayOut :: FG.Handle
  }

-- | The extent-independent pipelines, built once.
data ScenePipelines = ScenePipelines
  { mesh :: Mesh.Pipeline
  , shade :: Shade.Pipeline
  , luminance :: Luminance.Pipeline
  , tonemap :: Tonemap.Pipeline
  , gamma :: Gamma.Pipeline
  , bloom :: Bloom.Bloom
  , shadow :: Shadow.Pipeline
  , generator :: Voxels.ComputePipeline
  , knot :: Knot.Knot
  }

allocatePipelines :: Vk.Device -> ResourceT IO ScenePipelines
allocatePipelines dev = do
  mesh <- Mesh.allocatePipeline dev visFormat depthFormat
  shade <- Shade.allocatePipeline dev shadowParams
  luminance <- Luminance.allocatePipeline dev
  tonemap <- Tonemap.allocatePipeline dev
  gamma <- Gamma.allocatePipeline dev
  bloom <- Bloom.allocateBloom dev
  shadow <- Shadow.allocateShadow dev shadowParams shadowFormat depthFormat
  generator <- Voxels.allocateGenerator dev
  knot <- Knot.allocateKnot dev
  pure ScenePipelines{mesh, shade, luminance, tonemap, gamma, bloom, shadow, generator, knot}

{- | Allocate the extent-independent scene.

The shared SSBO tables, the EVSM shadow cubes, and the geometry generated by one
one-shot compute submit on @genQueue@ (voxel cave + knot + baked shadows). Built once;
survives resize. The shared buffers/images are CONCURRENT across @sharedFamilies@ (the
async graphics + compute pair); 'Nothing' leaves them EXCLUSIVE.
-}
allocateStatic
  :: VMA.Allocator
  -> Vk.Device
  -> (Vk.Queue, Word32)
  -- ^ queue + family for the one-shot generation submit
  -> ScenePipelines
  -> Maybe (Word32, Word32)
  -> ResourceT IO SceneStatic
allocateStatic allocator dev genQueue pls sharedFamilies = do
  -- Buffers the async compute resolve reads are CONCURRENT across the graphics +
  -- compute families.
  let
    shared = fmap (\(g, c) -> [g, c]) sharedFamilies
    objLayout = Objects.layout Cave.maxCubes
  -- The unified vertex SSBO (cube + knot), object table and draw commands are read by
  -- the resolve (compute) and the mesh/shadow passes (graphics), so they are shared.
  (_, vertexBuffer) <- deviceBuffer allocator Meshes.vertexBufferSize (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
  (_, meshTableBuffer) <- deviceBuffer allocator Meshes.meshTableBytes (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
  (_, objectsBuffer) <- deviceBuffer allocator (Objects.objectBufferBytes objLayout) (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
  (_, indirect) <-
    deviceBuffer allocator Objects.indirectBytes (Vk.BUFFER_USAGE_INDIRECT_BUFFER_BIT .|. Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
  let bufs = Voxels.GenBuffers{Voxels.objects = objectsBuffer, Voxels.indirect = indirect}

  -- The single lights buffer: orb draw (graphics), shadow render, resolve.
  (_, lights) <- deviceBuffer allocator Lights.bufferBytes (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
  -- Material table (read by the resolve on the async compute queue).
  (_, materialsBuf) <- deviceBuffer allocator Materials.bufferBytes (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
  -- Auto-exposure readback: the luminance pass writes it, the host reads it.
  (_, (lumBuffer, lumAllocation, lumMapped)) <- storageBuffer allocator Luminance.bufferBytes
  -- Staging for the bulk CPU meshes (cube + sphere), copied into the vertex buffer.
  (_, (staging, stagingPtr)) <- stagingBuffer allocator Meshes.cpuVertexBytes

  genSet <- Voxels.allocateGenSets dev pls.generator bufs
  meshSet <- Mesh.allocateSet dev pls.mesh vertexBuffer meshTableBuffer objectsBuffer
  -- Shared linear sampler (resolve, tonemap, bloom).
  (_, sampler) <- allocateLinearSampler dev

  -- EVSM shadow cube array: one cube per light (moments) + a shared depth cube.
  -- The resolve (async compute) samples the moments, so the array is CONCURRENT.
  (_, (shadowMoments, shadowCubeView, shadowRenderViews)) <-
    allocateCubeArray allocator dev shadowFormat shadowRes Lights.slots (Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) shared "shadow-moments"
  (_, (shadowDepthImage, shadowDepthView)) <-
    allocateArrayTarget allocator dev depthFormat shadowRes 6 Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT Vk.IMAGE_ASPECT_DEPTH_BIT "shadow-depth"
  (_, viewProjBuffer) <- deviceBuffer allocator (fromIntegral (Lights.slots * cubeFaces) * fromIntegral viewProjBytes) (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
  shadowSet <- Shadow.allocateSet dev pls.shadow vertexBuffer meshTableBuffer objectsBuffer viewProjBuffer
  knotGenSet <- Knot.allocateKnotSet dev pls.knot vertexBuffer

  -- One setup submit: upload the meshes/objects/lights/view-projections, generate the
  -- cave + knot, then render the EVSM shadow cubes (the lighting is fully runtime now).
  oneShot dev genQueue \cb -> do
    -- Seed the luminance readback so the first frame's peek is a defined 0
    -- (which the exposure guard maps to a deterministic max), not VMA garbage.
    Vk.cmdFillBuffer cb lumBuffer 0 Vk.WHOLE_SIZE 0
    Meshes.stageVertices cb stagingPtr staging vertexBuffer
    Meshes.uploadMeshTable cb meshTableBuffer
    Objects.uploadStaticObjects cb objectsBuffer objLayout
    Objects.writeOrbObjects cb objectsBuffer objLayout 0
    Objects.uploadDrawCommands cb indirect objLayout
    Lights.upload cb lights 0
    Materials.upload cb materialsBuf
    uploadViewProjs cb viewProjBuffer 0
    Voxels.recordGenerate pls.generator genSet genParams cb
    Knot.recordGenerate pls.knot knotGenSet Meshes.knotBase knotParams cb
    recordShadows cb pls shadowSet (shadowMoments, shadowDepthImage) shadowRenderViews shadowDepthView indirect

  pure
    SceneStatic
      { indirect
      , objectsBuffer
      , objLayout
      , meshSet
      , vertexBuffer
      , meshTableBuffer
      , materialsBuffer = materialsBuf
      , sampler
      , lumBuffer
      , lumAllocation
      , lumMapped
      , allocator
      , shadowMoments
      , shadowCubeView
      , shadowRenderViews
      , shadowDepthView
      , shadowDepthImage
      , lightsBuffer = lights
      , viewProjBuffer
      , shadowSet
      }

{- | Allocate the extent-dependent scene over a shared 'SceneStatic'.

The render targets, the bloom pyramid, and the descriptor sets binding extent-sized
views. No GPU submit — cheap enough to rerun on every resize. The visibility buffer is
CONCURRENT across @sharedFamilies@ (the async graphics + compute pair).

@wantProbe@ adds the debug-only luminance probe ('lumProbe'), which costs a copy per
frame; the windowed driver never reads it.
-}
allocateTargets
  :: VMA.Allocator
  -> Vk.Device
  -> ScenePipelines
  -> SceneStatic
  -> Vk.Extent2D
  -> Maybe (Word32, Word32)
  -> Bool
  -> ResourceT IO Scene
allocateTargets allocator dev pls static extent sharedFamilies wantProbe = do
  -- vis + depth also get TRANSFER_SRC so the headless driver can dump them.
  (_, (visImage, visView)) <-
    allocateTarget allocator dev visFormat extent (Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) Vk.IMAGE_ASPECT_COLOR_BIT (fmap (\(g, c) -> [g, c]) sharedFamilies) "visibility"
  (_, (depthImage, depthView)) <-
    allocateTarget allocator dev depthFormat extent (Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) Vk.IMAGE_ASPECT_DEPTH_BIT Nothing "depth"
  -- The post chain: shade → colorHDR → tone → display. Only tone (windowed blit)
  -- and display (headless readback) need TRANSFER_SRC.
  -- colorHDR is sampled by the first bloom downsample, so it needs SAMPLED too.
  (_, (colorHDRImage, colorHDRView)) <-
    allocateTarget allocator dev hdrFormat extent (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT) Vk.IMAGE_ASPECT_COLOR_BIT Nothing "colorHDR"
  (_, (toneImage, toneView)) <-
    allocateTarget allocator dev hdrFormat extent (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) Vk.IMAGE_ASPECT_COLOR_BIT Nothing "tone"
  (_, (displayImage, displayView)) <-
    allocateTarget allocator dev colorFormat extent (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) Vk.IMAGE_ASPECT_COLOR_BIT Nothing "display"
  vis <- newManagedImage visImage Vk.IMAGE_ASPECT_COLOR_BIT
  depth <- newManagedImage depthImage Vk.IMAGE_ASPECT_DEPTH_BIT
  colorHDR <- newManagedImage colorHDRImage Vk.IMAGE_ASPECT_COLOR_BIT
  tone <- newManagedImage toneImage Vk.IMAGE_ASPECT_COLOR_BIT
  display <- newManagedImage displayImage Vk.IMAGE_ASPECT_COLOR_BIT

  -- Bloom pyramid: one mipped image (base = half the scene extent); each mip is a
  -- tracked subresource and a down/up descriptor set (sharing the static sampler).
  let
    Vk.Extent2D w0 h0 = extent
    halfExtent = Vk.Extent2D (max 1 (w0 `div` 2)) (max 1 (h0 `div` 2))
    mipCount = bloomMipCount extent
    bloomExtents = V.generate mipCount \i -> Vk.Extent2D (max 1 ((w0 `div` 2) `shiftR` i)) (max 1 ((h0 `div` 2) `shiftR` i))
    -- The probe copies off the metered mip, so the chain needs TRANSFER_SRC for it.
    bloomUsage = Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT .|. (if wantProbe then Vk.IMAGE_USAGE_TRANSFER_SRC_BIT else zero)
    lumMip = lumMipFor mipCount
  (_, (bloomImage, bloomViews)) <-
    allocateMipChain allocator dev hdrFormat halfExtent (fromIntegral mipCount) bloomUsage "bloom"
  bloomMips <- V.generateM mipCount \i -> newManagedImageMip bloomImage Vk.IMAGE_ASPECT_COLOR_BIT (fromIntegral i)
  -- The luminance probe: a snapshot of the metered mip, taken before the upsample
  -- overwrites it. Written on the compute queue and read back on the graphics one, so
  -- it is CONCURRENT — the graph places plain transitions, never ownership transfers.
  probe <-
    if not wantProbe
      then pure Nothing
      else do
        (_, probeImage) <-
          allocateImage allocator dev hdrFormat (bloomExtents V.! lumMip) (Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) (fmap (\(g, c) -> [g, c]) sharedFamilies) "lumProbe"
        Just <$> newManagedImage probeImage Vk.IMAGE_ASPECT_COLOR_BIT
  downSets <- V.generateM mipCount \i -> Bloom.allocateSet dev pls.bloom.down static.sampler (if i == 0 then colorHDRView else bloomViews V.! (i - 1)) (bloomViews V.! i)
  upSets <- V.generateM (mipCount - 1) \i -> Bloom.allocateSet dev pls.bloom.up static.sampler (bloomViews V.! (i + 1)) (bloomViews V.! i)

  shadeSet <- Shade.allocateDescriptorSet dev pls.shade visView colorHDRView static.vertexBuffer static.lightsBuffer static.sampler static.shadowCubeView static.objectsBuffer static.materialsBuffer static.meshTableBuffer
  lumSet <- Luminance.allocateSet dev pls.luminance (bloomViews V.! lumMip) static.lumBuffer
  toneSet <- Tonemap.allocateSet dev pls.tonemap colorHDRView toneView static.sampler (bloomViews V.! 0)
  gammaSet <- Gamma.allocateSet dev pls.gamma toneView displayView

  pure
    Scene
      { static
      , targets = SceneTargets{vis, visView, depth, depthView, colorHDR, colorHDRView, tone, toneView, display, displayView}
      , shadeSet
      , lumSet
      , toneSet
      , gammaSet
      , bloomMips
      , bloomExtents
      , downSets
      , upSets
      , probe
      }

{- | The visibility and depth images after a frame, for headless debug dumps.

Their layouts are @GENERAL@ (vis) and @DEPTH_ATTACHMENT_OPTIMAL@ (depth).
-}
debugImages :: Scene -> (Vk.Image, Vk.Image)
debugImages scene = (scene.targets.vis.image, scene.targets.depth.image)

-- | The EVSM moments cube array (in @SHADER_READ_ONLY@), for a debug face dump.
shadowImage :: Scene -> Vk.Image
shadowImage scene = scene.static.shadowMoments

{- | The luminance probe after a frame (in @TRANSFER_DST_OPTIMAL@), with its extent.

Exactly the pixels the reduction averaged: 'lumMipIndex' of the downsample chain,
snapshotted before the upsample overwrote it. 'Nothing' unless the scene was allocated
with @wantProbe@.
-}
lumProbe :: Scene -> Maybe (ManagedImage, Vk.Extent2D)
lumProbe scene = fmap (\p -> (p, scene.bloomExtents V.! lumMipFor (V.length scene.bloomExtents))) scene.probe

-- | The SSBO stride: one @mat4@ view-projection.
viewProjBytes :: Int
viewProjBytes = sizeOf (undefined :: Mat4.Mat4)

-- | View-projections per light: one per shadow-cube face.
cubeFaces :: Word32
cubeFaces = 6

-- | Fill the view-projection SSBO (one @mat4@ per @(light, face)@) for time @t@.
uploadViewProjs :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Float -> m ()
uploadViewProjs cb buffer t = Upload.slice cb buffer 0 (shadowViewProjs t)

-- | Rewrite just the orbs' view-projections (from 'Lights.orbBase') for time @t@.
uploadOrbViewProjs :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> Float -> m ()
uploadOrbViewProjs cb buffer t =
  Upload.slice cb buffer (Lights.orbBase * cubeFaces) $
    concatMap (lightShadowViewProjs . Lights.position . (`Lights.orbLight` t)) Lights.orbs

{- | Render the EVSM shadow cubes.

For each light, a single multiview pass draws the cave-cube and knot occluders into
that light's six faces (moments), then the whole array becomes a static
@SHADER_READ_ONLY@ texture for the resolve.
-}
recordShadows
  :: (MonadIO m)
  => Vk.CommandBuffer
  -> ScenePipelines
  -> Vk.DescriptorSet
  -> (Vk.Image, Vk.Image)
  -> V.Vector Vk.ImageView
  -> Vk.ImageView
  -> Vk.Buffer
  -> m ()
recordShadows cb pls shadowSet (moments, depthImg) renderViews depthView indirect = liftIO do
  -- Wait for both writes and transfers to finish before rendering shadows.
  Vk.cmdPipelineBarrier
    cb
    (Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT .|. Vk.PIPELINE_STAGE_TRANSFER_BIT)
    (Vk.PIPELINE_STAGE_DRAW_INDIRECT_BIT .|. Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT .|. Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
    zero
    [zero{MemoryBarrier.srcAccessMask = Vk.ACCESS_SHADER_WRITE_BIT .|. Vk.ACCESS_TRANSFER_WRITE_BIT, MemoryBarrier.dstAccessMask = Vk.ACCESS_SHADER_READ_BIT .|. Vk.ACCESS_INDIRECT_COMMAND_READ_BIT} :: Vk.MemoryBarrier]
    []
    [ transition moments Vk.IMAGE_ASPECT_COLOR_BIT allLayers zero Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    , transition depthImg Vk.IMAGE_ASPECT_DEPTH_BIT 6 zero Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
    ]
  forM_ (zip [0 ..] (Lights.lights 0)) \(l, light) -> do
    {-
      Each light's shadow pass has two attachments:
      - Moments (R32G32B32A32_SFLOAT, the EVSM output): written to that light's own cube slice
      - Depth (D32_SFLOAT): a shared scratch buffer. Reusing the same buffer needs a manual barrier to avoid WAW hazard.
    -}
    when (l > 0) $
      Vk.cmdPipelineBarrier
        cb
        Vk.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
        Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
        zero
        []
        []
        [transition depthImg Vk.IMAGE_ASPECT_DEPTH_BIT 6 Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT (Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT .|. Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL]
    Vk.cmdUseRendering cb (shadowRenderingInfo (renderViews V.! l) depthView) do
      Vk.cmdSetViewport cb 0 [Vk.Viewport 0 0 (fromIntegral shadowRes) (fromIntegral shadowRes) 0 1]
      Vk.cmdSetScissor cb 0 [Vk.Rect2D (Vk.Offset2D 0 0) (Vk.Extent2D shadowRes shadowRes)]
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS pls.shadow.pipeline
      Shadow.pushShadow cb pls.shadow light (fromIntegral l * cubeFaces)
      Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_GRAPHICS pls.shadow.pipelineLayout 0 [shadowSet] []
      Vk.cmdDrawIndirect cb indirect Objects.occluderDrawOffset Objects.occluderDrawCount Objects.drawStride
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    zero
    []
    []
    [transition moments Vk.IMAGE_ASPECT_COLOR_BIT allLayers Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT Vk.ACCESS_SHADER_READ_BIT Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL]
  where
    allLayers = Lights.slots * cubeFaces
    transition img aspect n srcA dstA old new =
      SomeStruct
        zero
          { ImageMemoryBarrier.srcAccessMask = srcA
          , ImageMemoryBarrier.dstAccessMask = dstA
          , ImageMemoryBarrier.oldLayout = old
          , ImageMemoryBarrier.newLayout = new
          , ImageMemoryBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
          , ImageMemoryBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
          , ImageMemoryBarrier.image = img
          , ImageMemoryBarrier.subresourceRange = Vk.ImageSubresourceRange aspect 0 1 0 n
          }

{- | Move the orbs and re-render their shadow-cube slices for time @t@.

Recorded into the frame's graphics buffer before the scene graph. The glowstones never
move, so only the orbs' moment layers are refreshed — the rest of the EVSM array stays
baked ('recordShadows'). Updates the orbs' lights-SSBO entries and their shadow
view-projections, opens their moment slices as colour attachments, draws the cave-cube
+ knot occluders through each orb's multiview pass, then hands the slices back as a
sampled texture for the resolve.

The orbs share one scratch depth cube, so consecutive passes need a WAW barrier.
-}
recordOrbFrame :: (MonadIO m) => Vk.CommandBuffer -> ScenePipelines -> Scene -> Float -> m ()
recordOrbFrame cb pls scene t = liftIO $ unless (null Lights.orbs) do
  Lights.updateOrbs cb scene.static.lightsBuffer t
  uploadOrbViewProjs cb scene.static.viewProjBuffer t
  Objects.writeOrbObjects cb scene.static.objectsBuffer scene.static.objLayout t
  -- Transfer writes (lights + view-projs) → the shaders that read them, and open
  -- the orbs' moment slices + shared depth for rendering.
  Vk.cmdPipelineBarrier
    cb
    (Vk.PIPELINE_STAGE_TRANSFER_BIT .|. Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT .|. Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT)
    (Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT .|. Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT .|. Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT .|. Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT)
    zero
    [zero{MemoryBarrier.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT, MemoryBarrier.dstAccessMask = Vk.ACCESS_SHADER_READ_BIT} :: Vk.MemoryBarrier]
    []
    [ momentSlices Vk.ACCESS_SHADER_READ_BIT Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    , depthSlice zero Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
    ]
  forM_ (zip [0 ..] Lights.orbs) \(i, orb) -> do
    -- The shared depth cube is rewritten per orb: WAW between passes.
    when (i > 0) $
      Vk.cmdPipelineBarrier
        cb
        Vk.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
        Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
        zero
        []
        []
        [depthSlice Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT (Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT .|. Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT) Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL Vk.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL]
    let light = Lights.orbBase + i
    Vk.cmdUseRendering cb (shadowRenderingInfo (scene.static.shadowRenderViews V.! fromIntegral light) scene.static.shadowDepthView) do
      Vk.cmdSetViewport cb 0 [Vk.Viewport 0 0 (fromIntegral shadowRes) (fromIntegral shadowRes) 0 1]
      Vk.cmdSetScissor cb 0 [Vk.Rect2D (Vk.Offset2D 0 0) (Vk.Extent2D shadowRes shadowRes)]
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS pls.shadow.pipeline
      Shadow.pushShadow cb pls.shadow (Lights.orbLight orb t) (light * cubeFaces)
      Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_GRAPHICS pls.shadow.pipelineLayout 0 [scene.static.shadowSet] []
      Vk.cmdDrawIndirect cb scene.static.indirect Objects.occluderDrawOffset Objects.occluderDrawCount Objects.drawStride
  -- Hand the refreshed slices back to the resolve as a sampled texture.
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    (Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT .|. Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT)
    zero
    []
    []
    [momentSlices Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT Vk.ACCESS_SHADER_READ_BIT Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL]
  where
    momentSlices srcA dstA old new =
      layerBarrier scene.static.shadowMoments Vk.IMAGE_ASPECT_COLOR_BIT (Lights.orbBase * cubeFaces) (Lights.orbCount * cubeFaces) srcA dstA old new
    depthSlice srcA dstA old new =
      layerBarrier scene.static.shadowDepthImage Vk.IMAGE_ASPECT_DEPTH_BIT 0 cubeFaces srcA dstA old new

-- | An image-memory barrier over @layerCount@ layers starting at @baseLayer@.
layerBarrier :: Vk.Image -> Vk.ImageAspectFlags -> Word32 -> Word32 -> Vk.AccessFlags -> Vk.AccessFlags -> Vk.ImageLayout -> Vk.ImageLayout -> SomeStruct Vk.ImageMemoryBarrier
layerBarrier img aspect baseLayer layerCount srcA dstA old new =
  SomeStruct
    zero
      { ImageMemoryBarrier.srcAccessMask = srcA
      , ImageMemoryBarrier.dstAccessMask = dstA
      , ImageMemoryBarrier.oldLayout = old
      , ImageMemoryBarrier.newLayout = new
      , ImageMemoryBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
      , ImageMemoryBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
      , ImageMemoryBarrier.image = img
      , ImageMemoryBarrier.subresourceRange = Vk.ImageSubresourceRange aspect 0 1 baseLayer layerCount
      }

-- | Multiview rendering info for one light's shadow cube (all six faces, viewMask 0x3F).
shadowRenderingInfo :: Vk.ImageView -> Vk.ImageView -> Vk.RenderingInfo '[]
shadowRenderingInfo colorView depthView =
  (Dynamic.renderingInfo (fullScissor (Vk.Extent2D shadowRes shadowRes)) [(colorView, Vk.Float32 0 0 0 0)] (Just (depthView, 0.0)))
    { RenderingInfo.viewMask = 0x3F
    }

-- | Record @record@ into a fresh primary buffer and submit it, blocking until done.
oneShot :: Vk.Device -> (Vk.Queue, Word32) -> (Vk.CommandBuffer -> ResourceT IO ()) -> ResourceT IO ()
oneShot dev (queue, family) record = do
  pool <- commandPool dev family
  cb <- beginPrimary dev pool
  record cb
  Vk.endCommandBuffer cb
  (_, fence) <- Vk.withFence dev zero Nothing allocate
  Vk.queueSubmit queue [SomeStruct (zero{Vk.commandBuffers = [Vk.commandBufferHandle cb]} :: Vk.SubmitInfo '[])] fence
  _ <- Vk.waitForFences dev [fence] True maxBound
  pure ()

{- | Add the whole scene graph.

@geometry@ (raster → vis + depth) → @shade@ (resolve → HDR) → @luminance@
(auto-exposure readback) → @tonemap@ (exposure + curve) → @gamma@ (sRGB encode).
@computeQueue@ is the queue the compute passes run on ('FG.defaultQueue' to keep it
single-queue); @exposure@ scales the tonemap. Returns both post outputs; a driver
reads whichever it presents.
-}
addScenePasses
  :: FG.FrameGraph Recorder ()
  -> ScenePipelines
  -> Tweaks
  -> Scene
  -> FG.QueueId
  -> Vk.Extent2D
  -> Vec3
  -- ^ camera eye position
  -> Float
  -> Word32
  -- ^ debug mode (0 = beauty; 1 albedo, 2 metalness, 3 roughness, 4 normal)
  -> ResourceT IO PassOutputs
addScenePasses graph pls tweaks scene computeQueue extent eye exposure debugMode = do
  let SceneTargets{vis, visView, depth, depthView, colorHDR, tone, display} = scene.targets
  visH <- importManagedImage graph "visibility" vis
  depthH <- importManagedImage graph "depth" depth
  colorH <- importManagedImage graph "colorHDR" colorHDR
  toneH <- importManagedImage graph "tone" tone
  displayH <- importManagedImage graph "display" display

  visWritten <-
    FG.addPass graph "geometry" (geometrySetup visH depthH) \_ _ recorder -> do
      cb <- recorderCommandBuffer recorder
      let
        info = Dynamic.renderingInfo (fullScissor extent) [(visView, Vk.Uint32 0 0 0 0)] (Just (depthView, 0.0))
        dyn = (dynamicStateFor extent){depthTest = True, depthWrite = True, depthCompareOp = Vk.COMPARE_OP_GREATER}
      Vk.cmdUseRendering cb info do
        applyDynamicStates allDynamicStates cb dyn
        -- Every mesh (glowstones + cave cubes, the knot, the orb sphere) in one
        -- multi-draw: the pipeline pulls geometry + per-object transforms from the tables.
        Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS pls.mesh.pipeline
        pushCamera cb pls.mesh.pipelineLayout eye extent
        Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_GRAPHICS pls.mesh.pipelineLayout 0 [scene.static.meshSet] []
        Vk.cmdDrawIndirect cb scene.static.indirect Objects.mainDrawOffset Objects.mainDrawCount Objects.drawStride
      -- Producer-side transition for the (possibly async-compute) shade pass:
      -- done here so the COLOR_ATTACHMENT_OUTPUT source stage stays on a queue
      -- that supports it. The driver's semaphore wait must then cover
      -- 'shadeStage' (see the Headless submit) for the hand-off to be ordered.
      transitionImageTo cb vis (StorageRead shadeStage)

  colorWritten <-
    FG.addPass graph "shade" (shadeSetup visWritten colorH) \_ _ recorder -> do
      cb <- recorderCommandBuffer recorder
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE pls.shade.pipeline
      pushResolve cb pls.shade.pipelineLayout pls.shade.cameraPushSize tweaks.tuning eye extent debugMode
      Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE pls.shade.pipelineLayout 0 [scene.shadeSet] []
      Vk.cmdDispatch cb (groups w) (groups h) 1

  -- Bloom pyramid: progressively downsample the HDR image through
  -- the mip chain, then additively upsample. Each mip is its own tracked
  -- subresource of one image, so the per-mip barriers are intra-image.
  mipHs <- traverse (\(i, m) -> importManagedImage graph (T.pack ("bloom.mip" <> show i)) m) (zip [0 :: Int ..] (V.toList scene.bloomMips))
  let
    mipCount = length mipHs
    downPass src i =
      FG.addPass graph (T.pack ("bloom.down." <> show i)) (bloomSetup src (mipHs !! i)) \_ _ recorder -> do
        cb <- recorderCommandBuffer recorder
        Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE pls.bloom.down.pipeline
        Bloom.pushDownsample cb pls.bloom.down.pipelineLayout (i == 0)
        Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE pls.bloom.down.pipelineLayout 0 [scene.downSets V.! i] []
        dispatchMip cb (scene.bloomExtents V.! i)
    chainDown src i
      | i >= mipCount = pure []
      | otherwise = do hnd <- downPass src i; (hnd :) <$> chainDown hnd (i + 1)
  downHs <- chainDown colorWritten 0
  let
    upPass blur i =
      FG.addPass graph (T.pack ("bloom.up." <> show i)) (upSetup blur (downHs !! i)) \_ _ recorder -> do
        cb <- recorderCommandBuffer recorder
        Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE pls.bloom.up.pipeline
        Bloom.pushUpsample cb pls.bloom.up.pipelineLayout tweaks.bloomRadius
        Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE pls.bloom.up.pipelineLayout 0 [scene.upSets V.! i] []
        dispatchMip cb (scene.bloomExtents V.! i)
    chainUp blur i
      | i < 0 = pure blur
      | otherwise = do hnd <- upPass blur i; chainUp hnd (i - 1)
  -- Auto-exposure: one workgroup reduces a bloom mip to average log-luminance. Taken
  -- off the downsample chain, before the upsample renames the handle.
  let lumMip = lumMipFor mipCount
  _ <-
    FG.addPass graph "luminance" (luminanceSetup (downHs !! lumMip)) \_ _ recorder -> do
      cb <- recorderCommandBuffer recorder
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE pls.luminance.pipeline
      Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE pls.luminance.pipelineLayout 0 [scene.lumSet] []
      Vk.cmdDispatch cb 1 1 1
      -- Make the write host-visible for the CPU readback.
      Vk.cmdPipelineBarrier cb Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT Vk.PIPELINE_STAGE_HOST_BIT zero [hostVisible] [] []

  forM_ scene.probe \probe -> do
    probeH <- importManagedImage graph "lumProbe" probe
    FG.addPass graph "lumProbe" (probeSetup (downHs !! lumMip) probeH) \_ _ recorder -> do
      cb <- recorderCommandBuffer recorder
      let Vk.Extent2D pw ph = scene.bloomExtents V.! lumMip
      Vk.cmdCopyImage
        cb
        (scene.bloomMips V.! lumMip).image
        Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
        probe.image
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        [ Vk.ImageCopy
            (Vk.ImageSubresourceLayers Vk.IMAGE_ASPECT_COLOR_BIT (fromIntegral lumMip) 0 1)
            (Vk.Offset3D 0 0 0)
            (Vk.ImageSubresourceLayers Vk.IMAGE_ASPECT_COLOR_BIT 0 0 1)
            (Vk.Offset3D 0 0 0)
            (Vk.Extent3D pw ph 1)
        ]

  bloom0 <- chainUp (downHs !! (mipCount - 1)) (mipCount - 2)

  toneWritten <-
    FG.addPass graph "tonemap" (tonemapSetup colorWritten bloom0 toneH) \_ _ recorder -> do
      cb <- recorderCommandBuffer recorder
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE pls.tonemap.pipeline
      pushTonemap cb pls.tonemap.pipelineLayout exposure tweaks.bloomStrength debugMode
      Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE pls.tonemap.pipelineLayout 0 [scene.toneSet] []
      Vk.cmdDispatch cb (groups w) (groups h) 1

  displayWritten <-
    FG.addPass graph "gamma" (gammaSetup toneWritten displayH) \_ _ recorder -> do
      cb <- recorderCommandBuffer recorder
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE pls.gamma.pipeline
      Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE pls.gamma.pipelineLayout 0 [scene.gammaSet] []
      Vk.cmdDispatch cb (groups w) (groups h) 1

  pure PassOutputs{toneOut = toneWritten, displayOut = displayWritten}
  where
    Vk.Extent2D w h = extent
    groups n = (n + 7) `div` 8
    dispatchMip cb (Vk.Extent2D mw mh) = Vk.cmdDispatch cb (groups mw) (groups mh) 1
    hostVisible = zero{MemoryBarrier.srcAccessMask = Vk.ACCESS_SHADER_WRITE_BIT, MemoryBarrier.dstAccessMask = Vk.ACCESS_HOST_READ_BIT} :: Vk.MemoryBarrier
    geometrySetup visH depthH b = do
      _ <- FG.writeWith b depthH (usageFlags DepthAttachment)
      FG.writeWith b visH (usageFlags ColorAttachment)
    shadeSetup visWritten colorH b = do
      FG.setQueue b computeQueue
      _ <- FG.readWith b visWritten (usageFlags (StorageRead shadeStage))
      FG.writeWith b colorH (usageFlags (StorageWrite shadeStage))
    luminanceSetup srcH b = do
      FG.setQueue b computeQueue
      FG.setSideEffect b
      FG.readWith b srcH (usageFlags (StorageRead shadeStage))
    -- Probe snapshot: transfer-copy the metered mip into its own image.
    probeSetup srcH probeH b = do
      FG.setQueue b computeQueue
      FG.setSideEffect b
      _ <- FG.readWith b srcH (usageFlags TransferSrc)
      FG.writeWith b probeH (usageFlags TransferDst)
    -- Downsample: read the source mip, write the target mip.
    bloomSetup src dstH b = do
      FG.setQueue b computeQueue
      _ <- FG.readWith b src (usageFlags (StorageRead shadeStage))
      FG.writeWith b dstH (usageFlags (StorageWrite shadeStage))
    -- Upsample: read the blur source (next-smaller mip) and read+write the
    -- destination mip in place (the read+write is the intra-image barrier).
    upSetup blur destH b = do
      FG.setQueue b computeQueue
      _ <- FG.readWith b blur (usageFlags (StorageRead shadeStage))
      _ <- FG.readWith b destH (usageFlags (StorageRead shadeStage))
      FG.writeWith b destH (usageFlags (StorageWrite shadeStage))
    tonemapSetup colorWritten bloom0 toneH b = do
      FG.setQueue b computeQueue
      _ <- FG.readWith b colorWritten (usageFlags (StorageRead shadeStage))
      _ <- FG.readWith b bloom0 (usageFlags (StorageRead shadeStage))
      FG.writeWith b toneH (usageFlags (StorageWrite shadeStage))
    gammaSetup toneWritten displayH b = do
      FG.setQueue b computeQueue
      _ <- FG.readWith b toneWritten (usageFlags (StorageRead shadeStage))
      FG.writeWith b displayH (usageFlags (StorageWrite shadeStage))

-- | The point the camera looks at and orbits.
cameraTarget :: Vec3
cameraTarget = vec3 0 0 0

{- | The camera view-projection for @eye@ at @extent@ (geomancy, reverse-Z).

Near maps to 1, infinite far to 0 — depth clears to 0 and the test is GREATER (see
'geometrySetup').
-}
viewProjFor :: Vec3 -> Vk.Extent2D -> Mat4.Mat4
viewProjFor eye (Vk.Extent2D w h) = Mat4.matrixProduct (unTransform proj) (unTransform view)
  where
    proj = Projection.reverseDepthRH (70 * pi / 180) 0.05 (fromIntegral w) (fromIntegral h)
    view = View.lookAtRH eye cameraTarget (vec3 0 1 0)

-- | Push the camera view-projection (the mesh pipeline's sole push constant).
pushCamera :: Vk.CommandBuffer -> Vk.PipelineLayout -> Vec3 -> Vk.Extent2D -> IO ()
pushCamera cb layout eye extent =
  with camera \p ->
    Vk.cmdPushConstants cb layout Vk.SHADER_STAGE_VERTEX_BIT 0 (fromIntegral (sizeOf camera)) (castPtr p)
  where
    camera = Mesh.Camera{Mesh.viewProj = viewProjFor eye extent}

-- | Push the resolve's view-projection + eye position (DAIS + specular V) + shading knobs.
pushResolve :: Vk.CommandBuffer -> Vk.PipelineLayout -> Word32 -> Shade.Tuning -> Vec3 -> Vk.Extent2D -> Word32 -> IO ()
pushResolve cb layout pushSize tuning eyePos extent debugMode =
  -- Push the reflected range extent ('pushSize'), which is < the std430 'Storable'
  -- size (it trailing-pads to 16) — every real field lives below it.
  with camera \p ->
    Vk.cmdPushConstants cb layout Vk.SHADER_STAGE_COMPUTE_BIT 0 pushSize (castPtr p)
  where
    eye = withVec3 eyePos \x y z -> vec4 x y z 1
    camera =
      Shade.Camera
        { Shade.viewProj = viewProjFor eyePos extent
        , Shade.camPos = eye
        , Shade.debugMode = debugMode
        , Shade.lightCount = Lights.count
        , Shade.ambient = tuning.ambient
        , Shade.indirect = tuning.indirect
        , Shade.bleed = tuning.bleed
        , Shade.shadowBias = tuning.shadowBias
        , Shade.normalBias = tuning.normalBias
        }

-- | Push the tonemap's exposure + bloom strength + debug mode (COMPUTE stage).
pushTonemap :: Vk.CommandBuffer -> Vk.PipelineLayout -> Float -> Float -> Word32 -> IO ()
pushTonemap cb layout exposure bloomStrength debugMode =
  with pc \p ->
    Vk.cmdPushConstants cb layout Vk.SHADER_STAGE_COMPUTE_BIT 0 (fromIntegral (sizeOf pc)) (castPtr p)
  where
    pc = Tonemap.PC{Tonemap.exposure = exposure, Tonemap.bloomStrength = bloomStrength, Tonemap.debugMode = debugMode}

{- | The run-constant shading knobs, threaded from the command line.

Nothing is baked against them; they only feed pushes ('pushResolve', 'pushTonemap',
the bloom upsample).
-}
data Tweaks = Tweaks
  { tuning :: Shade.Tuning
  , bloomStrength :: Float
  -- ^ Bloom mix bias toward the pyramid.
  , bloomRadius :: Float
  -- ^ Bloom upsample tent radius, in UV.
  }
  deriving (Eq, Ord, Show)

-- | Bloom pair tuned for the cave: Jimenez's ~0.04 mix, a tight tent.
defaultTweaks :: Tweaks
defaultTweaks = Tweaks{tuning = Shade.defaultTuning, bloomStrength = 0.04, bloomRadius = 0.005}

{- | Bloom mip the luminance pass reduces.

Mip 0 is already half-resolution, so mip 2 is ⅛ of the scene extent — few enough
pixels for one workgroup, pre-averaged, and still wide enough to bin later.
-}
lumMipIndex :: Int
lumMipIndex = 2

-- | 'lumMipIndex', clamped to a chain of @mipCount@ mips.
lumMipFor :: Int -> Int
lumMipFor mipCount = min lumMipIndex (mipCount - 1)

{- | Bloom mip count for an extent.

Halve from half-resolution until a level would drop below 8 px, capped at 6.
-}
bloomMipCount :: Vk.Extent2D -> Int
bloomMipCount (Vk.Extent2D w h) =
  max 1 (min 6 (length (takeWhile (>= 8) (iterate (`div` 2) (min w h `div` 2)))))

{- | Read back the last frame's geometric-mean luminance.

The luminance pass wrote it; the driver derives an exposure from it. GPU→CPU
memory may be non-coherent, so the host cache is invalidated before the peek.
-}
readLuminance :: (MonadIO m) => Scene -> m Float
readLuminance scene = liftIO do
  VMA.invalidateAllocation scene.static.allocator scene.static.lumAllocation 0 Vk.WHOLE_SIZE
  peek (castPtr (scene.static.lumMapped `plusPtr` 4))
