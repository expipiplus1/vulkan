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
  , recordOrbUploads
  , readLuminance
  , debugImages
  , lumProbe
  , shadowImage
  , cameraTarget
  ) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Bits (shiftR, (.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, sizeOf)
import qualified Fragr as FG
import Geomancy (Vec3, vec3, vec4, withVec3)
import qualified Geomancy.Mat4 as Mat4
import Geomancy.Transform (scale3, unTransform)
import qualified Geomancy.Vulkan.Projection as Projection
import qualified Geomancy.Vulkan.View as View
import Say (sayErrString)
import qualified Vulkan.Core10 as MemoryBarrier (MemoryBarrier (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as RenderingInfo (RenderingInfo (..))
import qualified Vulkan.Core13 as Vk
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (DynamicState (..), allDynamicStates, applyDynamicStates, dynamicStateFor, fullScissor)
import Vulkan.Utils.FrameGraph.Buffer (ManagedBuffer)
import qualified Vulkan.Utils.FrameGraph.Buffer as Buf
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), Usage (..), describedImage, describedMip, describedSlice, importManagedImage, importScratchImage, newManagedImage, transitionImageTo, transitionImagesTo, usageFlags)
import Vulkan.Utils.FrameGraph.Recorder (Recorder, recordingCommandBuffer)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

import Buffer (deviceBuffer, readbackBuffer, stagingBuffer, storageBuffer)
import Driver (oneShot)
import qualified Pipeline.Bloom as Bloom
import qualified Pipeline.Cull as Cull
import qualified Pipeline.Gamma as Gamma
import qualified Pipeline.HiZ as HiZ
import qualified Pipeline.Knot as Knot
import qualified Pipeline.Luminance as Luminance
import qualified Pipeline.Mesh as Mesh
import qualified Pipeline.Shade as Shade
import qualified Pipeline.Shadow as Shadow
import Pipeline.Shadow.Params (Params (..))
import qualified Pipeline.Ssao as Ssao
import qualified Pipeline.Tonemap as Tonemap
import qualified Pipeline.Voxels as Voxels
import RenderTarget (allocateArrayTarget, allocateCubeArray, allocateImage, allocateLinearSampler, allocateMipChain, allocateNearestSampler, allocateTarget)
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

-- | SSAO obscurance factor (1 = open).
aoFormat :: Vk.Format
aoFormat = Vk.FORMAT_R16_SFLOAT

{- | SSAO prepass: world normal in xyz, view depth in w (0 = void) — the
shaders declare @rgba16f@, so this can't retune with the colour chain.
-}
normalsFormat :: Vk.Format
normalsFormat = Vk.FORMAT_R16G16B16A16_SFLOAT

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
  , normals :: ManagedImage
  -- ^ Half-res DAIS world normals ("Pipeline.Ssao"), w = 0 in the void.
  , normalsView :: Vk.ImageView
  , ao :: ManagedImage
  -- ^ Half-res SSAO factor: gathered, blurred back in place, sampled by the resolve.
  , aoView :: Vk.ImageView
  , aoBlur :: ManagedImage
  -- ^ Scratch between the two blur axes.
  , aoBlurView :: Vk.ImageView
  }

{- | Everything that survives a resize.

The generated geometry, the shared SSBO tables, the EVSM shadow cubes, and the
descriptor sets that read only these. Built once by 'allocateStatic' (which also runs
the one-shot generation submit); the extent-dependent 'Scene' is rebuilt on top of it
per swapchain.
-}
data SceneStatic = SceneStatic
  { indirect :: ManagedBuffer
  -- ^ The draw commands, graph-tracked: the cull pass rewrites the cube draws.
  , objectsBuffer :: Vk.Buffer
  -- ^ The shared object table (the orb slot is rewritten per frame).
  , objLayout :: Objects.Layout
  , meshSet :: Vk.DescriptorSet
  , vertexBuffer :: Vk.Buffer
  , meshTableBuffer :: Vk.Buffer
  , materialsBuffer :: Vk.Buffer
  , sampler :: Vk.Sampler
  -- ^ Shared linear sampler (resolve, tonemap, bloom).
  , nearestSampler :: Vk.Sampler
  -- ^ Shared nearest sampler with the full mip range (hiz build + cull).
  , lumBuffer :: Vk.Buffer
  , lumAllocation :: VMA.Allocation
  , lumMapped :: Ptr ()
  , allocator :: VMA.Allocator
  -- ^ For host-cache invalidation before the luminance readback.
  , bakedMoments :: Maybe ManagedImage
  {- ^ The static lights' slice of the EVSM moments cube array, baked once
  ('recordShadows'); 'Nothing' when every slot belongs to an orb.
  -}
  , shadowCubeView :: Vk.ImageView
  -- ^ @CUBE_ARRAY@ view for sampling in the resolve.
  , shadowRenderViews :: V.Vector Vk.ImageView
  -- ^ One @2D_ARRAY@ (6-face) render view per light, for the multiview shadow pass.
  , shadowDepthView :: Vk.ImageView
  -- ^ Shared 6-layer depth cube for the shadow render's depth test.
  , lightsBuffer :: Vk.Buffer
  -- ^ The shared lights SSBO (glowstone draw, orb draw, shadow render, resolve).
  , viewProjBuffer :: Vk.Buffer
  -- ^ Shadow view-projections SSBO (one @mat4@ per @(light, face)@).
  , shadowSet :: Vk.DescriptorSet
  -- ^ Occluder set for the shadow render (shared vertex/mesh/object tables + view-projs).
  , visMain :: ManagedBuffer
  -- ^ Camera instance remap (see "Scene.Objects"), graph-tracked like 'indirect'.
  , visOcc :: ManagedBuffer
  -- ^ Occluder instance remap, as 'visMain'.
  , orbShadow :: Maybe OrbShadow
  -- ^ The per-frame orb shadow refresh's tracked targets; 'Nothing' without orbs.
  , caveCount :: Word32
  -- ^ Generated cave cubes, read back once for the cull dispatch.
  }

-- | The images the @shadow.orbs@ pass renders, wrapped for layout tracking.
data OrbShadow = OrbShadow
  { moments :: ManagedImage
  -- ^ The orbs' slices of the EVSM moments array (from 'Lights.orbBase').
  , depth :: ManagedImage
  -- ^ The shared scratch depth cube.
  }

{- | The extent-dependent scene.

The render targets, the bloom pyramid, and the descriptor sets that bind extent-sized
views. Rebuilt on resize by 'allocateTargets' over a shared 'SceneStatic'.
-}
data Scene = Scene
  { static :: SceneStatic
  , targets :: SceneTargets
  , shadeSet :: Vk.DescriptorSet
  , normalsSet :: Vk.DescriptorSet
  , aoSet :: Vk.DescriptorSet
  , aoBlurXSet :: Vk.DescriptorSet
  -- ^ ao → aoBlur; 'aoBlurYSet' brings it back.
  , aoBlurYSet :: Vk.DescriptorSet
  , lumSet :: Vk.DescriptorSet
  , toneSet :: Vk.DescriptorSet
  , gammaSet :: Vk.DescriptorSet
  , gammaDebugSet :: Vk.DescriptorSet
  -- ^ Gamma over the raw shade output: debug views skip the tonemap.
  , bloomMips :: V.Vector ManagedImage
  -- ^ One tracked subresource per mip of the single bloom image.
  , bloomExtents :: V.Vector Vk.Extent2D
  , downSets :: V.Vector Vk.DescriptorSet
  , upSets :: V.Vector Vk.DescriptorSet
  , probe :: Maybe ManagedImage
  -- ^ Snapshot of the mip the luminance pass reduces ('lumProbe'); headless only.
  , hizMips :: V.Vector ManagedImage
  -- ^ Depth-pyramid mips ("Pipeline.HiZ"), each a tracked subresource.
  , hizExtents :: V.Vector Vk.Extent2D
  -- ^ Dispatch extents of the per-level reduces only; the tail sizes itself.
  , hizSets :: V.Vector Vk.DescriptorSet
  -- ^ One per per-level reduce; the remaining mips are the fused tail's.
  , hizTailSet :: Maybe Vk.DescriptorSet
  -- ^ 'Nothing' when the chain ends at the last per-level reduce.
  , cullSet :: Vk.DescriptorSet
  -- ^ The cull's buffers + the whole pyramid, sampled ("Pipeline.Cull").
  , hizPrimed :: IORef Bool
  {- ^ False until the first cull pass records; gates the occlusion test off
  the not-yet-built pyramid.
  -}
  }

{- | The graph outputs a driver can consume.

The passes are always added the same way; the driver picks one output and the
graph culls whatever nothing demands. @colorOut@ is the raw shade output (the
debug channels live here). Beauty flows @toneOut@ (tonemapped, display-linear)
→ @displayOut@ (gamma-encoded); in a debug view the gamma pass encodes
@colorOut@ directly, so @displayOut@ skips the bloom/tonemap chain and
@toneOut@ goes undemanded.
-}
data PassOutputs = PassOutputs
  { colorOut :: FG.Handle
  , toneOut :: FG.Handle
  , displayOut :: FG.Handle
  }

-- | The extent-independent pipelines, built once.
data ScenePipelines = ScenePipelines
  { mesh :: Pipeline
  , shade :: Pipeline
  , luminance :: Pipeline
  , tonemap :: Pipeline
  , gamma :: Pipeline
  , bloom :: Bloom.Bloom
  , shadow :: Pipeline
  , generator :: Pipeline
  , knot :: Pipeline
  , cull :: Pipeline
  , hiz :: HiZ.HiZ
  , ssao :: Ssao.Ssao
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
  cull <- Cull.allocatePipeline dev
  hiz <- HiZ.allocateHiZ dev
  ssao <- Ssao.allocateSsao dev
  pure ScenePipelines{mesh, shade, luminance, tonemap, gamma, bloom, shadow, generator, knot, cull, hiz, ssao}

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
  -- TRANSFER_SRC for the one-shot cube-count readback below.
  (_, indirect) <-
    deviceBuffer allocator Objects.indirectBytes (Vk.BUFFER_USAGE_INDIRECT_BUFFER_BIT .|. Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT .|. Vk.BUFFER_USAGE_TRANSFER_SRC_BIT) shared
  -- Per-draw instance remaps (an object id per drawn instance): identity except the
  -- cave range, which the per-frame cull compacts ("Pipeline.Cull").
  (_, visMain) <- deviceBuffer allocator (Objects.remapBytes objLayout) (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
  (_, visOcc) <- deviceBuffer allocator (Objects.occRemapBytes objLayout) (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
  let bufs = Voxels.GenBuffers{Voxels.objects = objectsBuffer, Voxels.indirect = indirect, Voxels.visMain = visMain, Voxels.visOcc = visOcc}

  -- The single lights buffer: orb draw (graphics), shadow render, resolve.
  (_, lights) <- deviceBuffer allocator Lights.bufferBytes (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
  -- Material table (read by the resolve on the async compute queue).
  (_, materialsBuf) <- deviceBuffer allocator Materials.bufferBytes (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
  -- Auto-exposure readback: the luminance pass writes it, the host reads it.
  (_, (lumBuffer, lumAllocation, lumMapped)) <- storageBuffer allocator Luminance.bufferBytes
  -- Staging for the bulk CPU meshes (cube + sphere), copied into the vertex buffer.
  (_, (staging, stagingPtr)) <- stagingBuffer allocator Meshes.cpuVertexBytes

  genSet <- Voxels.allocateGenSets dev pls.generator bufs
  meshSet <- Mesh.allocateSet dev pls.mesh vertexBuffer meshTableBuffer objectsBuffer visMain
  -- Shared linear sampler (resolve, tonemap, bloom).
  (_, sampler) <- allocateLinearSampler dev
  (_, nearestSampler) <- allocateNearestSampler dev

  -- EVSM shadow cube array: one cube per light (moments) + a shared depth cube.
  -- The resolve (async compute) samples the moments, so the array is CONCURRENT.
  (_, (shadowMoments, shadowCubeView, shadowRenderViews)) <-
    allocateCubeArray allocator dev shadowFormat shadowRes Lights.slots (Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) shared "shadow-moments"
  (_, (shadowDepthImage, shadowDepthView)) <-
    allocateArrayTarget allocator dev depthFormat shadowRes 6 Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT Vk.IMAGE_ASPECT_DEPTH_BIT "shadow-depth"
  (_, viewProjBuffer) <- deviceBuffer allocator (fromIntegral (Lights.slots * cubeFaces) * fromIntegral viewProjBytes) (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
  shadowSet <- Shadow.allocateSet dev pls.shadow vertexBuffer meshTableBuffer objectsBuffer viewProjBuffer visOcc
  knotGenSet <- Knot.allocateKnotSet dev pls.knot vertexBuffer
  (_, (countBuffer, countAlloc, countMapped)) <- readbackBuffer allocator 4

  -- The EVSM array is tracked as two slices tiling one image — the static
  -- lights' (baked once below; covers the unlit placeholder slot too) and the
  -- orbs' (refreshed per frame by the @shadow.orbs@ pass) — sharing the
  -- scratch depth cube's tracker. The bake advances the trackers, so the
  -- graph and the debug dumps pick up from the state it left.
  let
    staticCubes = Lights.slots - Lights.orbCount
    momentsSlice = describedSlice shadowFormat (Vk.Extent2D shadowRes shadowRes) shadowMoments Vk.IMAGE_ASPECT_COLOR_BIT
  bakedMoments <-
    if staticCubes == 0
      then pure Nothing
      else Just <$> momentsSlice 0 (staticCubes * cubeFaces)
  shadowDepth <- newManagedImage shadowDepthImage Vk.IMAGE_ASPECT_DEPTH_BIT
  orbShadow <-
    if Lights.orbCount == 0
      then pure Nothing
      else do
        moments <- momentsSlice (Lights.orbBase * cubeFaces) (Lights.orbCount * cubeFaces)
        pure (Just OrbShadow{moments, depth = shadowDepth})

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
    Objects.uploadStaticRemap cb visMain objLayout
    Objects.uploadStaticRemap cb visOcc objLayout
    Objects.seedOrbOccRemap cb visOcc objLayout
    Lights.upload cb lights 0
    Materials.upload cb materialsBuf
    uploadViewProjs cb viewProjBuffer 0
    Voxels.recordGenerate pls.generator genSet genParams cb
    Knot.recordGenerate pls.knot knotGenSet Meshes.knotBase knotParams cb
    -- Snapshot the generated cube total (glowstones + cave cubes) for 'caveCount'.
    Vk.cmdPipelineBarrier
      cb
      Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
      Vk.PIPELINE_STAGE_TRANSFER_BIT
      zero
      [zero{MemoryBarrier.srcAccessMask = Vk.ACCESS_SHADER_WRITE_BIT, MemoryBarrier.dstAccessMask = Vk.ACCESS_TRANSFER_READ_BIT} :: Vk.MemoryBarrier]
      []
      []
    Vk.cmdCopyBuffer cb indirect countBuffer [Vk.BufferCopy Objects.mainCubeCountOffset 0 4]
    recordShadows cb pls shadowSet bakedMoments (fmap (.moments) orbShadow) shadowDepth shadowRenderViews shadowDepthView indirect

  caveCount <- liftIO do
    VMA.invalidateAllocation allocator countAlloc 0 Vk.WHOLE_SIZE
    total <- peek (castPtr countMapped :: Ptr Word32)
    pure (total - objLayout.caveBase)
  sayErrString $ "cave cubes: " <> show caveCount <> " / " <> show Cave.maxCubes

  -- Wrap the cull's working set for graph tracking. The buffers start fresh:
  -- the setup submit above was fenced.
  indirectMB <- Buf.describedAs "draw commands" <$> Buf.newManagedBuffer indirect
  visMainMB <- Buf.describedAs "camera instance remap" <$> Buf.newManagedBuffer visMain
  visOccMB <- Buf.describedAs "occluder instance remap" <$> Buf.newManagedBuffer visOcc

  pure
    SceneStatic
      { indirect = indirectMB
      , objectsBuffer
      , objLayout
      , meshSet
      , vertexBuffer
      , meshTableBuffer
      , materialsBuffer = materialsBuf
      , sampler
      , nearestSampler
      , lumBuffer
      , lumAllocation
      , lumMapped
      , allocator
      , bakedMoments
      , shadowCubeView
      , shadowRenderViews
      , shadowDepthView
      , lightsBuffer = lights
      , viewProjBuffer
      , shadowSet
      , visMain = visMainMB
      , visOcc = visOccMB
      , orbShadow
      , caveCount
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
  -- SAMPLED so the windowed driver's depth-pyramid build can read it.
  (_, (depthImage, depthView)) <-
    allocateTarget allocator dev depthFormat extent (Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT) Vk.IMAGE_ASPECT_DEPTH_BIT Nothing "depth"
  -- The post chain: shade → colorHDR → tone → display. tone (windowed blit),
  -- display (headless readback) and colorHDR (windowed debug-view blit) are
  -- presentation sources, so they need TRANSFER_SRC.
  -- colorHDR is sampled by the first bloom downsample, so it needs SAMPLED too.
  (_, (colorHDRImage, colorHDRView)) <-
    allocateTarget allocator dev hdrFormat extent (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) Vk.IMAGE_ASPECT_COLOR_BIT Nothing "colorHDR"
  (_, (toneImage, toneView)) <-
    allocateTarget allocator dev hdrFormat extent (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) Vk.IMAGE_ASPECT_COLOR_BIT Nothing "tone"
  (_, (displayImage, displayView)) <-
    allocateTarget allocator dev colorFormat extent (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) Vk.IMAGE_ASPECT_COLOR_BIT Nothing "display"
  vis <- describedImage visFormat extent visImage Vk.IMAGE_ASPECT_COLOR_BIT
  depth <- describedImage depthFormat extent depthImage Vk.IMAGE_ASPECT_DEPTH_BIT
  colorHDR <- describedImage hdrFormat extent colorHDRImage Vk.IMAGE_ASPECT_COLOR_BIT
  tone <- describedImage hdrFormat extent toneImage Vk.IMAGE_ASPECT_COLOR_BIT
  display <- describedImage colorFormat extent displayImage Vk.IMAGE_ASPECT_COLOR_BIT

  -- Bloom pyramid: one mipped image (base = half the scene extent); each mip is a
  -- tracked subresource and a down/up descriptor set (sharing the static sampler).
  let
    Vk.Extent2D w0 h0 = extent
    halfExtent = halfExtentOf extent
    -- Level sizes of a 'halfExtent'-based pyramid (bloom and hiz alike).
    mipExtents n = V.generate n \i -> Vk.Extent2D (max 1 ((w0 `div` 2) `shiftR` i)) (max 1 ((h0 `div` 2) `shiftR` i))
    mipCount = bloomMipCount extent
    bloomExtents = mipExtents mipCount
    -- The probe copies off the metered mip, so the chain needs TRANSFER_SRC for it.
    bloomUsage = Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT .|. (if wantProbe then Vk.IMAGE_USAGE_TRANSFER_SRC_BIT else zero)
    lumMip = lumMipFor mipCount
  (_, (bloomImage, bloomViews)) <-
    allocateMipChain allocator dev hdrFormat halfExtent (fromIntegral mipCount) bloomUsage "bloom"
  bloomMips <- V.generateM mipCount \i -> describedMip hdrFormat (bloomExtents V.! i) bloomImage Vk.IMAGE_ASPECT_COLOR_BIT (fromIntegral i)
  -- The luminance probe: a snapshot of the metered mip, taken before the upsample
  -- overwrites it. Written on the compute queue and read back on the graphics one, so
  -- it is CONCURRENT — the graph places plain transitions, never ownership transfers.
  probe <-
    if not wantProbe
      then pure Nothing
      else do
        (_, probeImage) <-
          allocateImage allocator dev hdrFormat (bloomExtents V.! lumMip) (Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) (fmap (\(g, c) -> [g, c]) sharedFamilies) "lumProbe"
        Just <$> describedImage hdrFormat (bloomExtents V.! lumMip) probeImage Vk.IMAGE_ASPECT_COLOR_BIT
  downSets <- V.generateM mipCount \i -> Bloom.allocateSet dev pls.bloom.down static.sampler (if i == 0 then colorHDRView else bloomViews V.! (i - 1)) (bloomViews V.! i)
  upSets <- V.generateM (mipCount - 1) \i -> Bloom.allocateSet dev pls.bloom.up static.sampler (bloomViews V.! (i + 1)) (bloomViews V.! i)

  -- Depth pyramid for the cull's occlusion test: half-res base, min-reduced to 1×1
  -- ('HiZ.mipCount'). Fed by the passes in 'addScenePasses' — per-level reduces
  -- down to the first tail-sized mip, one fused dispatch for the rest — and
  -- sampled by the next frame's cull pass.
  let
    hizMipCount = HiZ.mipCount halfExtent
    hizAllExtents = mipExtents hizMipCount
    -- All the non-fitting levels, plus the first fitting one (1×1 always fits).
    hizReduceCount = 1 + V.length (V.takeWhile (not . HiZ.tailFits) hizAllExtents)
    hizExtents = V.take hizReduceCount hizAllExtents
    hizTailCount = hizMipCount - hizReduceCount
  (_, (hizImage, hizViews)) <-
    allocateMipChain allocator dev HiZ.format halfExtent (fromIntegral hizMipCount) (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT) "hiz"
  hizMips <- V.generateM hizMipCount \i -> describedMip HiZ.format (hizAllExtents V.! i) hizImage Vk.IMAGE_ASPECT_COLOR_BIT (fromIntegral i)
  -- The cull samples across levels ('textureLod'), so it gets a whole-chain view.
  hizFullView <- HiZ.allocateChainView dev hizImage hizMipCount
  hizSets <- V.generateM hizReduceCount \i ->
    HiZ.allocateSet dev pls.hiz.reduce static.nearestSampler (if i == 0 then depthView else hizViews V.! (i - 1)) (hizViews V.! i)
  hizTailSet <-
    if hizTailCount <= 0
      then pure Nothing
      else
        Just
          <$> HiZ.allocateTailSet
            dev
            pls.hiz.tail
            static.nearestSampler
            (hizViews V.! (hizReduceCount - 1))
            (V.slice hizReduceCount hizTailCount hizViews)
  cullSet <-
    Cull.allocateSet
      dev
      pls.cull
      Cull.CullBuffers
        { Cull.objects = static.objectsBuffer
        , Cull.indirect = static.indirect.buffer
        , Cull.visMain = static.visMain.buffer
        , Cull.visOcc = static.visOcc.buffer
        , Cull.lights = static.lightsBuffer
        }
      static.nearestSampler
      hizFullView
  hizPrimed <- liftIO (newIORef False)

  -- The SSAO chain ("Pipeline.Ssao"): half-res DAIS normals, the AO gather over
  -- the depth pyramid, and the two-axis bilateral blur ping-ponging ao → aoBlur
  -- → ao. All on the graphics queue; only the final AO factor crosses to the
  -- (possibly async-compute) resolve, so it alone is CONCURRENT.
  (_, (normalsImage, normalsView)) <-
    allocateTarget allocator dev normalsFormat halfExtent Vk.IMAGE_USAGE_STORAGE_BIT Vk.IMAGE_ASPECT_COLOR_BIT Nothing "normals"
  (_, (aoImage, aoView)) <-
    allocateTarget allocator dev aoFormat halfExtent (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT) Vk.IMAGE_ASPECT_COLOR_BIT (fmap (\(g, c) -> [g, c]) sharedFamilies) "ao"
  (_, (aoBlurImage, aoBlurView)) <-
    allocateTarget allocator dev aoFormat halfExtent Vk.IMAGE_USAGE_STORAGE_BIT Vk.IMAGE_ASPECT_COLOR_BIT Nothing "aoBlur"
  normals <- describedImage normalsFormat halfExtent normalsImage Vk.IMAGE_ASPECT_COLOR_BIT
  ao <- describedImage aoFormat halfExtent aoImage Vk.IMAGE_ASPECT_COLOR_BIT
  aoBlur <- describedImage aoFormat halfExtent aoBlurImage Vk.IMAGE_ASPECT_COLOR_BIT
  normalsSet <- Ssao.allocateNormalsSet dev pls.ssao.normals visView normalsView static.vertexBuffer static.objectsBuffer static.meshTableBuffer
  aoSet <- Ssao.allocateAoSet dev pls.ssao.ao static.nearestSampler hizFullView normalsView aoView
  aoBlurXSet <- Ssao.allocateBlurSet dev pls.ssao.blur normalsView aoView aoBlurView
  aoBlurYSet <- Ssao.allocateBlurSet dev pls.ssao.blur normalsView aoBlurView aoView

  shadeSet <- Shade.allocateDescriptorSet dev pls.shade visView colorHDRView static.vertexBuffer static.lightsBuffer static.sampler static.shadowCubeView static.objectsBuffer static.materialsBuffer static.meshTableBuffer aoView
  lumSet <- Luminance.allocateSet dev pls.luminance (bloomViews V.! lumMip) static.lumBuffer
  toneSet <- Tonemap.allocateSet dev pls.tonemap colorHDRView toneView static.sampler (bloomViews V.! 0)
  gammaSet <- Gamma.allocateSet dev pls.gamma toneView displayView
  gammaDebugSet <- Gamma.allocateSet dev pls.gamma colorHDRView displayView

  pure
    Scene
      { static
      , targets = SceneTargets{vis, visView, depth, depthView, colorHDR, colorHDRView, tone, toneView, display, displayView, normals, normalsView, ao, aoView, aoBlur, aoBlurView}
      , shadeSet
      , normalsSet
      , aoSet
      , aoBlurXSet
      , aoBlurYSet
      , lumSet
      , toneSet
      , gammaSet
      , gammaDebugSet
      , bloomMips
      , bloomExtents
      , downSets
      , upSets
      , probe
      , hizMips
      , hizExtents
      , hizSets
      , hizTailSet
      , cullSet
      , hizPrimed
      }

-- | The tracked visibility and depth images after a frame, for headless debug dumps.
debugImages :: Scene -> (ManagedImage, ManagedImage)
debugImages scene = (scene.targets.vis, scene.targets.depth)

-- | The moments slice holding light 0, for a debug face dump: the static lights', or the orbs' when there are none.
shadowImage :: Scene -> Maybe ManagedImage
shadowImage scene = scene.static.bakedMoments <|> fmap (.moments) scene.static.orbShadow

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

For each light, a single multiview pass draws the cave-cube and knot occluders
into that light's six faces (moments); the slices then become
@SHADER_READ_ONLY@ textures for the resolve. The static + orb slices must tile
the whole array (the resolve binds a view over every slot), and all image
transitions go through their trackers, so the graph passes and the debug dumps
pick up from the state the bake left.
-}
recordShadows
  :: (MonadIO m)
  => Vk.CommandBuffer
  -> ScenePipelines
  -> Vk.DescriptorSet
  -> Maybe ManagedImage
  -> Maybe ManagedImage
  -> ManagedImage
  -> V.Vector Vk.ImageView
  -> Vk.ImageView
  -> Vk.Buffer
  -> m ()
recordShadows cb pls shadowSet bakedMoments orbMoments depth renderViews depthView indirect = liftIO do
  let momentsSlices = catMaybes [bakedMoments, orbMoments]
  -- Generation (compute) + upload (transfer) writes → the shadow render's reads.
  Vk.cmdPipelineBarrier
    cb
    (Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT .|. Vk.PIPELINE_STAGE_TRANSFER_BIT)
    (Vk.PIPELINE_STAGE_DRAW_INDIRECT_BIT .|. Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT .|. Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
    zero
    [zero{MemoryBarrier.srcAccessMask = Vk.ACCESS_SHADER_WRITE_BIT .|. Vk.ACCESS_TRANSFER_WRITE_BIT, MemoryBarrier.dstAccessMask = Vk.ACCESS_SHADER_READ_BIT .|. Vk.ACCESS_INDIRECT_COMMAND_READ_BIT} :: Vk.MemoryBarrier]
    []
    []
  transitionImagesTo cb ((depth, DepthAttachment) : [(m, ColorAttachment) | m <- momentsSlices])
  forM_ (zip [0 ..] (Lights.lights 0)) \(l, light) -> do
    -- Every light rewrites the shared scratch depth cube: a tracked same-state
    -- write places the WAW barrier between the passes.
    when (l > 0) $ transitionImageTo cb depth DepthAttachment
    Vk.cmdUseRendering cb (shadowRenderingInfo (renderViews V.! l) depthView) do
      Vk.cmdSetViewport cb 0 [Vk.Viewport 0 0 (fromIntegral shadowRes) (fromIntegral shadowRes) 0 1]
      Vk.cmdSetScissor cb 0 [Vk.Rect2D (Vk.Offset2D 0 0) (Vk.Extent2D shadowRes shadowRes)]
      Pipeline.bind cb pls.shadow
      Shadow.pushShadow cb pls.shadow light (fromIntegral l * cubeFaces)
      Pipeline.bindSet cb pls.shadow 0 shadowSet
      Vk.cmdDrawIndirect cb indirect Objects.occluderDrawOffset Objects.occluderDrawCount Objects.drawStride
  transitionImagesTo cb [(m, Sampled shadeStage) | m <- momentsSlices]

{- | Upload the orbs' per-frame state for time @t@.

Light positions, shadow view-projections and the orb object rows, recorded
into the frame's graphics buffer ahead of the graph; the shadow slices
themselves are refreshed in-graph (the @shadow.orbs@ pass). Still a
hand-rolled sync site: the trailing barrier must cover every stage that reads
these tables inside the graph, and the leading one every access the previous,
possibly still in-flight, frame made to them — its reads and its own uploads.
-}
recordOrbUploads :: (MonadIO m) => Vk.CommandBuffer -> Scene -> Float -> m ()
recordOrbUploads cb scene t = liftIO $ unless (null Lights.orbs) do
  -- The previous frame's reads (WAR) and its own uploads (WAW) must retire
  -- before the transfers overwrite the tables. The WAR half only needs the
  -- execution ordering; the WAW half needs the old transfer writes made
  -- available and visible to the new ones — spelled out here, not left to
  -- chain through the trailing barrier's scopes.
  Vk.cmdPipelineBarrier
    cb
    (Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT .|. Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT .|. Vk.PIPELINE_STAGE_TRANSFER_BIT)
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    zero
    [zero{MemoryBarrier.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT, MemoryBarrier.dstAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT} :: Vk.MemoryBarrier]
    []
    []
  Lights.updateOrbs cb scene.static.lightsBuffer t
  uploadOrbViewProjs cb scene.static.viewProjBuffer t
  Objects.writeOrbObjects cb scene.static.objectsBuffer scene.static.objLayout t
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    (Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT .|. Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT)
    zero
    [zero{MemoryBarrier.srcAccessMask = Vk.ACCESS_TRANSFER_WRITE_BIT, MemoryBarrier.dstAccessMask = Vk.ACCESS_SHADER_READ_BIT} :: Vk.MemoryBarrier]
    []
    []

{- | The @shadow.orbs@ pass body.

Re-renders the orbs' shadow-cube slices for time @t@, each drawing its own
occluder set the cull pass just compacted to that orb's reach. The glowstones
never move, so only the orbs' moment layers are refreshed — the rest of the
EVSM array stays baked ('recordShadows').
-}
recordOrbShadows :: (MonadIO m) => Vk.CommandBuffer -> ScenePipelines -> Scene -> OrbShadow -> Float -> m ()
recordOrbShadows cb pls scene orbShadow t = liftIO do
  forM_ (zip [0 ..] Lights.orbs) \(i, orb) -> do
    -- The shared depth cube is rewritten per orb: a tracked same-state write
    -- places the WAW barrier between the renders.
    when (i > 0) $ transitionImageTo cb orbShadow.depth DepthAttachment
    let light = Lights.orbBase + i
    Vk.cmdUseRendering cb (shadowRenderingInfo (scene.static.shadowRenderViews V.! fromIntegral light) scene.static.shadowDepthView) do
      Vk.cmdSetViewport cb 0 [Vk.Viewport 0 0 (fromIntegral shadowRes) (fromIntegral shadowRes) 0 1]
      Vk.cmdSetScissor cb 0 [Vk.Rect2D (Vk.Offset2D 0 0) (Vk.Extent2D shadowRes shadowRes)]
      Pipeline.bind cb pls.shadow
      Shadow.pushShadow cb pls.shadow (Lights.orbLight orb t) (light * cubeFaces)
      Pipeline.bindSet cb pls.shadow 0 scene.static.shadowSet
      Vk.cmdDrawIndirect cb scene.static.indirect.buffer (Objects.orbOccDrawOffset i) Objects.occluderDrawCount Objects.drawStride
  -- Producer-side hand-off to the (possibly async-compute) resolve, like the
  -- geometry pass's: the COLOR_ATTACHMENT_OUTPUT source scope stays on a queue
  -- that supports it, and the resolve's declared read finds the state matching.
  transitionImageTo cb orbShadow.moments (Sampled shadeStage)

-- | Multiview rendering info for one light's shadow cube (all six faces, viewMask 0x3F).
shadowRenderingInfo :: Vk.ImageView -> Vk.ImageView -> Vk.RenderingInfo '[]
shadowRenderingInfo colorView depthView =
  (Dynamic.renderingInfo (fullScissor (Vk.Extent2D shadowRes shadowRes)) [(colorView, Vk.Float32 0 0 0 0)] (Just (depthView, 0.0)))
    { RenderingInfo.viewMask = 0x3F
    }

{- | Add the whole scene graph.

@cull@ (compact this frame's draws) → @shadow.orbs@ (refresh the orbs' shadow
slices) → @geometry@ (raster → vis + depth) → @shade@ (resolve → HDR) →
@luminance@ (auto-exposure readback) → @tonemap@ (exposure + curve) → @gamma@
(sRGB encode). @computeQueue@ is the queue the compute passes run on
('FG.defaultQueue' to keep it single-queue); @t@ places the orbs; @exposure@
scales the tonemap. Returns the 'PassOutputs'; a driver reads whichever it
presents.
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
  -- ^ scene time (orb positions, and the reach the cull filters occluders by)
  -> Float
  -> Word32
  -- ^ debug mode (0 = beauty; 1 albedo, 2 metalness, 3 roughness, 4 normal, 5 object id, 6 ao)
  -> ResourceT IO PassOutputs
addScenePasses graph pls tweaks scene computeQueue extent eye t exposure debugMode = do
  let SceneTargets{vis, visView, depth, depthView, colorHDR, tone, display, normals, ao, aoBlur} = scene.targets
  -- vis + depth are read outside the graph (the headless debug dumps), so their
  -- writers must survive; everything else is scratch — consumed only through the
  -- graph, so the presentation's demand decides which passes run at all.
  -- Dotted names group families into one record node in the dot dump.
  visH <- importManagedImage graph "geometry.vis" vis
  depthH <- importManagedImage graph "geometry.depth" depth
  colorH <- importScratchImage graph "post.color" colorHDR
  toneH <- importScratchImage graph "post.tone" tone
  displayH <- importScratchImage graph "post.display" display
  normalsH <- importScratchImage graph "ssao.normals" normals
  aoH <- importScratchImage graph "ssao.ao" ao
  aoBlurH <- importScratchImage graph "ssao.blur" aoBlur
  -- The cull's working set: graph-tracked buffers, so the compact→draw
  -- barriers fall out of the declared accesses.
  indirectH <- Buf.importScratchBuffer graph "cull.indirect" scene.static.indirect
  visMainH <- Buf.importScratchBuffer graph "cull.visMain" scene.static.visMain
  visOccH <- Buf.importScratchBuffer graph "cull.visOcc" scene.static.visOcc
  hizHs <- V.imapM (\i m -> importManagedImage graph (T.pack ("hiz.mip" <> show i)) m) scene.hizMips

  -- Compact this frame's draws: reset the two cube draw commands, then refill
  -- them (and the instance remaps) from the frustum + occlusion tests for the
  -- camera and the reach test around the orbs. The occlusion test samples the
  -- previous frame's pyramid — the imported mip versions, read here before
  -- this frame's rebuild renames them — and skips the first frame after
  -- 'allocateTargets' (nothing built the pyramid yet; a resize resets this).
  -- Kept alive by the draws' declared reads, no side effect needed.
  indirectReset <-
    FG.addPass graph "cull.reset" (FG.writeWith indirectH (Buf.usageFlags Buf.TransferDst)) \_ -> do
      cb <- recordingCommandBuffer
      Cull.reset cb scene.static.indirect.buffer scene.static.objLayout.caveBase
  let cullSetup = do
        V.forM_ hizHs \mipH -> FG.readWith mipH (usageFlags (StorageRead shadeStage))
        indirectCulled <- FG.writeWith indirectReset (Buf.usageFlags (Buf.StorageReadWrite shadeStage))
        visMainCulled <- FG.writeWith visMainH (Buf.usageFlags (Buf.StorageWrite shadeStage))
        visOccCulled <- FG.writeWith visOccH (Buf.usageFlags (Buf.StorageWrite shadeStage))
        pure (indirectCulled, visMainCulled, visOccCulled)
  (indirectCulled, visMainCulled, visOccCulled) <-
    FG.addPass graph "cull" cullSetup \_ -> do
      cb <- recordingCommandBuffer
      hizValid <- liftIO (readIORef scene.hizPrimed)
      Cull.record pls.cull scene.cullSet (cullParams hizValid) cb
      liftIO (writeIORef scene.hizPrimed True)

  -- Refresh the orbs' shadow slices, drawing the occluder set the cull just
  -- compacted for the same @t@ (an out-of-graph refresh would draw a set
  -- filtered for another orb time). See 'recordOrbShadows'; the upload half
  -- stays outside the graph ('recordOrbUploads').
  orbMoments <- forM scene.static.orbShadow \orb -> do
    momentsH <- importScratchImage graph "shadow.orbMoments" orb.moments
    orbDepthH <- importScratchImage graph "shadow.orbDepth" orb.depth
    let orbSetup = do
          FG.readWith indirectCulled (Buf.usageFlags Buf.IndirectRead)
          FG.readWith visOccCulled (Buf.usageFlags (Buf.StorageRead Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT))
          FG.writeWith_ orbDepthH (usageFlags DepthAttachment)
          FG.writeWith momentsH (usageFlags ColorAttachment)
    FG.addPass graph "shadow.orbs" orbSetup \_ -> do
      cb <- recordingCommandBuffer
      recordOrbShadows cb pls scene orb t

  (visWritten, depthWritten) <-
    FG.addPass graph "geometry" (geometrySetup indirectCulled visMainCulled visH depthH) \_ -> do
      cb <- recordingCommandBuffer
      let
        info = Dynamic.renderingInfo (fullScissor extent) [(visView, Vk.Uint32 0 0 0 0)] (Just (depthView, 0.0))
        dyn = (dynamicStateFor extent){depthTest = True, depthWrite = True, depthCompareOp = Vk.COMPARE_OP_GREATER}
      Vk.cmdUseRendering cb info do
        applyDynamicStates allDynamicStates cb dyn
        -- Every mesh (glowstones + cave cubes, the knot, the orb sphere) in one
        -- multi-draw: the pipeline pulls geometry + per-object transforms from the tables.
        Pipeline.bind cb pls.mesh
        pushCamera cb pls.mesh viewProj
        Pipeline.bindSet cb pls.mesh 0 scene.static.meshSet
        Vk.cmdDrawIndirect cb scene.static.indirect.buffer Objects.mainDrawOffset Objects.mainDrawCount Objects.drawStride
      -- Producer-side transition for the (possibly async-compute) shade pass:
      -- done here so the COLOR_ATTACHMENT_OUTPUT source stage stays on a queue
      -- that supports it. The driver's semaphore wait must then cover
      -- 'shadeStage' (see the Headless submit) for the hand-off to be ordered.
      transitionImageTo cb vis (StorageRead shadeStage)

  -- Depth pyramid ("Pipeline.HiZ"): min-reduce the depth buffer right after the
  -- raster, on the default (graphics) queue — depth stays EXCLUSIVE to the family
  -- that rendered it. Read by the SSAO gather below and, a frame later, by the
  -- next graph's cull pass (hence the side effect: demand from a future graph).
  let
    nReduce = V.length scene.hizSets
    hizPass (src, ws) (i, dstH) = do
      w <-
        FG.addPass graph (T.pack ("hiz." <> show i)) (hizSetup src dstH) \_ -> do
          cb <- recordingCommandBuffer
          Pipeline.bind cb pls.hiz.reduce
          Pipeline.bindSet cb pls.hiz.reduce 0 (scene.hizSets V.! i)
          dispatchMip cb (scene.hizExtents V.! i)
      pure (w, w : ws)
  (hizLast, hizReduced) <- foldM hizPass (depthWritten, []) (V.toList (V.indexed (V.take nReduce hizHs)))
  -- The fused tail: every remaining mip from the last reduced level, one dispatch.
  hizTail <- case scene.hizTailSet of
    Nothing -> pure []
    Just tailSet ->
      FG.addPass graph "hiz.tail" (hizTailSetup hizLast (V.drop nReduce hizHs)) \_ -> do
        cb <- recordingCommandBuffer
        Pipeline.bind cb pls.hiz.tail
        HiZ.pushTail cb pls.hiz.tail (fromIntegral (V.length hizHs - nReduce))
        Pipeline.bindSet cb pls.hiz.tail 0 tailSet
        Vk.cmdDispatch cb 1 1 1

  -- SSAO ("Pipeline.Ssao"), also graphics-queue: resolve half-res DAIS normals
  -- from the visibility buffer, then march the fresh pyramid for obscurance.
  normalsWritten <-
    FG.addPass graph "ssao.normals" (computeSetup [visWritten] normalsH) \_ -> do
      cb <- recordingCommandBuffer
      Pipeline.bind cb pls.ssao.normals
      Pipeline.push cb pls.ssao.normals Ssao.Prepass{Ssao.viewProj = viewProj}
      Pipeline.bindSet cb pls.ssao.normals 0 scene.normalsSet
      dispatchMip cb halfExtent
  aoWritten <-
    FG.addPass graph "ssao" (computeSetup (normalsWritten : hizReduced <> hizTail) aoH) \_ -> do
      cb <- recordingCommandBuffer
      Pipeline.bind cb pls.ssao.ao
      pushAo cb pls.ssao.ao tweaks eye extent
      Pipeline.bindSet cb pls.ssao.ao 0 scene.aoSet
      dispatchMip cb halfExtent
  -- Separable cross-bilateral blur: X into the scratch, Y back in place. The
  -- in-place Y write renames the ao handle, ordering it after the X pass's read.
  let blurPass name set axes src dstH =
        FG.addPass graph name (computeSetup [src, normalsWritten] dstH) \_ -> do
          cb <- recordingCommandBuffer
          Pipeline.bind cb pls.ssao.blur
          pushBlur cb pls.ssao.blur tweaks axes
          Pipeline.bindSet cb pls.ssao.blur 0 set
          dispatchMip cb halfExtent
  aoBlurredX <- blurPass "ssao.blur.x" scene.aoBlurXSet (1, 0) aoWritten aoBlurH
  aoBlurred <- blurPass "ssao.blur.y" scene.aoBlurYSet (0, 1) aoBlurredX aoWritten

  colorWritten <-
    FG.addPass graph "shade" (shadeSetup visWritten aoBlurred orbMoments colorH) \_ -> do
      cb <- recordingCommandBuffer
      Pipeline.bind cb pls.shade
      pushResolve cb pls.shade tweaks.tuning viewProj eye debugMode
      Pipeline.bindSet cb pls.shade 0 scene.shadeSet
      Vk.cmdDispatch cb (groups width) (groups height) 1

  -- Bloom pyramid: progressively downsample the HDR image through
  -- the mip chain, then additively upsample. Each mip is its own tracked
  -- subresource of one image, so the per-mip barriers are intra-image.
  mipHs <- traverse (\(i, m) -> importScratchImage graph (T.pack ("bloom.mip" <> show i)) m) (zip [0 :: Int ..] (V.toList scene.bloomMips))
  let
    mipCount = length mipHs
    downPass src i =
      FG.addPass graph (T.pack ("bloom.down." <> show i)) (bloomSetup src (mipHs !! i)) \_ -> do
        cb <- recordingCommandBuffer
        Pipeline.bind cb pls.bloom.down
        Bloom.pushDownsample cb pls.bloom.down (i == 0)
        Pipeline.bindSet cb pls.bloom.down 0 (scene.downSets V.! i)
        dispatchMip cb (scene.bloomExtents V.! i)
    chainDown src i
      | i >= mipCount = pure []
      | otherwise = do hnd <- downPass src i; (hnd :) <$> chainDown hnd (i + 1)
  downHs <- chainDown colorWritten 0
  let
    upPass blur i =
      FG.addPass graph (T.pack ("bloom.up." <> show i)) (upSetup blur (downHs !! i)) \_ -> do
        cb <- recordingCommandBuffer
        Pipeline.bind cb pls.bloom.up
        Bloom.pushUpsample cb pls.bloom.up tweaks.bloomRadius
        Pipeline.bindSet cb pls.bloom.up 0 (scene.upSets V.! i)
        dispatchMip cb (scene.bloomExtents V.! i)
    chainUp blur i
      | i < 0 = pure blur
      | otherwise = do hnd <- upPass blur i; chainUp hnd (i - 1)
  -- Auto-exposure: one workgroup reduces a bloom mip to average log-luminance. Taken
  -- off the downsample chain, before the upsample renames the handle. Not added for
  -- debug views: the HDR target carries the debug channel then, so metering it would
  -- pin the downsample chain (the pass is a side effect) only to poison the readback.
  let lumMip = lumMipFor mipCount
  when (debugMode == 0) $
    FG.addPass_ graph "luminance" (luminanceSetup (downHs !! lumMip)) do
      cb <- recordingCommandBuffer
      Pipeline.bind cb pls.luminance
      Pipeline.bindSet cb pls.luminance 0 scene.lumSet
      Vk.cmdDispatch cb 1 1 1
      -- Make the write host-visible for the CPU readback.
      Vk.cmdPipelineBarrier cb Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT Vk.PIPELINE_STAGE_HOST_BIT zero [hostVisible] [] []

  forM_ scene.probe \probe -> do
    probeH <- importManagedImage graph "bloom.probe" probe
    FG.addPass_ graph "lumProbe" (probeSetup (downHs !! lumMip) probeH) do
      cb <- recordingCommandBuffer
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
    FG.addPass graph "tonemap" (tonemapSetup colorWritten bloom0 toneH) \_ -> do
      cb <- recordingCommandBuffer
      Pipeline.bind cb pls.tonemap
      pushTonemap cb pls.tonemap exposure tweaks.bloomStrength
      Pipeline.bindSet cb pls.tonemap 0 scene.toneSet
      Vk.cmdDispatch cb (groups width) (groups height) 1

  -- Debug views skip the tonemap: gamma encodes the raw shade output, so reading
  -- @displayOut@ culls the whole bloom/tonemap chain on any swapchain.
  let (gammaSrc, gammaSet)
        | debugMode == 0 = (toneWritten, scene.gammaSet)
        | otherwise = (colorWritten, scene.gammaDebugSet)
  displayWritten <-
    FG.addPass graph "gamma" (gammaSetup gammaSrc displayH) \_ -> do
      cb <- recordingCommandBuffer
      Pipeline.bind cb pls.gamma
      Pipeline.bindSet cb pls.gamma 0 gammaSet
      Vk.cmdDispatch cb (groups width) (groups height) 1

  pure PassOutputs{colorOut = colorWritten, toneOut = toneWritten, displayOut = displayWritten}
  where
    Vk.Extent2D{width, height} = extent
    halfExtent = halfExtentOf extent
    -- One camera matrix per recorded frame, shared by every DAIS-style push.
    viewProj = viewProjFor eye extent
    groups n = (n + 7) `div` 8
    dispatchMip cb (Vk.Extent2D mw mh) = Vk.cmdDispatch cb (groups mw) (groups mh) 1
    hostVisible = zero{MemoryBarrier.srcAccessMask = Vk.ACCESS_SHADER_WRITE_BIT, MemoryBarrier.dstAccessMask = Vk.ACCESS_HOST_READ_BIT} :: Vk.MemoryBarrier
    cullParams hizValid =
      Cull.Params
        { Cull.viewProj = viewProj
        , Cull.caveBase = scene.static.objLayout.caveBase
        , Cull.caveCount = scene.static.caveCount
        , Cull.hizValid = if hizValid then 1 else 0
        , Cull.orbBase = Lights.orbBase
        , Cull.orbCount = Lights.orbCount
        , Cull.orbOccBase = scene.static.objLayout.total
        , Cull.orbOccCap = Objects.orbOccCap scene.static.objLayout
        }
    geometrySetup indirectCulled visMainCulled visH depthH = do
      FG.readWith indirectCulled (Buf.usageFlags Buf.IndirectRead)
      FG.readWith visMainCulled (Buf.usageFlags (Buf.StorageRead Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT))
      depthWritten <- FG.writeWith depthH (usageFlags DepthAttachment)
      visWritten <- FG.writeWith visH (usageFlags ColorAttachment)
      pure (visWritten, depthWritten)
    shadeSetup visWritten aoWritten orbMoments colorH = do
      FG.setQueue computeQueue
      FG.readWith visWritten (usageFlags (StorageRead shadeStage))
      FG.readWith aoWritten (usageFlags (StorageRead shadeStage))
      forM_ orbMoments \momentsH -> FG.readWith momentsH (usageFlags (Sampled shadeStage))
      FG.writeWith colorH (usageFlags (StorageWrite shadeStage))
    -- The SSAO passes' shared shape: storage reads in, one storage write out,
    -- on the pass's default (graphics) queue.
    computeSetup (srcs :: [FG.Handle]) dstH = do
      mapM_ (\r -> FG.readWith r (usageFlags (StorageRead shadeStage))) srcs
      FG.writeWith dstH (usageFlags (StorageWrite shadeStage))
    luminanceSetup srcH = do
      FG.setQueue computeQueue
      FG.setSideEffect
      FG.readWith srcH (usageFlags (StorageRead shadeStage))
    -- Probe snapshot: transfer-copy the metered mip into its own image.
    probeSetup srcH probeH = do
      FG.setQueue computeQueue
      FG.setSideEffect
      FG.readWith srcH (usageFlags TransferSrc)
      FG.writeWith_ probeH (usageFlags TransferDst)
    -- Downsample: read the source mip, write the target mip.
    bloomSetup src dstH = do
      FG.setQueue computeQueue
      FG.readWith src (usageFlags (StorageRead shadeStage))
      FG.writeWith dstH (usageFlags (StorageWrite shadeStage))
    -- Upsample: read the blur source (next-smaller mip) and read+write the
    -- destination mip in place (the read+write is the intra-image barrier).
    upSetup blur destH = do
      FG.setQueue computeQueue
      FG.readWith blur (usageFlags (StorageRead shadeStage))
      FG.readWith destH (usageFlags (StorageRead shadeStage))
      FG.writeWith destH (usageFlags (StorageWrite shadeStage))
    tonemapSetup colorWritten bloom0 toneH = do
      FG.setQueue computeQueue
      FG.readWith colorWritten (usageFlags (StorageRead shadeStage))
      FG.readWith bloom0 (usageFlags (StorageRead shadeStage))
      FG.writeWith toneH (usageFlags (StorageWrite shadeStage))
    gammaSetup srcH displayH = do
      FG.setQueue computeQueue
      FG.readWith srcH (usageFlags (StorageRead shadeStage))
      FG.writeWith displayH (usageFlags (StorageWrite shadeStage))
    hizSetup src dstH = do
      FG.setSideEffect
      FG.readWith src (usageFlags (StorageRead shadeStage))
      FG.writeWith dstH (usageFlags (StorageWrite shadeStage))
    hizTailSetup src dstHs = do
      FG.setSideEffect
      FG.readWith src (usageFlags (StorageRead shadeStage))
      forM (V.toList dstHs) \h ->
        FG.writeWith h (usageFlags (StorageWrite shadeStage))

-- | The point the camera looks at and orbits.
cameraTarget :: Vec3
cameraTarget = vec3 0 0 0

cameraFov, cameraNear :: Float
cameraFov = 70 * pi / 180
cameraNear = 0.05

{- | Half the extent, floor-rounded and clamped to 1: the SSAO targets' size and
the bloom/hi-z pyramid base.
-}
halfExtentOf :: Vk.Extent2D -> Vk.Extent2D
halfExtentOf (Vk.Extent2D w h) = Vk.Extent2D (max 1 (w `div` 2)) (max 1 (h `div` 2))

{- | The camera view-projection for @eye@ at @extent@ (geomancy, reverse-Z).

Near maps to 1, infinite far to 0 — depth clears to 0 and the test is GREATER (see
'geometrySetup').
-}
viewProjFor :: Vec3 -> Vk.Extent2D -> Mat4.Mat4
viewProjFor eye (Vk.Extent2D w h) = Mat4.matrixProduct (unTransform proj) (viewFor eye)
  where
    proj = Projection.reverseDepthRH cameraFov cameraNear (fromIntegral w) (fromIntegral h)

-- | The world-to-view half of 'viewProjFor'.
viewFor :: Vec3 -> Mat4.Mat4
viewFor eye = unTransform (View.lookAtRH eye cameraTarget (vec3 0 1 0))

{- | The projection's @(sx, sy)@ diagonal: @ndc.xy = (sx, sy) * view.xy / view.z@.

With 'cameraNear' over the pyramid depth this inverts the projection, so the AO
gather reconstructs view-space positions without a matrix inverse.
-}
projScales :: Vk.Extent2D -> (Float, Float)
projScales (Vk.Extent2D w h) = (sy * fromIntegral h / fromIntegral w, sy)
  where
    sy = recip (tan (cameraFov / 2))

-- | Push the camera view-projection (the mesh pipeline's sole push constant).
pushCamera :: (MonadIO m) => Vk.CommandBuffer -> Pipeline -> Mat4.Mat4 -> m ()
pushCamera cb pl viewProj = Pipeline.push cb pl Mesh.Camera{Mesh.viewProj = viewProj}

-- | Push the resolve's view-projection + eye position (DAIS + specular V) + shading knobs.
pushResolve :: (MonadIO m) => Vk.CommandBuffer -> Pipeline -> Shade.Tuning -> Mat4.Mat4 -> Vec3 -> Word32 -> m ()
pushResolve cb pl tuning viewProj eyePos debugMode =
  Pipeline.push cb pl camera
  where
    eye = withVec3 eyePos \x y z -> vec4 x y z 1
    camera =
      Shade.Camera
        { Shade.viewProj = viewProj
        , Shade.camPos = eye
        , Shade.debugMode = debugMode
        , Shade.lightCount = Lights.count
        , Shade.ambient = tuning.ambient
        , Shade.indirect = tuning.indirect
        , Shade.bleed = tuning.bleed
        , Shade.shadowBias = tuning.shadowBias
        , Shade.normalBias = tuning.normalBias
        }

-- | Push the AO gather's view matrix + unprojection scales + knobs.
pushAo :: (MonadIO m) => Vk.CommandBuffer -> Pipeline -> Tweaks -> Vec3 -> Vk.Extent2D -> m ()
pushAo cb pl tweaks eye extent =
  Pipeline.push
    cb
    pl
    Ssao.Ao
      { Ssao.view = viewFor eye
      , Ssao.sx = sx
      , Ssao.sy = sy
      , Ssao.zNear = cameraNear
      , Ssao.radius = tweaks.aoRadius
      , Ssao.intensity = tweaks.aoIntensity
      , Ssao.bias = tweaks.aoBias
      }
  where
    (sx, sy) = projScales extent

-- | Push one axis of the AO blur.
pushBlur :: (MonadIO m) => Vk.CommandBuffer -> Pipeline -> Tweaks -> (Int32, Int32) -> m ()
pushBlur cb pl tweaks (ax, ay) =
  Pipeline.push cb pl Ssao.Blur{Ssao.sharpness = tweaks.aoSharpness, Ssao.axisX = ax, Ssao.axisY = ay}

-- | Push the tonemap's exposure + bloom strength (COMPUTE stage).
pushTonemap :: (MonadIO m) => Vk.CommandBuffer -> Pipeline -> Float -> Float -> m ()
pushTonemap cb pl exposure bloomStrength =
  Pipeline.push cb pl Tonemap.PC{Tonemap.exposure = exposure, Tonemap.bloomStrength = bloomStrength}

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
  , aoRadius :: Float
  -- ^ SSAO gather radius, in world units.
  , aoIntensity :: Float
  -- ^ SSAO obscurance strength; 0 leaves the ambient terms untouched.
  , aoBias :: Float
  -- ^ SSAO horizon bias, in world units, against self-occlusion.
  , aoSharpness :: Float
  -- ^ AO blur edge-stop: a depth gap of z/sharpness costs one e-fold of weight.
  }
  deriving (Eq, Ord, Show)

-- | Bloom pair tuned for the cave (Jimenez's ~0.04 mix, a tight tent); AO sized to the knot.
defaultTweaks :: Tweaks
defaultTweaks = Tweaks{tuning = Shade.defaultTuning, bloomStrength = 0.04, bloomRadius = 0.005, aoRadius = 0.4, aoIntensity = 1, aoBias = 0.02, aoSharpness = 16}

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
