{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Everything that survives a resize.

'allocateStatic' builds the shared SSBO tables, runs the "Pipeline.Voxels" and
"Pipeline.Knot" generators, and bakes the EVSM shadow cubes — once, in a single
one-shot submit. The extent-dependent targets ("Rendering.Targets") are rebuilt on
top of it per swapchain.
-}
module Rendering.Static
  ( SceneStatic (..)
  , OrbShadow (..)
  , allocateStatic
  ) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Bits ((.|.))
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, poke)
import qualified Fragr as FG
import Say (sayErrString)
import qualified Vulkan.Core10 as MemoryBarrier (MemoryBarrier (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.FrameGraph.Buffer (ManagedBuffer)
import qualified Vulkan.Utils.FrameGraph.Buffer as Buf
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), SliceRegistry, Usage (..), claimOwnership, describedSlice, newManagedImage, transitionImageTo, transitionImagesTo)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

import Buffer (deviceBuffer, readbackBuffer, stagingBuffer, storageBuffer)
import Driver (oneShot)
import qualified Pipeline.Knot as Knot
import qualified Pipeline.Luminance as Luminance
import qualified Pipeline.Mesh as Mesh
import qualified Pipeline.Shadow as Shadow
import qualified Pipeline.Voxels as Voxels
import RenderTarget (allocateArrayTarget, allocateCubeArray, allocateLinearSampler, allocateNearestSampler)
import Rendering.Pipelines (ScenePipelines, depthFormat, shadeStage)
import qualified Rendering.Pipelines
import qualified Rendering.Shadows as Shadows
import qualified Scene.Cave as Cave
import qualified Scene.Lights as Lights
import qualified Scene.Materials as Materials
import qualified Scene.Meshes as Meshes
import qualified Scene.Objects as Objects

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

{- | Everything that survives a resize.

The generated geometry, the shared SSBO tables, the EVSM shadow cubes, and the
descriptor sets that read only these. Built once by 'allocateStatic' (which also runs
the one-shot generation submit); the extent-dependent scene ("Rendering.Targets") is
rebuilt on top of it per swapchain.
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
  , lumManaged :: ManagedBuffer
  , exposureBuffer :: Vk.Buffer
  , exposureAllocation :: VMA.Allocation
  , exposureMapped :: Ptr ()
  , exposureManaged :: ManagedBuffer
  -- ^ The tonemap exposure, written by the host ('setExposure' or the meter pass).
  , allocator :: VMA.Allocator
  -- ^ For host-cache maintenance around the mapped buffers.
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
  , lightsTable :: ManagedBuffer
  -- ^ 'lightsBuffer', tracked: the orb refresh writes it, the graph reads it.
  , viewProjBuffer :: Vk.Buffer
  -- ^ Shadow view-projections SSBO (one @mat4@ per @(light, face)@).
  , viewProjTable :: ManagedBuffer
  -- ^ 'viewProjBuffer', tracked.
  , objectsTable :: ManagedBuffer
  -- ^ 'objectsBuffer', tracked.
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

{- | Allocate the extent-independent scene.

The shared SSBO tables, the EVSM shadow cubes, and the geometry generated by one
one-shot compute submit on @genQueue@ (voxel cave + knot + baked shadows). Built once;
survives resize. The shared buffers/images are CONCURRENT across @sharedFamilies@ (the
async graphics + compute pair); 'Nothing' leaves them EXCLUSIVE.
-}
allocateStatic
  :: VMA.Allocator
  -> Vk.Device
  -> SliceRegistry
  -> (Vk.Queue, Word32)
  -- ^ queue + family for the one-shot generation submit
  -> ScenePipelines
  -> Maybe (Word32, Word32)
  -> ResourceT IO SceneStatic
allocateStatic allocator dev registry genQueue pls sharedFamilies = do
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
  lumManaged <- Buf.newManagedBuffer lumBuffer
  -- The host writes it (the meter pass, or the caller per frame), tonemap reads it.
  (_, (exposureBuffer, exposureAllocation, exposureMapped)) <- storageBuffer allocator 4
  exposureManaged <- Buf.newManagedBuffer exposureBuffer
  liftIO do
    poke (castPtr exposureMapped) (1.0 :: Float)
    VMA.flushAllocation allocator exposureAllocation 0 Vk.WHOLE_SIZE
  -- Staging for the bulk CPU meshes (cube + sphere), copied into the vertex buffer.
  (_, (staging, stagingPtr)) <- stagingBuffer allocator Meshes.cpuVertexBytes

  genSet <- Voxels.allocateGenSets dev pls.generator bufs
  meshSet <- Mesh.allocateSet dev pls.mesh vertexBuffer meshTableBuffer objectsBuffer visMain
  -- Shared linear sampler (resolve, tonemap, bloom).
  (_, sampler) <- allocateLinearSampler dev
  (_, nearestSampler) <- allocateNearestSampler dev

  -- EVSM shadow cube array: one cube per light (moments) + a shared depth cube.
  -- The resolve (async compute) samples the moments, but the array stays
  -- EXCLUSIVE — the one cross-queue image without STORAGE usage, so CONCURRENT
  -- would cost its DCC. The graph hands the slices across with real ownership
  -- transfers instead ('importOwnedImage' in "Rendering.Passes").
  (_, (shadowMoments, shadowCubeView, shadowRenderViews)) <-
    allocateCubeArray allocator dev Shadows.format Shadows.resolution Lights.slots (Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT) Nothing "shadow-moments"
  (_, (shadowDepthImage, shadowDepthView)) <-
    allocateArrayTarget allocator dev depthFormat Shadows.resolution 6 Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT Vk.IMAGE_ASPECT_DEPTH_BIT "shadow-depth"
  (_, viewProjBuffer) <- deviceBuffer allocator (fromIntegral (Lights.slots * Shadows.cubeFaces) * fromIntegral Shadows.viewProjBytes) (Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT) shared
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
    momentsSlice base n = describedSlice registry Shadows.format (Vk.Extent2D Shadows.resolution Shadows.resolution) shadowMoments Vk.IMAGE_ASPECT_COLOR_BIT base n
  bakedMoments <-
    if staticCubes == 0
      then pure Nothing
      else Just <$> momentsSlice 0 (staticCubes * Shadows.cubeFaces)
  shadowDepth <- newManagedImage registry shadowDepthImage Vk.IMAGE_ASPECT_DEPTH_BIT
  orbShadow <-
    if Lights.orbCount == 0
      then pure Nothing
      else do
        moments <- momentsSlice (Lights.orbBase * Shadows.cubeFaces) (Lights.orbCount * Shadows.cubeFaces)
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
    Shadows.uploadViewProjs cb viewProjBuffer 0
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

  -- The fenced bake left the EXCLUSIVE moments on the generation queue's family
  -- (the graph's default queue): claim them, so 'importOwnedImage' has an owner
  -- to derive the first cross-family hand-off from.
  forM_ (catMaybes [bakedMoments, fmap (.moments) orbShadow]) $
    claimOwnership FG.defaultQueue

  caveCount <- liftIO do
    VMA.invalidateAllocation allocator countAlloc 0 Vk.WHOLE_SIZE
    total <- peek (castPtr countMapped :: Ptr Word32)
    pure (total - objLayout.caveBase)
  sayErrString $ "cave cubes: " <> show caveCount <> " / " <> show Cave.maxCubes

  -- Wrap the cull's working set for graph tracking. The buffers start fresh:
  -- the setup submit above was fenced.
  lightsMB <- Buf.sharedAcrossQueues . Buf.describedAs "lights" <$> Buf.newManagedBuffer lights
  viewProjMB <- Buf.sharedAcrossQueues . Buf.describedAs "shadow view-projections" <$> Buf.newManagedBuffer viewProjBuffer
  objectsMB <- Buf.sharedAcrossQueues . Buf.describedAs "object table" <$> Buf.newManagedBuffer objectsBuffer
  indirectMB <- Buf.sharedAcrossQueues . Buf.describedAs "draw commands" <$> Buf.newManagedBuffer indirect
  visMainMB <- Buf.sharedAcrossQueues . Buf.describedAs "camera instance remap" <$> Buf.newManagedBuffer visMain
  visOccMB <- Buf.sharedAcrossQueues . Buf.describedAs "occluder instance remap" <$> Buf.newManagedBuffer visOcc

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
      , lumManaged
      , exposureBuffer
      , exposureAllocation
      , exposureMapped
      , exposureManaged
      , allocator
      , bakedMoments
      , shadowCubeView
      , shadowRenderViews
      , shadowDepthView
      , lightsBuffer = lights
      , lightsTable = lightsMB
      , viewProjTable = viewProjMB
      , objectsTable = objectsMB
      , viewProjBuffer
      , shadowSet
      , visMain = visMainMB
      , visOcc = visOccMB
      , orbShadow
      , caveCount
      }

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
    Shadows.recordCube cb pls.shadow shadowSet light (fromIntegral l * Shadows.cubeFaces) (renderViews V.! l) depthView indirect Objects.occluderDrawOffset
  transitionImagesTo cb [(m, Sampled shadeStage) | m <- momentsSlices]
