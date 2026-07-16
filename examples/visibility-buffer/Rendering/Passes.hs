{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| The visibility-buffer frame graph.

A voxel cave ("Cave"), a compute-generated knot and a dynamic orb, all drawn as one
unified mesh (shared vertex/mesh/object tables) and rasterized through an
@(objectId, triangleId)@ visibility buffer, then resolved by a single DAIS path.

The knot, the orb and the six glowstones share the lit central stage; the six side
chambers are dim backrooms, reached by halls and lit only by the colour that spills down
them — so the stage meters near middle grey and a backroom pins the auto-exposure ceiling.

'addScenePasses' assembles the graph over a "Rendering.Targets" scene.
-}
module Rendering.Passes
  ( Tweaks (..)
  , defaultTweaks
  , PassOutputs (..)
  , computeQueue
  , computeQueueId
  , familyPartition
  , addScenePasses
  ) where

import Control.Monad (foldM, forM, forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Data.IORef (readIORef, writeIORef)
import Data.Int (Int32)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32)
import qualified Fragr as FG
import Geomancy (Vec3, vec4, withVec3)
import qualified Geomancy.Mat4 as Mat4
import qualified Vulkan.Core10 as MemoryBarrier (MemoryBarrier (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (DynamicState (..), allDynamicStates, applyDynamicStates, dynamicStateFor, fullScissor)
import qualified Vulkan.Utils.FrameGraph.Buffer as Buf
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), Usage (..), importManagedImage, importOwnedImage, importScratchImage, transitionImageTo)
import Vulkan.Utils.FrameGraph.Recorder (Recorder, recordingCommandBuffer)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Zero (zero)

import qualified Pipeline.Bloom as Bloom
import qualified Pipeline.Cull as Cull
import qualified Pipeline.HiZ as HiZ
import qualified Pipeline.Mesh as Mesh
import qualified Pipeline.Shade as Shade
import qualified Pipeline.Ssao as Ssao
import qualified Pipeline.Tonemap as Tonemap
import Rendering.Pipelines (ScenePipelines, shadeStage)
import qualified Rendering.Pipelines
import qualified Rendering.Shadows as Shadows
import Rendering.Static (OrbShadow)
import qualified Rendering.Static
import Rendering.Targets (Scene, SceneTargets (..), halfExtentOf, lumMipFor, readLuminance, setExposure)
import qualified Rendering.Targets
import qualified Scene.Camera as Camera
import qualified Scene.Lights as Lights
import qualified Scene.Objects as Objects

{- | The graph outputs a driver can consume.

The passes are always added the same way; the driver picks one output and the
graph culls whatever nothing demands. @colorOut@ is the raw shade output (the
debug channels live here). Beauty flows @toneOut@ (tonemapped, display-linear)
→ @displayOut@ (gamma-encoded); in a debug view the gamma pass encodes
@colorOut@ directly, so @displayOut@ skips the bloom/tonemap chain and
@toneOut@ goes undemanded.
-}
data PassOutputs = PassOutputs
  { colorOut :: FG.Handle ManagedImage
  , toneOut :: FG.Handle ManagedImage
  , displayOut :: FG.Handle ManagedImage
  }

-- | The queue the shade/post passes run on under an async topology.
computeQueue :: FG.QueueId
computeQueue = FG.QueueId 1

{- | The queue id for the shade/post passes: 'computeQueue' when async, else the
default (graphics) queue, which collapses the graph to a single submit.
-}
computeQueueId :: Maybe a -> FG.QueueId
computeQueueId = maybe FG.defaultQueue (const computeQueue)

{- | The family partition 'FG.compileWith' schedules against.

Mirrors the drivers' queue tables; the host queue is deliberately outside
it (the host owns nothing).
-}
familyPartition :: Maybe (Word32, Word32) -> [(FG.QueueId, FG.FamilyId)]
familyPartition = maybe [] \(gfx, comp) ->
  [ (FG.defaultQueue, FG.FamilyId (fromIntegral gfx))
  , (computeQueue, FG.FamilyId (fromIntegral comp))
  ]

{- | The @orbs.upload@ pass body: refresh the three per-frame tables.

Barrier-free by construction — the pass declares the writes and every
consumer its reads, so the tracker places the transfer barriers (including
the write-after-read against the previous frame's still-running consumers,
whose state it carries across graphs).
-}
recordOrbUploads :: (MonadIO m) => Vk.CommandBuffer -> Scene -> Float -> m ()
recordOrbUploads cb scene t = liftIO do
  Lights.updateOrbs cb scene.static.lightsBuffer t
  Shadows.uploadOrbViewProjs cb scene.static.viewProjBuffer t
  Objects.writeOrbObjects cb scene.static.objectsBuffer scene.static.objLayout t

{- | The @shadow.orbs@ pass body.

Re-renders the orbs' shadow-cube slices for time @t@, each drawing its own
occluder set the cull pass just compacted to that orb's reach. The glowstones
never move, so only the orbs' moment layers are refreshed — the rest of the
EVSM array stays baked ("Rendering.Static").
-}
recordOrbShadows :: (MonadIO m) => Vk.CommandBuffer -> ScenePipelines -> Scene -> OrbShadow -> Float -> m ()
recordOrbShadows cb pls scene orbShadow t = liftIO do
  forM_ (zip [0 ..] Lights.orbs) \(i, orb) -> do
    -- The shared depth cube is rewritten per orb: a tracked same-state write
    -- places the WAW barrier between the renders.
    when (i > 0) $ transitionImageTo cb orbShadow.depth DepthAttachment
    let light = Lights.orbBase + i
    Shadows.recordCube
      cb
      pls.shadow
      scene.static.shadowSet
      (Lights.orbLight orb t)
      (light * Shadows.cubeFaces)
      (scene.static.shadowRenderViews V.! fromIntegral light)
      scene.static.shadowDepthView
      scene.static.indirect.buffer
      (Objects.orbOccDrawOffset i)

{- | Add the whole scene graph.

@cull@ (compact this frame's draws) → @shadow.orbs@ (refresh the orbs' shadow
slices) → @geometry@ (raster → vis + depth) → @shade@ (resolve → HDR) →
@luminance@ (auto-exposure readback) → @tonemap@ (exposure + curve) → @gamma@
(sRGB encode). @queue@ is the queue the compute passes run on
('computeQueueId' picks it from the topology); @t@ places the orbs. The tonemap
exposure comes from the metering buffer: with a host meter, a host pass
between @luminance@ and @tonemap@ writes it from this frame's own luminance;
without one the caller pre-writes it ('setExposure', e.g. a lagged windowed
EMA). Returns the 'PassOutputs'; a driver reads whichever it presents.
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
  -> Maybe (Float -> IO Float, FG.QueueId)
  {- ^ meter the exposure in-graph: a host pass on the given queue maps this
  frame's mean luminance to the exposure it tonemaps at
  -}
  -> Word32
  -- ^ debug mode (0 = beauty; 1 albedo, 2 metalness, 3 roughness, 4 normal, 5 object id, 6 ao)
  -> ResourceT IO PassOutputs
addScenePasses graph pls tweaks scene queue extent eye t hostMeter debugMode = do
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
  hizHs <- V.imapM (\i m -> importManagedImage graph (T.pack ("hiz.mip" <> show i)) m) scene.hiz.pyramidMips
  -- The per-frame tables the orbs move: tracked, so the upload's barriers (and
  -- the write-after-read against the previous frame) fall out of the declared
  -- accesses instead of a hand-maintained stage mask.
  lightsH <- Buf.importScratchBuffer graph "table.lights" scene.static.lightsTable
  viewProjH <- Buf.importScratchBuffer graph "table.viewProjs" scene.static.viewProjTable
  objectsH <- Buf.importScratchBuffer graph "table.objects" scene.static.objectsTable

  -- Refresh the orbs' rows in the three tables. Without orbs nothing moves, so
  -- the setup upload stands and the handles pass through unrenamed.
  (lights, viewProjs, objects) <-
    if null Lights.orbs
      then pure (lightsH, viewProjH, objectsH)
      else FG.addPass graph "orbs.upload" (uploadSetup lightsH viewProjH objectsH) \_ -> do
        cb <- recordingCommandBuffer
        recordOrbUploads cb scene t

  -- Compact this frame's draws: reset the two cube draw commands, then refill
  -- them (and the instance remaps) from the frustum + occlusion tests for the
  -- camera and the reach test around the orbs. The occlusion test samples the
  -- previous frame's pyramid — the imported mip versions, read here before
  -- this frame's rebuild renames them — and skips the first frame after
  -- 'Rendering.Targets.allocateTargets' (nothing built the pyramid yet; a
  -- resize resets this). Kept alive by the draws' declared reads, no side
  -- effect needed.
  indirectReset <-
    FG.addPass graph "cull.reset" (FG.writeWith indirectH Buf.TransferDst) \_ -> do
      cb <- recordingCommandBuffer
      Cull.reset cb scene.static.indirect.buffer scene.static.objLayout.caveBase
  let cullSetup = do
        V.forM_ hizHs \mipH -> FG.readWith mipH (StorageRead shadeStage)
        FG.readWith lights (Buf.StorageRead shadeStage)
        FG.readWith objects (Buf.StorageRead shadeStage)
        indirectCulled <- FG.writeWith indirectReset (Buf.StorageReadWrite shadeStage)
        visMainCulled <- FG.writeWith visMainH (Buf.StorageWrite shadeStage)
        visOccCulled <- FG.writeWith visOccH (Buf.StorageWrite shadeStage)
        pure (indirectCulled, visMainCulled, visOccCulled)
  (indirectCulled, visMainCulled, visOccCulled) <-
    FG.addPass graph "cull" cullSetup \_ -> do
      cb <- recordingCommandBuffer
      hizValid <- liftIO (readIORef scene.hiz.pyramidPrimed)
      Cull.record pls.cull scene.cullSet (cullParams hizValid) cb
      liftIO (writeIORef scene.hiz.pyramidPrimed True)

  -- The EVSM moments are EXCLUSIVE ("Rendering.Static"), so their slices import
  -- owned: the schedule derives the release/acquire pairs crossing to the
  -- resolve's queue — including the frame-boundary half, since the resolve's
  -- family still owns the slices when the next graph starts.
  bakedMoments <- forM scene.static.bakedMoments (importOwnedImage graph "shadow.baked")

  -- Refresh the orbs' shadow slices, drawing the occluder set the cull just
  -- compacted for the same @t@ (an out-of-graph refresh would draw a set
  -- filtered for another orb time). See 'recordOrbShadows'.
  orbMoments <- forM scene.static.orbShadow \orb -> do
    momentsH <- importOwnedImage graph "shadow.orbMoments" orb.moments
    orbDepthH <- importScratchImage graph "shadow.orbDepth" orb.depth
    let orbSetup = do
          FG.readWith indirectCulled Buf.IndirectRead
          FG.readWith visOccCulled (Buf.StorageRead Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT)
          FG.readWith viewProjs (Buf.StorageRead Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT)
          FG.readWith objects (Buf.StorageRead Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT)
          FG.writeWith_ orbDepthH DepthAttachment
          FG.writeWith momentsH ColorAttachment
    FG.addPass graph "shadow.orbs" orbSetup \_ -> do
      cb <- recordingCommandBuffer
      recordOrbShadows cb pls scene orb t

  (visWritten, depthWritten) <-
    FG.addPass graph "geometry" (geometrySetup indirectCulled visMainCulled objects visH depthH) \_ -> do
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
          dispatchMip cb (scene.hiz.pyramidReduceExtents V.! i)
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
    FG.addPass graph "ssao.normals" (normalsSetup visWritten objects normalsH) \_ -> do
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
    FG.addPass graph "shade" (shadeSetup visWritten aoBlurred bakedMoments orbMoments lights objects colorH) \_ -> do
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
  exposureH <- Buf.importManagedBuffer graph "exposure" scene.static.exposureManaged
  exposureRef <-
    if debugMode /= 0
      then pure exposureH
      else do
        lumH <- Buf.importManagedBuffer graph "lum" scene.static.lumManaged
        lumWritten <- FG.addPass graph "luminance" (luminanceSetup (downHs !! lumMip) lumH) \_ -> do
          cb <- recordingCommandBuffer
          Pipeline.bind cb pls.luminance
          Pipeline.bindSet cb pls.luminance 0 scene.lumSet
          Vk.cmdDispatch cb 1 1 1
          -- Without a meter pass the host polls the mapped value across frames
          -- (no wait to lean on), so make the write host-visible here; with one,
          -- the hand-off to the host queue is the schedule's.
          when (isNothing hostMeter) $
            Vk.cmdPipelineBarrier cb Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT Vk.PIPELINE_STAGE_HOST_BIT zero [hostVisible] [] []
        case hostMeter of
          Nothing -> pure exposureH
          Just (meter, hostQ) ->
            -- The device→host→device sandwich: this frame's own luminance
            -- meters this frame's tonemap.
            FG.addPass graph "host.meter" (meterSetup hostQ lumWritten exposureH) \_ -> liftIO do
              lum <- readLuminance scene
              setExposure scene =<< meter lum

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
    FG.addPass graph "tonemap" (tonemapSetup colorWritten bloom0 exposureRef toneH) \_ -> do
      cb <- recordingCommandBuffer
      Pipeline.bind cb pls.tonemap
      pushTonemap cb pls.tonemap tweaks.bloomStrength
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
    viewProj = Camera.viewProjFor eye extent
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
    -- The orb refresh: three transfer writes, tracked.
    uploadSetup lightsH viewProjH objectsH = do
      lights <- FG.writeWith lightsH Buf.TransferDst
      viewProjs <- FG.writeWith viewProjH Buf.TransferDst
      objects <- FG.writeWith objectsH Buf.TransferDst
      pure (lights, viewProjs, objects)
    geometrySetup indirectCulled visMainCulled objects visH depthH = do
      FG.readWith indirectCulled Buf.IndirectRead
      FG.readWith visMainCulled (Buf.StorageRead Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT)
      FG.readWith objects (Buf.StorageRead Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT)
      depthWritten <- FG.writeWith depthH DepthAttachment
      visWritten <- FG.writeWith visH ColorAttachment
      pure (visWritten, depthWritten)
    -- The DAIS normals pass rebuilds world normals from the object table.
    normalsSetup visWritten objects normalsH = do
      FG.readWith visWritten (StorageRead shadeStage)
      FG.readWith objects (Buf.StorageRead shadeStage)
      FG.writeWith normalsH (StorageWrite shadeStage)
    shadeSetup visWritten aoWritten bakedMoments orbMoments lights objects colorH = do
      FG.setQueue queue
      FG.readWith lights (Buf.StorageRead shadeStage)
      FG.readWith objects (Buf.StorageRead shadeStage)
      FG.readWith visWritten (StorageRead shadeStage)
      FG.readWith aoWritten (StorageRead shadeStage)
      forM_ bakedMoments \bakedH -> FG.readWith bakedH (Sampled shadeStage)
      forM_ orbMoments \momentsH -> FG.readWith momentsH (Sampled shadeStage)
      FG.writeWith colorH (StorageWrite shadeStage)
    -- The SSAO passes' shared shape: storage reads in, one storage write out,
    -- on the pass's default (graphics) queue.
    computeSetup (srcs :: [FG.Handle ManagedImage]) dstH = do
      mapM_ (\r -> FG.readWith r (StorageRead shadeStage)) srcs
      FG.writeWith dstH (StorageWrite shadeStage)
    luminanceSetup srcH lumH = do
      FG.setQueue queue
      FG.setSideEffect
      FG.readWith srcH (StorageRead shadeStage)
      FG.writeWith lumH (Buf.StorageWrite shadeStage)
    -- The host meter: read this frame's luminance, write the exposure.
    meterSetup hostQ lumWritten exposureH = do
      FG.setQueue hostQ
      FG.readWith lumWritten Buf.HostRead
      FG.writeWith exposureH Buf.HostWrite
    -- Probe snapshot: transfer-copy the metered mip into its own image.
    probeSetup srcH probeH = do
      FG.setQueue queue
      FG.setSideEffect
      FG.readWith srcH TransferSrc
      FG.writeWith_ probeH TransferDst
    -- Downsample: read the source mip, write the target mip.
    bloomSetup src dstH = do
      FG.setQueue queue
      FG.readWith src (StorageRead shadeStage)
      FG.writeWith dstH (StorageWrite shadeStage)
    -- Upsample: read the blur source (next-smaller mip) and read+write the
    -- destination mip in place (the read+write is the intra-image barrier).
    upSetup blur destH = do
      FG.setQueue queue
      FG.readWith blur (StorageRead shadeStage)
      FG.readWith destH (StorageRead shadeStage)
      FG.writeWith destH (StorageWrite shadeStage)
    tonemapSetup colorWritten bloom0 exposureRef toneH = do
      FG.setQueue queue
      FG.readWith colorWritten (StorageRead shadeStage)
      FG.readWith bloom0 (StorageRead shadeStage)
      FG.readWith exposureRef (Buf.StorageRead shadeStage)
      FG.writeWith toneH (StorageWrite shadeStage)
    gammaSetup srcH displayH = do
      FG.setQueue queue
      FG.readWith srcH (StorageRead shadeStage)
      FG.writeWith displayH (StorageWrite shadeStage)
    hizSetup src dstH = do
      FG.setSideEffect
      FG.readWith src (StorageRead shadeStage)
      FG.writeWith dstH (StorageWrite shadeStage)
    hizTailSetup src dstHs = do
      FG.setSideEffect
      FG.readWith src (StorageRead shadeStage)
      forM (V.toList dstHs) \h ->
        FG.writeWith h (StorageWrite shadeStage)

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
      { Ssao.view = Camera.viewFor eye
      , Ssao.sx = sx
      , Ssao.sy = sy
      , Ssao.zNear = Camera.near
      , Ssao.radius = tweaks.aoRadius
      , Ssao.intensity = tweaks.aoIntensity
      , Ssao.bias = tweaks.aoBias
      }
  where
    (sx, sy) = Camera.projScales extent

-- | Push one axis of the AO blur.
pushBlur :: (MonadIO m) => Vk.CommandBuffer -> Pipeline -> Tweaks -> (Int32, Int32) -> m ()
pushBlur cb pl tweaks (ax, ay) =
  Pipeline.push cb pl Ssao.Blur{Ssao.sharpness = tweaks.aoSharpness, Ssao.axisX = ax, Ssao.axisY = ay}

-- | Push the tonemap's bloom strength (COMPUTE stage); exposure rides the metering buffer.
pushTonemap :: (MonadIO m) => Vk.CommandBuffer -> Pipeline -> Float -> m ()
pushTonemap cb pl bloomStrength =
  Pipeline.push cb pl Tonemap.PC{Tonemap.bloomStrength = bloomStrength}

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
