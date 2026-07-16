{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Headless driver.

Renders the scene once at a fixed extent across two queues, copies the result to a
host image, saves a PNG and asserts deterministic checks.

The @geometry@ pass runs on the graphics queue and @shade@ + @readback@ on the
async compute queue (when the GPU has a distinct compute family); the
cross-queue submit ordering comes off the compiled schedule
('submitGraphQueued'). On shared hardware everything collapses to one buffer
and one submit.
-}
module Headless
  ( main
  ) where

import qualified Codec.Picture as JP
import Control.Monad (foldM, forM, forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.IntSet as IntSet
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek)
import qualified Fragr as FG
import qualified Fragr.Dot as Dot
import HeadlessBoot (HeadlessConfig (..), HeadlessVk (..), withHeadlessVk)
import ImageReadback (makeReadbackImage, savePng)
import Numeric.Half (Half)
import System.Exit (exitFailure)
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.Frame (allocateCommandPool)
import Vulkan.Utils.FrameGraph.Driver (QueueSlot (..), SubmitConfig (..), submitConfig, submitGraphQueued, waitSubmitted)
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), Usage (..), importScratchImage, newManagedImage, sliceLayers, transitionImageTo)
import Vulkan.Utils.FrameGraph.Recorder (Recorder, recordingCommandBuffer)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

import Buffer (readbackBuffer)
import Driver (oneShot)
import qualified Exposure
import Options (Options)
import qualified Options
import qualified Pipeline.Mesh as Mesh
import qualified Rendering.Passes as Passes
import qualified Rendering.Pipelines as Pipelines
import qualified Rendering.Shadows as Shadows
import qualified Rendering.Static as Static
import qualified Rendering.Targets as Targets
import Requirements (deviceRequirements)
import qualified Scene.Camera as Camera

main :: Options -> IO ()
main opts = runHeadless $ \HeadlessVk{allocator, device, queues} -> do
  let
    extent@(Vk.Extent2D width height) = opts.extent
    QueueFamilyIndex graphicsFamily = fst (qGraphics queues)
    QueueFamilyIndex computeFamily = fst (qCompute queues)
    -- Nothing when compute shares the graphics family: the whole graph then runs
    -- on one queue and one submit.
    async
      | graphicsFamily == computeFamily = Nothing
      | otherwise = Just computeFamily

  (image, saved, visImage, depthImage, lum) <- render opts allocator device queues async
  Vk.deviceWaitIdle device

  -- The gamma pass already encoded sRGB into the display image, so the PNG is
  -- saved as-is (the @--debug-mode@ channel if one was asked for); the checks
  -- run on the beauty render.
  savePng opts.output saved

  -- Dump the intermediate buffers (vis instance/triangle ids, depth); the depth
  -- buffer also yields the ray-miss count directly — no colour heuristics.
  voidPixels <- dumpDebug allocator device (snd (qGraphics queues), graphicsFamily) visImage depthImage extent

  let
    packPixel (JP.PixelRGBA8 r g b a) =
      fromIntegral r * 0x1000000 + fromIntegral g * 0x10000 + fromIntegral b * 0x100 + fromIntegral a
    distinct = IntSet.size $ IntSet.fromList [packPixel (JP.pixelAt image x y) | y <- [0 .. fromIntegral height - 1], x <- [0 .. fromIntegral width - 1]]
    total = fromIntegral width * fromIntegral height :: Int
    -- What the viewer would settle on, and what it wanted before the gain clamp.
    autoExposure = Exposure.target opts.meter lum
    unclamped = opts.meter.key / max 1e-5 lum
    checks :: [(String, Bool)]
    checks =
      [ ("mesh pipeline composes (compile-time)", Mesh.composes)
      , ("cave covers the view (few depth ray-misses)", voidPixels * 20 < total)
      , ("many distinct instance greys", distinct > 12)
      ]
        -- The async path is topology, not correctness: only assert it where the
        -- hardware offers a distinct compute family (single-queue collapse is a
        -- supported render path, not a failure).
        <> [("async compute path taken", True) | Just _ <- [async]]
  liftIO $ do
    putStrLn $ "ray-miss (void) pixels: " <> show voidPixels <> "/" <> show total <> "; distinct colours: " <> show distinct
    putStrLn $ "avg luminance: " <> show lum <> "; exposure: " <> show autoExposure <> " (unclamped " <> show unclamped <> ")"
    mapM_ (\(label, ok) -> putStrLn $ "[" <> (if ok then "PASS" else "FAIL") <> "] " <> label) checks
    unless (all snd checks) exitFailure
    putStrLn "All visibility-buffer checks passed."

runHeadless :: (HeadlessVk -> ResourceT IO ()) -> IO ()
runHeadless k =
  runResourceT $ do
    vk <-
      withHeadlessVk
        HeadlessConfig
          { appName = "Haskell Vulkan visibility buffer (headless)"
          , instanceReqs = []
          , deviceReqs = Dynamic.dynamicRenderingRequirements <> deviceRequirements
          , vmaFlags = zero
          }
    k vk

----------------------------------------------------------------
-- The frame
----------------------------------------------------------------

render
  :: Options
  -> VMA.Allocator
  -> Vk.Device
  -> Queues (QueueFamilyIndex, Vk.Queue)
  -> Maybe Word32
  -> ResourceT IO (JP.Image JP.PixelRGBA8, JP.Image JP.PixelRGBA8, ManagedImage, ManagedImage, Float)
render opts allocator dev queues async = do
  -- The two families share the visibility buffer when compute is async.
  let
    extent = opts.extent
    eye = Camera.eye opts.orbit
    (QueueFamilyIndex graphicsFamily, graphicsQueue) = qGraphics queues
    sharedFamilies = fmap (graphicsFamily,) async

  pls <- Pipelines.allocatePipelines dev
  sceneStatic <- Static.allocateStatic allocator dev (graphicsQueue, graphicsFamily) pls sharedFamilies
  sharedHiZ <- Targets.allocateSharedHiZ allocator dev extent
  scene <- Targets.allocateTargets allocator dev pls sceneStatic extent sharedHiZ sharedFamilies True
  (cpuImage, readback) <- makeReadbackImage allocator dev Pipelines.colorFormat extent
  cpuManaged <- newManagedImage cpuImage Vk.IMAGE_ASPECT_COLOR_BIT
  -- Pools once, for every graph run below; only the buffers are per-run.
  queueTable <- allocateQueueTable dev queues async

  let -- Build + run a fresh graph for one debug mode; returns the read-back image.
      -- Reading displayOut keeps the gamma pass alive (windowed reads toneOut).
      -- The graph's cull pass runs the same compaction as the windowed frame —
      -- the second execution onwards is occlusion-culled against the previous
      -- one's pyramid (same camera), so the depth-void check covers the culling too.
      runMode debugMode = do
        graph <- FG.newFrameGraph
        outs <- Passes.addScenePasses graph pls opts.tweaks scene (Passes.computeQueueId async) extent eye 0 (Just (pure . Exposure.target opts.meter, hostQueue)) debugMode
        cpuH <- importScratchImage graph "cpu" cpuManaged
        cpuWritten <- FG.addPass graph "readback" (readbackSetup outs.displayOut cpuH) \_ -> do
          -- The declared accesses moved display to TRANSFER_SRC and the cpu
          -- image to TRANSFER_DST; only the copy itself is left to record.
          cb <- recordingCommandBuffer
          let Vk.Extent2D w h = extent
          Vk.cmdCopyImage
            cb
            scene.targets.display.image
            Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
            cpuManaged.image
            Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
            [Vk.ImageCopy (sliceLayers scene.targets.display) (Vk.Offset3D 0 0 0) (sliceLayers cpuManaged) (Vk.Offset3D 0 0 0) (Vk.Extent3D w h 1)]
        -- The PNG peek is a graph pass on the host queue: its wait on the copy
        -- is the compiled schedule's (a timeline wait the driver performs), the
        -- cpu image's GENERAL/HOST transition lands producer-side, and the body
        -- runs after the submits.
        result <- liftIO (newIORef Nothing)
        FG.addPass_ graph "host.readback" (hostSetup cpuWritten) do
          img <- liftIO readback
          liftIO (writeIORef result (Just img))
        FG.compile graph
        when (debugMode == 0) $ liftIO . TIO.writeFile "visibility-buffer.dot" =<< liftIO (Dot.dump graph)
        runGraph dev queueTable graph
        liftIO (readIORef result) >>= maybe (error "headless: the host readback pass did not run") pure

  -- One run: the host meter pass sits between luminance and tonemap, so the
  -- frame tonemaps at its own metered exposure (the old meter-then-re-render
  -- double run is gone).
  img <- runMode 0
  lum <- Targets.readLuminance scene
  -- Before the debug modes below overwrite the HDR target (and thus the probe).
  forM_ (Targets.lumProbe scene) $ uncurry (dumpLumProbe allocator dev (graphicsQueue, graphicsFamily))
  -- Material/geometry debug views (each re-runs the graph with a debug mode).
  forM_ (zip [1 :: Word32 ..] ["albedo", "metalness", "roughness", "normal"]) \(mode, name) -> do
    dbg <- runMode mode
    liftIO $ savePng ("debug-mat-" <> name <> ".png") dbg
  -- The saved PNG honours @--debug-mode@, so a headless run can capture any
  -- channel; the checks and the probe stay on the beauty render above.
  saved <- if opts.debugMode == 0 then pure img else runMode opts.debugMode
  -- Last, since the copy leaves the moments cube in TRANSFER_SRC (nothing samples
  -- it after this).
  forM_ (Targets.shadowImage scene) $ dumpShadowFace allocator dev (graphicsQueue, graphicsFamily)
  let (visImage, depthImage) = Targets.debugImages scene
  pure (img, saved, visImage, depthImage, lum)
  where
    -- On the graphics queue, sandwiching the compute chain: on async hardware
    -- this makes the driver segment the graphics stream (geometry before the
    -- compute passes, the copy after them) instead of rejecting the schedule.
    readbackSetup displayOut cpuH = do
      FG.readWith displayOut TransferSrc
      FG.writeWith cpuH TransferDst
    hostSetup cpuWritten = do
      FG.setQueue hostQueue
      FG.setSideEffect
      FG.readWith cpuWritten HostRead

-- | The host queue: its passes run on the CPU, after the submits.
hostQueue :: FG.QueueId
hostQueue = FG.QueueId 2

{- | Record the graph across its queues, submit and wait for completion.

'submitGraphQueued' derives the cross-queue waits from the compiled schedule
(async: the shade/readback submit chains off the geometry work at exactly the
handed-over accesses' stages); this driver only maps 'FG.QueueId's to queues.

The queue table is built once ('allocateQueueTable') and reused by every
graph run: the buffers are per-run (freed with the enclosing scope), the
pools are not — they are expensive to create and exist to be reused.
-}
runGraph
  :: Vk.Device
  -> (FG.QueueId -> QueueSlot)
  -> FG.FrameGraph Recorder ()
  -> ResourceT IO ()
runGraph dev queueTable graph = do
  submitted <- submitGraphQueued graph (submitConfig dev queueTable){hostQueue = Just hostQueue}
  _ <- waitSubmitted dev maxBound submitted
  pure ()

-- | One command pool per queue the graph uses, for the whole run.
allocateQueueTable
  :: Vk.Device
  -> Queues (QueueFamilyIndex, Vk.Queue)
  -> Maybe Word32
  -> ResourceT IO (FG.QueueId -> QueueSlot)
allocateQueueTable dev queues async = do
  let (QueueFamilyIndex graphicsFamily, graphicsQueue) = qGraphics queues
  graphicsPool <- allocateCommandPool dev graphicsFamily
  computeSlot <- forM async \computeFamily -> do
    pool <- allocateCommandPool dev computeFamily
    pure QueueSlot{queue = snd (qCompute queues), family = computeFamily, pool}
  pure \q -> case computeSlot of
    Just slot | q == Passes.computeQueue -> slot
    _ -> QueueSlot{queue = graphicsQueue, family = graphicsFamily, pool = graphicsPool}

----------------------------------------------------------------
-- Debug dumps (intermediate buffers)
----------------------------------------------------------------

{- | Dump the intermediate buffers as PNGs for development.

The visibility buffer's instance-id and triangle-id channels (coloured per id) and the
depth buffer (grey; brighter = nearer, reverse-Z). Returns the ray-miss count: the
pixels where reverse-Z depth is still the cleared @0@ (nothing was drawn).
-}
dumpDebug :: VMA.Allocator -> Vk.Device -> (Vk.Queue, Word32) -> ManagedImage -> ManagedImage -> Vk.Extent2D -> ResourceT IO Int
dumpDebug allocator dev qf visImage depthImage extent@(Vk.Extent2D w h) = do
  visPtr <- copyImageToHostBuffer allocator dev qf visImage extent 8
  depthPtr <- copyImageToHostBuffer allocator dev qf depthImage extent 4
  liftIO $ do
    let
      wi = fromIntegral w
      hi = fromIntegral h
      depthAt i = peek (castPtr (depthPtr `plusPtr` (i * 4))) :: IO Float
    savePng "debug-instance.png"
      =<< JP.withImage wi hi \x y -> idColor <$> (peek (castPtr (visPtr `plusPtr` ((y * wi + x) * 8))) :: IO Word32)
    savePng "debug-triangle.png"
      =<< JP.withImage wi hi \x y -> idColor <$> (peek (castPtr (visPtr `plusPtr` ((y * wi + x) * 8 + 4))) :: IO Word32)
    -- One pass: the max depth (for the legible normalised dump, since reverse-Z
    -- crushes depth near 0) and the ray-miss count (depth still at cleared 0).
    (maxDepth, voidPixels) <-
      foldM
        (\(m, c) i -> do d <- depthAt i; pure (max m d, if d <= 0 then c + 1 else c))
        (1e-6, 0 :: Int)
        ([0 .. wi * hi - 1] :: [Int])
    savePng "debug-depth.png"
      =<< JP.withImage wi hi \x y -> do
        d <- depthAt (y * wi + x)
        let v = round (255 * max 0 (min 1 (d / maxDepth)))
        pure (JP.PixelRGBA8 v v v 255)
    pure voidPixels

{- | Dump the luminance probe: the exact mip the auto-exposure reduction averaged.

Two views of the same @rgba16f@ snapshot. @debug-luminance.hdr@ keeps the linear
radiance verbatim (float RGB, no clamp) — the artifact to measure. @debug-luminance.png@
is a 16-bit grey ramp of the per-pixel @log2@ luminance the reduction sums, over
'probeEvRange'. Prints the extent, the luminance range, and a host-side geometric mean
that must agree with the GPU's.
-}
dumpLumProbe :: VMA.Allocator -> Vk.Device -> (Vk.Queue, Word32) -> ManagedImage -> Vk.Extent2D -> ResourceT IO ()
dumpLumProbe allocator dev qf probe extent@(Vk.Extent2D w h) = do
  ptr <- copyImageToHostBuffer allocator dev qf probe extent 8
  liftIO $ do
    let
      wi = fromIntegral w
      hi = fromIntegral h
      -- rgba16f: four halfs per texel.
      channel i c = realToFrac <$> (peek (castPtr (ptr `plusPtr` (i * 8 + c * 2))) :: IO Half) :: IO Float
      rgbAt i = (,,) <$> channel i 0 <*> channel i 1 <*> channel i 2
      lumaOf (r, g, b) = 0.2126 * r + 0.7152 * g + 0.0722 * b
      (evLo, evHi) = probeEvRange
    rgbs <- V.generateM (wi * hi) rgbAt
    let
      lums = V.map lumaOf rgbs
      toPixelf x y = pure $ JP.PixelRGBF r g b
        where
          (r, g, b) = rgbs V.! (y * wi + x)
      toPixel16 x y = pure (round (65535 * max 0 (min 1 t)) :: JP.Pixel16)
        where
          l = max 1e-4 (lums V.! (y * wi + x))
          t = (logBase 2 l - evLo) / (evHi - evLo)
      -- The reduction's own formula, on the host: it must match `avg luminance`.
      geoMean = exp (V.sum (V.map (log . max 1e-4) lums) / fromIntegral (V.length lums))
    JP.withImage wi hi toPixelf >>= BSL.writeFile "debug-luminance.hdr" . JP.encodeHDR
    JP.withImage wi hi toPixel16 >>= JP.writePng "debug-luminance.png"
    putStrLn $
      "luminance probe: "
        <> show wi
        <> "x"
        <> show hi
        <> " px; luminance "
        <> show (V.minimum lums)
        <> " .. "
        <> show (V.maximum lums)
        <> "; geoMean(cpu) "
        <> show geoMean

-- | EV window the probe's grey ramp spans; wide enough for cave dark to emitter core.
probeEvRange :: (Float, Float)
probeEvRange = (-14, 2)

{- | Dump the +X face of light 0's EVSM shadow cube (array layer 0).

Reconstruct the light-space distance from the first moment (@ln(m.r)/C@) as greyscale.
-}
dumpShadowFace :: VMA.Allocator -> Vk.Device -> (Vk.Queue, Word32) -> ManagedImage -> ResourceT IO ()
dumpShadowFace allocator dev qf moments = do
  let res = fromIntegral Shadows.resolution
  ptr <- copyImageToHostBuffer allocator dev qf moments (Vk.Extent2D Shadows.resolution Shadows.resolution) 16
  liftIO $
    savePng "debug-shadow.png"
      =<< JP.withImage res res \x y -> do
        r <- peek (castPtr (ptr `plusPtr` ((y * res + x) * 16))) :: IO Float
        let
          d = if r > 1 then log r / 30 else 1 -- C = 30; empty texels read as far
          v = round (255 * max 0 (min 1 d))
        pure (JP.PixelRGBA8 v v v 255)

{- | Copy the first layer of a tracked image's slice to a mapped host buffer.

The @TRANSFER_SRC@ transition comes from the image's tracker (and covers the
whole slice, advancing it). Returns the invalidated mapped pointer. @bpp@ is
the format's bytes per pixel; the image must have been created with
@TRANSFER_SRC@ usage.
-}
copyImageToHostBuffer :: VMA.Allocator -> Vk.Device -> (Vk.Queue, Word32) -> ManagedImage -> Vk.Extent2D -> Int -> ResourceT IO (Ptr ())
copyImageToHostBuffer allocator dev qf mi (Vk.Extent2D w h) bpp = do
  let size = fromIntegral (fromIntegral w * fromIntegral h * bpp) :: Vk.DeviceSize
  (_, (buffer, alloc, mapped)) <- readbackBuffer allocator size
  oneShot dev qf \cb -> do
    transitionImageTo cb mi TransferSrc
    Vk.cmdCopyImageToBuffer
      cb
      mi.image
      Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
      buffer
      [Vk.BufferImageCopy 0 0 0 (sliceLayers mi) (Vk.Offset3D 0 0 0) (Vk.Extent3D w h 1)]
  VMA.invalidateAllocation allocator alloc 0 Vk.WHOLE_SIZE
  pure mapped

-- | A distinct colour per id (cosine palette); id 0 is black.
idColor :: Word32 -> JP.PixelRGBA8
idColor 0 = JP.PixelRGBA8 0 0 0 255
idColor n = JP.PixelRGBA8 (chan 0.0) (chan 0.33) (chan 0.67) 255
  where
    hval = fromIntegral n * 0.61803398875 :: Float
    chan o = round (255 * (0.4 + 0.5 * cos (6.2831853 * (o + hval))))
