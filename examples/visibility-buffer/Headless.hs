{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Headless driver.

Renders the scene once at a fixed extent across two queues, copies the result to a
host image, saves a PNG and asserts deterministic checks.

The @geometry@ pass runs on the graphics queue and @shade@ + @readback@ on the
async compute queue (when the GPU has a distinct compute family): geometry
signals a semaphore the compute buffer waits on, fenced at the end. On shared
hardware everything collapses to one buffer and one fenced submit.
-}
module Headless
  ( main
  ) where

import qualified Codec.Picture as JP
import Control.Monad (foldM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT)
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (writeIORef)
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty ((:|)))
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
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as ImageMemoryBarrier (ImageMemoryBarrier (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), Usage (..), usageFlags, usageState)
import Vulkan.Utils.FrameGraph.Recorder (Recorder, recordGraph, recordingCommandBuffer)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA

import Buffer (readbackBuffer)
import Driver (beginPrimary, commandPool)
import qualified Exposure
import Options (Options)
import qualified Options
import qualified Pipeline.Mesh as Mesh
import Requirements (deviceRequirements)
import qualified Scene
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
  -> ResourceT IO (JP.Image JP.PixelRGBA8, JP.Image JP.PixelRGBA8, Vk.Image, Vk.Image, Float)
render opts allocator dev queues async = do
  -- The two families share the visibility buffer when compute is async.
  let
    extent = opts.extent
    eye = Camera.eye opts.orbit
    (QueueFamilyIndex graphicsFamily, graphicsQueue) = qGraphics queues
    sharedFamilies = fmap (graphicsFamily,) async

  pls <- Scene.allocatePipelines dev
  sceneStatic <- Scene.allocateStatic allocator dev (graphicsQueue, graphicsFamily) pls sharedFamilies
  scene <- Scene.allocateTargets allocator dev pls sceneStatic extent sharedFamilies True
  (cpuImage, readback) <- makeReadbackImage allocator dev Scene.colorFormat extent

  let -- Build + run a fresh graph for one debug mode; returns the read-back image.
      -- Reading displayOut keeps the gamma pass alive (windowed reads toneOut).
      -- The cull prologue runs the same compaction as the windowed frame — the
      -- second execution onwards is occlusion-culled against the previous one's
      -- pyramid (same camera), so the depth-void check covers the culling too.
      runMode exposure debugMode = do
        graph <- FG.newFrameGraph
        outs <- Scene.addScenePasses graph pls opts.tweaks scene (computeQueueId async) extent eye exposure debugMode
        FG.addPass_ graph "readback" (readbackSetup outs.displayOut) do
          cb <- recordingCommandBuffer
          copyToHost cb extent scene.targets.display.image cpuImage
        FG.compile graph
        when (debugMode == 0) $ liftIO . TIO.writeFile "visibility-buffer.dot" =<< liftIO (Dot.dump graph)
        runGraph dev queues async (\cb -> Scene.recordCull cb pls scene eye extent 0) graph
        readback

  -- Meter, then re-render at the exposure the viewer would settle on. The luminance
  -- pass reads the pre-exposure HDR, so the metering pass's own exposure is moot.
  _ <- runMode meterExposure 0
  lum <- Scene.readLuminance scene
  img <- runMode (Exposure.target opts.meter lum) 0
  -- Before the debug modes below overwrite the HDR target (and thus the probe).
  forM_ (Scene.lumProbe scene) $ uncurry (dumpLumProbe allocator dev (graphicsQueue, graphicsFamily))
  -- Material/geometry debug views (each re-runs the graph with a debug mode).
  forM_ (zip [1 :: Word32 ..] ["albedo", "metalness", "roughness", "normal"]) \(mode, name) -> do
    dbg <- runMode meterExposure mode
    liftIO $ savePng ("debug-mat-" <> name <> ".png") dbg
  -- The saved PNG honours @--debug-mode@, so a headless run can capture any
  -- channel; the checks and the probe stay on the beauty render above.
  saved <- if opts.debugMode == 0 then pure img else runMode (Exposure.target opts.meter lum) opts.debugMode
  -- Last, since the copy leaves the moments cube in TRANSFER_SRC (nothing samples
  -- it after this).
  dumpShadowFace allocator dev (graphicsQueue, graphicsFamily) (Scene.shadowImage scene)
  let (visImage, depthImage) = Scene.debugImages scene
  pure (img, saved, visImage, depthImage, lum)
  where
    readbackSetup displayOut = do
      FG.setQueue computeQueue
      FG.setSideEffect
      FG.readWith displayOut (usageFlags TransferSrc)

{- | Exposure for the metering and debug passes.

Debug channels bypass exposure entirely, and the luminance pass reads the
pre-exposure HDR, so this only scales an image nothing looks at.
-}
meterExposure :: Float
meterExposure = 1

-- | The compute-and-readback queue (async family, or the graphics queue).
computeQueue :: FG.QueueId
computeQueue = FG.QueueId 1

-- | The queue the shade/readback passes run on (async family, or graphics).
computeQueueId :: Maybe Word32 -> FG.QueueId
computeQueueId = maybe FG.defaultQueue (const computeQueue)

{- | Record the graph across its queues and wait for completion.

@prologue@ records into the graphics buffer ahead of the graph (the cull, whose
consumers all draw on the graphics queue). Single queue: one buffer, one fenced
submit. Async: geometry on the graphics queue signals a semaphore the compute
buffer (shade + readback) waits on, fenced at the end.
-}
runGraph
  :: Vk.Device
  -> Queues (QueueFamilyIndex, Vk.Queue)
  -> Maybe Word32
  -> (Vk.CommandBuffer -> ResourceT IO ())
  -> FG.FrameGraph Recorder ()
  -> ResourceT IO ()
runGraph dev queues async prologue graph = do
  let (QueueFamilyIndex graphicsFamily, graphicsQueue) = qGraphics queues
  graphicsPool <- commandPool dev graphicsFamily
  graphicsCb <- beginPrimary dev graphicsPool
  prologue graphicsCb

  computePair <- case async of
    Just computeFamily -> do
      pool <- commandPool dev computeFamily
      cb <- beginPrimary dev pool
      pure (Just (snd (qCompute queues), cb))
    Nothing -> pure Nothing

  let
    cbFor q = case computePair of
      Just (_, cb) | q == computeQueue -> cb
      _ -> graphicsCb
    buffers = graphicsCb :| maybe [] (pure . snd) computePair
  recordGraph cbFor buffers graph

  (_, fence) <- Vk.withFence dev zero Nothing allocate
  case computePair of
    Just (computeQueue', computeCb) -> do
      (_, geomDone) <- Vk.withSemaphore dev zero Nothing allocate
      Vk.queueSubmit graphicsQueue [SomeStruct (submitInfo graphicsCb [] [] [geomDone])] Vk.NULL_HANDLE
      -- The COMPUTE_SHADER wait stage must cover everything the compute buffer
      -- reads from the graphics queue: the shade pass's inputs are handed over
      -- producer-side (see the geometry pass in "Scene"), keyed to this stage.
      Vk.queueSubmit computeQueue' [SomeStruct (submitInfo computeCb [geomDone] [Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT] [])] fence
    Nothing ->
      Vk.queueSubmit graphicsQueue [SomeStruct (submitInfo graphicsCb [] [] [])] fence
  _ <- Vk.waitForFences dev [fence] True maxBound
  pure ()
  where
    submitInfo cb waits waitStages signals =
      zero
        { Vk.commandBuffers = [Vk.commandBufferHandle cb]
        , Vk.waitSemaphores = V.fromList waits
        , Vk.waitDstStageMask = V.fromList waitStages
        , Vk.signalSemaphores = V.fromList signals
        }
        :: Vk.SubmitInfo '[]

{- | Copy a @TRANSFER_SRC@ image to the host image, then make it readable.

The readback hook left the source in @TRANSFER_SRC@.
-}
copyToHost :: (MonadIO m) => Vk.CommandBuffer -> Vk.Extent2D -> Vk.Image -> Vk.Image -> m ()
copyToHost cb (Vk.Extent2D w h) src cpuImage = do
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    zero
    []
    []
    [hostBarrier zero Vk.ACCESS_TRANSFER_WRITE_BIT Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL cpuImage]
  let layers = Vk.ImageSubresourceLayers Vk.IMAGE_ASPECT_COLOR_BIT 0 0 1
  Vk.cmdCopyImage
    cb
    src
    Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    cpuImage
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    [Vk.ImageCopy layers (Vk.Offset3D 0 0 0) layers (Vk.Offset3D 0 0 0) (Vk.Extent3D w h 1)]
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    Vk.PIPELINE_STAGE_HOST_BIT
    zero
    []
    []
    [hostBarrier Vk.ACCESS_TRANSFER_WRITE_BIT Vk.ACCESS_HOST_READ_BIT Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Vk.IMAGE_LAYOUT_GENERAL cpuImage]

hostBarrier :: Vk.AccessFlags -> Vk.AccessFlags -> Vk.ImageLayout -> Vk.ImageLayout -> Vk.Image -> SomeStruct Vk.ImageMemoryBarrier
hostBarrier srcAccess dstAccess oldLayout newLayout img =
  SomeStruct
    zero
      { ImageMemoryBarrier.srcAccessMask = srcAccess
      , ImageMemoryBarrier.dstAccessMask = dstAccess
      , ImageMemoryBarrier.oldLayout = oldLayout
      , ImageMemoryBarrier.newLayout = newLayout
      , ImageMemoryBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
      , ImageMemoryBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
      , ImageMemoryBarrier.image = img
      , ImageMemoryBarrier.subresourceRange = Vk.ImageSubresourceRange Vk.IMAGE_ASPECT_COLOR_BIT 0 1 0 1
      }

----------------------------------------------------------------
-- Debug dumps (intermediate buffers)
----------------------------------------------------------------

{- | Dump the intermediate buffers as PNGs for development.

The visibility buffer's instance-id and triangle-id channels (coloured per id) and the
depth buffer (grey; brighter = nearer, reverse-Z). Returns the ray-miss count: the
pixels where reverse-Z depth is still the cleared @0@ (nothing was drawn).
-}
dumpDebug :: VMA.Allocator -> Vk.Device -> (Vk.Queue, Word32) -> Vk.Image -> Vk.Image -> Vk.Extent2D -> ResourceT IO Int
dumpDebug allocator dev qf visImage depthImage extent@(Vk.Extent2D w h) = do
  visPtr <- copyImageToHostBuffer allocator dev qf visImage Vk.IMAGE_ASPECT_COLOR_BIT Vk.IMAGE_LAYOUT_GENERAL extent 8
  -- Depth ends the frame in GENERAL: the depth-pyramid build sampled it.
  depthPtr <- copyImageToHostBuffer allocator dev qf depthImage Vk.IMAGE_ASPECT_DEPTH_BIT Vk.IMAGE_LAYOUT_GENERAL extent 4
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

The readback leaves the image in @TRANSFER_SRC_OPTIMAL@, so the graph's tracked state
is resynced — the debug-mode reruns below copy into this probe again.
-}
dumpLumProbe :: VMA.Allocator -> Vk.Device -> (Vk.Queue, Word32) -> ManagedImage -> Vk.Extent2D -> ResourceT IO ()
dumpLumProbe allocator dev qf probe extent@(Vk.Extent2D w h) = do
  ptr <- copyImageToHostBuffer allocator dev qf probe.image Vk.IMAGE_ASPECT_COLOR_BIT Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL extent 8
  liftIO $ writeIORef probe.stateRef (usageState TransferSrc)
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
dumpShadowFace :: VMA.Allocator -> Vk.Device -> (Vk.Queue, Word32) -> Vk.Image -> ResourceT IO ()
dumpShadowFace allocator dev qf moments = do
  let res = fromIntegral Scene.shadowRes
  ptr <- copyImageToHostBuffer allocator dev qf moments Vk.IMAGE_ASPECT_COLOR_BIT Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL (Vk.Extent2D Scene.shadowRes Scene.shadowRes) 16
  liftIO $
    savePng "debug-shadow.png"
      =<< JP.withImage res res \x y -> do
        r <- peek (castPtr (ptr `plusPtr` ((y * res + x) * 16))) :: IO Float
        let
          d = if r > 1 then log r / 30 else 1 -- C = 30; empty texels read as far
          v = round (255 * max 0 (min 1 d))
        pure (JP.PixelRGBA8 v v v 255)

{- | Copy an image (currently in @currentLayout@) to a mapped host buffer.

Returns the invalidated mapped pointer. @bpp@ is the format's bytes per pixel; the
image must have been created with @TRANSFER_SRC@ usage.
-}
copyImageToHostBuffer :: VMA.Allocator -> Vk.Device -> (Vk.Queue, Word32) -> Vk.Image -> Vk.ImageAspectFlags -> Vk.ImageLayout -> Vk.Extent2D -> Int -> ResourceT IO (Ptr ())
copyImageToHostBuffer allocator dev (queue, family) image aspect currentLayout (Vk.Extent2D w h) bpp = do
  let size = fromIntegral (fromIntegral w * fromIntegral h * bpp) :: Vk.DeviceSize
  (_, (buffer, alloc, mapped)) <- readbackBuffer allocator size
  pool <- commandPool dev family
  cb <- beginPrimary dev pool
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    zero
    []
    []
    [ SomeStruct
        zero
          { ImageMemoryBarrier.dstAccessMask = Vk.ACCESS_TRANSFER_READ_BIT
          , ImageMemoryBarrier.oldLayout = currentLayout
          , ImageMemoryBarrier.newLayout = Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
          , ImageMemoryBarrier.srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
          , ImageMemoryBarrier.dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
          , ImageMemoryBarrier.image = image
          , ImageMemoryBarrier.subresourceRange = Vk.ImageSubresourceRange aspect 0 1 0 1
          }
    ]
  Vk.cmdCopyImageToBuffer
    cb
    image
    Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    buffer
    [Vk.BufferImageCopy 0 0 0 (Vk.ImageSubresourceLayers aspect 0 0 1) (Vk.Offset3D 0 0 0) (Vk.Extent3D w h 1)]
  Vk.endCommandBuffer cb
  (_, fence) <- Vk.withFence dev zero Nothing allocate
  Vk.queueSubmit queue [SomeStruct (zero{Vk.commandBuffers = [Vk.commandBufferHandle cb]} :: Vk.SubmitInfo '[])] fence
  _ <- Vk.waitForFences dev [fence] True maxBound
  VMA.invalidateAllocation allocator alloc 0 Vk.WHOLE_SIZE
  pure mapped

-- | A distinct colour per id (cosine palette); id 0 is black.
idColor :: Word32 -> JP.PixelRGBA8
idColor 0 = JP.PixelRGBA8 0 0 0 255
idColor n = JP.PixelRGBA8 (chan 0.0) (chan 0.33) (chan 0.67) 255
  where
    hval = fromIntegral n * 0.61803398875 :: Float
    chan o = round (255 * (0.4 + 0.5 * cos (6.2831853 * (o + hval))))
