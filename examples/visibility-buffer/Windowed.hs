{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Windowed driver (GLFW).

Presents the shaded scene, re-rendering every frame at the native window resolution.
Arrow keys orbit the camera and @-@/@+@ dolly ('Camera.update'); @.@ dumps the view;
the top-row digits retarget the presentation (@0@ beauty, @1@–@6@ the debug
channels, as @--debug-mode@); @g@ dumps the live frame's graph — as culled by the
current view — to @visibility-buffer-live.dot@.

Runs the /same/ 'Rendering.Passes.addScenePasses' graph as the headless driver, across the
same queue topology: the shade/post passes go to 'Rendering.Passes.computeQueue' on hardware with
an async family, and the graph collapses to one graphics submit otherwise. The
blit (colour → swapchain, finalized to PRESENT_SRC) and present always stay on
graphics, so the WSI semaphores ride one queue; the framegraph derives the
cross-queue handshake for the blit's read of the compute-written target. The
graph is always built whole; the blit reads one 'Rendering.Passes.PassOutputs' handle —
picked by the swapchain format ('srgbEncoding') and the selected view — and the
graph culls whatever the presentation doesn't demand (a debug view drops the
whole bloom/tonemap machinery, luminance metering included).

The render targets are per-frame-in-flight ('SceneSlot', the frame's 'rrData'),
one set each so the two in-flight frames never share a target across the queue
split; a slot rebuilds its targets when the swapchain generation moves
('reconcileScene').
-}
module Windowed
  ( main
  ) where

import Control.Exception (handle, onException)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, runResourceT)
import qualified Control.Monad.Trans.Resource as ResourceT
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import Data.Word (Word32, Word64)
import qualified Fragr as FG
import qualified Fragr.Snapshot as FS
import qualified Fragr.Snapshot.Dot as FSDot
import qualified Fragr.Snapshot.JSON as FSJson
import GHC.Clock (getMonotonicTime)
import qualified Graphics.UI.GLFW as GLFW
import Say (sayErrString)
import UnliftIO.Exception (displayException)
import qualified Vulkan.Core10 as Vk
import Vulkan.Exception (VulkanException (..))
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.Frame (Frame (..), InitRecycledResources, acquireFrameImage, presentFrameImage)
import Vulkan.Utils.FrameGraph.Driver (QueueSlot (..), frameSubmitConfig, submitGraphQueued)
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), SliceRegistry, Usage (..), newSliceRegistry)
import Vulkan.Utils.FrameGraph.Recorder (recordingCommandBuffer)
import Vulkan.Utils.FrameGraph.Swapchain (forgetSwapchainImages, importSwapchain, newSwapchainImages, presentSwapchain)
import qualified Vulkan.Utils.Init.GLFW.Window as Window
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.Swapchain (Swapchain (..), SwapchainConfig (..), defaultSwapchainConfig, srgbEncoding)
import Vulkan.Utils.VulkanContext (RecycledResources (..), VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, noOnFrame, runWindowLoop)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA
import WindowedBoot (WindowedConfig (..), withWindowedVk)

import Blit (blitImage)
import qualified Exposure
import Options (Options)
import qualified Options
import qualified Rendering.Passes as Passes
import qualified Rendering.Pipelines as Pipelines
import qualified Rendering.Static as Static
import Rendering.Targets (Scene)
import qualified Rendering.Targets as Targets
import Requirements (deviceRequirements)
import qualified Scene.Camera as Camera

main :: Options -> IO ()
main opts = prettyError . runResourceT $ do
  Window.withGLFW
  window <- Window.createWindow "Haskell Vulkan 👀 Visibility Buffer 🖼 Frame Graph" opts.width opts.height
  Window.showWindow window

  (vc, vma, initialSC) <- withWindowedVk windowConfig (Window.glfwAdapter window)
  let
    dev = vcDevice vc
    QueueFamilyIndex graphicsFamily = fst (qGraphics (vcQueues vc))
    QueueFamilyIndex computeFamily = fst (qCompute (vcQueues vc))
    genQueue = (snd (qGraphics (vcQueues vc)), graphicsFamily)
    -- Async compute when the compute family is distinct: the shade/post passes
    -- then run on 'computeQueue' and the cross-queue resources are CONCURRENT
    -- across the pair. 'Nothing' collapses the graph onto the graphics queue.
    sharedFamilies
      | graphicsFamily == computeFamily = Nothing
      | otherwise = Just (graphicsFamily, computeFamily)
  sayErrString $ case sharedFamilies of
    Nothing -> "single queue, family " <> show graphicsFamily
    Just (gf, cf) -> "async compute, graphics family " <> show gf <> " + compute family " <> show cf

  pls <- Pipelines.allocatePipelines dev
  registry <- newSliceRegistry
  -- The geometry, tables and baked shadows are extent-independent: build them once
  -- (a one-shot generation submit) so resize only rebuilds the render targets.
  sceneStatic <- Static.allocateStatic vma dev registry genQueue pls sharedFamilies
  -- Animation clock, seeded once so resize (which rebuilds the targets) doesn't reset it.
  startTime <- liftIO getMonotonicTime
  -- Swapchain generation, bumped per (re)build so the per-frame slots rebuild their targets.
  genRef <- liftIO (newIORef (0 :: Word64))
  -- Interactive state, program-lifetime so resize keeps it.
  controls <- liftIO do
    camera <- newIORef opts.orbit
    mode <- newIORef opts.debugMode
    dump <- newIORef False
    prevTime <- newIORef startTime
    pure Controls{camera, mode, dump, prevTime}
  -- Discrete keys (a callback rather than the poll loop): '.' dumps the current
  -- view; 'g' requests a graph dump off the next frame; the top-row digits
  -- retarget the presentation ('Options.debugViews'). Keypad digits are
  -- Key'Pad*, deliberately unbound.
  liftIO $ GLFW.setKeyCallback window $ Just \_ key _ state _ ->
    when (state == GLFW.KeyState'Pressed) do
      when (key == GLFW.Key'Period) (Camera.dump =<< readIORef controls.camera)
      when (key == GLFW.Key'G) (writeIORef controls.dump True)
      forM_ (lookup key (zip [GLFW.Key'0 ..] (map fst Options.debugViews))) \mode -> do
        prev <- readIORef controls.mode
        writeIORef controls.mode mode
        unless (prev == mode) $ sayErrString ("presenting " <> viewName mode)

  onFrame <- liftIO (exerciseResize opts window)
  runWindowLoop
    vc
    initialSC
    (Window.drawableSize window)
    (Window.shouldQuit window)
    WindowLoop
      { wlMkState = allocateBindings vma dev registry genRef
      , wlMkRecycled = allocateSceneSlot
      , wlRender = renderScene opts vc vma registry pls sceneStatic sharedFamilies window controls startTime
      , wlOnFrame = onFrame
      , wlOnExit = noOnExit
      }

{- | The @--exercise-resize@ soak: a per-frame hook stepping through the
resizes, then quitting.

Every step lands on a different extent, so each retires the swapchain and
rebuilds the per-swapchain state — driving the path a window manager
otherwise would, without a hand on the window.
-}
exerciseResize :: Options -> GLFW.Window -> IO (Word64 -> Word64 -> ResourceT IO ())
exerciseResize opts window = case opts.exerciseResize of
  0 -> pure noOnFrame
  n -> do
    counter <- newIORef (0 :: Int)
    pure \_ _ -> liftIO do
      k <- atomicModifyIORef' counter \c -> (c + 1, c + 1)
      let (step, r) = k `divMod` settleFrames
      when (r == 0) $
        if step <= n
          then uncurry (GLFW.setWindowSize window) (extents !! ((step - 1) `mod` length extents))
          else GLFW.setWindowShouldClose window True
  where
    -- Consecutive steps differ (cyclically too), so none is a no-op resize.
    extents :: [(Int, Int)]
    extents = [(1800, 1100), (640, 480), (2200, 1300), (1024, 768)]
    settleFrames = 90

windowConfig :: WindowedConfig
windowConfig =
  WindowedConfig
    { appName = "Haskell Vulkan visibility buffer"
    , instanceReqs = []
    , deviceReqs = Dynamic.dynamicRenderingRequirements <> deviceRequirements
    , vmaFlags = zero
    , swapchainConfig =
        defaultSwapchainConfig
          { scRequiredUsageFlags = [Vk.IMAGE_USAGE_TRANSFER_DST_BIT, Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT]
          , scRequiredFormatFeatures = [Vk.FORMAT_FEATURE_BLIT_DST_BIT]
          , -- sRGB preferred so the linear @tone@ target blits straight to the
            -- swapchain; a surface without one (MoltenVK leads with UNORM) gets
            -- the gamma pass's @display@ — 'renderScene' checks the pick either way.
            scSurfaceFormatPreferences = [srgbEncoding]
          }
    }

-- | Console name of a presented view (the digit key / @--debug-mode@ value).
viewName :: Word32 -> String
viewName n = maybe (show n) (\name -> show n <> " " <> name) (lookup n Options.debugViews)

prettyError :: IO () -> IO ()
prettyError = handle (\e@(VulkanException _) -> sayErrString (displayException e))

-- | Interactive state shared between the key callback and the render loop.
data Controls = Controls
  { camera :: IORef Camera.Orbit
  , mode :: IORef Word32
  -- ^ The presented view (digit keys, seeded from @--debug-mode@).
  , dump :: IORef Bool
  -- ^ One-shot graph-dump request (@g@).
  , prevTime :: IORef Double
  -- ^ Previous frame's clock, for the per-frame delta.
  }

----------------------------------------------------------------
-- Per-swapchain bindings
----------------------------------------------------------------

data Bindings = Bindings
  { swapImages :: Vector ManagedImage
  -- ^ One layout-tracked wrapper per swapchain image.
  , exposure :: IORef Float
  -- ^ Auto-exposure, smoothed from the luminance readback across frames.
  , generation :: Word64
  -- ^ Bumped per swapchain (re)build; a slot rebuilds its targets on a change.
  , sharedHiZ :: Targets.SharedHiZ
  {- ^ The one depth pyramid both frames' targets bind: rebuilt and late-culled
  against in-frame, 1-frame history for the early cull's trim.
  -}
  }

{- | Per-swapchain bindings: swapchain wrappers, exposure EMA, and the shared hi-Z.

The render targets are /not/ here — they are per-frame-in-flight ('SceneSlot'),
one set each, so a multi-queue split can't race them across frames. This holds
what the ping-pong must /not/ double-buffer: the swapchain wrappers (acquire hands
out an idle image), the generation the slots rebuild against, and the hi-Z pyramid
(shared so the cull reads last frame's, not two frames back — 'Rendering.Targets.SharedHiZ').
The internal state (via the returned key) frees these when the swapchain is
recreated; the static geometry/shadows are untouched.
-}
allocateBindings :: VMA.Allocator -> Vk.Device -> SliceRegistry -> IORef Word64 -> Swapchain -> ResourceT IO (Bindings, ReleaseKey)
allocateBindings allocator dev registry genRef sc = do
  sayErrString $ "swapchain " <> show sc.sExtent.width <> "x" <> show sc.sExtent.height <> " " <> show sc.sFormat <> if srgbEncoding sc.sFormat then " (blit encodes)" else " (gamma pass encodes)"
  exposure <- liftIO (newIORef 1.0)
  generation <- liftIO $ atomicModifyIORef' genRef \g -> (g + 1, g + 1)
  -- Registered before running: a mid-build failure then unwinds through
  -- ResourceT's own cleanup, so partial allocations can't leak past this scope
  -- (surfacing as a VMA leak abort that masks the actual error).
  (key, st) <- ResourceT.allocate ResourceT.createInternalState ResourceT.closeInternalState
  bindings <-
    liftIO $
      ResourceT.runInternalState
        do
          -- Recreation hands the retired swapchain's image handles back to the
          -- driver: purge their wrappers with this state, so the next
          -- generation's images can re-wrap recycled handles.
          _ <- ResourceT.register (forgetSwapchainImages registry sc)
          swapImages <- newSwapchainImages registry sc
          sharedHiZ <- Targets.allocateSharedHiZ allocator dev registry sc.sExtent
          pure Bindings{swapImages, exposure, generation, sharedHiZ}
        st
  pure (bindings, key)

----------------------------------------------------------------
-- Per-frame-in-flight scene targets (rrData)
----------------------------------------------------------------

{- | One frame-in-flight's own copy of the extent-dependent scene.

The render targets and the descriptor sets naming them are touched by both
frames in flight — this frame records while the other executes — so each slot
carries its own set as the frame's 'rrData'. Built lazily on first use and
rebuilt when the swapchain generation moves ('reconcileScene').
-}
newtype SceneSlot = SceneSlot
  { current :: IORef (Maybe SlotState)
  }

data SlotState = SlotState
  { generation :: Word64
  , scene :: Scene
  , owned :: ResourceT.InternalState
  -- ^ Holds this slot's targets; closed to free them (on reconcile or exit).
  }

{- | Allocate an empty slot, freeing whatever it holds at program exit.

Runs once per frame-in-flight at boot ('wlMkRecycled'); the swapchain extent is
not known here, so the scene is built on the slot's first 'reconcileScene'.
-}
allocateSceneSlot :: InitRecycledResources (ResourceT IO) SceneSlot
allocateSceneSlot _vc _ix _pools = do
  current <- liftIO (newIORef Nothing)
  _ <-
    ResourceT.register $
      readIORef current >>= mapM_ (ResourceT.closeInternalState . (.owned))
  pure SceneSlot{current}

{- | This slot's scene at the current generation, rebuilding a stale one.

A generation match is the steady-state path: one 'IORef' read. A miss (first use,
or a resize bumped the generation) frees the old targets — safe with no device
wait, since the ring waited this slot's previous frame before handing it back —
and builds a fresh set at the current extent.
-}
reconcileScene
  :: VMA.Allocator
  -> Vk.Device
  -> SliceRegistry
  -> Pipelines.ScenePipelines
  -> Static.SceneStatic
  -> Targets.SharedHiZ
  -> Maybe (Word32, Word32)
  -> Word64
  -> Vk.Extent2D
  -> SceneSlot
  -> IO Scene
reconcileScene allocator dev registry pls static sharedHiZ sharedFamilies generation extent slot =
  readIORef slot.current >>= \case
    Just s | s.generation == generation -> pure s.scene
    prev -> do
      mapM_ (ResourceT.closeInternalState . (.owned)) prev
      -- Drop the stale reference before re-wrapping: the rebuilt targets may
      -- recycle the just-destroyed image handles, and the slice registry only
      -- forgets the old wrappers once nothing reachable holds them.
      writeIORef slot.current Nothing
      owned <- ResourceT.createInternalState
      -- 'owned' is parked in the slot rather than registered into an enclosing
      -- ResourceT scope, so a mid-build failure needs its own close here or it
      -- leaks past every scope.
      scene <-
        ResourceT.runInternalState
          (Targets.allocateTargets allocator dev registry pls static extent sharedHiZ sharedFamilies False)
          owned
          `onException` ResourceT.closeInternalState owned
      writeIORef slot.current (Just SlotState{generation, scene, owned})
      pure scene

----------------------------------------------------------------
-- Per-frame rendering
----------------------------------------------------------------

renderScene
  :: Options
  -> VulkanContext SceneSlot
  -> VMA.Allocator
  -> SliceRegistry
  -> Pipelines.ScenePipelines
  -> Static.SceneStatic
  -> Maybe (Word32, Word32)
  -> GLFW.Window
  -> Controls
  -> Double
  -> Bindings
  -> Frame SceneSlot
  -> ResourceT IO ()
renderScene opts vc vma registry pls sceneStatic sharedFamilies window controls startTime bindings f = do
  (acquireResult, imageIndex) <- acquireFrameImage vc f
  now <- liftIO getMonotonicTime
  let t = realToFrac (now - startTime) :: Float
  dt <- liftIO $ do
    prev <- readIORef controls.prevTime
    writeIORef controls.prevTime now
    pure (realToFrac (now - prev) :: Float)
  -- Advance the orbit camera from held keys over this frame's delta.
  orbit <- liftIO (Camera.update window dt controls.camera)
  mode <- liftIO (readIORef controls.mode)
  let
    eye = Camera.eye orbit
    sc = fSwapchain f
    extent = sc.sExtent

  -- This frame-in-flight's own targets, rebuilt only when the swapchain
  -- generation moved (resize); otherwise the cached set.
  scene <- liftIO $ reconcileScene vma (vcDevice vc) registry pls sceneStatic bindings.sharedHiZ sharedFamilies bindings.generation extent f.fRecycled.rrData

  -- Auto-exposure over the previous frame's mean luminance, pre-written into
  -- the metering buffer (the readback lags a frame; the adaptation hides it).
  -- NOT an in-graph host meter: a deferred host pass would gate renderFinished
  -- behind a host signal that is not yet submitted when we present, which WSI
  -- forbids (VUID-vkQueuePresentKHR-pWaitSemaphores-03268) — and running the
  -- tail inline would stall the loop mid-frame. Held on debug views: the
  -- luminance pass isn't added then ("Rendering.Passes"), so the readback would be stale.
  liftIO $ do
    e <- readIORef bindings.exposure
    when (mode == 0) do
      prevLum <- Targets.readLuminance scene
      let e' = Exposure.adapt opts.meter dt e (Exposure.target opts.meter prevLum)
      writeIORef bindings.exposure e'
      Targets.setExposure scene e'

  graph <- FG.newFrameGraph
  -- The shade/post passes run on 'Rendering.Passes.computeQueue' under an async
  -- topology, else on the default (graphics) queue; the blit and present stay
  -- on graphics either way.
  outs <- Passes.addScenePasses graph pls opts.tweaks scene (Passes.computeQueueId sharedFamilies) extent eye t Nothing mode
  (swapchainH, swapManaged) <- importSwapchain graph bindings.swapImages imageIndex

  -- An sRGB swapchain encodes on the blit, so debug views present the raw shade
  -- output and beauty the linear @tone@ target; a UNORM pick blits the gamma
  -- pass's @display@ target verbatim for every view. Reading only one output
  -- lets the graph cull the rest ("Rendering.Passes" — a debug view drops the
  -- whole bloom/tonemap machinery either way).
  let (blitSrc, blitSrcImage)
        | not (srgbEncoding sc.sFormat) = (outs.displayOut, scene.targets.display.image)
        | mode /= 0 = (outs.colorOut, scene.targets.colorHDR.image)
        | otherwise = (outs.toneOut, scene.targets.tone.image)

  blitted <-
    FG.addPass graph "blit" (blitSetup blitSrc swapchainH) \_ -> do
      cb <- recordingCommandBuffer
      blitImage extent blitSrcImage swapManaged.image cb
  presentSwapchain graph blitted

  -- The family partition mirrors 'queueTable' below, so the schedule already
  -- dedups ownership hand-offs per family (and rejects two-family releases).
  FG.compileWith (Passes.familyPartition sharedFamilies) graph

  -- A requested dump captures this frame's compiled graph,
  -- including which passes the current view's demand culled.
  liftIO $ dumpGraph controls.dump mode graph

  let
    dev = vcDevice vc
    graphicsSlot =
      QueueSlot
        { queue = snd (qGraphics (vcQueues vc))
        , family = let QueueFamilyIndex fam = fst (qGraphics (vcQueues vc)) in fam
        , pool = qGraphics (rrCommandPools f.fRecycled)
        }
    -- Route 'Rendering.Passes.computeQueue' to the async family (its own pool,
    -- recycled with the frame); every other id — geometry, blit, present —
    -- lands on graphics.
    queueTable q = case sharedFamilies of
      Just (_, computeFamily)
        | q == Passes.computeQueue ->
            QueueSlot
              { queue = snd (qCompute (vcQueues vc))
              , family = computeFamily
              , pool = qCompute (rrCommandPools f.fRecycled)
              }
      _ -> graphicsSlot
  _ <- submitGraphQueued graph $ frameSubmitConfig dev f imageIndex queueTable
  presentFrameImage vc f acquireResult imageIndex
  where
    blitSetup srcH swapchainH = do
      FG.readWith srcH TransferSrc
      FG.writeWith swapchainH TransferDst

dumpGraph :: IORef Bool -> Word32 -> FG.FrameGraph a b -> IO ()
dumpGraph flag mode graph = do
  wantDump <- readIORef flag
  when wantDump do
    writeIORef flag False
    snapshot <- FS.snapshot graph
    sayErrString $ "dumping dot graph to " <> show pathDot <> ", presenting " <> viewName mode
    TIO.writeFile pathDot $ FSDot.render FSDot.defaultOptions snapshot
    sayErrString $ "dumping viewer json to " <> show pathJson
    TIO.writeFile pathJson $ FSJson.render (FSJson.fromSnapshot snapshot)
  where
    pathDot = "visibility-buffer-live.dot"
    pathJson = "visibility-buffer-live.json"
