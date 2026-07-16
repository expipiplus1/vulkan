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

Runs the /same/ 'Scene.addScenePasses' graph as the headless driver, across the
same queue topology: the shade/post passes go to 'computeQueue' on hardware with
an async family, and the graph collapses to one graphics submit otherwise. The
blit (colour → swapchain, finalized to PRESENT_SRC) and present always stay on
graphics, so the WSI semaphores ride one queue; the framegraph derives the
cross-queue handshake for the blit's read of the compute-written target. The
graph is always built whole; the blit reads one 'Scene.PassOutputs' handle —
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

import Control.Exception (handle)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, runResourceT)
import qualified Control.Monad.Trans.Resource as ResourceT
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import Data.Word (Word32, Word64)
import qualified Fragr as FG
import qualified Fragr.Dot as Dot
import GHC.Clock (getMonotonicTime)
import qualified Graphics.UI.GLFW as GLFW
import Say (sayErrString)
import UnliftIO.Exception (displayException)
import qualified Vulkan.Core10 as Vk
import Vulkan.Exception (VulkanException (..))
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.Frame (Frame (..), InitRecycledResources, acquireFrameImage, presentFrameImage)
import Vulkan.Utils.FrameGraph.Driver (QueueSlot (..), frameSubmitConfig, submitGraphQueued)
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), Usage (..))
import Vulkan.Utils.FrameGraph.Recorder (recordingCommandBuffer)
import Vulkan.Utils.FrameGraph.Swapchain (importSwapchain, newSwapchainImages, presentSwapchain)
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
import Requirements (deviceRequirements)
import Scene (Scene)
import qualified Scene
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

  pls <- Scene.allocatePipelines dev
  -- The geometry, tables and baked shadows are extent-independent: build them once
  -- (a one-shot generation submit) so resize only rebuilds the render targets.
  sceneStatic <- Scene.allocateStatic vma dev genQueue pls sharedFamilies
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

  runWindowLoop
    vc
    initialSC
    (Window.drawableSize window)
    (Window.shouldQuit window)
    WindowLoop
      { wlMkState = allocateBindings vma dev genRef
      , wlMkRecycled = allocateSceneSlot
      , wlRender = renderScene opts vc vma pls sceneStatic sharedFamilies window controls startTime
      , wlOnFrame = noOnFrame
      , wlOnExit = noOnExit
      }

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

-- | The queue the shade/post passes run on under an async topology.
computeQueue :: FG.QueueId
computeQueue = FG.QueueId 1

{- | The queue id for the shade/post passes: 'computeQueue' when async, else the
default (graphics) queue, which collapses the graph to a single submit.
-}
computeQueueId :: Maybe a -> FG.QueueId
computeQueueId = maybe FG.defaultQueue (const computeQueue)

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
  , sharedHiZ :: Scene.SharedHiZ
  -- ^ The one depth pyramid both frames' targets bind, for 1-frame occlusion history.
  }

{- | Per-swapchain bindings: swapchain wrappers, exposure EMA, and the shared hi-Z.

The render targets are /not/ here — they are per-frame-in-flight ('SceneSlot'),
one set each, so a multi-queue split can't race them across frames. This holds
what the ping-pong must /not/ double-buffer: the swapchain wrappers (acquire hands
out an idle image), the generation the slots rebuild against, and the hi-Z pyramid
(shared so the cull reads last frame's, not two frames back — 'Scene.SharedHiZ').
The internal state (via the returned key) frees these when the swapchain is
recreated; the static geometry/shadows are untouched.
-}
allocateBindings :: VMA.Allocator -> Vk.Device -> IORef Word64 -> Swapchain -> ResourceT IO (Bindings, ReleaseKey)
allocateBindings allocator dev genRef sc = do
  sayErrString $ "swapchain " <> show sc.sFormat <> if srgbEncoding sc.sFormat then " (blit encodes)" else " (gamma pass encodes)"
  exposure <- liftIO (newIORef 1.0)
  generation <- liftIO $ atomicModifyIORef' genRef \g -> (g + 1, g + 1)
  st <- ResourceT.createInternalState
  bindings <-
    liftIO $ flip ResourceT.runInternalState st do
      swapImages <- newSwapchainImages sc
      sharedHiZ <- Scene.allocateSharedHiZ allocator dev sc.sExtent
      pure Bindings{swapImages, exposure, generation, sharedHiZ}
  key <- ResourceT.register $ ResourceT.closeInternalState st
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
  -> Scene.ScenePipelines
  -> Scene.SceneStatic
  -> Scene.SharedHiZ
  -> Maybe (Word32, Word32)
  -> Word64
  -> Vk.Extent2D
  -> SceneSlot
  -> IO Scene
reconcileScene allocator dev pls static sharedHiZ sharedFamilies generation extent slot =
  readIORef slot.current >>= \case
    Just s | s.generation == generation -> pure s.scene
    prev -> do
      mapM_ (ResourceT.closeInternalState . (.owned)) prev
      owned <- ResourceT.createInternalState
      scene <-
        flip ResourceT.runInternalState owned $
          Scene.allocateTargets allocator dev pls static extent sharedHiZ sharedFamilies False
      writeIORef slot.current (Just SlotState{generation, scene, owned})
      pure scene

----------------------------------------------------------------
-- Per-frame rendering
----------------------------------------------------------------

renderScene
  :: Options
  -> VulkanContext SceneSlot
  -> VMA.Allocator
  -> Scene.ScenePipelines
  -> Scene.SceneStatic
  -> Maybe (Word32, Word32)
  -> GLFW.Window
  -> Controls
  -> Double
  -> Bindings
  -> Frame SceneSlot
  -> ResourceT IO ()
renderScene opts vc vma pls sceneStatic sharedFamilies window controls startTime bindings f = do
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
  scene <- liftIO $ reconcileScene vma (vcDevice vc) pls sceneStatic bindings.sharedHiZ sharedFamilies bindings.generation extent f.fRecycled.rrData

  -- Auto-exposure over the previous frame's mean luminance, pre-written into
  -- the metering buffer (the readback lags a frame; the adaptation hides it).
  -- NOT an in-graph host meter: a deferred host pass would gate renderFinished
  -- behind a host signal that is not yet submitted when we present, which WSI
  -- forbids (VUID-vkQueuePresentKHR-pWaitSemaphores-03268) — and running the
  -- tail inline would stall the loop mid-frame. Held on debug views: the
  -- luminance pass isn't added then ("Scene"), so the readback would be stale.
  liftIO $ do
    e <- readIORef bindings.exposure
    when (mode == 0) do
      prevLum <- Scene.readLuminance scene
      let e' = Exposure.adapt opts.meter dt e (Exposure.target opts.meter prevLum)
      writeIORef bindings.exposure e'
      Scene.setExposure scene e'

  graph <- FG.newFrameGraph
  -- The shade/post passes run on 'computeQueue' under an async topology, else on
  -- the default (graphics) queue; the blit and present stay on graphics either
  -- way. The orbs' table refresh is a scene pass now ("Scene").
  outs <- Scene.addScenePasses graph pls opts.tweaks scene (computeQueueId sharedFamilies) extent eye t Nothing mode
  (swapchainH, swapManaged) <- importSwapchain graph bindings.swapImages imageIndex

  -- An sRGB swapchain encodes on the blit, so debug views present the raw shade
  -- output and beauty the linear @tone@ target; a UNORM pick blits the gamma
  -- pass's @display@ target verbatim for every view. Reading only one output
  -- lets the graph cull the rest ("Scene" — a debug view drops the whole
  -- bloom/tonemap machinery either way).
  let (blitSrc, blitSrcImage)
        | not (srgbEncoding sc.sFormat) = (outs.displayOut, scene.targets.display.image)
        | mode /= 0 = (outs.colorOut, scene.targets.colorHDR.image)
        | otherwise = (outs.toneOut, scene.targets.tone.image)

  blitted <-
    FG.addPass graph "blit" (blitSetup blitSrc swapchainH) \_ -> do
      cb <- recordingCommandBuffer
      blitImage extent blitSrcImage swapManaged.image cb
  presentSwapchain graph blitted

  FG.compile graph

  -- A requested 'g' dump captures this frame's compiled graph — including
  -- which passes the current view's demand culled.
  wantDump <- liftIO (readIORef controls.dump)
  when wantDump $ liftIO do
    writeIORef controls.dump False
    TIO.writeFile "visibility-buffer-live.dot" =<< Dot.dump graph
    sayErrString ("graph dumped to visibility-buffer-live.dot, presenting " <> viewName mode)

  let
    dev = vcDevice vc
    graphicsSlot =
      QueueSlot
        { queue = snd (qGraphics (vcQueues vc))
        , family = let QueueFamilyIndex fam = fst (qGraphics (vcQueues vc)) in fam
        , pool = qGraphics (rrCommandPools f.fRecycled)
        }
    -- Route 'computeQueue' to the async family (its own pool, recycled with the
    -- frame); every other id — geometry, blit, present — lands on graphics.
    queueTable q = case sharedFamilies of
      Just (_, computeFamily)
        | q == computeQueue ->
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
