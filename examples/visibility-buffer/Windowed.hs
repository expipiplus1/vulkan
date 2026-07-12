{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Windowed driver (GLFW).

Presents the shaded scene, re-rendering every frame at the native window resolution.
Arrow keys orbit the camera and @-@/@+@ dolly ('Camera.update'); @.@ dumps the view;
the top-row digits retarget the presentation (@0@ beauty, @1@–@6@ the debug
channels, as @--debug-mode@); @g@ dumps the live frame's graph — as culled by the
current view — to @visibility-buffer-live.dot@.

Runs the /same/ 'Scene.addScenePasses' graph as the headless driver, but keeps it
single-queue — @cbFor@ maps every pass (geometry, shade) to the graphics command
buffer, so 'FG.executeQueued' degenerates to one buffer and one submit — then
appends a @blit@ (colour → swapchain) finalized to PRESENT_SRC. The graph is
always built whole; the blit reads one 'Scene.PassOutputs' handle — picked by
the swapchain format ('srgbEncoding') and the selected view — and the graph
culls whatever the presentation doesn't demand (a debug view drops the whole
bloom/tonemap machinery, luminance metering included). The per-swapchain scene
targets are recreated on resize by 'runWindowLoop'.
-}
module Windowed
  ( main
  ) where

import Control.Exception (handle)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, closeInternalState, createInternalState, register, runInternalState, runResourceT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import Data.Word (Word32)
import qualified Fragr as FG
import qualified Fragr.Dot as Dot
import GHC.Clock (getMonotonicTime)
import qualified Graphics.UI.GLFW as GLFW
import Say (sayErrString)
import UnliftIO.Exception (displayException)
import qualified Vulkan.Core10 as Vk
import Vulkan.Exception (VulkanException (..))
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage)
import Vulkan.Utils.FrameGraph.Driver (QueueSlot (..), frameSubmitConfig, submitGraphQueued)
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), Usage (..))
import Vulkan.Utils.FrameGraph.Recorder (recordingCommandBuffer)
import Vulkan.Utils.FrameGraph.Swapchain (importSwapchain, newSwapchainImages, presentSwapchain)
import qualified Vulkan.Utils.Init.GLFW.Window as Window
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.Swapchain (Swapchain (..), SwapchainConfig (..), defaultSwapchainConfig, srgbEncoding)
import Vulkan.Utils.VulkanContext (RecycledResources (..), VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, noOnFrame, noRecycledResources, runWindowLoop)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA
import WindowedBoot (WindowedConfig (..), withWindowedVk)

import Blit (blitImage)
import qualified Exposure
import Options (Options)
import qualified Options
import Requirements (deviceRequirements)
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
    genQueue = (snd (qGraphics (vcQueues vc)), graphicsFamily)

  pls <- Scene.allocatePipelines dev
  -- The geometry, tables and baked shadows are extent-independent: build them once
  -- (a one-shot generation submit) so resize only rebuilds the render targets.
  sceneStatic <- Scene.allocateStatic vma dev genQueue pls Nothing
  -- Animation clock, seeded once so resize (which rebuilds the targets) doesn't reset it.
  startTime <- liftIO getMonotonicTime
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
      { wlMkState = allocateBindings vma dev pls sceneStatic
      , wlMkRecycled = noRecycledResources
      , wlRender = renderScene opts vc pls window controls startTime
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
  { scene :: Scene.Scene
  -- ^ Scene targets at the current swapchain extent (native resolution).
  , swapImages :: Vector ManagedImage
  -- ^ One layout-tracked wrapper per swapchain image.
  , exposure :: IORef Float
  -- ^ Auto-exposure, smoothed from the luminance readback across frames.
  }

{- | Build the per-swapchain render targets over the shared 'Scene.SceneStatic'.

Registered in an internal resource state (via the returned key) so recreating the
swapchain frees the old extent's targets instead of leaking them; the static
geometry/shadows are untouched.
-}
allocateBindings
  :: VMA.Allocator
  -> Vk.Device
  -> Scene.ScenePipelines
  -> Scene.SceneStatic
  -> Swapchain
  -> ResourceT IO (Bindings, ReleaseKey)
allocateBindings allocator dev pls sceneStatic sc = do
  sayErrString $ "swapchain " <> show sc.sFormat <> if srgbEncoding sc.sFormat then " (blit encodes)" else " (gamma pass encodes)"
  exposure <- liftIO (newIORef 1.0)
  st <- createInternalState
  bindings <-
    liftIO . flip runInternalState st $ do
      scene <- Scene.allocateTargets allocator dev pls sceneStatic sc.sExtent Nothing False
      swapImages <- newSwapchainImages sc
      pure Bindings{scene, swapImages, exposure}
  key <- register (closeInternalState st)
  pure (bindings, key)

----------------------------------------------------------------
-- Per-frame rendering
----------------------------------------------------------------

renderScene
  :: Options
  -> VulkanContext rr
  -> Scene.ScenePipelines
  -> GLFW.Window
  -> Controls
  -> Double
  -> Bindings
  -> Frame rr
  -> ResourceT IO ()
renderScene opts vc pls window controls startTime bindings f = do
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
      prevLum <- Scene.readLuminance bindings.scene
      let e' = Exposure.adapt opts.meter dt e (Exposure.target opts.meter prevLum)
      writeIORef bindings.exposure e'
      Scene.setExposure bindings.scene e'

  graph <- FG.newFrameGraph
  -- Single-queue: the compute passes stay on the default (graphics) queue.
  -- The orbs' table refresh is a scene pass now ("Scene").
  outs <- Scene.addScenePasses graph pls opts.tweaks bindings.scene FG.defaultQueue extent eye t Nothing mode
  (swapchainH, swapManaged) <- importSwapchain graph bindings.swapImages imageIndex

  -- An sRGB swapchain encodes on the blit, so debug views present the raw shade
  -- output and beauty the linear @tone@ target; a UNORM pick blits the gamma
  -- pass's @display@ target verbatim for every view. Reading only one output
  -- lets the graph cull the rest ("Scene" — a debug view drops the whole
  -- bloom/tonemap machinery either way).
  let (blitSrc, blitSrcImage)
        | not (srgbEncoding sc.sFormat) = (outs.displayOut, bindings.scene.targets.display.image)
        | mode /= 0 = (outs.colorOut, bindings.scene.targets.colorHDR.image)
        | otherwise = (outs.toneOut, bindings.scene.targets.tone.image)

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

  let dev = vcDevice vc
  _ <-
    submitGraphQueued
      ( frameSubmitConfig dev f imageIndex \_ ->
          QueueSlot
            { queue = snd (qGraphics (vcQueues vc))
            , family = let QueueFamilyIndex fam = fst (qGraphics (vcQueues vc)) in fam
            , pool = qGraphics (rrCommandPools f.fRecycled)
            }
      )
      graph
  presentFrameImage vc f acquireResult imageIndex
  where
    blitSetup srcH swapchainH = do
      FG.readWith srcH TransferSrc
      FG.writeWith swapchainH TransferDst
