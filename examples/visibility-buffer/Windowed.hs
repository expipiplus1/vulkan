{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Windowed driver (GLFW).

Presents the shaded scene, re-rendering every frame at the native window resolution.
Arrow keys orbit the camera; @-@/@+@ dolly ('updateOrbit').

Runs the /same/ 'Scene.addScenePasses' graph as the headless driver, but keeps it
single-queue — @cbFor@ maps every pass (geometry, shade) to the graphics command
buffer, so 'FG.executeQueued' degenerates to one buffer and one submit — then
appends a @blit@ (colour → swapchain) and a @present@ pass. The per-swapchain
scene targets are recreated on resize by 'runWindowLoop'.
-}
module Windowed
  ( main
  ) where

import Control.Exception (handle)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, closeInternalState, createInternalState, register, runInternalState, runResourceT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Fragr as FG
import GHC.Clock (getMonotonicTime)
import Geomancy.Vec3 (Vec3, vec3, withVec3)
import qualified Graphics.UI.GLFW as GLFW
import Say (sayErrString)
import UnliftIO.Exception (displayException)
import qualified Vulkan.Core10 as Vk
import Vulkan.Exception (VulkanException (..))
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame)
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), Usage (..), importManagedImage, newManagedImage, usageFlags)
import Vulkan.Utils.FrameGraph.Recorder (recordGraph, recorderCommandBuffer)
import qualified Vulkan.Utils.Init.GLFW.Window as Window
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.Swapchain (Swapchain (..), SwapchainConfig (..), defaultSwapchainConfig)
import Vulkan.Utils.VulkanContext (RecycledResources (..), VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, noOnFrame, runWindowLoop)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA
import WindowedBoot (WindowedConfig (..), withWindowedVk)

import Blit (blitImage)
import Driver (beginPrimary)
import Requirements (deviceRequirements)
import qualified Scene

main :: IO ()
main = prettyError . runResourceT $ do
  Window.withGLFW
  window <- Window.createWindow "Haskell Vulkan 👀 Visibility Buffer 🖼 Frame Graph" 1024 1024
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
  -- Orbit camera + previous-frame time, both program-lifetime so resize keeps the view.
  camRef <- liftIO (newIORef initialOrbit)
  prevRef <- liftIO (newIORef startTime)

  runWindowLoop
    vc
    initialSC
    (Window.drawableSize window)
    (Window.shouldQuit window)
    WindowLoop
      { wlMkState = createBindings vma dev pls sceneStatic
      , wlRender = \bindings f -> renderScene vc pls window camRef prevRef startTime bindings f
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
          }
    }

prettyError :: IO () -> IO ()
prettyError = handle (\e@(VulkanException _) -> sayErrString (displayException e))

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
createBindings :: VMA.Allocator -> Vk.Device -> Scene.ScenePipelines -> Scene.SceneStatic -> Swapchain -> ResourceT IO (Bindings, ReleaseKey)
createBindings allocator dev pls sceneStatic sc = do
  exposure <- liftIO (newIORef 1.0)
  st <- createInternalState
  bindings <-
    liftIO . flip runInternalState st $ do
      scene <- Scene.allocateTargets allocator dev pls sceneStatic sc.sExtent Nothing
      swapImages <- traverse (\img -> newManagedImage img Vk.IMAGE_ASPECT_COLOR_BIT) sc.sImages
      pure Bindings{scene, swapImages, exposure}
  key <- register (closeInternalState st)
  pure (bindings, key)

----------------------------------------------------------------
-- Per-frame rendering
----------------------------------------------------------------

renderScene :: VulkanContext -> Scene.ScenePipelines -> GLFW.Window -> IORef Orbit -> IORef Double -> Double -> Bindings -> Frame -> ResourceT IO ()
renderScene vc pls window camRef prevRef startTime bindings f = do
  (acquireResult, imageIndex) <- acquireFrameImage vc f
  now <- liftIO getMonotonicTime
  let t = realToFrac (now - startTime) :: Float
  -- Advance the orbit camera from held keys over this frame's delta.
  orbit <- liftIO $ do
    prev <- readIORef prevRef
    writeIORef prevRef now
    updateOrbit window (realToFrac (now - prev)) camRef
  let
    eye = orbitEye orbit
    sc = fSwapchain f
    extent = sc.sExtent
    swapManaged = bindings.swapImages V.! fromIntegral imageIndex

  -- Auto-exposure: smooth toward middle-grey over the previous frame's mean
  -- luminance (the readback lags a frame; the smoothing hides it).
  exposure <- liftIO $ do
    prevLum <- Scene.readLuminance bindings.scene
    e <- readIORef bindings.exposure
    let
      target = if prevLum > 1e-5 then max 0.05 (min 20 (0.18 / prevLum)) else e
      e' = e + (target - e) * 0.05
    writeIORef bindings.exposure e'
    pure e'

  graph <- FG.newFrameGraph
  -- Single-queue: the compute passes stay on the default (graphics) queue. This
  -- reads toneOut, so the sRGB swapchain encodes gamma on blit (see "Scene").
  outs <- Scene.addScenePasses graph pls bindings.scene FG.defaultQueue extent eye exposure 0
  swapchainH <- importManagedImage graph "swapchain" swapManaged

  blitted <-
    FG.addPass graph "blit" (blitSetup outs.toneOut swapchainH) \_ _ recorder -> do
      cb <- recorderCommandBuffer recorder
      blitImage extent bindings.scene.targets.tone.image swapManaged.image cb
  _ <-
    FG.addPass graph "present" (\b -> FG.writeWith b blitted (usageFlags Present)) \_ _ _ ->
      pure ()

  FG.compile graph

  let dev = vcDevice vc
  graphicsCb <- beginPrimary dev (rrCommandPool f.fRecycled)
  -- Move the orb + refresh its shadow slice ahead of the scene graph (same buffer).
  Scene.recordOrbFrame graphicsCb pls bindings.scene t
  recordGraph (const graphicsCb) (graphicsCb :| []) graph

  queueSubmitFrame vc f imageIndex [graphicsCb]
  presentFrameImage vc f acquireResult imageIndex
  where
    blitSetup toneOut swapchainH b = do
      _ <- FG.readWith b toneOut (usageFlags TransferSrc)
      FG.writeWith b swapchainH (usageFlags TransferDst)

----------------------------------------------------------------
-- Orbit camera
----------------------------------------------------------------

-- | Orbit camera: spherical coordinates about 'Scene.cameraTarget'.
data Orbit = Orbit
  { azimuth :: Float
  , elevation :: Float
  , distance :: Float
  }

initialOrbit :: Orbit
initialOrbit = Orbit{azimuth = 1.474, elevation = 0.311, distance = 0.327}

-- | Eye position for an orbit state.
orbitEye :: Orbit -> Vec3
orbitEye o =
  withVec3 Scene.cameraTarget \tx ty tz ->
    vec3 (tx + o.distance * ce * ca) (ty + o.distance * se) (tz + o.distance * ce * sa)
  where
    ca = cos o.azimuth
    sa = sin o.azimuth
    ce = cos o.elevation
    se = sin o.elevation

{- | Advance the orbit from held keys over @dt@ seconds.

Arrows orbit (left/right azimuth, up/down elevation), @-@/@+@ dolly. Elevation is
clamped shy of the poles; distance to a sane range.
-}
updateOrbit :: GLFW.Window -> Float -> IORef Orbit -> IO Orbit
updateOrbit window dt ref = do
  o <- readIORef ref
  l <- held GLFW.Key'Left
  r <- held GLFW.Key'Right
  u <- held GLFW.Key'Up
  d <- held GLFW.Key'Down
  outward <- (||) <$> held GLFW.Key'Minus <*> held GLFW.Key'PadSubtract
  inward <- (||) <$> held GLFW.Key'Equal <*> held GLFW.Key'PadAdd
  let
    rot = 1.6 * dt
    axis neg pos = (if pos then rot else 0) - (if neg then rot else 0)
    o' =
      Orbit
        { azimuth = o.azimuth + axis l r
        , elevation = clamp (-1.45) 1.45 (o.elevation + axis d u)
        , distance = clamp 0.15 3.0 (o.distance * exp (0.9 * dt * (bit outward - bit inward)))
        }
  writeIORef ref o'
  pure o'
  where
    held k = (== GLFW.KeyState'Pressed) <$> GLFW.getKey window k
    clamp lo hi = max lo . min hi
    bit b = if b then 1 else 0
