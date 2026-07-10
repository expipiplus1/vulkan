{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Windowed driver (GLFW).

Presents the shaded scene, re-rendering every frame at the native window resolution.
Arrow keys orbit the camera and @-@/@+@ dolly ('Camera.update'); @.@ dumps the view.

Runs the /same/ 'Scene.addScenePasses' graph as the headless driver, but keeps it
single-queue — @cbFor@ maps every pass (geometry, shade) to the graphics command
buffer, so 'FG.executeQueued' degenerates to one buffer and one submit — then
appends a @blit@ (colour → swapchain) finalized to PRESENT_SRC. The blit source
follows the swapchain format ('srgbEncoding'). The per-swapchain scene targets
are recreated on resize by 'runWindowLoop'.
-}
module Windowed
  ( main
  ) where

import Control.Exception (handle)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, closeInternalState, createInternalState, register, runInternalState, runResourceT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Fragr as FG
import GHC.Clock (getMonotonicTime)
import qualified Graphics.UI.GLFW as GLFW
import Say (sayErrString)
import UnliftIO.Exception (displayException)
import qualified Vulkan.Core10 as Vk
import Vulkan.Exception (VulkanException (..))
import Vulkan.Extensions.VK_KHR_surface (ColorSpaceKHR (..), SurfaceFormatKHR (..))
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame)
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), Usage (..), importManagedImage, newManagedImage, usageFlags)
import Vulkan.Utils.FrameGraph.Recorder (recordGraph, recordingCommandBuffer)
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
  -- Orbit camera + previous-frame time, both program-lifetime so resize keeps the view.
  camRef <- liftIO (newIORef opts.orbit)
  prevRef <- liftIO (newIORef startTime)
  -- '.' dumps the current view (discrete, so a key callback rather than the poll loop).
  liftIO $ GLFW.setKeyCallback window $ Just \_ key _ state _ ->
    when (key == GLFW.Key'Period && state == GLFW.KeyState'Pressed) $
      Camera.dump =<< readIORef camRef

  runWindowLoop
    vc
    initialSC
    (Window.drawableSize window)
    (Window.shouldQuit window)
    WindowLoop
      { wlMkState = allocateBindings vma dev pls sceneStatic
      , wlRender = \bindings f -> renderScene opts vc pls window camRef prevRef startTime bindings f
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
          , scSurfaceFormatPreferences = [srgbEncoding]
          }
    }

{- | Does presenting through this surface format sRGB-encode blitted-in linear colour?

Preferred, so the linear @tone@ target blits straight to the swapchain. Surfaces
that offer no such format (MoltenVK leads with UNORM) get the gamma pass's
@display@ target instead — 'renderScene' checks the picked format either way.
-}
srgbEncoding :: SurfaceFormatKHR -> Bool
srgbEncoding sf =
  sf.format `elem` ([Vk.FORMAT_B8G8R8A8_SRGB, Vk.FORMAT_R8G8B8A8_SRGB, Vk.FORMAT_A8B8G8R8_SRGB_PACK32] :: [Vk.Format])
    && sf.colorSpace == COLOR_SPACE_SRGB_NONLINEAR_KHR

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
      swapImages <- traverse (\img -> newManagedImage img Vk.IMAGE_ASPECT_COLOR_BIT) sc.sImages
      pure Bindings{scene, swapImages, exposure}
  key <- register (closeInternalState st)
  pure (bindings, key)

----------------------------------------------------------------
-- Per-frame rendering
----------------------------------------------------------------

renderScene
  :: Options
  -> VulkanContext
  -> Scene.ScenePipelines
  -> GLFW.Window
  -> IORef Camera.Orbit
  -> IORef Double
  -> Double
  -> Bindings
  -> Frame
  -> ResourceT IO ()
renderScene opts vc pls window camRef prevRef startTime bindings f = do
  (acquireResult, imageIndex) <- acquireFrameImage vc f
  now <- liftIO getMonotonicTime
  let t = realToFrac (now - startTime) :: Float
  dt <- liftIO $ do
    prev <- readIORef prevRef
    writeIORef prevRef now
    pure (realToFrac (now - prev) :: Float)
  -- Advance the orbit camera from held keys over this frame's delta.
  orbit <- liftIO (Camera.update window dt camRef)
  let
    eye = Camera.eye orbit
    sc = fSwapchain f
    extent = sc.sExtent
    swapManaged = bindings.swapImages V.! fromIntegral imageIndex

  -- Auto-exposure over the previous frame's mean luminance (the readback lags a
  -- frame; the adaptation hides it).
  exposure <- liftIO $ do
    prevLum <- Scene.readLuminance bindings.scene
    e <- readIORef bindings.exposure
    let e' = Exposure.adapt opts.meter dt e (Exposure.target opts.meter prevLum)
    writeIORef bindings.exposure e'
    pure e'

  graph <- FG.newFrameGraph
  -- Single-queue: the compute passes stay on the default (graphics) queue.
  outs <- Scene.addScenePasses graph pls opts.tweaks bindings.scene FG.defaultQueue extent eye exposure opts.debugMode
  swapchainH <- importManagedImage graph "swapchain" swapManaged

  -- An sRGB swapchain encodes on the blit from the linear @tone@ target; a UNORM
  -- pick blits the gamma pass's @display@ target verbatim. Reading only one output
  -- lets the graph cull the other path (see "Scene").
  let (blitSrc, blitSrcImage)
        | srgbEncoding sc.sFormat = (outs.toneOut, bindings.scene.targets.tone.image)
        | otherwise = (outs.displayOut, bindings.scene.targets.display.image)

  blitted <-
    FG.addPass graph "blit" (blitSetup blitSrc swapchainH) \_ -> do
      cb <- recordingCommandBuffer
      blitImage extent blitSrcImage swapManaged.image cb
  FG.finalize graph blitted (usageFlags Present)

  FG.compile graph

  let dev = vcDevice vc
  graphicsCb <- beginPrimary dev (rrCommandPool f.fRecycled)
  -- Move the orb + refresh its shadow slice ahead of the scene graph (same buffer).
  Scene.recordOrbFrame graphicsCb pls bindings.scene t
  recordGraph (const graphicsCb) (graphicsCb :| []) graph

  queueSubmitFrame vc f imageIndex [graphicsCb]
  presentFrameImage vc f acquireResult imageIndex
  where
    blitSetup toneOut swapchainH = do
      FG.readWith toneOut (usageFlags TransferSrc)
      FG.writeWith swapchainH (usageFlags TransferDst)
