{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Compute-glitch demoscene: the whole frame is generated and rendered on the
GPU, then mangled by a compute shader on its way to the swapchain, in time with
a synthesized cracktro tune ("Music.Tune") whose playhead drives the scene's
knobs ("Tune").

'record' lays out the four stages of a frame and the barriers between them, all
on the graphics queue in one command buffer.
-}
module Main where

import Blit (blitImage)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT)
import Data.Bits ((.|.))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Vector as V
import Debug.Trace (traceM)
import GHC.Clock (getMonotonicTime)
import qualified Music.Player as Player
import qualified Music.Tune
import qualified Sound.OpenAL.FFI.AL as AL
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import Vulkan.Utils.Barrier (bufferBarrier, imageBarrier, transitionColorAttachment, transitionDepthAttachment)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (DynamicState (..), allDynamicStates, applyDynamicStates, dynamicStateFor, fullScissor)
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame, recordCommands)
import qualified Vulkan.Utils.Init.GLFW.Window as Window
import Vulkan.Utils.Pipeline (Pipeline)
import Vulkan.Utils.Swapchain (Swapchain (..), SwapchainConfig (..), defaultSwapchainConfig, unormEncoding)
import Vulkan.Utils.VulkanContext (VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, noOnFrame, noRecycledResources, runWindowLoop)
import Vulkan.Zero (zero)
import WindowedBoot (WindowedConfig (..), withWindowedVk)

import Camera (Camera)
import qualified Camera
import qualified Glitch
import Particles (Particles)
import qualified Particles
import PingPong (PingPong (..), Slot)
import qualified PingPong
import Targets (Targets (..))
import qualified Targets
import Tune (Knobs, knobsAt)
import qualified Tune
import Tunnel (Tunnel)
import qualified Tunnel

main :: IO ()
main = runResourceT do
  Window.withGLFW
  window <- Window.createWindow appName windowWidth windowHeight
  Window.showWindow window
  (vc, vma, initialSC) <-
    withWindowedVk
      WindowedConfig
        { appName = appName
        , instanceReqs = []
        , deviceReqs = Dynamic.dynamicRenderingRequirements
        , vmaFlags = zero
        , swapchainConfig = blittedSwapchain
        }
      (Window.glfwAdapter window)
  let dev = vc.vcDevice

  cameras <- Camera.allocateSlots vma
  tunnel <- Tunnel.allocateTunnel vma dev Targets.sceneFormat Targets.depthFormat cameras
  particles <- Particles.allocateParticles vma dev Targets.sceneFormat Targets.depthFormat cameras
  glitch <- Glitch.allocateGlitch dev
  tune <- allocateTune

  startedAt <- liftIO getMonotonicTime
  previous <- liftIO $ newIORef Previous{frameAt = startedAt, songPos = Tune.beforeStart, twist = 0}
  submitted <- liftIO $ newIORef 0
  let demo = Demo{tunnel, particles, glitch, cameras, tune, startedAt, previous, submitted}

  runWindowLoop
    vc
    initialSC
    (Window.drawableSize window)
    (Window.shouldQuit window)
    WindowLoop
      { wlMkState = Targets.allocateTargets vma dev glitch
      , wlMkRecycled = noRecycledResources
      , wlRender = drawFrame vc demo
      , wlOnFrame = noOnFrame
      , wlOnExit = noOnExit
      }

appName :: Text
appName = "Haskell 😍 Vulkan"

windowWidth, windowHeight :: Int
windowWidth = 1280
windowHeight = div windowWidth 16 * 9

{- | The frame arrives by blit rather than by rendering: @TRANSFER_DST@ and the
blit feature, plus @COLOR_ATTACHMENT@ so the swapchain helper can still build
image views. A UNORM surface keeps the post pass's display-referred colours as
they are — an sRGB pick would re-encode them.
-}
blittedSwapchain :: SwapchainConfig
blittedSwapchain =
  defaultSwapchainConfig
    { scRequiredUsageFlags =
        [ Vk.IMAGE_USAGE_TRANSFER_DST_BIT
        , Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
        ]
    , scRequiredFormatFeatures = [Vk.FORMAT_FEATURE_BLIT_DST_BIT]
    , scSurfaceFormatPreferences = [unormEncoding]
    }

{- | The tune, synthesized up front and staged on a looping OpenAL source.

AL copies the samples in 'Player.load', so the synth buffer is freed before the
loop even starts. Playback begins on the first submitted frame ('startTune').
-}
allocateTune :: ResourceT IO AL.Source
allocateTune = do
  _context <- snd <$> allocate Player.open Player.close
  (_buffer, source) <-
    snd
      <$> allocate
        (Music.Tune.withTune (Player.load AL.FORMAT_STEREO16 (fromIntegral Music.Tune.sampleRate)))
        Player.delete
  liftIO $ Player.setLooping source True
  pure source

-- | Everything built once at startup that a frame needs.
data Demo = Demo
  { tunnel :: Tunnel
  , particles :: Particles
  , glitch :: Pipeline
  , cameras :: PingPong Camera
  , tune :: AL.Source
  , startedAt :: Double
  -- ^ Monotonic seconds; the scene's motion runs off this.
  , previous :: IORef Previous
  , submitted :: IORef Int
  {- ^ Frames whose commands reached the queue. A swapchain error aborts a
  frame mid-record while 'fIndex' still advances, so the first-frame one-shots
  and the ping-pong key on this count instead.
  -}
  }

-- | What the previous frame left behind for this one.
data Previous = Previous
  { frameAt :: Double
  , songPos :: Float
  , twist :: Double
  -- ^ The tunnel's accumulated rotation angle (see 'Tunnel.twistRate').
  }

-- | What a frame's commands run off, sampled once on the CPU.
data Cue = Cue
  { time :: Float
  -- ^ Seconds since startup: the motion.
  , dt :: Float
  , twist :: Float
  , knobs :: Knobs
  -- ^ Read off the tune's own playhead: the sync.
  , slot :: Slot
  , firstFrame :: Bool
  }

-- | Clamped so a debugger pause or a window drag can't slingshot the sim.
maxFrameStep :: Double
maxFrameStep = 0.1

{- | The song position is the source's own playhead, so the knobs cannot drift
from what's audible — across loops too, since the source and 'knobsAt' wrap
together. The playhead advances in mixer updates (~20 ms), but only the knobs
see those steps.
-}
sample :: Demo -> IO Cue
sample demo = do
  submitted <- readIORef demo.submitted
  now <- getMonotonicTime
  when (submitted == 0) $ startTune demo now
  songNow <- Player.playTime demo.tune
  was <- readIORef demo.previous
  let
    knobs = knobsAt songNow
    dt = min maxFrameStep (now - was.frameAt)
    twist = was.twist + realToFrac (Tunnel.twistRate * knobs.spin) * dt
  writeIORef demo.previous Previous{frameAt = now, songPos = songNow, twist}
  traceEvents was.songPos songNow
  pure
    Cue
      { time = realToFrac (now - demo.startedAt)
      , dt = realToFrac dt
      , twist = realToFrac twist
      , knobs
      , slot = PingPong.slot submitted
      , firstFrame = submitted == 0
      }

{- | Song zero is the first submitted frame: initialization and the target
prefills are behind us, so the tune starts here.
-}
startTune :: Demo -> Double -> IO ()
startTune demo now = do
  Player.play demo.tune
  traceM $ "tune zero @ " <> show now

{- | Trace every tune boundary crossed this frame: scheduled versus actual song
position shows the frame-quantization lag, and listening along shows the drift.
-}
traceEvents :: Float -> Float -> IO ()
traceEvents from to =
  forM_ (Tune.eventsBetween from to) \(scheduled, label) ->
    traceM $ label <> " (scheduled @ " <> show scheduled <> ") happens @ " <> show to

drawFrame :: VulkanContext () -> Demo -> PingPong Targets -> Frame () -> ResourceT IO ()
drawFrame vc demo targets f = do
  (acquireResult, imageIndex) <- acquireFrameImage vc f
  cue <- liftIO $ sample demo
  let
    swapchain = fSwapchain f
    extent = swapchain.sExtent
    swapImage = swapchain.sImages V.! fromIntegral imageIndex
  liftIO $ Camera.write (PingPong.current cue.slot demo.cameras) (Camera.projection (aspect extent))
  commands <-
    recordCommands vc f $
      record demo cue (PingPong.current cue.slot targets) swapImage extent
  queueSubmitFrame vc f imageIndex [commands]
  liftIO $ modifyIORef' demo.submitted (+ 1)
  presentFrameImage vc f acquireResult imageIndex

aspect :: Vk.Extent2D -> Float
aspect extent = fromIntegral extent.width / fromIntegral extent.height

record :: (MonadIO m) => Demo -> Cue -> Targets -> Vk.Image -> Vk.Extent2D -> Vk.CommandBuffer -> m ()
record demo cue targets swapImage extent cb = do
  when cue.firstFrame $ Particles.zeroState demo.particles cb

  awaitPreviousFrame demo cue cb
  Tunnel.grow demo.tunnel cue.time cue.twist cue.knobs cb
  Particles.step demo.particles cue.slot cue.time cue.dt cue.knobs cb
  handToVertexPull demo cue cb

  transitionColorAttachment cb targets.scene.image
  transitionDepthAttachment cb targets.depth.image
  Vk.cmdUseRendering cb (sceneRenderingInfo extent targets) do
    applyDynamicStates
      allDynamicStates
      cb
      (dynamicStateFor extent)
        { depthTest = True
        , depthWrite = True
        , depthCompareOp = Vk.COMPARE_OP_LESS
        }
    Tunnel.draw demo.tunnel cue.slot cb
    Particles.drawSwarm demo.particles cue.slot cue.knobs cb

  handSceneToGlitch targets cb
  Glitch.dispatch demo.glitch extent targets.glitchSet cue.time cue.knobs cb

  handGlitchToBlit targets swapImage cb
  blitImage extent targets.post.image swapImage cb
  handSwapchainToPresent swapImage cb

sceneRenderingInfo :: Vk.Extent2D -> Targets -> Vk.RenderingInfo '[]
sceneRenderingInfo extent targets =
  Dynamic.renderingInfo
    (fullScissor extent)
    [(targets.scene.view, background)]
    (Just (targets.depth.view, farPlane))
  where
    background = Vk.Float32 0.015 0 0.03 1
    farPlane = 1.0

----------------------------------------------------------------
-- Barriers
----------------------------------------------------------------

{- | Close the cross-frame buffer hazards before the geometry dispatches: the
previous frame's vertex pulls, and its sim write. Queue submission order carries
this across command buffers.
-}
awaitPreviousFrame :: (MonadIO m) => Demo -> Cue -> Vk.CommandBuffer -> m ()
awaitPreviousFrame demo cue cb =
  Vk.cmdPipelineBarrier
    cb
    (Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT .|. Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT)
    Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
    zero
    []
    [ readBeforeWrite demo.tunnel.vertices
    , writeBeforeRead (PingPong.previous cue.slot demo.particles.state)
    , readBeforeWrite (PingPong.current cue.slot demo.particles.state)
    ]
    []

-- | The freshly grown buffers, handed to this frame's vertex shaders.
handToVertexPull :: (MonadIO m) => Demo -> Cue -> Vk.CommandBuffer -> m ()
handToVertexPull demo cue cb =
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
    Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT
    zero
    []
    [ writeBeforeRead demo.tunnel.vertices
    , writeBeforeRead (PingPong.current cue.slot demo.particles.state)
    ]
    []

-- | Reads must finish before the writes that overwrite what they read.
readBeforeWrite :: Vk.Buffer -> SomeStruct Vk.BufferMemoryBarrier
readBeforeWrite = bufferBarrier Vk.ACCESS_SHADER_READ_BIT Vk.ACCESS_SHADER_WRITE_BIT

-- | Writes must be visible to the reads that consume them.
writeBeforeRead :: Vk.Buffer -> SomeStruct Vk.BufferMemoryBarrier
writeBeforeRead = bufferBarrier Vk.ACCESS_SHADER_WRITE_BIT Vk.ACCESS_SHADER_READ_BIT

{- | The scene, just written as a colour attachment, handed to the glitch
dispatch as a read-only storage image (storage access needs @GENERAL@), and the
glitch target opened for writing.
-}
handSceneToGlitch :: (MonadIO m) => Targets -> Vk.CommandBuffer -> m ()
handSceneToGlitch targets cb =
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
    zero
    []
    []
    [ imageBarrier
        Vk.IMAGE_ASPECT_COLOR_BIT
        Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
        Vk.ACCESS_SHADER_READ_BIT
        Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        Vk.IMAGE_LAYOUT_GENERAL
        targets.scene.image
    , imageBarrier
        Vk.IMAGE_ASPECT_COLOR_BIT
        zero
        Vk.ACCESS_SHADER_WRITE_BIT
        Vk.IMAGE_LAYOUT_UNDEFINED
        Vk.IMAGE_LAYOUT_GENERAL
        targets.post.image
    ]

-- | The glitched image ready to be blitted from, onto a swapchain image ready to take it.
handGlitchToBlit :: (MonadIO m) => Targets -> Vk.Image -> Vk.CommandBuffer -> m ()
handGlitchToBlit targets swapImage cb =
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    zero
    []
    []
    [ imageBarrier
        Vk.IMAGE_ASPECT_COLOR_BIT
        Vk.ACCESS_SHADER_WRITE_BIT
        Vk.ACCESS_TRANSFER_READ_BIT
        Vk.IMAGE_LAYOUT_GENERAL
        Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
        targets.post.image
    , imageBarrier
        Vk.IMAGE_ASPECT_COLOR_BIT
        zero
        Vk.ACCESS_TRANSFER_WRITE_BIT
        Vk.IMAGE_LAYOUT_UNDEFINED
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        swapImage
    ]

handSwapchainToPresent :: (MonadIO m) => Vk.Image -> Vk.CommandBuffer -> m ()
handSwapchainToPresent swapImage cb =
  Vk.cmdPipelineBarrier
    cb
    Vk.PIPELINE_STAGE_TRANSFER_BIT
    Vk.PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    zero
    []
    []
    [ imageBarrier
        Vk.IMAGE_ASPECT_COLOR_BIT
        Vk.ACCESS_TRANSFER_WRITE_BIT
        zero
        Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
        Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR
        swapImage
    ]
