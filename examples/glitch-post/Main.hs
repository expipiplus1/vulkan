{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Compute-glitch demoscene: the whole frame is generated and rendered on
the GPU, then mangled by a compute shader on its way to the swapchain. Four
stages per frame, all on the graphics queue in one command buffer:

1. __Geometry__ (compute): one dispatch grows the neon wormhole —
   checkerboarded ring tiles flying down a twisting tube, threaded through
   a tumbling octahedron — writing eye-space vertices into an SSBO
   ("Tunnel"); a second steps the spark swarm ("Particles"), whose state
   /persists/ across frames by ping-ponging between two SSBOs keyed by
   frame parity (read what the previous frame wrote, write the other).
   Once a cycle the sim springs the whole swarm into a 5×7-bitfont banner
   and lets it burst apart again.

2. __Scene__ (graphics, dynamic rendering): the vertex shaders pull straight
   from the SSBOs — neither pipeline has any vertex input — and project
   with the matrix from a set-0 uniform buffer (one slot per frame in
   flight, keyed by frame parity), into an offscreen colour target with a
   real depth attachment. The sparks draw twice with depth writes toggled
   off: the swarm and a dim 180°-rotated echo, varied only by push
   constants on the graphics pipeline.

3. __Glitch__ (compute): a compute shader reads the scene image and writes a
   corrupted version — datamosh blocks, a sweeping scanline tear, chromatic
   aberration, a poor-man's bloom, CRT dressing — into a second storage
   image ("Glitch").

4. __Blit__: the glitched image is blitted to the acquired swapchain image
   (converting RGBA→whatever the surface wants), which then moves to
   @PRESENT_SRC@.

Every offscreen target is per swapchain /image/, so no two in-flight frames
ever touch the same one (the image-available semaphore orders reuse) and the
pipeline barriers between the stages deal with intra-frame ordering: SSBO
writes → vertex pulls, attachment write → compute read, compute write → blit
read, blit write → present. The shared buffers also carry cross-frame
hazards — the previous frame still pulls vertices while this one records —
all closed by the barrier in front of the geometry dispatches, reaching
across command buffers through queue submission order. The first frame
zero-fills the particle buffers (age 0 routes every particle through the
sim's respawn path).
-}
module Main where

import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, register, release, runResourceT)
import Data.Bits ((.|.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Vector as V
import Debug.Trace (traceM)
import Foreign.Marshal.Array (pokeArray)
import GHC.Clock (getMonotonicTime)
import Glitch (GlitchPipeline (..), createGlitchDescriptorSets, createGlitchPipeline, dispatchGlitch)
import qualified Music.Player as Player
import Music.Tune (withTune)
import Particles (ParticlePipeline, createParticleBuffers, createParticlePipeline, createParticleSets, createParticleTargets, dispatchParticleSim, drawParticles)
import RenderTarget (createDepthTarget, createTarget)
import qualified Sound.OpenAL.FFI.AL as AL
import Tune (Knobs (..), eventsBetween, knobsAt)
import Tunnel (CameraSlot (..), TunnelPipeline (..), createCameraSlots, createTunnelGeometrySet, createTunnelPipeline, createTunnelVertexBuffer, dispatchTunnelGeometry, tunnelProjection, tunnelTwistRate, tunnelVertexCount)
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.Core13 as Vk
import Vulkan.Utils.Barrier (bufferBarrier, imageBarrier, transitionColorAttachment, transitionDepthAttachment)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (DynamicState (..), allDynamicStates, applyDynamicStates, dynamicStateFor, fullScissor)
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, presentFrameImage, queueSubmitFrame, recordCommands)
import qualified Vulkan.Utils.Init.GLFW.Window as Window
import Vulkan.Utils.Swapchain (Swapchain (..), SwapchainConfig (..), defaultSwapchainConfig)
import Vulkan.Utils.VulkanContext (VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, noOnFrame, runWindowLoop)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as VMA
import WindowedBoot (WindowedConfig (..), withWindowedVk)

{- | The offscreen format, deliberately independent of the (often BGRA)
swapchain format: @R8G8B8A8_UNORM@ has mandatory storage-image support, and
the final blit converts to whatever the surface uses.
-}
sceneFormat :: Vk.Format
sceneFormat = Vk.FORMAT_R8G8B8A8_UNORM

depthFormat :: Vk.Format
depthFormat = Vk.FORMAT_D32_SFLOAT

main :: IO ()
main = runResourceT $ do
  Window.withGLFW
  window <- Window.createWindow appName windowWidth windowHeight
  Window.showWindow window
  (vc, vma, initialSC) <-
    withWindowedVk
      WindowedConfig
        { wcAppName = appName
        , wcInstanceReqs = []
        , wcDeviceReqs = Dynamic.dynamicRenderingRequirements
        , wcVmaFlags = zero
        , wcSwapchainConfig =
            defaultSwapchainConfig
              { scRequiredUsageFlags =
                  -- TRANSFER_DST for the final blit; COLOR_ATTACHMENT so the
                  -- swapchain helper can still build image views.
                  [ Vk.IMAGE_USAGE_TRANSFER_DST_BIT
                  , Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                  ]
              , scRequiredFormatFeatures = [Vk.FORMAT_FEATURE_BLIT_DST_BIT]
              }
        }
      (Window.glfwAdapter window)
  let dev = vcDevice vc

  tunnelPL <- createTunnelPipeline dev sceneFormat depthFormat
  vertexBuffer <- createTunnelVertexBuffer vma
  cameraSlots <- createCameraSlots vma dev (tpDescriptorSetLayout tunnelPL) framesInFlight vertexBuffer
  geometrySet <- createTunnelGeometrySet dev (tpGeometrySetLayout tunnelPL) vertexBuffer
  particlePL <- createParticlePipeline dev sceneFormat depthFormat
  particleBuffers <- createParticleBuffers vma
  particleTargets <- createParticleTargets vma
  (particleSimSets, particleDrawSets) <-
    createParticleSets dev particlePL particleBuffers particleTargets (csBuffer <$> cameraSlots)
  glitchPL <- createGlitchPipeline dev

  -- The tune: synthesized up front (44100 s16le stereo, pl_synth's only
  -- format) and staged on a looping OpenAL source — the track loops at
  -- exactly 'Tune.loopLength', so source and knobs wrap together. AL
  -- copies the samples in 'Player.load', so the synth buffer is freed
  -- before the loop starts. Playback begins on the first rendered frame,
  -- where 'dSongStart' calls song zero.
  _alContext <- snd <$> allocate Player.open (void . Player.close)
  (_tuneBuffer, tuneSource) <-
    snd <$> allocate (withTune (Player.load AL.FORMAT_STEREO16 44100)) Player.delete
  liftIO $ Player.setLooping tuneSource True

  start <- liftIO getMonotonicTime
  lastFrame <- liftIO $ newIORef start
  songPos <- liftIO $ newIORef 0
  twist <- liftIO $ newIORef 0
  let demo =
        Demo
          { dTunnel = tunnelPL
          , dGlitch = glitchPL
          , dParticles = particlePL
          , dCameraSlots = cameraSlots
          , dGeometrySet = geometrySet
          , dVertexBuffer = vertexBuffer
          , dParticleBuffers = particleBuffers
          , dParticleSimSets = particleSimSets
          , dParticleDrawSets = particleDrawSets
          , dStart = start
          , dLastFrame = lastFrame
          , dSongPos = songPos
          , dTwist = twist
          , dTuneSource = tuneSource
          }

  runWindowLoop
    vc
    initialSC
    (Window.drawableSize window)
    (Window.shouldQuit window)
    WindowLoop
      { wlMkState = createTargets vma dev glitchPL
      , wlRender = drawFrame vc demo
      , wlOnFrame = noOnFrame
      , wlOnExit = noOnExit
      }

appName :: Text
appName = "Haskell 😍 Vulkan"

windowWidth, windowHeight :: Int
windowWidth = 1280
windowHeight = div windowWidth 16 * 9

{- | Max frames in flight, per the "Vulkan.Utils.Frame" model (one frame
recording on the CPU while the previous executes on the GPU); sizes the
camera-slot ring and the particle ping-pong.
-}
framesInFlight :: Int
framesInFlight = 2

-- | Everything built once at startup that 'drawFrame' needs.
data Demo = Demo
  { dTunnel :: TunnelPipeline
  , dGlitch :: GlitchPipeline
  , dParticles :: ParticlePipeline
  , dCameraSlots :: V.Vector CameraSlot
  -- ^ Indexed by frame parity.
  , dGeometrySet :: Vk.DescriptorSet
  , dVertexBuffer :: Vk.Buffer
  -- ^ The tunnel's vertex SSBO.
  , dParticleBuffers :: V.Vector Vk.Buffer
  -- ^ The two ping-pong state buffers.
  , dParticleSimSets :: V.Vector Vk.DescriptorSet
  -- ^ Indexed by frame parity.
  , dParticleDrawSets :: V.Vector Vk.DescriptorSet
  -- ^ Indexed by frame parity.
  , dStart :: Double
  -- ^ Start time (monotonic seconds); drives the motion.
  , dLastFrame :: IORef Double
  -- ^ The previous frame's timestamp, for the sim's dt.
  , dSongPos :: IORef Float
  -- ^ The previous frame's song position, for the boundary-event trace.
  , dTuneSource :: AL.Source
  {- ^ The staged tune, looping at 'Tune.loopLength'. Played on the first
  rendered frame — initialization and the target prefills behind us — and
  its playhead is the song position the knobs run off.
  -}
  , dTwist :: IORef Double
  {- ^ The tunnel's accumulated whole-tube rotation angle. The spin
  /direction/ is a knob ('kSpin'), so the angle is its integral —
  pushing rate × time instead would snap the rotation whenever the
  direction reverses.
  -}
  }

----------------------------------------------------------------
-- Per-swapchain targets (caller-owned)
----------------------------------------------------------------

-- | The offscreen images and descriptor sets, all indexed by swapchain image.
data Targets = Targets
  { tScene :: V.Vector (Vk.Image, Vk.ImageView)
  -- ^ Scene target: colour attachment, then compute-read storage image.
  , tDepth :: V.Vector (Vk.Image, Vk.ImageView)
  , tPost :: V.Vector (Vk.Image, Vk.ImageView)
  -- ^ Glitched output: compute-write storage image, then blit source.
  , tGlitchSets :: V.Vector Vk.DescriptorSet
  -- ^ Binds the matching scene and post views.
  }

{- | One scene\/depth\/post target triple per swapchain image — like the
swapchain's own images, no two in-flight frames ever target the same triple —
plus the descriptor set binding each triple's views. Returns a single
'ReleaseKey' freeing the lot (fired on a swapchain swap).
-}
createTargets
  :: VMA.Allocator
  -> Vk.Device
  -> GlitchPipeline
  -> Swapchain
  -> ResourceT IO (Targets, ReleaseKey)
createTargets vma dev glitchPL sc = do
  scene <- V.forM (sImageViews sc) \_ ->
    createTarget
      vma
      dev
      sceneFormat
      (sExtent sc)
      (Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. Vk.IMAGE_USAGE_STORAGE_BIT)
      Vk.IMAGE_ASPECT_COLOR_BIT
      "glitch scene target"
  depth <- V.forM (sImageViews sc) \_ ->
    createDepthTarget vma dev depthFormat (sExtent sc)
  post <- V.forM (sImageViews sc) \_ ->
    createTarget
      vma
      dev
      sceneFormat
      (sExtent sc)
      (Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT)
      Vk.IMAGE_ASPECT_COLOR_BIT
      "glitch post target"
  (poolKey, sets) <-
    createGlitchDescriptorSets
      dev
      (gpDescriptorSetLayout glitchPL)
      (V.zip (snd . snd <$> scene) (snd . snd <$> post))
  groupKey <- register do
    release poolKey
    V.forM_ (scene <> depth <> post) \((imageKey, viewKey), _) ->
      release viewKey >> release imageKey
  pure
    ( Targets
        { tScene = fmap snd scene
        , tDepth = fmap snd depth
        , tPost = fmap snd post
        , tGlitchSets = sets
        }
    , groupKey
    )

----------------------------------------------------------------
-- Per-frame draw
----------------------------------------------------------------

drawFrame
  :: VulkanContext
  -> Demo
  -> Targets
  -> Frame
  -> ResourceT IO ()
drawFrame vc Demo{..} targets f = do
  (acquireResult, imageIndex) <- acquireFrameImage vc f
  now <- liftIO getMonotonicTime
  lastT <- liftIO $ readIORef dLastFrame
  liftIO $ writeIORef dLastFrame now
  -- Song zero is the first rendered frame: initialization and the target
  -- prefills are behind us, so the tune starts here.
  when (fIndex f == 1) $ liftIO do
    Player.play dTuneSource
    traceM $ "tune zero @ " <> show now
  -- The song position is the source's own playhead, so the knobs cannot
  -- drift from what's audible — across loops too, since the source and
  -- 'knobsAt' wrap at the same 'Tune.loopLength'. The playhead advances
  -- in mixer updates (~20 ms), but only the knobs see those steps; the
  -- scene's motion runs off the monotonic clock.
  songNow <- liftIO $ Player.playTime dTuneSource
  songPrev <- liftIO $ readIORef dSongPos
  liftIO $ writeIORef dSongPos songNow
  let
    t = realToFrac (now - dStart) :: Float
    -- Clamped so a debugger pause or window drag doesn't slingshot the sim.
    dt = realToFrac (min 0.1 (now - lastT)) :: Float
    -- The frame's performance knobs — the single source of truth, pushed
    -- to every shader. Tune turns the song position into what the scene
    -- does: the banner over the calm sections (bursting onto the drops),
    -- the kick envelope, the section energy, the snare-roll builds, and
    -- the drop flashes.
    knobs = knobsAt songNow
    ii = fromIntegral imageIndex :: Int
    Vk.Extent2D w h = sExtent
    -- Keyed by frame parity: frame N+2 cannot begin recording until frame N
    -- retires, so overwriting N's slot here is safe (see 'createCameraSlots').
    parity = fromIntegral (fIndex f) `mod` framesInFlight
    camera = dCameraSlots V.! parity
    -- The sim reads what the previous frame (opposite parity) wrote.
    prevParticles = dParticleBuffers V.! (1 - parity)
    curParticles = dParticleBuffers V.! parity

  -- Stub events at every tune boundary crossed this frame: scheduled
  -- versus actual song position shows the frame-quantization lag, and
  -- listening against the player shows the clock drift.
  liftIO $ forM_ (eventsBetween songPrev songNow) \(scheduled, label) ->
    traceM $ label <> " (scheduled @ " <> show scheduled <> ") happens @ " <> show songNow

  -- Integrate the spin into the twist angle (see 'dTwist').
  twist <- liftIO do
    old <- readIORef dTwist
    let new = old + realToFrac (tunnelTwistRate * kSpin knobs * dt)
    writeIORef dTwist new
    pure (realToFrac new :: Float)

  liftIO $ pokeArray (csProjection camera) (tunnelProjection (fromIntegral w / fromIntegral h))

  commands <- recordCommands vc f \cb -> do
    let
      (sceneImage, sceneView) = tScene targets V.! ii
      (depthImage, depthView) = tDepth targets V.! ii
      (postImage, _) = tPost targets V.! ii
      swapImage = sImages V.! ii

    -- First frame only: zero both particle buffers, so age 0 routes every
    -- particle through the sim's respawn path.
    when (fIndex f == 1) do
      V.forM_ dParticleBuffers \b -> Vk.cmdFillBuffer cb b 0 Vk.WHOLE_SIZE 0
      Vk.cmdPipelineBarrier
        cb
        Vk.PIPELINE_STAGE_TRANSFER_BIT
        Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
        zero
        []
        ( fmap
            (bufferBarrier Vk.ACCESS_TRANSFER_WRITE_BIT (Vk.ACCESS_SHADER_READ_BIT .|. Vk.ACCESS_SHADER_WRITE_BIT))
            dParticleBuffers
        )
        []

    -- Stage 1: grow this frame's tunnel and step the sparks, both on the
    -- GPU. The barrier in front covers every cross-frame buffer hazard —
    -- the previous frame's vertex pulls (WAR on the tunnel SSBO and this
    -- parity's state buffer) and its sim write (RAW on the other) — queue
    -- submission order makes it reach across command buffers.
    Vk.cmdPipelineBarrier
      cb
      (Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT .|. Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT)
      Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
      zero
      []
      [ bufferBarrier Vk.ACCESS_SHADER_READ_BIT Vk.ACCESS_SHADER_WRITE_BIT dVertexBuffer
      , bufferBarrier Vk.ACCESS_SHADER_WRITE_BIT Vk.ACCESS_SHADER_READ_BIT prevParticles
      , bufferBarrier Vk.ACCESS_SHADER_READ_BIT Vk.ACCESS_SHADER_WRITE_BIT curParticles
      ]
      []
    dispatchTunnelGeometry dTunnel dGeometrySet t twist knobs cb
    dispatchParticleSim dParticles (dParticleSimSets V.! parity) t dt knobs cb
    -- The freshly written buffers, handed to this frame's vertex shaders.
    Vk.cmdPipelineBarrier
      cb
      Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
      Vk.PIPELINE_STAGE_VERTEX_SHADER_BIT
      zero
      []
      [ bufferBarrier Vk.ACCESS_SHADER_WRITE_BIT Vk.ACCESS_SHADER_READ_BIT dVertexBuffer
      , bufferBarrier Vk.ACCESS_SHADER_WRITE_BIT Vk.ACCESS_SHADER_READ_BIT curParticles
      ]
      []

    -- Stage 2: the wormhole and the sparks, offscreen, depth-tested.
    transitionColorAttachment cb sceneImage
    transitionDepthAttachment cb depthImage
    Vk.cmdUseRendering cb (sceneRenderingInfo sceneView depthView) do
      applyDynamicStates
        allDynamicStates
        cb
        (dynamicStateFor sExtent)
          { depthTest = True
          , depthWrite = True
          , depthCompareOp = Vk.COMPARE_OP_LESS
          }
      Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS (tpPipeline dTunnel)
      Vk.cmdBindDescriptorSets
        cb
        Vk.PIPELINE_BIND_POINT_GRAPHICS
        (tpPipelineLayout dTunnel)
        0
        [csSet camera]
        []
      Vk.cmdDraw cb (fromIntegral tunnelVertexCount) 1 0 0
      -- The sparks still depth-test against the scene but don't occlude:
      -- both pipelines share the dynamic-state set, so the toggle carries
      -- over the bind. Two draws differing only in pushed constants — the
      -- dim, 180°-rotated echo first, then the swarm, so the brighter
      -- always wins overlaps (without blending, "dim" paints darker, not
      -- more transparent). At full banner phase the echo's brightness
      -- reaches zero: skip the draw rather than stamp black triangles
      -- over the text.
      Vk.cmdSetDepthWriteEnable cb False
      let echoBrightness = 0.3 * (1 - kBanner knobs)
      when (echoBrightness > 0.02) $
        drawParticles dParticles (dParticleDrawSets V.! parity) 1 echoBrightness knobs cb
      drawParticles dParticles (dParticleDrawSets V.! parity) 0 1 knobs cb

    -- Stage 3: hand the scene to compute, open the glitch target, dispatch.
    Vk.cmdPipelineBarrier
      cb
      Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
      zero
      []
      []
      [sceneToComputeRead sceneImage, undefinedToComputeWrite postImage]
    dispatchGlitch dGlitch sExtent (tGlitchSets targets V.! ii) t knobs cb

    -- Stage 4: blit the glitched image onto the swapchain and present.
    Vk.cmdPipelineBarrier
      cb
      Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
      Vk.PIPELINE_STAGE_TRANSFER_BIT
      zero
      []
      []
      [postToTransferSrc postImage, swapchainToTransferDst swapImage]
    blitToSwapchain cb sExtent postImage swapImage
    swapchainToPresent cb swapImage

  queueSubmitFrame vc f imageIndex [commands]
  presentFrameImage vc f acquireResult imageIndex
  where
    Swapchain{sExtent, sImages} = fSwapchain f
    sceneRenderingInfo :: Vk.ImageView -> Vk.ImageView -> Vk.RenderingInfo '[]
    sceneRenderingInfo sceneView depthView =
      Dynamic.renderingInfo
        (fullScissor sExtent)
        [(sceneView, Vk.Float32 0.015 0 0.03 1)]
        (Just (depthView, 1.0)) -- clear depth to the far plane

----------------------------------------------------------------
-- Barriers and the blit
----------------------------------------------------------------

{- | The scene image, just written as a colour attachment, handed to the
glitch dispatch as a read-only storage image (storage access needs @GENERAL@).
-}
sceneToComputeRead :: Vk.Image -> SomeStruct Vk.ImageMemoryBarrier
sceneToComputeRead =
  imageBarrier
    Vk.IMAGE_ASPECT_COLOR_BIT
    Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    Vk.ACCESS_SHADER_READ_BIT
    Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    Vk.IMAGE_LAYOUT_GENERAL

-- | The glitch target → @GENERAL@ for the compute write, discarding contents.
undefinedToComputeWrite :: Vk.Image -> SomeStruct Vk.ImageMemoryBarrier
undefinedToComputeWrite =
  imageBarrier
    Vk.IMAGE_ASPECT_COLOR_BIT
    zero
    Vk.ACCESS_SHADER_WRITE_BIT
    Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_GENERAL

-- | The glitch target after the compute write, ready to be blitted from.
postToTransferSrc :: Vk.Image -> SomeStruct Vk.ImageMemoryBarrier
postToTransferSrc =
  imageBarrier
    Vk.IMAGE_ASPECT_COLOR_BIT
    Vk.ACCESS_SHADER_WRITE_BIT
    Vk.ACCESS_TRANSFER_READ_BIT
    Vk.IMAGE_LAYOUT_GENERAL
    Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL

-- | The swapchain image → transfer destination, discarding contents.
swapchainToTransferDst :: Vk.Image -> SomeStruct Vk.ImageMemoryBarrier
swapchainToTransferDst =
  imageBarrier
    Vk.IMAGE_ASPECT_COLOR_BIT
    zero
    Vk.ACCESS_TRANSFER_WRITE_BIT
    Vk.IMAGE_LAYOUT_UNDEFINED
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

-- | The swapchain image after the blit, ready to present.
swapchainToPresent :: (MonadIO m) => Vk.CommandBuffer -> Vk.Image -> m ()
swapchainToPresent cb image =
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
        image
    ]

-- | 1:1 blit; also converts the offscreen RGBA to the surface format.
blitToSwapchain
  :: (MonadIO m) => Vk.CommandBuffer -> Vk.Extent2D -> Vk.Image -> Vk.Image -> m ()
blitToSwapchain cb (Vk.Extent2D w h) src dst =
  Vk.cmdBlitImage
    cb
    src
    Vk.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    dst
    Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    [ Vk.ImageBlit
        { Vk.srcSubresource = colorLayers
        , Vk.srcOffsets = offsets
        , Vk.dstSubresource = colorLayers
        , Vk.dstOffsets = offsets
        }
    ]
    Vk.FILTER_NEAREST
  where
    offsets = (Vk.Offset3D 0 0 0, Vk.Offset3D (fromIntegral w) (fromIntegral h) 1)
    colorLayers = Vk.ImageSubresourceLayers Vk.IMAGE_ASPECT_COLOR_BIT 0 0 1
