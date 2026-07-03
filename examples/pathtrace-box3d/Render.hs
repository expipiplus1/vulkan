{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The two frame loops over the same simulation: 'animate' renders numbered
PNG frames headlessly (tiled dispatches, fixed physics rate), 'windowed' runs
live in an SDL2 window (wall-clock physics, mouse input, the "Present" pass
onto the swapchain).
-}
module Render
  ( Options (..)
  , animate
  , windowed
  ) where

import qualified Codec.Picture as JP
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, register, release)
import Data.Bits ((.|.))
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word32, Word64)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (poke, sizeOf)
import GHC.Clock (getMonotonicTime)
import Geomancy.UVec2 (uvec2)
import Geomancy.Vec3 (vec3)
import HeadlessBoot (submitAndWaitFor)
import ImageReadback (captureImageRGBA8)
import Linear.Affine (Point (..))
import Linear.V2 (V2 (..))
import qualified SDL
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Random (mkStdGen)
import Text.Printf (printf)
import Vulkan.CStruct.Utils (FixedArray, lowerArrayPtr)
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferDeviceAddressInfo (..), getBufferDeviceAddress)
import qualified Vulkan.Core13 as Vk
import qualified Vulkan.Extensions.VK_KHR_surface as KHR
import Vulkan.Utils.Barrier (transitionColorAttachment, transitionPresent)
import Vulkan.Utils.Descriptors (bufferWrite)
import qualified Vulkan.Utils.DynamicRendering as Dynamic
import Vulkan.Utils.DynamicState (allDynamicStates, applyDynamicStates, dynamicStateFor, fullScissor)
import Vulkan.Utils.Frame (acquireFrameImage, presentFrameImage, queueSubmitFrame, recordCommands)
import qualified Vulkan.Utils.Frame as VkFrame
import Vulkan.Utils.Init.SDL2.Window (drawableSize)
import qualified Vulkan.Utils.SpirV.Array as Array
import Vulkan.Utils.SpirV.DeviceAddress (DeviceAddress (..))
import Vulkan.Utils.Swapchain (Swapchain (..))
import Vulkan.Utils.VulkanContext (VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, noOnFrame, runWindowLoop)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA

import Pathtracer (BvhNode, Camera, Frame (..), Sphere)
import qualified Pathtracer
import qualified Physics
import qualified Present
import qualified Scene

data Options = Options
  { width :: Int
  , height :: Int
  , samples :: Maybe Word32
  -- ^ Samples per pixel; defaults to 512 headless, 16 windowed
  , bounces :: Word32
  , spheres :: Int
  , seed :: Word32
  , fov :: Float
  , fps :: Int
  , duration :: Float
  -- ^ Animation length, seconds
  , orbit :: Float
  -- ^ Camera orbit speed, degrees per second
  , explodeEvery :: Float
  -- ^ Explosion period, seconds (0 disables)
  , popcornEvery :: Float
  -- ^ Popcorn-toss period, seconds (0 disables)
  , popImpulse :: Float
  -- ^ Popcorn impulse per projected area, N·s/m²
  , boomImpulse :: Float
  -- ^ Explosion impulse per projected area, N·s/m²
  , boomRadius :: Float
  -- ^ Explosion full-strength radius, metres
  , timeOfDay :: Float
  -- ^ Starting hour of the 24-hour day
  , dayLength :: Float
  -- ^ Seconds of animation per 24-hour day
  , settle :: Float
  -- ^ Virtual seconds simulated before the first frame
  , timeout :: Double
  -- ^ GPU wait budget, seconds
  , tile :: Int
  {- ^ Tile edge for headless dispatches, pixels (the per-submission work
  cap); 0 sizes it automatically from the sample count
  -}
  , outDir :: FilePath
  , outFfmpeg :: Maybe FilePath
  -- ^ Stitch the frames into this video after rendering (headless only)
  , window :: Bool
  -- ^ Interactive SDL2 window instead of PNG frames
  }

--------------------------------------------------------------------------------
-- Headless frame loop
--------------------------------------------------------------------------------

animate
  :: VMA.Allocator
  -> Vk.Device
  -> Word32
  -> Options
  -> ResourceT IO ()
animate allocator dev computeQueueFamilyIndex opts = do
  let
    width = opts.width
    height = opts.height
    workgroup = Pathtracer.workgroup

    (ground, balls0) = Scene.elevatedScene opts.spheres opts.seed
    sphereCount = 1 + length balls0

    -- 2n-1 nodes for n leaves: the count is fixed, only contents move.
    nodeCount = 2 * sphereCount - 1

    aspect = fromIntegral width / fromIntegral height

  (world, balls) <- liftIO $ Physics.buildWorld ground balls0
  liftIO $ Physics.settleWorld world opts.settle

  -- Output storage buffer: one RGBA32F texel per pixel, mapped GPU_TO_CPU.
  (_, (outBuffer, outAllocation, outInfo)) <-
    VMA.withBuffer
      allocator
      zero
        { Vk.size = fromIntegral $ width * height * 4 * sizeOf (0 :: Float)
        , Vk.usage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
        }
      zero
        { VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , VMA.usage = VMA.MEMORY_USAGE_GPU_TO_CPU
        }
      allocate

  -- Scene and camera buffers are rewritten from the host every frame.
  (_, (sceneBuffer, _sceneAllocation, sceneInfo)) <-
    VMA.withBuffer
      allocator
      zero
        { Vk.size = fromIntegral (sphereCount * Array.std430Stride (Proxy @Sphere))
        , Vk.usage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
        }
      zero
        { VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , VMA.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        }
      allocate

  (_, (camBuffer, _camAllocation, camInfo)) <-
    VMA.withBuffer
      allocator
      zero
        { Vk.size = fromIntegral (sizeOf (cameraAt opts aspect 0))
        , Vk.usage = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
        }
      zero
        { VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , VMA.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        }
      allocate

  let nodeStride = Array.std430Stride (Proxy @BvhNode)
  (_, (nodeBuffer, _nodeAllocation, nodeInfo)) <-
    VMA.withBuffer
      allocator
      zero
        { Vk.size = fromIntegral (nodeCount * nodeStride)
        , Vk.usage =
            Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
              .|. Vk.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
        }
      zero
        { VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , VMA.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        }
      allocate
  bvhBase <- getBufferDeviceAddress dev zero{buffer = nodeBuffer}

  -- Headless renders are offline: default to enough samples to tame the
  -- night scenes, where the glowies are the only light.
  tracer <- Pathtracer.allocatePipeline dev (fromMaybe 512 opts.samples) opts.bounces

  (_, descriptorPool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 1
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 1
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 2
            ]
        }
      Nothing
      allocate
  [descriptorSet] <-
    Vk.allocateDescriptorSets
      dev
      zero
        { Vk.descriptorPool = descriptorPool
        , Vk.setLayouts = [tracer.descriptorSetLayout]
        }

  Vk.updateDescriptorSets
    dev
    [ bufferWrite descriptorSet 0 Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER camBuffer
    , bufferWrite descriptorSet 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER sceneBuffer
    , bufferWrite descriptorSet 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER outBuffer
    ]
    []

  (_, commandPool) <-
    Vk.withCommandPool
      dev
      zero
        { CommandPoolCreateInfo.queueFamilyIndex = computeQueueFamilyIndex
        , CommandPoolCreateInfo.flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
        }
      Nothing
      allocate
  (_, [cb]) <-
    Vk.withCommandBuffers
      dev
      zero
        { Vk.commandPool = commandPool
        , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
      allocate
  computeQueue <- Vk.getDeviceQueue dev computeQueueFamilyIndex 0

  liftIO $ createDirectoryIfMissing True opts.outDir
  let
    dt = 1 / fromIntegral opts.fps :: Float
    frameCount = ceiling (opts.duration * fromIntegral opts.fps) :: Int
    timeoutNanos = round (opts.timeout * 1e9)

    -- Frames render in tiles (the shader's @tileOffset@), one submission
    -- each: a full-frame submission at high sample counts outlives the GPU's
    -- hang-recovery watchdog, which kills the context after ~1 s — no fence
    -- budget (--timeout) can save a submission the driver gave up on, and a
    -- context loss is unrecoverable. Tile area × samples caps the work in a
    -- single submission, so the auto edge shrinks as --samples grows: big
    -- enough to keep the GPU fed (a small dispatch leaves it mostly idle and
    -- a tile's wall time can't go below one pixel's serial sample loop — the
    -- irreducible unit), small enough that the heaviest tile stays several
    -- times under the watchdog. Extreme sample counts converge to the
    -- per-pixel floor whatever the tile size.
    samples = max 1 (fromIntegral (fromMaybe 512 opts.samples)) :: Int
    bounces = max 1 (fromIntegral opts.bounces) :: Int
    tileBudget = 500 * 1000 * 1000 :: Int -- ray-bounces per submission
    autoEdge = floor (sqrt (fromIntegral (tileBudget `div` (samples * bounces)) :: Double))
    tileSize
      | opts.tile > 0 = alignTile opts.tile
      | otherwise = min 256 (alignTile autoEdge)
    alignTile e = max workgroup (e - e `mod` workgroup)

    pixelAddr :: Int -> Int -> Ptr (FixedArray 4 Float)
    pixelAddr x y =
      plusPtr
        (VMA.mappedData outInfo)
        (((y * width) + x) * 4 * sizeOf (0 :: Float))

  genRef <- liftIO $ newIORef (mkStdGen (fromIntegral opts.seed + 42))
  sceneRef <- liftIO (Physics.readScene ground balls >>= newIORef)
  boomTimer <- liftIO $ Physics.newKickTimer opts.explodeEvery
  popTimer <- liftIO $ Physics.newKickTimer opts.popcornEvery
  started <- liftIO getMonotonicTime
  for_ ([0 .. frameCount - 1] :: [Int]) $ \i -> do
    -- Physics: advance the world to this frame (frame 0 is the initial pose),
    -- then fold the step's move events into the persistent scene vector.
    let t = fromIntegral i * dt
    spheres <- liftIO $ do
      when (i > 0) $ do
        Physics.tickKick boomTimer opts.explodeEvery t (Physics.explode opts.boomImpulse opts.boomRadius world)
        Physics.tickKick popTimer opts.popcornEvery t (Physics.popcorn opts.popImpulse genRef balls)
        Physics.stepWorld world dt
      scene <- readIORef sceneRef >>= Physics.applyMoveEvents world
      writeIORef sceneRef scene
      Physics.beaconize
        <$> Physics.kickEnvelope boomTimer t
        <*> Physics.kickEnvelope popTimer t
        <*> pure scene

    -- Transfer: the scene -> Scene SSBO; rebuilt BVH -> node buffer; orbited
    -- camera -> UBO. All three are host-mapped and the GPU is idle between
    -- frames, so plain pokes suffice.
    let bvhFlats = Scene.flattenBvh (Scene.buildBvh (zip [0 ..] (map Scene.sphereAabb (VS.toList spheres))))
    liftIO $ do
      Array.pokeStd430 (VMA.mappedData sceneInfo) spheres
      Array.pokeStd430
        (VMA.mappedData nodeInfo)
        (VS.fromList (map (Scene.toBvhNode bvhBase nodeStride) bvhFlats))
      poke
        (castPtr (VMA.mappedData camInfo))
        (cameraAt opts aspect (fromIntegral i * dt))

    -- Compute: tiled dispatches render the frame (see 'tileSize').
    let
      frame =
        Frame
          { root = DeviceAddress bvhBase
          , resolution = uvec2 (fromIntegral width) (fromIntegral height)
          , seed = opts.seed + fromIntegral i * 9781
          , tileOffset = uvec2 0 0 -- set per tile below
          , pad0 = 0
          }
      renderTile :: Int -> Int -> ResourceT IO ()
      renderTile x0 y0 = do
        let
          colsThis = min tileSize (width - x0)
          rowsThis = min tileSize (height - y0)
          tileFrame = frame{tileOffset = uvec2 (fromIntegral x0) (fromIntegral y0)}
        Vk.resetCommandBuffer cb zero
        Vk.useCommandBuffer cb zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} do
          Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE tracer.pipeline
          Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE tracer.pipelineLayout 0 [descriptorSet] []
          liftIO $ with tileFrame $ \pFrame ->
            Vk.cmdPushConstants
              cb
              tracer.pipelineLayout
              Vk.SHADER_STAGE_COMPUTE_BIT
              0
              (fromIntegral (sizeOf tileFrame))
              (castPtr pFrame)
          Vk.cmdDispatch
            cb
            (ceiling (realToFrac colsThis / realToFrac @_ @Float workgroup))
            (ceiling (realToFrac rowsThis / realToFrac @_ @Float workgroup))
            1
        submitAndWaitFor timeoutNanos dev computeQueue cb $
          "Timed out waiting for compute at frame "
            <> show i
            <> ", tile "
            <> show (x0, y0)
            <> " after "
            <> show opts.timeout
            <> "s (raise --timeout)"
    sequence_
      [ renderTile x0 y0
      | y0 <- [0, tileSize .. height - 1] :: [Int]
      , x0 <- [0, tileSize .. width - 1] :: [Int]
      ]

    -- Readback: tonemap to RGBA8 and write the numbered frame.
    image <- captureImageRGBA8 allocator outAllocation width height $ \x y -> do
      let ptr = pixelAddr x y
      [r, g, b, a] <- fmap (\f -> round (min 1 f * 255)) <$> peekArray 4 (lowerArrayPtr ptr)
      pure $ JP.PixelRGBA8 r g b a
    liftIO $
      BSL.writeFile
        (opts.outDir </> printf "frame-%05d.png" i)
        (JP.encodePng image)

    liftIO $ when ((i + 1) `mod` opts.fps == 0) $ do
      now <- getMonotonicTime
      printf
        "frame %d/%d (%.1f frames/s)\n"
        (i + 1)
        frameCount
        (fromIntegral (i + 1) / (now - started))

  liftIO $ Physics.destroyWorld world

--------------------------------------------------------------------------------
-- Windowed interactive mode
--------------------------------------------------------------------------------

{- | One frame in flight's host-written buffers (scene, camera, BVH nodes).
Two frames overlap (record + execute), so two 'Side's alternate by frame
parity: by the time a parity comes around again, its previous user's GPU work
has retired.
-}
data Side = Side
  { sceneBuffer :: Vk.Buffer
  , scenePtr :: Ptr ()
  , camBuffer :: Vk.Buffer
  , camPtr :: Ptr ()
  , nodePtr :: Ptr ()
  , bvhBase :: Word64
  }

{- | Per-swapchain state: the extent-sized texel buffer the tracer renders
into and the descriptor sets binding it (for the tracer and the present pass).
-}
data Bindings = Bindings
  { traceSets :: V.Vector Vk.DescriptorSet
  -- ^ One per 'Side'.
  , presentSet :: Vk.DescriptorSet
  }

windowed :: VulkanContext -> VMA.Allocator -> Swapchain -> SDL.Window -> Options -> ResourceT IO ()
windowed vc vma initialSC sdlWindow opts = do
  let dev = vcDevice vc

  -- The physics world and per-frame host state, exactly as headless.
  let
    (ground, balls0) = Scene.elevatedScene opts.spheres opts.seed
    sphereCount = 1 + length balls0
    nodeCount = 2 * sphereCount - 1
    nodeStride = Array.std430Stride (Proxy @BvhNode)
  (world, balls) <- liftIO $ Physics.buildWorld ground balls0
  liftIO $ Physics.settleWorld world opts.settle
  sceneRef <- liftIO (Physics.readScene ground balls >>= newIORef)
  genRef <- liftIO $ newIORef (mkStdGen (fromIntegral opts.seed + 42))
  epoch <- liftIO getMonotonicTime
  clockRef <- liftIO (newIORef epoch)
  mouseRef <-
    liftIO . newIORef $
      (P (V2 (fromIntegral (opts.width `div` 2)) (fromIntegral (opts.height `div` 2))) :: Point V2 Int32)
  -- The @--explode-every@/@--popcorn-every@ timers, as in headless mode;
  -- clicks fire through the same timers so the beacon fades after them too.
  boomTimer <- liftIO $ Physics.newKickTimer opts.explodeEvery
  popTimer <- liftIO $ Physics.newKickTimer opts.popcornEvery
  -- The last picked sphere's scene index (every click raycasts the world).
  selectedRef <- liftIO $ newIORef (Nothing :: Maybe Int)

  sides <- V.replicateM 2 $ do
    (_, (sceneBuffer, _, sceneInfo)) <-
      VMA.withBuffer
        vma
        zero
          { Vk.size = fromIntegral (sphereCount * Array.std430Stride (Proxy @Sphere))
          , Vk.usage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
          }
        hostWritten
        allocate
    (_, (camBuffer, _, camInfo)) <-
      VMA.withBuffer
        vma
        zero
          { Vk.size = fromIntegral (sizeOf (undefined :: Camera))
          , Vk.usage = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
          }
        hostWritten
        allocate
    (_, (nodeBuffer, _, nodeInfo)) <-
      VMA.withBuffer
        vma
        zero
          { Vk.size = fromIntegral (nodeCount * nodeStride)
          , Vk.usage =
              Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
                .|. Vk.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT
          }
        hostWritten
        allocate
    bvhBase <- getBufferDeviceAddress dev zero{buffer = nodeBuffer}
    pure
      Side
        { sceneBuffer
        , scenePtr = VMA.mappedData sceneInfo
        , camBuffer
        , camPtr = VMA.mappedData camInfo
        , nodePtr = VMA.mappedData nodeInfo
        , bvhBase
        }

  -- The tracer pipeline, as headless (interactive sample counts); the present
  -- pipeline alongside it, targeting the swapchain's format.
  tracer <- Pathtracer.allocatePipeline dev (fromMaybe 16 opts.samples) opts.bounces
  present <- Present.allocatePipeline dev (KHR.format (sFormat initialSC))

  let
    mkBindings :: Swapchain -> ResourceT IO (Bindings, ReleaseKey)
    mkBindings sc = do
      let Vk.Extent2D scW scH = sExtent sc
      (outKey, (outBuffer, _, _)) <-
        VMA.withBuffer
          vma
          zero
            { Vk.size = fromIntegral scW * fromIntegral scH * 4 * fromIntegral (sizeOf (0 :: Float))
            , Vk.usage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT
            }
          zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}
          allocate
      (poolKey, descriptorPool) <-
        Vk.withDescriptorPool
          dev
          zero
            { Vk.maxSets = 3
            , Vk.poolSizes =
                [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 2
                , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 5
                ]
            }
          Nothing
          allocate
      [trace0, trace1, presentSet] <-
        Vk.allocateDescriptorSets
          dev
          zero
            { Vk.descriptorPool = descriptorPool
            , Vk.setLayouts =
                V.fromList ([tracer.descriptorSetLayout, tracer.descriptorSetLayout] <> map snd present.setLayouts)
            }
      Vk.updateDescriptorSets
        dev
        ( V.fromList $
            concat
              [ [ bufferWrite tset 0 Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER side.camBuffer
                , bufferWrite tset 1 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER side.sceneBuffer
                , bufferWrite tset 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER outBuffer
                ]
              | (tset, side) <- zip [trace0, trace1] (V.toList sides)
              ]
              ++ [bufferWrite presentSet 0 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER outBuffer]
        )
        []
      bindingsKey <- register (mapM_ release ([poolKey, outKey] :: [ReleaseKey]))
      pure
        ( Bindings
            { traceSets = V.fromList [trace0, trace1]
            , presentSet = presentSet
            }
        , bindingsKey
        )

    renderFrame :: Bindings -> VkFrame.Frame -> ResourceT IO ()
    renderFrame bindings f = do
      (acquireResult, imageIndex) <- acquireFrameImage vc f
      let
        sc = VkFrame.fSwapchain f
        Vk.Extent2D scW scH = sExtent sc
        swapImage = sImages sc V.! fromIntegral imageIndex
        swapView = sImageViews sc V.! fromIntegral imageIndex
        parity = fromIntegral (VkFrame.fIndex f `mod` 2) :: Int
        side = sides V.! parity
        groups n = (n + fromIntegral Pathtracer.workgroup - 1) `div` fromIntegral Pathtracer.workgroup

      -- Input: the mouse position (fed by 'pollInput') orbits the camera.
      P (V2 mouseX mouseY) <- liftIO (readIORef mouseRef)
      V2 winW winH <- SDL.get (SDL.windowSize sdlWindow)
      (spheres, elapsed) <- liftIO $ do
        now <- getMonotonicTime
        let sinceEpoch = realToFrac (now - epoch) :: Float
        -- The periodic kicks, dispatched to the same handlers as the clicks
        -- (console messages included).
        Physics.tickKick boomTimer opts.explodeEvery sinceEpoch (Physics.explode opts.boomImpulse opts.boomRadius world)
        Physics.tickKick popTimer opts.popcornEvery sinceEpoch (Physics.popcorn opts.popImpulse genRef balls)

        -- Physics: step by the (clamped) wall-clock delta.
        before <- readIORef clockRef
        writeIORef clockRef now
        Physics.stepWorld world (realToFrac (min 0.1 (now - before)))
        scene <- readIORef sceneRef >>= Physics.applyMoveEvents world
        writeIORef sceneRef scene
        pulsed <-
          Physics.beaconize
            <$> Physics.kickEnvelope boomTimer sinceEpoch
            <*> Physics.kickEnvelope popTimer sinceEpoch
            <*> pure scene
        pure (pulsed, sinceEpoch)

      -- Transfer: this parity's buffers were last touched two frames ago,
      -- and that frame's GPU work has fully retired (see 'Side').
      let
        bvhFlats = Scene.flattenBvh (Scene.buildBvh (zip [0 ..] (map Scene.sphereAabb (VS.toList spheres))))
        camera =
          cameraFromMouse
            opts
            (fromIntegral scW / fromIntegral scH)
            elapsed
            (fromIntegral mouseX / fromIntegral (max 1 winW))
            (fromIntegral mouseY / fromIntegral (max 1 winH))
      liftIO $ do
        Array.pokeStd430 side.scenePtr spheres
        Array.pokeStd430 side.nodePtr (VS.fromList (map (Scene.toBvhNode side.bvhBase nodeStride) bvhFlats))
        poke (castPtr side.camPtr) camera

      -- Compute: trace into the texel buffer, then draw it onto the swapchain
      -- image with the fullscreen present pass. One queue, so plain barriers
      -- order everything — including against the previous frame's submissions.
      let framePush =
            Frame
              { root = DeviceAddress side.bvhBase
              , resolution = uvec2 scW scH
              , seed = opts.seed + fromIntegral (VkFrame.fIndex f) * 9781
              , tileOffset = uvec2 0 0 -- interactive sample counts fit one dispatch
              , pad0 = 0
              }
      commands <- recordCommands vc f \cb -> do
        -- The previous frame's present pass read of the shared texel buffer
        -- must complete before this frame's tracer overwrites it.
        Vk.cmdPipelineBarrier
          cb
          Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT
          Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
          zero
          [memoryBarrier Vk.ACCESS_SHADER_READ_BIT Vk.ACCESS_SHADER_WRITE_BIT]
          []
          []
        Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE tracer.pipeline
        Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE tracer.pipelineLayout 0 [bindings.traceSets V.! parity] []
        liftIO $ with framePush $ \pFrame ->
          Vk.cmdPushConstants
            cb
            tracer.pipelineLayout
            Vk.SHADER_STAGE_COMPUTE_BIT
            0
            (fromIntegral (sizeOf framePush))
            (castPtr pFrame)
        Vk.cmdDispatch cb (groups scW) (groups scH) 1

        -- Tracer writes → the present pass's fragment reads.
        Vk.cmdPipelineBarrier
          cb
          Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT
          Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT
          zero
          [memoryBarrier Vk.ACCESS_SHADER_WRITE_BIT Vk.ACCESS_SHADER_READ_BIT]
          []
          []
        transitionColorAttachment cb swapImage
        Vk.cmdUseRendering
          cb
          (Dynamic.colorAttachmentRenderingInfo (fullScissor (sExtent sc)) swapView (Vk.Float32 0 0 0 1))
          do
            applyDynamicStates allDynamicStates cb (dynamicStateFor (sExtent sc))
            Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_GRAPHICS present.pipeline
            Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_GRAPHICS present.pipelineLayout 0 [bindings.presentSet] []
            liftIO $ with scW $ \pWidth ->
              Vk.cmdPushConstants
                cb
                present.pipelineLayout
                Vk.SHADER_STAGE_FRAGMENT_BIT
                0
                (fromIntegral (sizeOf scW))
                (castPtr pWidth)
            Vk.cmdDraw cb 3 1 0 0
        transitionPresent cb swapImage
      queueSubmitFrame vc f imageIndex [commands]
      presentFrameImage vc f acquireResult imageIndex

    -- Event-driven input, doubling as the loop's quit poller: the
    -- SDL_GetMouseState-style queries proved unreliable on the Wayland
    -- backend, so clicks and motion come from the event queue instead
    -- (button-press events are edges by definition).
    pollInput :: IO Bool
    pollInput = do
      events <- SDL.pollEvents
      for_ events $ \event -> case SDL.eventPayload event of
        SDL.MouseButtonEvent mb
          | SDL.mouseButtonEventMotion mb == SDL.Pressed -> do
              now <- getMonotonicTime
              let sinceEpoch = realToFrac (now - epoch)
              -- Every click first picks: rebuild the frame's camera from the
              -- click position (which steers the orbit, so the cursor pixel
              -- is self-consistent) and raycast before the kick shuffles
              -- the pile.
              V2 winW winH <- SDL.get (SDL.windowSize sdlWindow)
              let
                P (V2 px py) = SDL.mouseButtonEventPos mb
                nx = fromIntegral px / fromIntegral (max 1 winW)
                ny = fromIntegral py / fromIntegral (max 1 winH)
                aspect = fromIntegral winW / fromIntegral (max 1 winH)
                cam = cameraFromMouse opts aspect sinceEpoch nx ny
              scene <- readIORef sceneRef
              writeIORef selectedRef =<< Physics.pickSphere world scene cam nx ny
              case SDL.mouseButtonEventButton mb of
                SDL.ButtonLeft -> Physics.fireKick popTimer sinceEpoch (Physics.popcorn opts.popImpulse genRef balls)
                SDL.ButtonRight -> Physics.fireKick boomTimer sinceEpoch (Physics.explode opts.boomImpulse opts.boomRadius world)
                _ -> pure ()
        SDL.MouseMotionEvent mm ->
          writeIORef mouseRef (SDL.mouseMotionEventPos mm)
        _ -> pure ()
      pure (any isQuitEvent events)

    -- Window close, Q, or Escape — as Vulkan.Utils.Init.SDL2.Window.shouldQuit.
    isQuitEvent :: SDL.Event -> Bool
    isQuitEvent event = case SDL.eventPayload event of
      SDL.QuitEvent -> True
      SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released False (SDL.Keysym _ code _)) ->
        code == SDL.KeycodeQ || code == SDL.KeycodeEscape
      _ -> False

  runWindowLoop
    vc
    initialSC
    (drawableSize sdlWindow)
    pollInput
    WindowLoop
      { wlMkState = mkBindings
      , wlRender = renderFrame
      , wlOnFrame = noOnFrame
      , wlOnExit = noOnExit
      }
  liftIO $ Physics.destroyWorld world
  where
    hostWritten =
      zero
        { AllocationCreateInfo.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , AllocationCreateInfo.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        }

--------------------------------------------------------------------------------
-- Cameras
--------------------------------------------------------------------------------

{- | The orbiting look-at camera: the classic RTOW viewpoint swung around the
scene's Y axis at @--orbit@ degrees per second, under the sky of the moment.
-}
cameraAt :: Options -> Float -> Float -> Camera
cameraAt opts aspect t =
  Scene.buildCamera aspect opts.fov lookFrom (vec3 0 0.5 0) (vec3 0 1 0) (hoursAt opts t)
  where
    a0 = atan2 3 13
    a = a0 + opts.orbit * pi / 180 * t
    radius = sqrt (13 * 13 + 3 * 3)
    lookFrom = vec3 (radius * cos a) 2 (radius * sin a)

{- | The RTOW viewpoint steered by the mouse: horizontal position swings the
camera around the scene's Y axis (a full turn across the window), vertical
position raises it between grazing and looking down at the pile.
-}
cameraFromMouse :: Options -> Float -> Float -> Float -> Float -> Camera
cameraFromMouse opts aspect elapsed nx ny =
  Scene.buildCamera aspect opts.fov lookFrom (vec3 0 0.5 0) (vec3 0 1 0) (hoursAt opts elapsed)
  where
    a = atan2 3 13 + (nx - 0.5) * 2 * pi
    radius = sqrt (13 * 13 + 3 * 3)
    height = 0.75 + (1 - min 1 (max 0 ny)) * 7
    lookFrom = vec3 (radius * cos a) height (radius * sin a)

{- | The time-of-day clock: @--time-of-day@ hours plus @--day-length@-scaled
elapsed seconds.
-}
hoursAt :: Options -> Float -> Float
hoursAt opts elapsed = opts.timeOfDay + elapsed * 24 / max 1 opts.dayLength

memoryBarrier :: Vk.AccessFlags -> Vk.AccessFlags -> Vk.MemoryBarrier
memoryBarrier srcAccess dstAccess =
  zero{Vk.srcAccessMask = srcAccess, Vk.dstAccessMask = dstAccess}
