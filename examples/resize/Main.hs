{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| Julia-set viewer, driven by a per-frame 'FG.FrameGraph' the same way
regardless of queue topology.

The fractal only changes when the window resizes or the cursor moves; its
steady state is "re-present the same image". Each frame declares a graph of up
to three passes — @julia@ (compute → offscreen), @blit@ (offscreen → swapchain)
and a terminal 'presentSwapchain' (swapchain → PRESENT_SRC) — adding @julia@ only when the fractal
parameters changed and @blit@ only when the acquired swapchain image does not
already hold the current fractal. fragr places exactly the barriers the
surviving passes need: a fully idle frame records /nothing/ and just re-presents.

The one topology-dependent line is @'FG.setQueue' computeQueueId@ on the
@julia@ pass, where @computeQueueId@ is chosen once at startup: the graphics
queue when compute shares its family, an async-compute queue when it doesn't.
Everything else — pruning, barrier placement, per-queue command routing and
the cross-queue submit handshake — falls out of 'submitGraphQueued': on shared
hardware every pass lands on one queue and it degenerates to a single command
buffer and submit; on async hardware the graphics submit waits the compute
one's timeline at the stages the schedule says it hands over. The offscreen
image is CONCURRENT across the two families, so no queue-family ownership
transfer is needed — only that handshake, and only on the (rare) frames that
recompute. What stays app-side is strictly frame-level ('executeAdaptive').
-}
module Main
  ( main
  ) where

import Blit (blitImage)
import Control.Exception (handle)
import Control.Lens.Getter ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits ((.|.))
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import qualified Fragr as FG
import Julia (allocateJuliaDescriptorSets, allocateJuliaPipeline, juliaWorkgroupX, juliaWorkgroupY)
import Linear.Affine (Point (..))
import Linear.Metric (norm)
import Linear.V2
import qualified SDL
import Say (sayErrString)
import UnliftIO.Exception (displayException, mask_)
import UnliftIO.Foreign (allocaBytes, plusPtr, poke)
import qualified Vulkan.Core10 as Vk
import Vulkan.Exception
import Vulkan.Utils.Frame (Frame (..), SubmitExtras (..), acquireFrameImage, noExtras, presentFrameImage)
import Vulkan.Utils.FrameGraph.Driver (QueueSlot (..), SubmitConfig (..), frameSubmitConfig, submitGraphQueued)
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), Usage (..), importManagedImage, newManagedImage, sharedAcrossQueues)
import Vulkan.Utils.FrameGraph.Recorder (Recorder, recordingCommandBuffer)
import Vulkan.Utils.FrameGraph.Swapchain (importSwapchain, newSwapchainImages, presentSwapchain)
import Vulkan.Utils.Init.SDL2.Window (createWindow, drawableSize, sdl2Adapter, shouldQuit, withSDL)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.Swapchain (Swapchain (..), SwapchainConfig (..), defaultSwapchainConfig, unormEncoding)
import Vulkan.Utils.VulkanContext (RecycledResources (..), VulkanContext (..))
import Vulkan.Utils.WindowLoop (WindowLoop (..), noOnExit, runWindowLoop)
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA
import WindowedBoot (WindowedConfig (..), withWindowedVk)

main :: IO ()
main = prettyError . runResourceT $ do
  withSDL

  let
    initWidth = 1280
    initHeight = 720

  sdlWindow <- createWindow "Haskell ❤️ Vulkan" initWidth initHeight
  SDL.showWindow sdlWindow

  (vc, vma, initialSC) <- withWindowedVk windowConfig (sdl2Adapter sdlWindow)
  let dev = vcDevice vc

  juliaPL <- allocateJuliaPipeline dev

  -- One startup decision: does compute get its own queue family? If so, the
  -- julia pass runs on QueueId 1 and hands the offscreen image over with a
  -- timeline semaphore; otherwise everything stays on the graphics queue.
  topology <- detectTopology vc
  let sharedFamilies = fmap (\as -> (as.graphicsFamily, as.computeFamily)) topology

  -- Colour phase, advanced once per recompute; global so it survives resizes.
  colorRef <- liftIO (newIORef 0)

  runWindowLoop
    vc
    initialSC
    (drawableSize sdlWindow)
    (shouldQuit sdlWindow)
    WindowLoop
      { wlMkState = allocateBindings dev vma juliaPL sharedFamilies
      , wlRender = \bindings f -> renderJulia vc juliaPL topology colorRef bindings f
      , wlOnFrame = \start end -> reportFrameTime (end - start)
      , wlOnExit = noOnExit
      }

windowConfig :: WindowedConfig
windowConfig =
  WindowedConfig
    { appName = "Haskell ❤️ Vulkan"
    , instanceReqs = []
    , deviceReqs = []
    , vmaFlags = zero
    , swapchainConfig =
        defaultSwapchainConfig
          { scRequiredUsageFlags =
              -- TRANSFER_DST for the blit; COLOR_ATTACHMENT so the
              -- swapchain helper can still build (view-compatible) image
              -- views for each image.
              [ Vk.IMAGE_USAGE_TRANSFER_DST_BIT
              , Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
              ]
          , scRequiredFormatFeatures = [Vk.FORMAT_FEATURE_BLIT_DST_BIT]
          , scSurfaceFormatPreferences = [unormEncoding]
          }
    }

prettyError :: IO () -> IO ()
prettyError =
  handle (\e@(VulkanException _) -> sayErrString (displayException e))

----------------------------------------------------------------
-- Queue topology
----------------------------------------------------------------

{- | Everything the async-compute path needs, decided once from the queue
layout. Absent (the frame stays single-queue) when compute shares the graphics
family.
-}
data AsyncSetup = AsyncSetup
  { graphicsFamily :: Word32
  , computeFamily :: Word32
  , computeQueue :: Vk.Queue
  , lastBlitDone :: IORef Word64
  {- ^ Frame index of the last blit that read the shared offscreen image; the
  next compute waits for it before overwriting (cross-frame write-after-read).
  -}
  }

detectTopology :: (MonadResource m) => VulkanContext -> m (Maybe AsyncSetup)
detectTopology vc = do
  let
    QueueFamilyIndex graphicsFamily = fst (qGraphics (vcQueues vc))
    QueueFamilyIndex computeFamily = fst (qCompute (vcQueues vc))
  if graphicsFamily == computeFamily
    then pure Nothing
    else do
      lastBlitDone <- liftIO (newIORef 0)
      pure $
        Just
          AsyncSetup
            { graphicsFamily
            , computeFamily
            , computeQueue = snd (qCompute (vcQueues vc))
            , lastBlitDone
            }

-- | The queue the julia pass runs on under an async topology (see 'computeQueueId').
computeQueue :: FG.QueueId
computeQueue = FG.QueueId 1

-- | The queue the julia pass is assigned to (async family, or the graphics queue).
computeQueueId :: Maybe AsyncSetup -> FG.QueueId
computeQueueId = maybe FG.defaultQueue (const computeQueue)

----------------------------------------------------------------
-- Per-swapchain bindings
----------------------------------------------------------------

data Bindings = Bindings
  { offscreen :: ManagedImage
  {- ^ The single compute target, imported so its tracked layout persists across
  frames (recreated per swapchain because its extent tracks the window).
  -}
  , juliaDescriptorSet :: Vk.DescriptorSet
  , swapImages :: Vector ManagedImage
  -- ^ One layout-tracked wrapper per swapchain image.
  , lastConstants :: IORef (Maybe JuliaConstants)
  -- ^ Fractal parameters last computed; a change makes the frame dirty.
  , freshImages :: IORef IntSet
  -- ^ Swapchain image indices that already hold the current fractal.
  }

allocateBindings
  :: Vk.Device
  -> VMA.Allocator
  -> Pipeline
  -> Maybe (Word32, Word32)
  -- ^ (graphics, compute) families to share the offscreen image across, if async.
  -> Swapchain
  -> ResourceT IO (Bindings, ReleaseKey)
allocateBindings dev allocator jp sharedFamilies sc = do
  -- A single offscreen RGBA8 storage image (+ view). Compute writes here; a
  -- blit then copies (and converts RGBA→BGRA) to the acquired swapchain image.
  (imageKey, (image, _, _)) <-
    VMA.withImage allocator (offscreenImageInfo sharedFamilies (sExtent sc)) offscreenAllocInfo allocate
  (viewKey, view) <- Vk.withImageView dev (offscreenViewInfo image) Nothing allocate

  (poolKey, juliaSets) <-
    allocateJuliaDescriptorSets dev jp [view]

  -- CONCURRENT under an async topology (see 'offscreenImageInfo'), which is
  -- also the only topology that accesses it across queues.
  offscreen <-
    maybe id (const sharedAcrossQueues) sharedFamilies
      <$> newManagedImage image Vk.IMAGE_ASPECT_COLOR_BIT
  swapImages <- newSwapchainImages sc
  lastConstants <- liftIO (newIORef Nothing)
  freshImages <- liftIO (newIORef IntSet.empty)

  -- runWindowLoop fires exactly one release key on resize: free the pool (and
  -- its sets) first, then the view, then the image. The swapchain images belong
  -- to the swapchain, so their wrappers need no release.
  bindingsKey <- register (mapM_ release ([poolKey, viewKey, imageKey] :: [ReleaseKey]))

  pure
    ( Bindings
        { offscreen
        , juliaDescriptorSet = V.head juliaSets
        , swapImages
        , lastConstants
        , freshImages
        }
    , bindingsKey
    )

offscreenFormat :: Vk.Format
offscreenFormat = Vk.FORMAT_R8G8B8A8_UNORM

offscreenImageInfo :: Maybe (Word32, Word32) -> Vk.Extent2D -> Vk.ImageCreateInfo '[]
offscreenImageInfo sharedFamilies (Vk.Extent2D w h) =
  zero
    { Vk.imageType = Vk.IMAGE_TYPE_2D
    , Vk.format = offscreenFormat
    , Vk.extent = Vk.Extent3D w h 1
    , Vk.mipLevels = 1
    , Vk.arrayLayers = 1
    , Vk.samples = Vk.SAMPLE_COUNT_1_BIT
    , Vk.tiling = Vk.IMAGE_TILING_OPTIMAL
    , Vk.usage =
        Vk.IMAGE_USAGE_STORAGE_BIT .|. Vk.IMAGE_USAGE_TRANSFER_SRC_BIT
    , Vk.initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
    , -- Shared across the compute and graphics families (async path) so the
      -- handover needs only a timeline wait, no ownership transfer.
      Vk.sharingMode = maybe Vk.SHARING_MODE_EXCLUSIVE (const Vk.SHARING_MODE_CONCURRENT) sharedFamilies
    , Vk.queueFamilyIndices = maybe [] (\(g, c) -> [g, c]) sharedFamilies
    }

offscreenAllocInfo :: VMA.AllocationCreateInfo
offscreenAllocInfo = zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}

offscreenViewInfo :: Vk.Image -> Vk.ImageViewCreateInfo '[]
offscreenViewInfo image =
  zero
    { Vk.image = image
    , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
    , Vk.format = offscreenFormat
    , Vk.subresourceRange = colorSubresourceRange
    }

colorSubresourceRange :: Vk.ImageSubresourceRange
colorSubresourceRange =
  Vk.ImageSubresourceRange
    { Vk.aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
    , Vk.baseMipLevel = 0
    , Vk.levelCount = 1
    , Vk.baseArrayLayer = 0
    , Vk.layerCount = 1
    }

----------------------------------------------------------------
-- Per-frame rendering
----------------------------------------------------------------

renderJulia
  :: VulkanContext
  -> Pipeline
  -> Maybe AsyncSetup
  -> IORef Float
  -- ^ colour-scheme phase, shared across swapchains so it survives resizes
  -> Bindings
  -> Frame
  -> ResourceT IO ()
renderJulia vc jp topology colorRef bindings f = do
  constants <- computeConstants (sExtent sc)
  lastConstants <- liftIO (readIORef bindings.lastConstants)
  let dirty = Just constants /= lastConstants

  (acquireResult, imageIndex) <- acquireFrameImage vc f
  freshImages <- liftIO (readIORef bindings.freshImages)
  let
    ix = fromIntegral imageIndex :: Int
    -- Compute only on a parameter change; blit unless this image already holds
    -- the current fractal (a dirty frame invalidates every other image).
    needBlit = dirty || not (IntSet.member ix freshImages)

  graph <- FG.newFrameGraph
  offscreenH <- importManagedImage graph "offscreen" bindings.offscreen
  (swapchainH, swapManaged) <- importSwapchain graph bindings.swapImages imageIndex

  colorPhase <- liftIO (readIORef colorRef)
  offscreenReady <-
    if dirty
      then FG.addPass graph "julia" (juliaSetup offscreenH) \_written -> do
        cb <- recordingCommandBuffer
        dispatchJulia jp (sExtent sc) constants colorPhase bindings.juliaDescriptorSet cb
      else pure offscreenH

  swapchainReady <-
    if needBlit
      then FG.addPass graph "blit" (blitSetup offscreenReady swapchainH) \_blitted -> do
        cb <- recordingCommandBuffer
        blitImage (sExtent sc) bindings.offscreen.image swapManaged.image cb
      else pure swapchainH

  -- Always present, even when every render pass got culled (an idle re-present).
  presentSwapchain graph swapchainReady

  FG.compile graph
  executeAdaptive vc f imageIndex topology dirty needBlit graph
  presentFrameImage vc f acquireResult imageIndex

  liftIO $ do
    when dirty $ do
      writeIORef bindings.lastConstants (Just constants)
      -- Advance the colour phase once per recompute, so the palette visibly
      -- rotates exactly on the frames that render (and freezes when idle).
      modifyIORef' colorRef (+ colorStep)
    when needBlit $
      modifyIORef' bindings.freshImages $
        if dirty then const (IntSet.singleton ix) else IntSet.insert ix
  where
    sc = fSwapchain f
    juliaSetup offscreenH = do
      FG.setQueue (computeQueueId topology)
      FG.writeWith offscreenH (StorageWrite Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT)
    blitSetup offscreenReady swapchainH = do
      FG.readWith offscreenReady TransferSrc
      FG.writeWith swapchainH TransferDst

----------------------------------------------------------------
-- Frame-level submit policy (Layer 3)
--
-- The intra-frame cross-queue handshake is 'submitGraphQueued''s, derived
-- from the compiled schedule; only what spans frames stays here.
----------------------------------------------------------------

{- | Submit the compiled graph with this frame's extras.

The swapchain semaphores and host timeline ride the graphics submit, the
cross-frame WAR wait the compute one (specific to reusing one offscreen
image), and the returned completions feed the frame recycler.
-}
executeAdaptive
  :: VulkanContext
  -> Frame
  -> Word32
  -> Maybe AsyncSetup
  -> Bool
  -- ^ whether the julia (compute) pass ran this frame
  -> Bool
  -- ^ whether this frame blits (reads the offscreen image) — for the WAR fence
  -> FG.FrameGraph Recorder ()
  -> ResourceT IO ()
executeAdaptive vc f imageIndex topology dirty didBlit graph = do
  let dev = vcDevice vc

  -- The julia pass is the only compute-queue pass, added iff @dirty@ (and dirty
  -- implies needBlit, so it always survives culling): compute runs exactly when
  -- an async topology is present and the frame is dirty. Its buffer comes from
  -- the frame's compute pool (recycled with the frame, never rebuilt); the WAR
  -- read travels with the slot so the compute extras can never lose it.
  computeSlot <- case topology of
    Just as | dirty -> do
      prevBlitDone <- liftIO (readIORef as.lastBlitDone)
      pure (Just (as.computeQueue, qCompute (rrCommandPools (fRecycled f)), prevBlitDone, as.computeFamily))
    _ -> pure Nothing

  let
    QueueFamilyIndex graphicsFam = fst (qGraphics (vcQueues vc))
    queueTable q = case computeSlot of
      Just (queue, pool, _, fam) | q == computeQueue -> QueueSlot{queue, family = fam, pool}
      _ ->
        QueueSlot
          { queue = snd (qGraphics (vcQueues vc))
          , family = graphicsFam
          , pool = qGraphics (rrCommandPools (fRecycled f))
          }
    base = frameSubmitConfig dev f imageIndex queueTable
    extrasFor q
      -- The cross-frame WAR wait: the offscreen image the julia pass overwrites
      -- may still feed a previous frame's in-flight blit — a hazard between
      -- graphs, invisible to this frame's schedule.
      | Just (_, _, prevBlitDone, _) <- computeSlot
      , q == computeQueue =
          noExtras{waits = [(fHostTimeline f, Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT, prevBlitDone)]}
      | otherwise = base.extras q

  mask_ do
    -- The base config registers every queue's completion into fGPUWork before
    -- anything submits, so the recycler waits the whole graph — even a mid-way
    -- submit failure only costs it the wait timeout.
    _ <- submitGraphQueued base{extras = extrasFor} graph
    liftIO do
      -- The WAR fence must see every blit, not just dirty frames': a clean frame
      -- re-blitting a stale swapchain image still reads the offscreen image, and
      -- the next compute waits the timeline only up to the last recorded value.
      for_ topology \as ->
        when didBlit $ writeIORef as.lastBlitDone (fIndex f)

----------------------------------------------------------------
-- Julia dispatch
----------------------------------------------------------------

-- | The mouse-and-extent-derived fractal parameters pushed to the compute shader.
data JuliaConstants = JuliaConstants
  { scale :: V2 Float
  , offset :: V2 Float
  , c :: V2 Float
  , escapeRadius :: Float
  }
  deriving stock (Eq)

-- | Derive the fractal parameters from the cursor position over the image.
computeConstants :: (MonadIO m) => Vk.Extent2D -> m JuliaConstants
computeConstants (Vk.Extent2D imageWidth imageHeight) = do
  P m <- SDL.getAbsoluteMouseLocation
  let
    m' :: V2 Float
    m' = fmap realToFrac m / imageSizeF
    c :: V2 Float
    c = (m' * 2) - 1
    r = 0.5 * (1 + sqrt (4 * norm c + 1))
    imageSizeF = realToFrac <$> V2 imageWidth imageHeight
    aspect = pure (recip (min (imageSizeF ^. _x) (imageSizeF ^. _y)))
  pure
    JuliaConstants
      { scale = aspect * 2 * pure r
      , offset = negate (imageSizeF * aspect) * pure r
      , c
      , escapeRadius = 12
      }

-- | Palette phase added per recompute; a full colour cycle every @1/colorStep@ renders.
colorStep :: Float
colorStep = 0.01

-- | Bind the Julia pipeline, push the constants, and dispatch over the image.
dispatchJulia
  :: (MonadUnliftIO m)
  => Pipeline
  -> Vk.Extent2D
  -> JuliaConstants
  -> Float
  -- ^ colour-scheme phase, advanced once per dispatch (see 'colorStep')
  -> Vk.DescriptorSet
  -> Vk.CommandBuffer
  -> m ()
dispatchJulia jp (Vk.Extent2D imageWidth imageHeight) constants colorPhase descriptorSet cb = do
  Pipeline.bind cb jp

  -- The byte count and stage flags come from the layout's kept range; only the
  -- field offsets are hand-written (the constants aren't one Storable block).
  let range = case jp.layout.pushRanges of
        [r] -> r
        rs -> error ("dispatchJulia: expected one push range, got " <> show rs)
  allocaBytes (fromIntegral range.size) $ \p -> do
    liftIO $ poke (p `plusPtr` 0) constants.scale
    liftIO $ poke (p `plusPtr` 8) constants.offset
    liftIO $ poke (p `plusPtr` 16) constants.c
    liftIO $ poke (p `plusPtr` 24) constants.escapeRadius
    liftIO $ poke (p `plusPtr` 28) colorPhase
    Vk.cmdPushConstants cb jp.layout.pipelineLayout range.stageFlags 0 range.size p

  Pipeline.bindSet cb jp 0 descriptorSet
  Vk.cmdDispatch
    cb
    ((imageWidth + juliaWorkgroupX - 1) `quot` juliaWorkgroupX)
    ((imageHeight + juliaWorkgroupY - 1) `quot` juliaWorkgroupY)
    1

----------------------------------------------------------------
-- Frame timing
----------------------------------------------------------------

reportFrameTime :: (MonadIO m) => Word64 -> m ()
reportFrameTime nsec = do
  let
    frameTimeNSec = realToFrac nsec :: Double
    targetHz = 60
    frameTimeBudgetMSec = recip targetHz * 1e3
    frameTimeMSec = frameTimeNSec / 1e6
    frameBudgetPercent = ceiling (100 * frameTimeMSec / frameTimeBudgetMSec) :: Int
  when (frameBudgetPercent > 50) $
    sayErrString (show frameTimeMSec <> "ms \t" <> show frameBudgetPercent <> "%")
