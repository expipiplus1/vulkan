{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{-| Julia-set viewer, driven by a per-frame 'FG.FrameGraph' the same way
regardless of queue topology.

The fractal only changes when the window resizes or the cursor moves; its
steady state is "re-present the same image". Each frame declares a graph of up
to three passes — @julia@ (compute → offscreen), @blit@ (offscreen → swapchain)
and @present@ (swapchain → PRESENT_SRC) — adding @julia@ only when the fractal
parameters changed and @blit@ only when the acquired swapchain image does not
already hold the current fractal. fragr places exactly the barriers the
surviving passes need: a fully idle frame records /nothing/ and just re-presents.

The one topology-dependent line is @'FG.setQueue' b computeQueueId@ on the
@julia@ pass, where @computeQueueId@ is chosen once at startup: the graphics
queue when compute shares its family, an async-compute queue when it doesn't.
Everything else — pruning, barrier placement, and the per-queue routing of
commands — falls out of 'FG.executeQueued': on shared hardware every pass lands
on one queue and it degenerates to a single command buffer and submit; on async
hardware @executeAdaptive@ submits the compute buffer signalling a timeline the
graphics submit waits on. The offscreen image is CONCURRENT across the two
families, so no queue-family ownership transfer is needed — only the timeline
handshake, and only on the (rare) frames that recompute.
-}
module Main
  ( main
  ) where

import Blit (blitImage)
import Control.Exception (handle, mask_)
import Control.Lens.Getter ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Bits ((.|.))
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import qualified Fragr as FG
import Julia (JuliaPipeline (..), createJuliaDescriptorSets, createJuliaPipeline, juliaWorkgroupX, juliaWorkgroupY)
import Linear.Affine (Point (..))
import Linear.Metric (norm)
import Linear.V2
import qualified SDL
import Say (sayErrString)
import UnliftIO.Exception (displayException)
import UnliftIO.Foreign (allocaBytes, plusPtr, poke)
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (TimelineSemaphoreSubmitInfo (..))
import Vulkan.Exception
import Vulkan.Utils.Frame (Frame (..), acquireFrameImage, allocateTimelineSemaphore, presentFrameImage)
import Vulkan.Utils.FrameGraph.Image (ManagedImage (..), Usage (..), importManagedImage, newManagedImage, usageFlags)
import Vulkan.Utils.FrameGraph.Recorder (Recorder, recordGraph, recorderCommandBuffer)
import Vulkan.Utils.Init.SDL2.Window (createWindow, drawableSize, sdl2Adapter, shouldQuit, withSDL)
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.Swapchain (Swapchain (..), SwapchainConfig (..), defaultSwapchainConfig)
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

  juliaPL <- createJuliaPipeline dev

  -- One startup decision: does compute get its own queue family? If so, the
  -- julia pass runs on QueueId 1 and hands the offscreen image over with a
  -- timeline semaphore; otherwise everything stays on the graphics queue.
  topology <- detectTopology vc
  let sharedFamilies = fmap (\as -> (as.asGraphicsFamily, as.asComputeFamily)) topology

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
  { asGraphicsFamily :: Word32
  , asComputeFamily :: Word32
  , asComputeQueue :: Vk.Queue
  , asReadyTimeline :: Vk.Semaphore
  -- ^ Compute signals this at the frame index; the graphics blit waits on it.
  , asLastBlitDone :: IORef Word64
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
      (_, readyTimeline) <- allocateTimelineSemaphore (vcDevice vc) 0
      lastBlitDone <- liftIO (newIORef 0)
      pure $
        Just
          AsyncSetup
            { asGraphicsFamily = graphicsFamily
            , asComputeFamily = computeFamily
            , asComputeQueue = snd (qCompute (vcQueues vc))
            , asReadyTimeline = readyTimeline
            , asLastBlitDone = lastBlitDone
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
  { bOffscreen :: ManagedImage
  {- ^ The single compute target, imported so its tracked layout persists across
  frames (recreated per swapchain because its extent tracks the window).
  -}
  , bJuliaDescriptorSet :: Vk.DescriptorSet
  , bSwapImages :: Vector ManagedImage
  -- ^ One layout-tracked wrapper per swapchain image.
  , bLastConstants :: IORef (Maybe JuliaConstants)
  -- ^ Fractal parameters last computed; a change makes the frame dirty.
  , bFreshImages :: IORef IntSet
  -- ^ Swapchain image indices that already hold the current fractal.
  }

allocateBindings
  :: Vk.Device
  -> VMA.Allocator
  -> JuliaPipeline
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
    createJuliaDescriptorSets dev (jpDescriptorSetLayout jp) [view]

  offscreen <- newManagedImage image Vk.IMAGE_ASPECT_COLOR_BIT
  swapImages <- traverse (\img -> newManagedImage img Vk.IMAGE_ASPECT_COLOR_BIT) (sImages sc)
  lastConstants <- liftIO (newIORef Nothing)
  freshImages <- liftIO (newIORef IntSet.empty)

  -- runWindowLoop fires exactly one release key on resize: free the pool (and
  -- its sets) first, then the view, then the image. The swapchain images belong
  -- to the swapchain, so their wrappers need no release.
  bindingsKey <- register (mapM_ release ([poolKey, viewKey, imageKey] :: [ReleaseKey]))

  pure
    ( Bindings
        { bOffscreen = offscreen
        , bJuliaDescriptorSet = V.head juliaSets
        , bSwapImages = swapImages
        , bLastConstants = lastConstants
        , bFreshImages = freshImages
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
  -> JuliaPipeline
  -> Maybe AsyncSetup
  -> IORef Float
  -- ^ colour-scheme phase, shared across swapchains so it survives resizes
  -> Bindings
  -> Frame
  -> ResourceT IO ()
renderJulia vc jp topology colorRef bindings f = do
  constants <- computeConstants (sExtent sc)
  lastConstants <- liftIO (readIORef bindings.bLastConstants)
  let dirty = Just constants /= lastConstants

  (acquireResult, imageIndex) <- acquireFrameImage vc f
  freshImages <- liftIO (readIORef bindings.bFreshImages)
  let
    ix = fromIntegral imageIndex :: Int
    -- Compute only on a parameter change; blit unless this image already holds
    -- the current fractal (a dirty frame invalidates every other image).
    needBlit = dirty || not (IntSet.member ix freshImages)
    swapManaged = bSwapImages bindings V.! ix

  graph <- FG.newFrameGraph
  offscreenH <- importManagedImage graph "offscreen" bindings.bOffscreen
  swapchainH <- importManagedImage graph "swapchain" swapManaged

  colorPhase <- liftIO (readIORef colorRef)
  offscreenReady <-
    if dirty
      then FG.addPass graph "julia" (juliaSetup offscreenH) \_written _resources recorder -> do
        cb <- recorderCommandBuffer recorder
        dispatchJulia jp (sExtent sc) constants colorPhase bindings.bJuliaDescriptorSet cb
      else pure offscreenH

  swapchainReady <-
    if needBlit
      then FG.addPass graph "blit" (blitSetup offscreenReady swapchainH) \_blitted _resources recorder -> do
        cb <- recorderCommandBuffer recorder
        blitImage (sExtent sc) bindings.bOffscreen.image swapManaged.image cb
      else pure swapchainH

  -- Always present: writing the imported swapchain marks a side effect, so the
  -- pass survives culling and its hook brings the image to PRESENT_SRC (a no-op
  -- barrier when it is already there, i.e. an idle re-present).
  _present <- FG.addPass graph "present" (\b -> FG.writeWith b swapchainReady (usageFlags Present)) \_presented _resources _recorder ->
    pure ()

  FG.compile graph
  executeAdaptive vc f imageIndex topology dirty needBlit graph
  presentFrameImage vc f acquireResult imageIndex

  liftIO $ do
    when dirty $ do
      writeIORef bindings.bLastConstants (Just constants)
      -- Advance the colour phase once per recompute, so the palette visibly
      -- rotates exactly on the frames that render (and freezes when idle).
      modifyIORef' colorRef (+ colorStep)
    when needBlit $
      modifyIORef' bindings.bFreshImages $
        if dirty then const (IntSet.singleton ix) else IntSet.insert ix
  where
    sc = fSwapchain f
    juliaSetup offscreenH b = do
      FG.setQueue b (computeQueueId topology)
      FG.writeWith b offscreenH (usageFlags (StorageWrite Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT))
    blitSetup offscreenReady swapchainH b = do
      _ <- FG.readWith b offscreenReady (usageFlags TransferSrc)
      FG.writeWith b swapchainH (usageFlags TransferDst)

----------------------------------------------------------------
-- Windowed submit bridge (Layer 2) + app policy (Layer 3)
--
-- The topology-agnostic 'recordingBackend' (route each pass to its queue's
-- buffer) now lives in "Vulkan.Utils.FrameGraph.Image"; only the submit policy
-- below is app-specific.
----------------------------------------------------------------

{- | Record the compiled graph across its queues and submit. On a single-queue
schedule this is one command buffer and one graphics submit; when the julia pass
landed on a distinct compute queue, the compute work is recorded into its own
buffer and submitted signalling @asReadyTimeline@, which the graphics submit
then waits on.

The command-buffer allocation and the two-submit shape (Layer 2, windowed) plus
the cross-frame WAR fence (Layer 3, specific to reusing one offscreen image) stay
here; only 'recordingBackend' above is topology-agnostic mechanism.
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

  graphicsCb <- beginPrimary dev (rrCommandPool (fRecycled f))
  -- The julia pass is the only compute-queue pass, added iff @dirty@ (and dirty
  -- implies needBlit, so it always survives culling): compute runs exactly when
  -- an async topology is present and the frame is dirty. Its buffer travels with
  -- the 'AsyncSetup' that submits it.
  computePair <- case topology of
    Just as | dirty -> do
      (_, computePool) <-
        Vk.withCommandPool dev zero{CommandPoolCreateInfo.queueFamilyIndex = as.asComputeFamily} Nothing allocate
      cb <- beginPrimary dev computePool
      pure (Just (as, cb))
    _ -> pure Nothing

  let
    -- Route each pass's commands to its queue's buffer (see 'FG.setQueue').
    cbFor q = case computePair of
      Just (_, cb) | q == computeQueue -> cb
      _ -> graphicsCb
    buffers = graphicsCb :| maybe [] (pure . snd) computePair
  recordGraph cbFor buffers graph

  liftIO . mask_ $ do
    submitFrame vc f imageIndex graphicsCb computePair
    -- The WAR fence must see every blit, not just dirty frames': a clean frame
    -- re-blitting a stale swapchain image still reads the offscreen image, and
    -- the next compute waits the timeline only up to the last recorded value.
    for_ topology \as ->
      when didBlit $ writeIORef as.asLastBlitDone (fIndex f)

-- | Allocate a primary command buffer from the pool and begin it, one-time-submit.
beginPrimary :: (MonadResource m, MonadFail m) => Vk.Device -> Vk.CommandPool -> m Vk.CommandBuffer
beginPrimary dev pool = do
  (_, [cb]) <-
    Vk.withCommandBuffers
      dev
      zero{Vk.commandPool = pool, Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY, Vk.commandBufferCount = 1}
      allocate
  Vk.beginCommandBuffer cb zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
  pure cb

{- | The submit(s): a compute submit (when the compute buffer exists) signalling
the ready timeline at this frame index and waiting on the last blit, then the
graphics submit waiting on image-available (and the ready timeline, when compute
ran) and signalling render-finished plus the host timeline.
-}
submitFrame
  :: VulkanContext
  -> Frame
  -> Word32
  -> Vk.CommandBuffer
  -> Maybe (AsyncSetup, Vk.CommandBuffer)
  -> IO ()
submitFrame vc Frame{..} imageIndex graphicsCb computePair = do
  case computePair of
    Just (as, cb) -> do
      prevBlitDone <- readIORef as.asLastBlitDone
      let computeSubmit =
            zero
              { Vk.waitSemaphores = [fHostTimeline]
              , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_COMPUTE_SHADER_BIT]
              , Vk.commandBuffers = [Vk.commandBufferHandle cb]
              , Vk.signalSemaphores = [as.asReadyTimeline]
              }
              ::& zero{waitSemaphoreValues = [prevBlitDone], signalSemaphoreValues = [fIndex]}
                :& ()
      Vk.queueSubmit as.asComputeQueue [SomeStruct computeSubmit] Vk.NULL_HANDLE
    Nothing -> pure ()

  let
    renderFinished = sRenderFinished fSwapchain V.! fromIntegral imageIndex
    RecycledResources{rrImageAvailable} = fRecycled
    graphicsQueue = snd (qGraphics (vcQueues vc))
    -- Only the wait side differs by whether compute ran; the buffer and signals
    -- (render-finished + host timeline) are the same either way.
    (waitSemaphores, waitStages, waitValues) = case computePair of
      Just (as, _) ->
        ( [rrImageAvailable, as.asReadyTimeline]
        , [Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vk.PIPELINE_STAGE_TRANSFER_BIT]
        , [0, fIndex]
        )
      Nothing ->
        ( [rrImageAvailable]
        , [Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT]
        , [0]
        )
    graphicsSubmit =
      zero
        { Vk.waitSemaphores = waitSemaphores
        , Vk.waitDstStageMask = waitStages
        , Vk.commandBuffers = [Vk.commandBufferHandle graphicsCb]
        , Vk.signalSemaphores = [renderFinished, fHostTimeline]
        }
        ::& zero{waitSemaphoreValues = waitValues, signalSemaphoreValues = [0, fIndex]}
          :& ()
  Vk.queueSubmit graphicsQueue [SomeStruct graphicsSubmit] Vk.NULL_HANDLE

  -- Host-side wait bookkeeping: the graphics timeline always, the compute
  -- timeline when it ran. The blit's WAR fence is the caller's, per frame.
  atomicModifyIORef' fGPUWork (\jobs -> ((fHostTimeline, fIndex) : jobs, ()))
  for_ computePair \(as, _) ->
    atomicModifyIORef' fGPUWork (\jobs -> ((as.asReadyTimeline, fIndex) : jobs, ()))

----------------------------------------------------------------
-- Julia dispatch
----------------------------------------------------------------

-- | The mouse-and-extent-derived fractal parameters pushed to the compute shader.
data JuliaConstants = JuliaConstants
  { jcScale :: V2 Float
  , jcOffset :: V2 Float
  , jcC :: V2 Float
  , jcEscapeRadius :: Float
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
      { jcScale = aspect * 2 * pure r
      , jcOffset = negate (imageSizeF * aspect) * pure r
      , jcC = c
      , jcEscapeRadius = 12
      }

-- | Palette phase added per recompute; a full colour cycle every @1/colorStep@ renders.
colorStep :: Float
colorStep = 0.01

-- | Bind the Julia pipeline, push the constants, and dispatch over the image.
dispatchJulia
  :: (MonadUnliftIO m)
  => JuliaPipeline
  -> Vk.Extent2D
  -> JuliaConstants
  -> Float
  -- ^ colour-scheme phase, advanced once per dispatch (see 'colorStep')
  -> Vk.DescriptorSet
  -> Vk.CommandBuffer
  -> m ()
dispatchJulia jp (Vk.Extent2D imageWidth imageHeight) constants colorPhase descriptorSet cb = do
  Vk.cmdBindPipeline cb Vk.PIPELINE_BIND_POINT_COMPUTE (jpPipeline jp)

  let constantBytes = 4 * (2 + 2 + 2 + 1 + 1) :: Int
  allocaBytes constantBytes $ \p -> do
    liftIO $ poke (p `plusPtr` 0) constants.jcScale
    liftIO $ poke (p `plusPtr` 8) constants.jcOffset
    liftIO $ poke (p `plusPtr` 16) constants.jcC
    liftIO $ poke (p `plusPtr` 24) constants.jcEscapeRadius
    liftIO $ poke (p `plusPtr` 28) colorPhase
    Vk.cmdPushConstants cb (jpPipelineLayout jp) Vk.SHADER_STAGE_COMPUTE_BIT 0 (fromIntegral constantBytes) p

  Vk.cmdBindDescriptorSets cb Vk.PIPELINE_BIND_POINT_COMPUTE (jpPipelineLayout jp) 0 [descriptorSet] []
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
