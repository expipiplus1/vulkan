{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Per-frame state and the recycling-Frame loop. Each frame owns a binary
image-available semaphore and a command pool — those are 'RecycledResources'
that get handed back to a channel in 'VulkanContext' once the frame's GPU work
has completed. (The present-wait/render-finished semaphore is per swapchain
image, on the 'Vulkan.Utils.Swapchain.Swapchain', because it is only safe to
reuse once its image is re-acquired — not when the frame's render finishes.)

The host-side timeline semaphore (@fHostTimeline@) lives across frames:
each frame increments it to its own 'fIndex' on the GPU, and the host
waits on it inside the spawned wait-and-recycle thread.

This module requires Vulkan 1.2-level timeline-semaphore support. See
'frameInstanceRequirements' / 'frameDeviceRequirements' for the
extension/feature requirements to merge into your boot sequence.
-}
module Vulkan.Utils.Frame
  ( Frame (..)
  , initialFrame
  , advanceFrame
  , runFrame
  , recordCommands
  , queueSubmitFrame
  , acquireFrameImage
  , presentFrameImage
  , drainFrames
  , allocateTimelineSemaphore
  , allocateCommandPool
  , allocatePrimary
  , SubmitExtras (..)
  , noExtras
  , frameSubmitExtras
  , waitTwice
  , frameInstanceRequirements
  , frameDeviceRequirements
  , syncDeviceRequirements
  , InitRecycledResources
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (finally, mask_, throwIO)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource
import Data.Foldable (for_, toList)
import Data.IORef
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Data.Word
import System.IO (hPutStrLn, stderr)
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore as Timeline
import Vulkan.Core13 (PhysicalDeviceSynchronization2Features)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlagBits2 (..), PipelineStageFlags2)
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (SubmitInfo2 (..), queueSubmit2)
import qualified Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 as CommandBufferSubmitInfo (CommandBufferSubmitInfo (..))
import qualified Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 as SemaphoreSubmitInfo (SemaphoreSubmitInfo (..))
import Vulkan.Exception (VulkanException (..))
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import qualified Vulkan.Extensions.VK_KHR_swapchain as KHR
import Vulkan.Requirement (DeviceRequirement, InstanceRequirement (..))
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.RefCounted (resourceTRefCount)
import qualified Vulkan.Utils.Requirements.TH as U
import Vulkan.Utils.Swapchain (Swapchain (..), sRelease)
import Vulkan.Utils.VulkanContext (RecycledResources (..), VulkanContext (..))
import Vulkan.Zero (zero)

data Frame rr = Frame
  { fIndex :: Word64
  -- ^ Monotonic, used as the timeline-semaphore signal value for this frame.
  , fSwapchain :: Swapchain
  {- ^ The swapchain this frame targets. Held by reference so a frame
  in flight keeps its swapchain alive across recreation.
  -}
  , fRecycled :: RecycledResources rr
  {- ^ This frame's image-available semaphore + command pool — borrowed from
  the recycle channel; returned at retire time.
  -}
  , fHostTimeline :: Vk.Semaphore
  {- ^ Long-lived timeline semaphore. Each frame increments it to 'fIndex'
  on the GPU; the host wait thread blocks on this.
  -}
  , fGPUWork :: IORef [(Vk.Semaphore, Word64)]
  {- ^ (Timeline semaphore, value) pairs the host wait thread will block on.
  Appended to by 'queueSubmitFrame'.
  -}
  , fDeferredWork :: IORef [IO ()]
  {- ^ Host work the recycle thread runs before the 'fGPUWork' wait — e.g.
  a frame graph's host-pass tail, which blocks on its own timeline values
  and signals values listed in 'fGPUWork'. Runs in registration order, off
  the render thread.
  -}
  , fResources :: (ReleaseKey, InternalState)
  {- ^ ResourceT scope for frame-local allocations; closed when the frame
  retires. The 'ReleaseKey' lives in the outer ResourceT so the
  scope is freed cleanly even on early shutdown.
  -}
  }

{- | Instance-level requirements for the recycling 'Frame' machinery. Merge
with your application's other 'InstanceRequirement's at instance creation.

Required because checking @PhysicalDeviceTimelineSemaphoreFeatures@ at
physical-device pick time goes through @VkPhysicalDeviceFeatures2@, which
needs either Vulkan 1.1+ or this extension.
-}
frameInstanceRequirements :: [InstanceRequirement]
frameInstanceRequirements =
  [ RequireInstanceExtension
      Nothing
      KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
      minBound
  ]

{- | Timeline semaphores and synchronization2.

The two primitives everything here synchronizes with: 'queueSubmitFrame' and
'allocateTimelineSemaphore', and any submit built by hand. Both are core in
1.3 and universally available in practice, so a headless boot wants them too
— 'frameDeviceRequirements' is these plus a swapchain.
-}
syncDeviceRequirements :: [DeviceRequirement]
syncDeviceRequirements =
  [U.reqs|
    VK_KHR_timeline_semaphore
    PhysicalDeviceTimelineSemaphoreFeatures.timelineSemaphore
    VK_KHR_synchronization2
    PhysicalDeviceSynchronization2Features.synchronization2
  |]

{- | The device-level requirements needed by 'runFrame' / 'queueSubmitFrame' /
'allocateTimelineSemaphore'. Merge into your other 'DeviceRequirement's when
calling 'Vulkan.Utils.Initialization.allocateDeviceFromRequirements'.
-}
frameDeviceRequirements :: [DeviceRequirement]
frameDeviceRequirements = [U.reqs|VK_KHR_swapchain|] <> syncDeviceRequirements

----------------------------------------------------------------
-- Construction
----------------------------------------------------------------

type InitRecycledResources m rr = VulkanContext rr -> Int -> Queues Vk.CommandPool -> m rr

{- | Build the initial frame with one spare 'RecycledResources' seeded
into the recycle channel. That, plus the set attached to this frame,
caps max-in-flight at 2 (CPU recording + GPU executing the previous).
-}
initialFrame
  :: forall rr m
   . (MonadResource m)
  => VulkanContext rr
  -> Swapchain
  -> InitRecycledResources m rr
  -> m (Frame rr)
initialFrame vc fSwapchain mkRecycled = do
  fResources <- allocate createInternalState closeInternalState
  fRecycled <- mkRecycledResources vc $ mkRecycled vc 0
  spare <- mkRecycledResources vc $ mkRecycled vc 1
  liftIO (vcRecycleBin vc spare)
  (_, fHostTimeline) <- allocateTimelineSemaphore (vcDevice vc) 0
  fGPUWork <- liftIO $ newIORef mempty
  fDeferredWork <- liftIO $ newIORef mempty
  liftIO $ runInternalState (resourceTRefCount (sRelease fSwapchain)) (snd fResources)
  pure Frame{fIndex = 1, ..}

{- | Build the next frame, taking one set of recycled resources from the bin.
Caller passes the (possibly-recreated) 'Swapchain'.
-}
advanceFrame
  :: (MonadResource m)
  => VulkanContext rr
  -> Swapchain
  -- ^ Same as old, or freshly recreated
  -> Frame rr
  -- ^ The just-finished frame
  -> m (Frame rr)
advanceFrame vc sc f = do
  fResources <- allocate createInternalState closeInternalState
  fRecycled <-
    liftIO $
      vcRecycleNib vc >>= \case
        Left block -> block
        Right rr -> pure rr
  fGPUWork <- liftIO $ newIORef mempty
  fDeferredWork <- liftIO $ newIORef mempty
  liftIO $ runInternalState (resourceTRefCount (sRelease sc)) (snd fResources)
  pure
    Frame
      { fIndex = succ (fIndex f)
      , fSwapchain = sc
      , fRecycled
      , fHostTimeline = fHostTimeline f
      , fGPUWork
      , fDeferredWork
      , fResources
      }

----------------------------------------------------------------
-- Loop
----------------------------------------------------------------

{- | Run a per-frame action against this frame's per-frame ResourceT scope,
then asynchronously wait for the GPU work and recycle. The deferred host
work and the wait/recycle run in a forked thread so the next frame can
begin recording immediately.

Anything 'allocate'd inside @action@ is freed when the frame retires.
-}
runFrame :: VulkanContext rr -> Frame rr -> ResourceT IO a -> IO a
runFrame vc f action =
  runInternalState action (snd (fResources f))
    `finally` waitAndRecycle vc f

waitAndRecycle :: VulkanContext rr -> Frame rr -> IO ()
waitAndRecycle vc f = do
  waits <- readIORef (fGPUWork f)
  deferred <- readIORef (fDeferredWork f)
  void . forkIO $ do
    -- The frame's host work first: it blocks on its own timeline values and
    -- signals values the wait below (and in-flight submits) depend on.
    sequence_ (reverse deferred)
    unless (null waits) $ do
      let waitInfo =
            zero
              { semaphores = V.fromList (fst <$> waits)
              , values = V.fromList (snd <$> waits)
              }
      r <- waitTwice (vcDevice vc) waitInfo oneSecond
      case r of
        Vk.TIMEOUT -> hPutStrLn stderr "Frame wait timed out (1s) — GPU may be hung"
        _ -> pure ()
    -- Pool reuse: reset each distinct pool, dropping all recorded buffers.
    -- No RELEASE_RESOURCES: the point of recycling a pool is to keep its
    -- arena warm for the next frame's buffers.
    for_ (nub (toList (rrCommandPools (fRecycled f)))) $ \pool ->
      Vk.resetCommandPool (vcDevice vc) pool zero
    -- Free the per-frame ResourceT scope. Must precede the channel deposit so
    -- the deposit signals "all per-frame cleanup done" — otherwise the next
    -- frame could pick up the recycled pool while this frame's cleanup is
    -- still calling vkFreeCommandBuffers on it.
    release (fst (fResources f))
    -- Hand the borrowed resources back to whoever's waiting on them.
    vcRecycleBin vc (fRecycled f)
  where
    oneSecond :: Word64
    oneSecond = 1000000000

{- | Allocate a primary command buffer from this frame's recycled command pool,
begin it with 'Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT', run the caller's
recording action, end recording, and return the buffer ready to hand to
'queueSubmitFrame'.

For a non-standard begin shape (secondary level, different usage flags,
inheritance info) call 'Vk.withCommandBuffers' and 'Vk.useCommandBuffer'
directly.
-}
recordCommands
  :: (MonadResource m, MonadFail m)
  => VulkanContext rr
  -> Frame rr
  -> (Vk.CommandBuffer -> m ())
  -> m Vk.CommandBuffer
{-# INLINE recordCommands #-}
recordCommands vc Frame{fRecycled} record = do
  (_, [cb]) <-
    Vk.withCommandBuffers
      (vcDevice vc)
      zero
        { Vk.commandPool = qGraphics (rrCommandPools fRecycled)
        , Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , Vk.commandBufferCount = 1
        }
      allocate
  Vk.useCommandBuffer cb zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} $
    record cb
  pure cb

{- | Submit a per-frame command buffer batch and record the timeline-wait
bookkeeping the host wait thread will block on.

Builds the standard frame submit from context/frame: waits on the frame's
image-available semaphore at @COLOR_ATTACHMENT_OUTPUT@, signals the
swapchain's per-image render-finished semaphore (at @imageIndex@) plus its
timeline value, and submits on the graphics queue.

For a non-standard submit shape (multiple submit infos, different wait
stage, extra signals), call 'queueSubmit2' directly and append
@(fHostTimeline f, fIndex f)@ to @fGPUWork f@.
-}
queueSubmitFrame
  :: (MonadIO m)
  => VulkanContext rr
  -> Frame rr
  -> Word32
  {- ^ Acquired image index (from 'acquireFrameImage'); selects the per-image
  present-wait semaphore to signal.
  -}
  -> V.Vector Vk.CommandBuffer
  -> m ()
{-# INLINE queueSubmitFrame #-}
queueSubmitFrame vc Frame{..} imageIndex cbs = liftIO . mask_ $ do
  queueSubmit2 gQ [SomeStruct submitInfo] Vk.NULL_HANDLE
  atomicModifyIORef' fGPUWork $ \jobs -> ((fHostTimeline, fIndex) : jobs, ())
  where
    gQ = snd (qGraphics (vcQueues vc))
    renderFinished = sRenderFinished fSwapchain V.! fromIntegral imageIndex
    -- The two WSI semaphores are binary (mandated: acquire signals one,
    -- present waits on one) and ignore their values; the timeline is ours.
    submitInfo =
      zero
        { waitSemaphoreInfos =
            [zero{SemaphoreSubmitInfo.semaphore = rrImageAvailable, SemaphoreSubmitInfo.stageMask = PIPELINE_STAGE_2_TOP_OF_PIPE_BIT}]
        , commandBufferInfos =
            fmap (\cb -> SomeStruct zero{CommandBufferSubmitInfo.commandBuffer = Vk.commandBufferHandle cb}) cbs
        , signalSemaphoreInfos =
            [ zero{SemaphoreSubmitInfo.semaphore = renderFinished, SemaphoreSubmitInfo.stageMask = PIPELINE_STAGE_2_ALL_COMMANDS_BIT}
            , zero{SemaphoreSubmitInfo.semaphore = fHostTimeline, SemaphoreSubmitInfo.stageMask = PIPELINE_STAGE_2_ALL_COMMANDS_BIT, SemaphoreSubmitInfo.value = fIndex}
            ]
        }
        :: SubmitInfo2 '[]
    RecycledResources{rrImageAvailable} = fRecycled

{- | Acquire the next swapchain image for this frame, signalling the frame's
image-available semaphore on completion.

The acquire result is returned alongside the image index so the caller can
thread it into 'presentFrameImage', which honours 'SUBOPTIMAL_KHR' from
either side by raising 'ERROR_OUT_OF_DATE_KHR' to drive a swapchain
recreation. Timeouts and unexpected results are also translated to
'ERROR_OUT_OF_DATE_KHR' — the main loop's swapchain-recreation path is the
right place to recover.
-}
acquireFrameImage :: (MonadIO m) => VulkanContext rr -> Frame rr -> m (Vk.Result, Word32)
{-# INLINE acquireFrameImage #-}
acquireFrameImage vc Frame{..} =
  liftIO $
    acquire >>= \case
      r@(Vk.SUCCESS, _) -> pure r
      r@(Vk.SUBOPTIMAL_KHR, _) -> pure r
      _ -> throwIO (VulkanException Vk.ERROR_OUT_OF_DATE_KHR)
  where
    acquire =
      KHR.acquireNextImageKHRSafe
        (vcDevice vc)
        (sSwapchain fSwapchain)
        oneSecond
        (rrImageAvailable fRecycled)
        Vk.NULL_HANDLE

    oneSecond :: Word64
    oneSecond = 1000000000

{- | Present this frame's acquired image, waiting on the swapchain's per-image
render-finished semaphore (at @imageIndex@). Presents on the graphics queue
(@qGraphics . vcQueues@).

If either the prior acquire (passed in) or this present reports
'SUBOPTIMAL_KHR', raises 'ERROR_OUT_OF_DATE_KHR' so the main loop
recreates the swapchain.
-}
presentFrameImage :: (MonadIO m) => VulkanContext rr -> Frame rr -> Vk.Result -> Word32 -> m ()
{-# INLINE presentFrameImage #-}
presentFrameImage vc f acquireResult imageIndex = liftIO $ do
  presentResult <-
    KHR.queuePresentKHR
      gQ
      zero
        { KHR.waitSemaphores = [renderFinished]
        , KHR.swapchains = [sSwapchain (fSwapchain f)]
        , KHR.imageIndices = [imageIndex]
        }
  when (acquireResult == Vk.SUBOPTIMAL_KHR || presentResult == Vk.SUBOPTIMAL_KHR) $
    throwIO (VulkanException Vk.ERROR_OUT_OF_DATE_KHR)
  where
    renderFinished = sRenderFinished (fSwapchain f) V.! fromIntegral imageIndex
    gQ = snd (qGraphics (vcQueues vc))

{- | Shutdown drain: spawn the unrendered current frame's wait/recycle thread,
then block on the recycle channel until both this frame's and the previous
in-flight frame's deposits have arrived. After this returns, every forked
wait thread has run its per-frame cleanup, so the outer 'ResourceT' is safe
to tear down GPU resources.

Assumes max-in-flight is 2 (see 'initialFrame').
-}
drainFrames :: VulkanContext rr -> Frame rr -> IO ()
drainFrames vc f = do
  waitAndRecycle vc f
  let take1 = vcRecycleNib vc >>= either id pure
  _ <- take1
  _ <- take1
  pure ()

----------------------------------------------------------------
-- Small helpers
----------------------------------------------------------------

-- | Allocate a timeline semaphore initialised to the given value.
allocateTimelineSemaphore :: (MonadResource m) => Vk.Device -> Word64 -> m (ReleaseKey, Vk.Semaphore)
allocateTimelineSemaphore dev initial =
  Vk.withSemaphore
    dev
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_TIMELINE initial :& ())
    Nothing
    allocate

----------------------------------------------------------------
-- Internals
----------------------------------------------------------------

{- | Build one set of recycled resources: a binary image-available semaphore
+ a command pool keyed to the graphics queue family. (The present-wait
semaphore is per swapchain image, on the 'Swapchain', not here.)
-}
mkRecycledResources
  :: (MonadResource m)
  => VulkanContext rr
  -> (Queues Vk.CommandPool -> m rr)
  -> m (RecycledResources rr)
mkRecycledResources vc mkData = do
  (_, rrImageAvailable) <-
    Vk.withSemaphore
      dev
      (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ())
      Nothing
      allocate
  -- One pool per distinct family, shared by every role on it.
  byFamily <-
    fmap Map.fromList $
      traverse (\fam -> fmap (fam,) (allocateCommandPool dev fam)) (nub (toList families))
  let rrCommandPools = fmap (byFamily Map.!) families
  rrData <- mkData rrCommandPools
  pure RecycledResources{..}
  where
    dev = vcDevice vc
    families = fmap (\(QueueFamilyIndex fam, _) -> fam) (vcQueues vc)

-- | Allocate a command pool for the family, released with the scope.
allocateCommandPool :: (MonadResource m) => Vk.Device -> Word32 -> m Vk.CommandPool
allocateCommandPool dev family = do
  (_, pool) <- Vk.withCommandPool dev zero{CommandPoolCreateInfo.queueFamilyIndex = family} Nothing allocate
  pure pool

-- | Allocate a primary command buffer from the pool and begin it, one-time-submit.
allocatePrimary :: (MonadResource m) => Vk.Device -> Vk.CommandPool -> m Vk.CommandBuffer
allocatePrimary dev pool = do
  (_, cbs) <-
    Vk.withCommandBuffers
      dev
      zero{Vk.commandPool = pool, Vk.level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY, Vk.commandBufferCount = 1}
      allocate
  let cb = V.head cbs
  Vk.beginCommandBuffer cb zero{CommandBufferBeginInfo.flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
  pure cb

{- | Frame-level waits and signals spliced into one submit.

Timeline semaphores carry their value; a binary semaphore's value is
ignored (pass 0).
-}
data SubmitExtras = SubmitExtras
  { waits :: [(Vk.Semaphore, PipelineStageFlags2, Word64)]
  , signals :: [(Vk.Semaphore, Word64)]
  }

noExtras :: SubmitExtras
noExtras = SubmitExtras [] []

{- | The canonical windowed frame extras.

Wait image-available; signal this image's render-finished and the host
timeline at the frame index — the shape 'queueSubmitFrame' hard-codes,
for drivers that assemble their own submits.
-}
frameSubmitExtras :: Frame rr -> Word32 -> SubmitExtras
frameSubmitExtras f imageIndex =
  SubmitExtras
    { waits = [(rrImageAvailable (fRecycled f), PIPELINE_STAGE_2_TOP_OF_PIPE_BIT, 0)]
    , signals =
        [ (sRenderFinished (fSwapchain f) V.! fromIntegral imageIndex, 0)
        , (fHostTimeline f, fIndex f)
        ]
    }

{- | Wait for some semaphores; if the wait times out, give the device one
more chance with a zero timeout. Catches the case where the host was
suspended during the wait and the GPU has actually finished.
-}
waitTwice :: Vk.Device -> SemaphoreWaitInfo -> Word64 -> IO Vk.Result
waitTwice dev waitInfo t =
  Timeline.waitSemaphoresSafe dev waitInfo t >>= \case
    Vk.TIMEOUT -> Timeline.waitSemaphores dev waitInfo 0
    r -> pure r
