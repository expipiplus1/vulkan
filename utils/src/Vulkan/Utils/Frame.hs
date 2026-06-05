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
  , withTimelineSemaphore
  , frameInstanceRequirements
  , frameDeviceRequirements
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (finally, mask_, throwIO)
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource
import Data.IORef
import qualified Data.Vector as V
import Data.Word
import System.IO (hPutStrLn, stderr)
import Vulkan.CStruct.Extends (SomeStruct (..), pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as CommandBufferBeginInfo (CommandBufferBeginInfo (..))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore as Timeline
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

-- | Per-frame state.
data Frame = Frame
  { fIndex :: Word64
  -- ^ Monotonic, used as the timeline-semaphore signal value for this frame.
  , fSwapchain :: Swapchain
  {- ^ The swapchain this frame targets. Held by reference so a frame
  in flight keeps its swapchain alive across recreation.
  -}
  , fRecycled :: RecycledResources
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

{- | The device-level requirements needed by 'runFrame' / 'queueSubmitFrame' /
'withTimelineSemaphore'. Merge into your other 'DeviceRequirement's when
calling 'Vulkan.Utils.Initialization.createDeviceFromRequirements'.
-}
frameDeviceRequirements :: [DeviceRequirement]
frameDeviceRequirements =
  [U.reqs|
    VK_KHR_swapchain
    VK_KHR_timeline_semaphore
    PhysicalDeviceTimelineSemaphoreFeatures.timelineSemaphore
  |]

----------------------------------------------------------------
-- Construction
----------------------------------------------------------------

{- | Build the initial frame with one spare 'RecycledResources' seeded
into the recycle channel. That, plus the set attached to this frame,
caps max-in-flight at 2 (CPU recording + GPU executing the previous).
-}
initialFrame :: (MonadResource m) => VulkanContext -> Swapchain -> m Frame
initialFrame vc fSwapchain = do
  fRecycled <- mkRecycledResources vc
  spare <- mkRecycledResources vc
  liftIO (vcRecycleBin vc spare)
  (_, fHostTimeline) <- withTimelineSemaphore (vcDevice vc) 0
  fGPUWork <- liftIO $ newIORef mempty
  fResources <- allocate createInternalState closeInternalState
  liftIO $ runInternalState (resourceTRefCount (sRelease fSwapchain)) (snd fResources)
  pure Frame{fIndex = 1, ..}

{- | Build the next frame, taking one set of recycled resources from the bin.
Caller passes the (possibly-recreated) 'Swapchain'.
-}
advanceFrame
  :: (MonadResource m)
  => VulkanContext
  -> Swapchain
  -- ^ Same as old, or freshly recreated
  -> Frame
  -- ^ The just-finished frame
  -> m Frame
advanceFrame vc sc f = do
  fRecycled <-
    liftIO $
      vcRecycleNib vc >>= \case
        Left block -> block
        Right rr -> pure rr
  fGPUWork <- liftIO $ newIORef mempty
  fResources <- allocate createInternalState closeInternalState
  liftIO $ runInternalState (resourceTRefCount (sRelease sc)) (snd fResources)
  pure
    Frame
      { fIndex = succ (fIndex f)
      , fSwapchain = sc
      , fRecycled
      , fHostTimeline = fHostTimeline f
      , fGPUWork
      , fResources
      }

----------------------------------------------------------------
-- Loop
----------------------------------------------------------------

{- | Run a per-frame action against this frame's per-frame ResourceT scope,
then asynchronously wait for the GPU work and recycle. The wait/recycle
runs in a forked thread so the next frame can begin recording immediately.

Anything 'allocate'd inside @action@ is freed when the frame retires.
-}
runFrame :: VulkanContext -> Frame -> ResourceT IO a -> IO a
runFrame vc f action =
  runInternalState action (snd (fResources f))
    `finally` waitAndRecycle vc f

waitAndRecycle :: VulkanContext -> Frame -> IO ()
waitAndRecycle vc f = do
  waits <- readIORef (fGPUWork f)
  void . forkIO $ do
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
    -- Pool reuse: reset, dropping all recorded buffers.
    Vk.resetCommandPool
      (vcDevice vc)
      (rrCommandPool (fRecycled f))
      Vk.COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
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
  => VulkanContext
  -> Frame
  -> (Vk.CommandBuffer -> m ())
  -> m Vk.CommandBuffer
{-# INLINE recordCommands #-}
recordCommands vc Frame{fRecycled} record = do
  (_, [cb]) <-
    Vk.withCommandBuffers
      (vcDevice vc)
      zero
        { Vk.commandPool = rrCommandPool fRecycled
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
stage, extra signals), call 'Vk.queueSubmit' directly and append
@(fHostTimeline f, fIndex f)@ to @fGPUWork f@.
-}
queueSubmitFrame
  :: (MonadIO m)
  => VulkanContext
  -> Frame
  -> Word32
  -- ^ Acquired image index (from 'acquireFrameImage'); selects the per-image
  -- present-wait semaphore to signal.
  -> V.Vector Vk.CommandBuffer
  -> m ()
{-# INLINE queueSubmitFrame #-}
queueSubmitFrame vc Frame{..} imageIndex cbs = liftIO . mask_ $ do
  Vk.queueSubmit gQ [SomeStruct submitInfo] Vk.NULL_HANDLE
  atomicModifyIORef' fGPUWork $ \jobs -> ((fHostTimeline, fIndex) : jobs, ())
  where
    gQ = snd (qGraphics (vcQueues vc))
    renderFinished = sRenderFinished fSwapchain V.! fromIntegral imageIndex
    submitInfo =
      zero
        { Vk.waitSemaphores = [rrImageAvailable]
        , Vk.waitDstStageMask = [Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT]
        , Vk.commandBuffers = fmap Vk.commandBufferHandle cbs
        , Vk.signalSemaphores = [renderFinished, fHostTimeline]
        }
        ::& zero
          { waitSemaphoreValues = [1]
          , signalSemaphoreValues = [1, fIndex]
          }
          :& ()
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
acquireFrameImage :: (MonadIO m) => VulkanContext -> Frame -> m (Vk.Result, Word32)
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
presentFrameImage :: (MonadIO m) => VulkanContext -> Frame -> Vk.Result -> Word32 -> m ()
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
drainFrames :: VulkanContext -> Frame -> IO ()
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
withTimelineSemaphore :: (MonadResource m) => Vk.Device -> Word64 -> m (ReleaseKey, Vk.Semaphore)
withTimelineSemaphore dev initial =
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
mkRecycledResources :: (MonadResource m) => VulkanContext -> m RecycledResources
mkRecycledResources vc = do
  (_, rrImageAvailable) <-
    Vk.withSemaphore
      dev
      (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ())
      Nothing
      allocate
  (_, rrCommandPool) <-
    Vk.withCommandPool
      dev
      zero{CommandPoolCreateInfo.queueFamilyIndex = qfi}
      Nothing
      allocate
  pure RecycledResources{..}
  where
    dev = vcDevice vc
    (QueueFamilyIndex qfi, _) = qGraphics (vcQueues vc)

{- | Wait for some semaphores; if the wait times out, give the device one
more chance with a zero timeout. Catches the case where the host was
suspended during the wait and the GPU has actually finished.
-}
waitTwice :: Vk.Device -> SemaphoreWaitInfo -> Word64 -> IO Vk.Result
waitTwice dev waitInfo t =
  Timeline.waitSemaphoresSafe dev waitInfo t >>= \case
    Vk.TIMEOUT -> Timeline.waitSemaphores dev waitInfo 0
    r -> pure r
