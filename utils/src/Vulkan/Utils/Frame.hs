{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-| Per-frame state and the recycling-Frame loop. Each frame owns a binary
image-available semaphore, a binary render-finished semaphore, and a
command pool — those three are 'RecycledResources' that get handed back
to a channel in 'VulkanContext' once the frame's GPU work has completed.

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
  , queueSubmitFrame
  , drainFrames
  , withTimelineSemaphore
  , frameInstanceRequirements
  , frameDeviceRequirements
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (finally, mask_)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.IORef
import qualified Data.Vector as V
import Data.Word
import System.IO (hPutStrLn, stderr)
import Vulkan.CStruct.Extends (SomeStruct, pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as CommandPoolCreateInfo (CommandPoolCreateInfo (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore as Timeline
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import Vulkan.Requirement (DeviceRequirement, InstanceRequirement (..))
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex (..))
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.RefCounted (resourceTRefCount)
import qualified Vulkan.Utils.Requirements.TH as U
import Vulkan.Utils.Swapchain (Swapchain, sRelease)
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
  {- ^ This frame's image-available / render-finished / command-pool — all
  borrowed from the recycle channel; returned at retire time.
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

{- | Submit GPU work for this frame and record the timeline semaphore + value
the wait thread will block on.

Wraps 'queueSubmit' to keep the submit and the bookkeeping atomic.
-}
queueSubmitFrame
  :: Vk.Queue
  -> Frame
  -> V.Vector (SomeStruct Vk.SubmitInfo)
  -> Vk.Semaphore
  -- ^ Timeline semaphore that will be signalled to @value@
  -> Word64
  -- ^ Value the timeline reaches once this submit completes
  -> IO ()
queueSubmitFrame q f ss sem value = mask_ $ do
  Vk.queueSubmit q ss Vk.NULL_HANDLE
  atomicModifyIORef' (fGPUWork f) ((,()) . ((sem, value) :))

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

{- | Build one set of recycled resources: two binary semaphores + a
command pool keyed to the graphics queue family.
-}
mkRecycledResources :: (MonadResource m) => VulkanContext -> m RecycledResources
mkRecycledResources vc = do
  let
    dev = vcDevice vc
    QueueFamilyIndex qfi = fst (qGraphics (vcQueues vc))
  (_, rrImageAvailable) <-
    Vk.withSemaphore
      dev
      (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ())
      Nothing
      allocate
  (_, rrRenderFinished) <-
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

{- | Wait for some semaphores; if the wait times out, give the device one
more chance with a zero timeout. Catches the case where the host was
suspended during the wait and the GPU has actually finished.
-}
waitTwice :: Vk.Device -> SemaphoreWaitInfo -> Word64 -> IO Vk.Result
waitTwice dev waitInfo t =
  Timeline.waitSemaphoresSafe dev waitInfo t >>= \case
    Vk.TIMEOUT -> Timeline.waitSemaphores dev waitInfo 0
    r -> pure r
