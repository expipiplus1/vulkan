{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Per-frame state and the recycling-Frame loop. Each frame owns a binary
-- image-available semaphore, a binary render-finished semaphore, and a
-- command pool — those three are 'RecycledResources' that get handed back
-- to a channel in 'VkResources' once the frame's GPU work has completed.
--
-- The host-side timeline semaphore (@fHostTimeline@) lives across frames:
-- each frame increments it to its own 'fIndex' on the GPU, and the host
-- waits on it inside the spawned wait-and-recycle thread.
module Frame
  ( Frame(..)
  , numConcurrentFrames
  , initialFrame
  , advanceFrame
  , runFrame
  , queueSubmitFrame
  , withTimelineSemaphore
  , frameInstanceRequirements
  , frameDeviceRequirements
  ) where

import           Control.Concurrent             ( forkIO )
import           Control.Monad                  ( replicateM_
                                                , unless
                                                , void
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Resource   ( InternalState
                                                , MonadResource
                                                , ReleaseKey
                                                , ResourceT
                                                , allocate
                                                , closeInternalState
                                                , createInternalState
                                                , release
                                                , runInternalState
                                                )
import qualified Data.Vector                   as V
import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                )
import           Data.Word
import           Say                            ( sayErr )
import           Swapchain                      ( Swapchain )
import           UnliftIO                       ( atomicModifyIORef'
                                                , finally
                                                , mask_
                                                )
import           VkResources                    ( Queues(..)
                                                , RecycledResources(..)
                                                , VkResources(..)
                                                )
import           Vulkan.CStruct.Extends         ( SomeStruct
                                                , pattern (:&)
                                                , pattern (::&)
                                                )
import           Vulkan.Core10
import qualified Vulkan.Core10                 as CommandPoolCreateInfo
                                                ( CommandPoolCreateInfo(..) )
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
                                               as Timeline
import           Vulkan.Extensions.VK_KHR_get_physical_device_properties2
                                                ( pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME )
import           Vulkan.Requirement             ( DeviceRequirement
                                                , InstanceRequirement(..)
                                                )
import           Vulkan.Utils.QueueAssignment   ( QueueFamilyIndex(..) )
import qualified Vulkan.Utils.Requirements.TH  as U
import           Vulkan.Zero                    ( zero )

-- | Instance-level requirements for the recycling 'Frame' machinery. Merge
-- with your example's other 'InstanceRequirement's when calling
-- 'Vulkan.Utils.Init.SDL2.withInstance' (or equivalent).
--
-- Required because checking @PhysicalDeviceTimelineSemaphoreFeatures@ at
-- physical-device pick time goes through @VkPhysicalDeviceFeatures2@, which
-- needs either Vulkan 1.1+ or this extension.
frameInstanceRequirements :: [InstanceRequirement]
frameInstanceRequirements =
  [ RequireInstanceExtension
      Nothing
      KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
      minBound
  ]

-- | The device-level requirements needed by 'runFrame' / 'queueSubmitFrame' /
-- 'withTimelineSemaphore'. Merge into your example's other 'DeviceRequirement's
-- when calling 'createDeviceFromRequirements'.
frameDeviceRequirements :: [DeviceRequirement]
frameDeviceRequirements = [U.reqs|
    VK_KHR_timeline_semaphore
    PhysicalDeviceTimelineSemaphoreFeatures.timelineSemaphore
  |]

-- | How many frames to keep in flight. Determines how many spare
-- 'RecycledResources' get pre-populated into the recycle channel at startup.
numConcurrentFrames :: Int
numConcurrentFrames = 3

-- | Per-frame state.
data Frame = Frame
  { fIndex        :: Word64
    -- ^ Monotonic, used as the timeline-semaphore signal value for this frame.
  , fSwapchain    :: Swapchain
    -- ^ The swapchain this frame targets. Held by reference so a frame
    -- in flight keeps its swapchain alive across recreation.
  , fRecycled     :: RecycledResources
    -- ^ This frame's image-available / render-finished / command-pool — all
    -- borrowed from the recycle channel; returned at retire time.
  , fHostTimeline :: Semaphore
    -- ^ Long-lived timeline semaphore. Each frame increments it to 'fIndex'
    -- on the GPU; the host wait thread blocks on this.
  , fGPUWork      :: IORef [(Semaphore, Word64)]
    -- ^ (Timeline semaphore, value) pairs the host wait thread will block on.
    -- Appended to by 'queueSubmitFrame'.
  , fResources    :: (ReleaseKey, InternalState)
    -- ^ ResourceT scope for frame-local allocations; closed when the frame
    -- retires. The 'ReleaseKey' lives in the outer ResourceT so the
    -- scope is freed cleanly even on early shutdown.
  }

----------------------------------------------------------------
-- Construction
----------------------------------------------------------------

-- | Build the initial frame and pre-populate the recycle channel with
-- @'numConcurrentFrames' - 1@ spare 'RecycledResources'.
initialFrame :: MonadResource m => VkResources -> Swapchain -> m Frame
initialFrame vr fSwapchain = do
  replicateM_ (numConcurrentFrames - 1) $ do
    rr <- mkRecycledResources vr
    liftIO (vrRecycleBin vr rr)
  fRecycled        <- mkRecycledResources vr
  (_, fHostTimeline) <- withTimelineSemaphore (vrDevice vr) 0
  fGPUWork         <- liftIO $ newIORef mempty
  fResources       <- allocate createInternalState closeInternalState
  pure Frame { fIndex = 1, .. }

-- | Build the next frame, taking one set of recycled resources from the bin.
-- Caller passes the (possibly-recreated) 'Swapchain'.
advanceFrame
  :: MonadResource m
  => VkResources
  -> Swapchain        -- ^ Same as old, or freshly recreated
  -> Frame            -- ^ The just-finished frame
  -> m Frame
advanceFrame vr sc f = do
  fRecycled <- liftIO $ vrRecycleNib vr >>= \case
    Left  block -> block
    Right rr    -> pure rr
  fGPUWork   <- liftIO $ newIORef mempty
  fResources <- allocate createInternalState closeInternalState
  pure Frame { fIndex        = succ (fIndex f)
             , fSwapchain    = sc
             , fRecycled
             , fHostTimeline = fHostTimeline f
             , fGPUWork
             , fResources
             }

----------------------------------------------------------------
-- Loop
----------------------------------------------------------------

-- | Run a per-frame action against this frame's per-frame ResourceT scope,
-- then asynchronously wait for the GPU work and recycle. The wait/recycle
-- runs in a forked thread so the next frame can begin recording immediately.
--
-- Anything 'allocate'd inside @action@ is freed when the frame retires.
runFrame :: VkResources -> Frame -> ResourceT IO a -> IO a
runFrame vr f action =
  runInternalState action (snd (fResources f))
    `finally` waitAndRecycle vr f

waitAndRecycle :: VkResources -> Frame -> IO ()
waitAndRecycle vr f = do
  waits <- readIORef (fGPUWork f)
  void . forkIO $ do
    unless (null waits) $ do
      let waitInfo = zero { semaphores = V.fromList (fst <$> waits)
                          , values     = V.fromList (snd <$> waits)
                          }
      r <- waitTwice (vrDevice vr) waitInfo oneSecond
      case r of
        TIMEOUT -> sayErr "Frame wait timed out (1s) — GPU may be hung"
        _       -> pure ()
    -- Pool reuse: reset, dropping all recorded buffers.
    resetCommandPool (vrDevice vr)
                     (rrCommandPool (fRecycled f))
                     COMMAND_POOL_RESET_RELEASE_RESOURCES_BIT
    -- Hand the borrowed resources back to whoever's waiting on them.
    vrRecycleBin vr (fRecycled f)
    -- Free the per-frame ResourceT scope.
    release (fst (fResources f))
 where
  oneSecond :: Word64
  oneSecond = 1000000000

-- | Submit GPU work for this frame and record the timeline semaphore + value
-- the wait thread will block on.
--
-- Wraps 'queueSubmit' to keep the submit and the bookkeeping atomic.
queueSubmitFrame
  :: Queue
  -> Frame
  -> V.Vector (SomeStruct SubmitInfo)
  -> Semaphore       -- ^ Timeline semaphore that will be signalled to @value@
  -> Word64          -- ^ Value the timeline reaches once this submit completes
  -> IO ()
queueSubmitFrame q f ss sem value = mask_ $ do
  queueSubmit q ss NULL_HANDLE
  atomicModifyIORef' (fGPUWork f) ((, ()) . ((sem, value) :))

----------------------------------------------------------------
-- Small helpers
----------------------------------------------------------------

-- | Allocate a timeline semaphore initialised to the given value.
withTimelineSemaphore
  :: MonadResource m => Device -> Word64 -> m (ReleaseKey, Semaphore)
withTimelineSemaphore dev initial =
  withSemaphore dev
                (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_TIMELINE initial :& ())
                Nothing
                allocate

----------------------------------------------------------------
-- Internals
----------------------------------------------------------------

-- | Build one set of recycled resources: two binary semaphores + a
-- command pool keyed to the graphics queue family.
mkRecycledResources :: MonadResource m => VkResources -> m RecycledResources
mkRecycledResources vr = do
  let dev = vrDevice vr
      QueueFamilyIndex qfi = fst (qGraphics (vrQueues vr))
  (_, rrImageAvailable) <- withSemaphore
    dev
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ())
    Nothing
    allocate
  (_, rrRenderFinished) <- withSemaphore
    dev
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ())
    Nothing
    allocate
  (_, rrCommandPool) <- withCommandPool
    dev
    zero { CommandPoolCreateInfo.queueFamilyIndex = qfi }
    Nothing
    allocate
  pure RecycledResources { .. }

-- | Wait for some semaphores; if the wait times out, give the device one
-- more chance with a zero timeout. Catches the case where the host was
-- suspended during the wait and the GPU has actually finished.
waitTwice :: Device -> SemaphoreWaitInfo -> Word64 -> IO Result
waitTwice dev waitInfo t = Timeline.waitSemaphoresSafe dev waitInfo t >>= \case
  TIMEOUT -> Timeline.waitSemaphores dev waitInfo 0
  r       -> pure r
