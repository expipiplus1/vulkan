{-# LANGUAGE OverloadedLists #-}
module MonadFrame
  ( F
  , runFrame
  , liftV
  , allocateGlobal
  , allocateGlobal_
  , frameRefCount
  , askFrame
  , asksFrame
  , finalQueueSubmitFrame
  , queueSubmitFrame
  ) where


import           Cleanup
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , ask
                                                , asks
                                                , runReaderT
                                                )
import           Control.Monad.Trans.Resource
import           Data.Vector                    ( Vector )
import           Frame
import           HasVulkan
import           InstrumentDecs                 ( withSpan_ )
import           MonadVulkan
import           RefCounted
import           UnliftIO
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import           Vulkan.Zero                    ( Zero(zero) )

newtype F a = F {unF :: ReaderT Frame V a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , HasVulkan
                   )

instance MonadUnliftIO F where
  withRunInIO a = F $ withRunInIO (\r -> a (r . unF))

----------------------------------------------------------------
-- Vulkan Operations
----------------------------------------------------------------

-- | Runs a frame and spawns a thread to wait for the GPU work to complete, at
-- which point the frame-specific resources are collected.
runFrame :: Frame -> F a -> V a
runFrame f@Frame {..} (F r) = runReaderT r f `finally` do
  let recycleResources = do
        withSpan_ "resetCommandPool"
          $ resetCommandPool' (fCommandPool fRecycledResources) zero
        pure fRecycledResources
      finalRetire = withSpan_ "final retire" $ retireFrame f
  liftIO (readIORef fWorkProgress) >>= \case
    -- If we have no work on the GPU we can recycle things here and now
    NoWorkSubmitted   -> runCleanup (recycleResources, finalRetire)
    -- Otherwise we need to wait for whatever GPU work we submitted to
    -- complete, make sure the frame semaphore is incremented and push the work
    -- to the cleanup queue
    SomeWorkSubmitted -> do
      graphicsQueue <- getGraphicsQueue
      queueSubmit
        graphicsQueue
        [ SomeStruct
            (   zero { waitDstStageMask = [PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT]
                     , signalSemaphores = [fRenderFinishedHostSemaphore]
                     }
            ::& zero { signalSemaphoreValues = [fIndex] }
            :&  ()
            )
        ]
        NULL_HANDLE
      pushCleanup fCleaner recycleResources finalRetire
    AllWorkSubmitted -> pushCleanup fCleaner recycleResources finalRetire

-- | Submit the specified work and set 'fWorkProgress' to 'SomeWorkSubmitted'
queueSubmitFrame :: Vector (SomeStruct SubmitInfo) -> F ()
queueSubmitFrame ss = do
  workProgress <- asksFrame fWorkProgress
  q            <- getGraphicsQueue
  mask $ \_ -> do
    liftIO $ writeIORef workProgress SomeWorkSubmitted
    queueSubmit q ss NULL_HANDLE

-- | Submit the specified work and set 'fWorkProgress' to 'AllWorkSubmitted'
--
-- A 'SubmitInfo' must increment 'fRenderFinishedHostSemaphore' to 'fIndex'
finalQueueSubmitFrame :: Vector (SomeStruct SubmitInfo) -> F ()
finalQueueSubmitFrame ss = do
  workProgress <- asksFrame fWorkProgress
  q            <- getGraphicsQueue
  mask $ \_ -> do
    liftIO $ writeIORef workProgress AllWorkSubmitted
    queueSubmit q ss NULL_HANDLE

liftV :: V a -> F a
liftV = F . lift

----------------------------------------------------------------
-- Resource handling
----------------------------------------------------------------

-- | By default resources allocated will only last until the frame is retired,
-- i.e. the GPU work is complete.
--
-- To allocate something globally use 'allocateGlobal'
instance MonadResource F where
  liftResourceT r = do
    i <- asksFrame (snd . fResources)
    liftIO $ runInternalState r i

-- | Allocate a resource in the 'V' scope
allocateGlobal
  :: F a
  -- ^ Create to be calle dnow
  -> (a -> F ())
  -- ^ Destroy, to be called at program termination
  -> F (ReleaseKey, a)
allocateGlobal create destroy = do
  createIO <- toIO create
  run      <- askRunInIO
  F $ allocate createIO (run . destroy)

-- | c.f. 'bracket' and 'bracket_'
allocateGlobal_ :: F a -> F () -> F (ReleaseKey, a)
allocateGlobal_ create destroy = allocateGlobal create (const destroy)

-- | Free frame resources, the frame must have finished GPU execution first.
retireFrame :: MonadIO m => Frame -> m ()
retireFrame Frame {..} = release (fst fResources)

-- | Make sure a reference is held until this frame is retired
frameRefCount :: RefCounted -> F ()
frameRefCount = resourceTRefCount

----------------------------------------------------------------
-- Small Operations
----------------------------------------------------------------

-- | Get the current 'Frame'
askFrame :: F Frame
askFrame = F ask

-- | Get a function of the current 'Frame'
asksFrame :: (Frame -> a) -> F a
asksFrame = F . asks
