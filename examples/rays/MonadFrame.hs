module MonadFrame
  ( F
  , runFrame
  , liftV
  , queueSubmitFrame
  , allocateGlobal
  , allocateGlobal_
  , frameRefCount
  , askFrame
  , asksFrame
  ) where


import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Reader     ( ReaderT
                                                , ask
                                                , asks
                                                , runReaderT
                                                )
import           Control.Monad.Trans.Resource
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Data.Word
import           Frame
import           GHC.IO.Exception               ( IOErrorType(TimeExpired)
                                                , IOException(IOError)
                                                )
import           MonadVulkan
import           RefCounted
import           Say                            ( sayErrString )
import           UnliftIO
import           Vulkan.CStruct.Extends         ( SomeStruct )
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
  waits <- liftIO $ readIORef fGPUWork
  let oneSecond = 1e9 -- one second
  spawn_ $ do
    -- Wait for the GPU work to finish (if we have any)
    unless (null waits) $ do
      let waitInfo = zero { semaphores = V.fromList (fst <$> waits)
                          , values     = V.fromList (snd <$> waits)
                          }
      waitSemaphoresSafe' waitInfo oneSecond >>= \case
        TIMEOUT ->
          timeoutError "Timed out (1s) waiting for frame to finish on Device"
        _ -> pure ()

    -- Free resources wanted elsewhere now, all those in RecycledResources
    resetCommandPool' (fCommandPool fRecycledResources) zero

    -- Signal we're done by making the recycled resources available
    bin <- V $ asks ghRecycleBin
    liftIO $ bin fRecycledResources

    -- Destroy frame-specific resources at our leisure
    retireFrame f

-- | 'queueSubmit' and add wait for the 'Fence' before retiring the frame.
queueSubmitFrame
  :: Queue -> Vector (SomeStruct SubmitInfo) -> Semaphore -> Word64 -> F ()
queueSubmitFrame q ss sem value = do
  gpuWork <- asksFrame fGPUWork
  -- Make sure we don't get interrupted between submitting the work and
  -- recording the wait
  mask $ \_ -> do
    queueSubmit q ss NULL_HANDLE
    liftIO $ atomicModifyIORef' gpuWork ((, ()) . ((sem, value) :))

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
retireFrame Frame {..} = do
  sayErrString ("retiring frame " <> show fIndex)
  release (fst fResources)

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

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

timeoutError :: MonadIO m => String -> m a
timeoutError message =
  liftIO . throwIO $ IOError Nothing TimeExpired "" message Nothing Nothing
