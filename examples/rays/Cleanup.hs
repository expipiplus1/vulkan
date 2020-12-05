{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}

module Cleanup where

import           Control.Concurrent.Chan.Unagi
import           Control.Exception              ( throwIO )
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Word
import           GHC.IO.Exception               ( IOErrorType(TimeExpired)
                                                , IOException(IOError)
                                                )
import           MonadVulkan
import           NoThunks.Class                 ( InspectHeap(..)
                                                , NoThunks
                                                )
import           Vulkan.Core10
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import           Vulkan.NamedType
import           Vulkan.Zero

data Cleaner = Cleaner
  { cChanIn  :: (V RecycledResources, V ()) -> IO ()
  , cChanOut :: IO (V RecycledResources, V ())
  }
  deriving NoThunks via InspectHeap Cleaner

newCleaner :: Word64 -> Semaphore -> V Cleaner
newCleaner nextIndex sem = do
  (inChan, outChan) <- liftIO newChan
  let cChanIn  = writeChan inChan
      cChanOut = readChan outChan
  spawn_ $ cleanupThread cChanOut nextIndex sem
  pure Cleaner { .. }

pushCleanup :: Cleaner -> V RecycledResources -> V () -> V ()
pushCleanup Cleaner {..} recycle discard = liftIO $ cChanIn (recycle, discard)

-- | A thread which watches the frame finished semaphore and performs frame
-- cleanup when it advances.
--
-- A frame should push work onto the cleanup queue iff if increments the
-- semaphore.
cleanupThread
  :: IO (V RecycledResources, V ())
  -- ^ An IO action which resets any resources and returns the set of resources
  -- ready to be used.
  -> Word64
  -- ^ The index to wait for before recycling the resources
  -> Semaphore
  -- ^ The timeline semaphore containing that index
  -> V a
cleanupThread getCleanup nextIndex sem = do
  -- Make sure we have something worth waiting for, otherwise we could be
  -- waiting for a semaphore which won't increment
  firstCleanup <- liftIO getCleanup

  -- Wait for the semaphore to reach our value
  let waitInfo  = zero { semaphores = [sem], values = [nextIndex] }
      oneSecond = 1e9
  waitTwice waitInfo oneSecond >>= \case
    TIMEOUT ->
      timeoutError "Timed out (1s) waiting for frame to finish on Device"
    _ -> pure ()

  -- See if we can release more than one frame
  v <- getSemaphoreCounterValue' sem
  let nextIndex'     = succ v
      numExtraFrames = fromIntegral (v - nextIndex)

  runCleanup firstCleanup
  replicateM_ numExtraFrames $ runCleanup =<< liftIO getCleanup

  cleanupThread getCleanup nextIndex' sem

runCleanup :: (V RecycledResources, V ()) -> V ()
runCleanup (releaseResources, finalRetire) = do
  -- Get the next resources and send them down the line
  rs  <- releaseResources
  -- Signal we're done by making the recycled resources available
  bin <- V $ asks ghRecycleBin
  liftIO $ bin rs
  -- Release anything else
  finalRetire

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

-- | Wait for some semaphores, if the wait times out give the frame one last
-- chance to complete with a zero timeout.
--
-- It could be that the program was suspended during the preceding
-- wait causing it to timeout, this will check if it actually
-- finished.
waitTwice :: SemaphoreWaitInfo -> ("timeout" ::: Word64) -> V Result
waitTwice waitInfo t = waitSemaphoresSafe' waitInfo t >>= \case
  TIMEOUT -> waitSemaphores' waitInfo 0
  r       -> pure r

timeoutError :: MonadIO m => String -> m a
timeoutError message =
  liftIO . throwIO $ IOError Nothing TimeExpired "" message Nothing Nothing
