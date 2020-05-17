{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | This module defines the 'Frame' data type, as well as functions for using
-- it easily. The 'F' monad is a reader for a 'Frame' and can be consumed by
-- 'runFrame'.
module Frame
  where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource  as ResourceT
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad                  ( void )
import qualified SDL
import           UnliftIO                       ( MonadUnliftIO(..)
                                                , askRunInIO
                                                , mask
                                                , toIO
                                                )
import           UnliftIO.MVar
import           UnliftIO.Exception             ( throwString
                                                , finally
                                                )

import           Data.IORef
import           Data.Word
import           Data.Vector                    ( Vector
                                                , cons
                                                )

import           Vulkan.Core10                 as Vk
                                         hiding ( createDevice
                                                , createFramebuffer
                                                , createImageView
                                                , createInstance
                                                , withBuffer
                                                , withImage
                                                )
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.CStruct.Extends         ( SomeStruct )
import           Vulkan.Zero

import           MonadVulkan

-- | A record of everything required to render a single frame of the
-- application.
data Frame = Frame
  { fIndex                   :: Word64
  , -- SDL Stuff
    fWindow                  :: SDL.Window
    -- Vulkan items
  , fSurface                 :: SurfaceKHR
  , fSwapchain               :: SwapchainKHR
  , fSwapchainFormat         :: Format
  , fRenderPass              :: RenderPass
  , fImageExtent             :: Extent2D
  , fImageAvailableSemaphore :: Semaphore
  , fRenderFinishedSemaphore :: Semaphore
  , fPipeline                :: Pipeline
  , fFramebuffers            :: Word32 -> Framebuffer
  , fReleaseSwapchain        :: RefCounted
    -- Scheduling. TODO, abstract this
  , -- | This 'MVar' will be signaled when this frame has finished rendering on
    -- the GPU
    fCurrentPresented        :: MVar ()
  , -- | These 'MVar's track when previous frames have finished executing on
    -- the GPU
    fLastPresented           :: MVar ()
  , fSecondLastPresented     :: MVar ()
  , fThirdLastPresented      :: MVar ()
    -- | When did we start rendering this frame, in ns
  , fStartTime               :: Word64
    -- | The 'InternalState' for tracking frame-only resources.
  , fResources               :: (ReleaseKey, ResourceT.InternalState)
    -- | A list of 'Fences' of GPU work submitted for this frame.
  , fGPUWork                 :: IORef (Vector Fence)
  }

numConcurrentFrames :: Int
numConcurrentFrames = 3

-- | A monad for running a single frame
newtype F a = F { unF :: ReaderT Frame V a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadFail
                   , MonadIO
                   , HasVulkan
                   )

instance MonadUnliftIO F where
  withRunInIO a = F $ withRunInIO (\r -> a (r . unF))

-- | By default resources allocated will only last until the frame is retired.
--
-- To allocate something globally use 'allocateGlobal'
instance MonadResource F where
  liftResourceT r = do
    i <- asksFrame (snd . fResources)
    liftIO $ runInternalState r i

-- | Allocate a resource in the 'V' scope
allocateGlobal :: F a -> (a -> F ()) -> F (ReleaseKey, a)
allocateGlobal create destroy = do
  createIO  <- toIO create
  run       <- askRunInIO
  F $ allocate createIO (run . destroy)

-- | c.f. 'bracket' and 'bracket_'
allocateGlobal_ :: F a -> F () -> F (ReleaseKey, a)
allocateGlobal_ create destroy = allocateGlobal create (const destroy)

-- | Run a frame
--
-- The frame will be retired by another thread when all the fences added by
-- 'queueSubmitFrame' have been signaled.
runFrame :: Frame -> F a -> V a
runFrame f (F r) = runReaderT r f `finally` do
  fences <- liftIO $ readIORef (fGPUWork f)
  -- Wait for this frame to be presented in another thread before retiring
  spawn_ $ do
    waitForFencesSafe' fences True 1e9 >>= \case
      TIMEOUT -> throwString "Timed out waiting for frame to finish on the GPU"
      _       -> pure ()
    commandPool <- getCommandPool (commandPoolIndex f)
    resetCommandPool' commandPool zero

    putMVar (fCurrentPresented f) ()
    retireFrame f

askFrame :: F Frame
askFrame = F ask

asksFrame :: (Frame -> a) -> F a
asksFrame = F . asks

-- | Get a fresh command pool for this frame, it will be reset upon frame
-- retirement
frameCommandPool :: F CommandPool
frameCommandPool = do
  poolIndex <- commandPoolIndex <$> askFrame
  F . lift . getCommandPool $ fromIntegral poolIndex

commandPoolIndex :: Frame -> Int
commandPoolIndex Frame{..} = fromIntegral fIndex `mod` numConcurrentFrames

-- | Free frame resources, the frame must have finished GPU execution first.
retireFrame :: MonadIO m => Frame -> m ()
retireFrame Frame {..} =
  release (fst fResources)

-- | 'queueSubmit' and add wait for the 'Fence' before retiring the frame.
queueSubmitFrame :: Queue -> Vector (SomeStruct SubmitInfo) -> Fence -> F ()
queueSubmitFrame q ss fence = do
  queueSubmit q ss fence
  gpuWork <- asksFrame fGPUWork
  liftIO $ atomicModifyIORef' gpuWork ((, ()) . cons fence)

----------------------------------------------------------------
-- Ref counting helper
----------------------------------------------------------------

-- A 'RefCounted' will perform the specified action when the count reaches 0
data RefCounted = RefCounted
  { rcCount  :: IORef Word
  , rcAction :: IO ()
  }

newRefCounted :: MonadIO m => IO () -> m RefCounted
newRefCounted rcAction = do
  rcCount <- liftIO $ newIORef 1
  pure RefCounted { .. }

-- | Decrement the ref counted value, the action will be run promptly and in
-- this thread if the counter reached 0.
releaseRefCounted :: MonadIO m => RefCounted -> m ()
releaseRefCounted RefCounted {..} = liftIO $ mask $ \_ ->
  atomicModifyIORef' rcCount (\c -> (pred c, pred c)) >>= \case
    0 -> rcAction
    _ -> pure ()

useRefCounted :: MonadIO m => RefCounted -> m ()
useRefCounted RefCounted {..} =
  liftIO $ atomicModifyIORef' rcCount (\c -> (succ c, ()))

-- | Make sure a reference is held until this frame is retired
frameRefCount :: RefCounted -> F ()
frameRefCount r = void $ allocate_ (useRefCounted r) (releaseRefCounted r)
