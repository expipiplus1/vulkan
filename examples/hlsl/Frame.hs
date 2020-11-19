-- | Defines the 'Frame' type, most interesting operations regarding 'Frame's
-- can be found in 'MonadFrame'
module Frame where

import           Control.Concurrent             ( MVar
                                                , newEmptyMVar
                                                )
import           Control.Concurrent             ( newMVar )
import           Control.Concurrent.MVar        ( takeMVar )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Trans.Resource   ( InternalState
                                                , ReleaseKey
                                                , allocate
                                                , closeInternalState
                                                , createInternalState
                                                )
import           Data.IORef
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Seq
import           Data.Word
import           MonadVulkan
import qualified Pipeline
import qualified SDL
import           SDL                            ( Window )
import qualified SDL.Video.Vulkan              as SDL
import           Swapchain
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Zero

numConcurrentFrames :: Int
numConcurrentFrames = 3

-- | All the information required to render a single frame
data Frame = Frame
  { fIndex                       :: Word64 -- ^ Which number frame is this
    -- SDL things
  , fWindow                      :: SDL.Window
    -- Vulkan things
  , fSurface                     :: SurfaceKHR
  , fSwapchainResources          :: SwapchainResources
  , fPipeline                    :: Pipeline
  , fRenderFinishedHostSemaphore :: Semaphore
    -- ^ A timeline semaphore which increments to fIndex when this frame is
    -- done, the host can wait on this semaphore
  , fPriorResources              :: Seq (MVar RecycledResources)
    -- ^ The resources of prior frames, waiting to be taken, TODO, would a
    -- queue be better here...
  , fRecycledResources           :: RecycledResources
    -- ^ Resources which aren't destroyed during this frame, but instead are
    -- used by another frame which executes after this one is retired, (passed
    -- to it via the following MVar)
  , fRenderFinishedMVar          :: MVar RecycledResources
    -- ^ An 'MVar' which is empty until the rendering is finished for this
    -- frame, when this frame is retired it's recycled resources are placed in
    -- this 'MVar' for a potential future frame to plunder.
    --
    -- Make sure not to pass any resources which were created with a frame-only
    -- scope however!
  , fGPUWork                     :: IORef [(Semaphore, Word64)]
    -- ^ Timeline semaphores and corresponding wait values, updates as the
    -- frame progresses.
  , fResources                   :: (ReleaseKey, InternalState)
    -- ^ The 'InternalState' for tracking frame-local resources along with the
    -- key to release it in the global scope. This will be released when the
    -- frame is done with GPU work.
  }

-- | These are resources which are reused by a later frame when the current
-- frame is retired
data RecycledResources = RecycledResources
  { fImageAvailableSemaphore :: Semaphore
    -- ^ A binary semaphore passed to 'acquireNextImageKHR'
  , fRenderFinishedSemaphore :: Semaphore
    -- ^ A binary semaphore to synchronize rendering and presenting
  }

initialRecycledResources :: V RecycledResources
initialRecycledResources = do
  (_, fImageAvailableSemaphore) <- withSemaphore'
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ())
  (_, fRenderFinishedSemaphore) <- withSemaphore'
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ())
  pure RecycledResources { .. }

initialFrame :: Window -> SurfaceKHR -> V Frame
initialFrame fWindow fSurface = do
  let fIndex = 1
  SDL.V2 width height <- SDL.vkGetDrawableSize fWindow
  let windowSize   = Extent2D (fromIntegral width) (fromIntegral height)
      oldSwapchain = NULL_HANDLE
  fSwapchainResources <- allocSwapchainResources oldSwapchain
                                                 windowSize
                                                 fSurface

  -- TODO: Cache this
  (_releasePipeline, fPipeline) <- Pipeline.createPipeline
    (srRenderPass fSwapchainResources)

  -- Don't keep the release key, this semaphore lives for the lifetime of the
  -- application
  (_, fRenderFinishedHostSemaphore) <- withSemaphore'
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_TIMELINE 0 :& ())

  fPriorResources <- Seq.replicateM
    (numConcurrentFrames - 1)
    (liftIO . newMVar =<< initialRecycledResources)
  fRecycledResources  <- initialRecycledResources

  fRenderFinishedMVar <- liftIO newEmptyMVar
  fGPUWork            <- liftIO $ newIORef mempty
  -- Create this resource object at the global level so it's closed correctly
  -- on exception
  fResources          <- allocate createInternalState closeInternalState

  pure Frame { .. }

-- | Create the next frame
advanceFrame :: Frame -> V Frame
advanceFrame f = do
  -- Wait for a prior frame to finish, then we can steal it's resources!
  let rs :|> r = fPriorResources f
  fRecycledResources <- liftIO $ takeMVar r

  let fPriorResources     = r :<| rs
      fRenderFinishedMVar = r

  -- The per-frame resource helpers need to be created fresh
  fGPUWork   <- liftIO $ newIORef mempty
  fResources <- allocate createInternalState closeInternalState

  pure Frame { fIndex                       = succ (fIndex f)
             , fWindow                      = fWindow f
             , fSurface                     = fSurface f
             , fSwapchainResources          = fSwapchainResources f
             , fPipeline                    = fPipeline f
             , fPriorResources
             , fRenderFinishedHostSemaphore = fRenderFinishedHostSemaphore f
             , fRenderFinishedMVar
             , fGPUWork
             , fResources
             , fRecycledResources
             }
