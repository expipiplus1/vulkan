-- | Defines the 'Frame' type, most interesting operations regarding 'Frame's
-- can be found in 'MonadFrame'
module Frame where

import           Control.Monad                  ( replicateM_ )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Trans.Reader     ( asks )
import           Control.Monad.Trans.Resource   ( InternalState
                                                , ReleaseKey
                                                , allocate
                                                , closeInternalState
                                                , createInternalState
                                                )
import           Data.IORef
import           Data.Word
import           MonadVulkan
import qualified Pipeline
import qualified SDL
import           SDL                            ( Window )
import qualified SDL.Video.Vulkan              as SDL
import           Say
import           Swapchain
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Utils.QueueAssignment
import           Vulkan.Zero
import Data.Vector (Vector)

-- | Must be positive, duh
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
  , fPipelineLayout              :: PipelineLayout
  , fDescriptorSets              :: Vector DescriptorSet
  , fShaderBindingTable          :: Buffer
  , fRenderFinishedHostSemaphore :: Semaphore
    -- ^ A timeline semaphore which increments to fIndex when this frame is
    -- done, the host can wait on this semaphore
  , fRecycledResources           :: RecycledResources
    -- ^ Resources which can be used for this frame and are then passed on to a
    -- later frame.
  , fGPUWork                     :: IORef [(Semaphore, Word64)]
    -- ^ Timeline semaphores and corresponding wait values, updates as the
    -- frame progresses.
  , fResources                   :: (ReleaseKey, InternalState)
    -- ^ The 'InternalState' for tracking frame-local resources along with the
    -- key to release it in the global scope. This will be released when the
    -- frame is done with GPU work.
  }

initialRecycledResources :: V RecycledResources
initialRecycledResources = do
  (_, fImageAvailableSemaphore) <- withSemaphore'
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ())

  (_, fRenderFinishedSemaphore) <- withSemaphore'
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ())

  graphicsQueueFamilyIndex <- getGraphicsQueueFamilyIndex
  (_, fCommandPool)        <- withCommandPool' zero
    { queueFamilyIndex = unQueueFamilyIndex graphicsQueueFamilyIndex
    }

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
  -- TODO: Recreate this if the swapchain format changes
  (_releaseDescriptorSetLayout, descriptorSetLayout) <-
    Pipeline.createRTDescriptorSetLayout
  (_releasePipelineLayout, fPipelineLayout) <- Pipeline.createRTPipelineLayout
    descriptorSetLayout
  (_releasePipeline, fPipeline) <- Pipeline.createPipeline
    fPipelineLayout
  (_releaseSBT, fShaderBindingTable) <- Pipeline.createShaderBindingTable
    fPipeline
  fDescriptorSets <- Pipeline.createRTDescriptorSets
    descriptorSetLayout
    (srImageViews fSwapchainResources)

  -- Don't keep the release key, this semaphore lives for the lifetime of the
  -- application
  (_, fRenderFinishedHostSemaphore) <- withSemaphore'
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_TIMELINE 0 :& ())

  bin <- V $ asks ghRecycleBin
  replicateM_ (numConcurrentFrames - 1)
    $   liftIO
    .   bin
    =<< initialRecycledResources
  fRecycledResources <- initialRecycledResources

  fGPUWork           <- liftIO $ newIORef mempty
  -- Create this resource object at the global level so it's closed correctly
  -- on exception
  fResources         <- allocate createInternalState closeInternalState

  pure Frame { .. }

-- | Create the next frame
advanceFrame :: Bool -> Frame -> V Frame
advanceFrame needsNewSwapchain f = do
  -- Wait for a prior frame to finish, then we can steal it's resources!
  nib                <- V $ asks ghRecycleNib
  fRecycledResources <- liftIO $ nib >>= \case
    Left block -> do
      sayErr "CPU is running ahead"
      block
    Right rs -> pure rs

  fSwapchainResources <- if needsNewSwapchain
    then recreateSwapchainResources (fWindow f) (fSwapchainResources f)
    else pure $ fSwapchainResources f

  -- The per-frame resource helpers need to be created fresh
  fGPUWork   <- liftIO $ newIORef mempty
  fResources <- allocate createInternalState closeInternalState

  pure Frame { fIndex                       = succ (fIndex f)
             , fWindow                      = fWindow f
             , fSurface                     = fSurface f
             , fSwapchainResources
             , fPipeline                    = fPipeline f
             , fPipelineLayout              = fPipelineLayout f
             , fDescriptorSets              = fDescriptorSets f
             , fShaderBindingTable          = fShaderBindingTable f
             , fRenderFinishedHostSemaphore = fRenderFinishedHostSemaphore f
             , fGPUWork
             , fResources
             , fRecycledResources
             }
