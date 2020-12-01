{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Defines the 'Frame' type, most interesting operations regarding 'Frame's
-- can be found in 'MonadFrame'
module Frame where

import           AccelerationStructure
import           Camera
import           Control.Arrow                  ( Arrow((&&&)) )
import           Control.Monad                  ( zipWithM )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Trans.Reader     ( asks )
import           Control.Monad.Trans.Resource   ( InternalState
                                                , ReleaseKey
                                                , allocate
                                                , closeInternalState
                                                , createInternalState
                                                )
import           Data.Foldable
import           Data.IORef
import qualified Data.Vector                   as V
import           Data.Word
import           Foreign.Ptr                    ( Ptr
                                                , castPtr
                                                )
import           Foreign.Storable
import           GHC.Generics
import           MonadVulkan
import           NoThunks.Class
import           Orphans                        ( )
import qualified Pipeline
import qualified SDL
import           SDL                            ( Window )
import qualified SDL.Video.Vulkan              as SDL
import           Scene
import           Swapchain
import           Vulkan.CStruct.Extends
import           Vulkan.Core10
import           Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
import           Vulkan.Extensions.VK_KHR_acceleration_structure
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Utils.QueueAssignment
import           Vulkan.Zero
import           VulkanMemoryAllocator

-- | Must be positive, duh
numConcurrentFrames :: Word64
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
  , fAccelerationStructure       :: AccelerationStructureKHR
  , fShaderBindingTable          :: Buffer
  , fShaderBindingTableAddress   :: DeviceAddress
  , fCameraMatricesBuffer        :: Buffer
  , fCameraMatricesAllocation    :: Allocation
  , fCameraMatricesBufferData    :: Ptr CameraMatrices
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
  deriving (Generic, NoThunks)

initialRecycledResources :: Word64 -> DescriptorSet -> V RecycledResources
initialRecycledResources index fDescriptorSet = do
  (_, fImageAvailableSemaphore) <- withSemaphore'
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ())

  (_, fRenderFinishedSemaphore) <- withSemaphore'
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_BINARY 0 :& ())

  graphicsQueueFamilyIndex <- getGraphicsQueueFamilyIndex
  (_, fCommandPool)        <- withCommandPool' zero
    { queueFamilyIndex = unQueueFamilyIndex graphicsQueueFamilyIndex
    }
  let fCameraMatricesOffset =
        index * fromIntegral (sizeOf (undefined :: CameraMatrices))

  pure RecycledResources { .. }

-- | Create a 'Frame' from scratch
initialFrame :: Window -> SurfaceKHR -> V Frame
initialFrame fWindow fSurface = do
  let fIndex = 1

  -- Create our swapchain for this 'Window'
  -- These resources will last for longer than this frame
  SDL.V2 width height <- SDL.vkGetDrawableSize fWindow
  let windowSize   = Extent2D (fromIntegral width) (fromIntegral height)
      oldSwapchain = NULL_HANDLE
  fSwapchainResources <- allocSwapchainResources oldSwapchain
                                                 windowSize
                                                 fSurface

  sceneBuffers                <- makeSceneBuffers

  -- The acceleration structure
  (_, fAccelerationStructure) <- createTLAS sceneBuffers

  -- Create the RT pipeline
  (_, descriptorSetLayout   ) <- Pipeline.createRTDescriptorSetLayout
  (_, fPipelineLayout) <- Pipeline.createRTPipelineLayout descriptorSetLayout
  (_, fPipeline, numGroups)   <- Pipeline.createPipeline fPipelineLayout
  (_, fShaderBindingTable)    <- Pipeline.createShaderBindingTable fPipeline
                                                                   numGroups
  fShaderBindingTableAddress <- getBufferDeviceAddress' zero
    { buffer = fShaderBindingTable
    }
  descriptorSets <- Pipeline.createRTDescriptorSets
    descriptorSetLayout
    fAccelerationStructure
    sceneBuffers
    (fromIntegral numConcurrentFrames)

  (_, (fCameraMatricesBuffer, fCameraMatricesAllocation, bufferAllocInfo)) <-
    withBuffer'
      zero
        { size  = numConcurrentFrames * fromIntegral
                    (sizeOf (error "sizeof evaluated" :: CameraMatrices))
        , usage = BUFFER_USAGE_UNIFORM_BUFFER_BIT
        }
      zero { flags         = ALLOCATION_CREATE_MAPPED_BIT
           , usage         = MEMORY_USAGE_CPU_TO_GPU
           , requiredFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT
           }
  let fCameraMatricesBufferData =
        castPtr @() @CameraMatrices (mappedData bufferAllocInfo)

  -- Don't keep the release key, this semaphore lives for the lifetime of the
  -- application
  (_, fRenderFinishedHostSemaphore) <- withSemaphore'
    (zero ::& SemaphoreTypeCreateInfo SEMAPHORE_TYPE_TIMELINE 0 :& ())

  -- Create the 'RecycledResources' necessary to kick off the rest of the
  -- concurrent frames and push them into the chan.
  let (ourDescriptorSet, otherDescriptorSets) =
        (V.head &&& (toList . V.tail)) descriptorSets
  ~(fRecycledResources : otherRecycledResources) <- zipWithM
    initialRecycledResources
    [0 ..]
    (ourDescriptorSet : otherDescriptorSets)
  bin <- V $ asks ghRecycleBin
  liftIO $ for_ otherRecycledResources bin

  fGPUWork   <- liftIO $ newIORef mempty
  -- Create the frame resource tracker at the global level so it's closed
  -- correctly on exception
  fResources <- allocate createInternalState closeInternalState

  pure Frame { .. }

-- | Create the next frame
advanceFrame :: Bool -> Frame -> V Frame
advanceFrame needsNewSwapchain f = do
  -- Wait for a prior frame to finish, then we can steal it's resources!
  nib                <- V $ asks ghRecycleNib
  fRecycledResources <- withSpan_ "CPU is ahead" $ liftIO $ nib >>= \case
    Left  block -> block
    Right rs    -> pure rs

  fSwapchainResources <- if needsNewSwapchain
    then recreateSwapchainResources (fWindow f) (fSwapchainResources f)
    else pure $ fSwapchainResources f

  -- The per-frame resource helpers need to be created fresh
  fGPUWork   <- liftIO $ newIORef mempty
  fResources <- allocate createInternalState closeInternalState

  let f' = Frame
        { fIndex                       = succ (fIndex f)
        , fWindow                      = fWindow f
        , fSurface                     = fSurface f
        , fSwapchainResources
        , fPipeline                    = fPipeline f
        , fPipelineLayout              = fPipelineLayout f
        , fShaderBindingTable          = fShaderBindingTable f
        , fShaderBindingTableAddress   = fShaderBindingTableAddress f
        , fAccelerationStructure       = fAccelerationStructure f
        , fCameraMatricesBuffer        = fCameraMatricesBuffer f
        , fCameraMatricesAllocation    = fCameraMatricesAllocation f
        , fCameraMatricesBufferData    = fCameraMatricesBufferData f
        , fRenderFinishedHostSemaphore = fRenderFinishedHostSemaphore f
        , fGPUWork
        , fResources
        , fRecycledResources
        }
  pure f'
