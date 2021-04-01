{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module MonadVulkan where

import           AutoApply
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar        ( newEmptyMVar
                                                , putMVar
                                                , readMVar
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Word
import           GHC.Generics                   ( Generic )
import           HasVulkan
import           InstrumentDecs
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax     ( addTopDecls )
import           MonadXr                        ( MonadXr
                                                , Xr
                                                )
import qualified MonadXr                       as Xr
import           NoThunks.Class
import           Orphans                        ( )
import           UnliftIO                       ( Async
                                                , MonadUnliftIO(withRunInIO)
                                                , mask
                                                , toIO
                                                )
import           UnliftIO.Async                 ( asyncWithUnmask
                                                , uninterruptibleCancel
                                                )
import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import           Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
                                                ( getBufferDeviceAddress )
import           Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore
                                               as Timeline
import           Vulkan.Extensions.VK_KHR_acceleration_structure
import           Vulkan.Extensions.VK_KHR_ray_tracing_pipeline
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
import           Vulkan.Utils.CommandCheck
import           Vulkan.Utils.Debug             ( nameObject )
import           Vulkan.Utils.QueueAssignment
import           VulkanMemoryAllocator         as VMA
                                         hiding ( getPhysicalDeviceProperties )

----------------------------------------------------------------
-- Define the monad in which most of the program will run
----------------------------------------------------------------

-- | @V@ keeps track of a bunch of "global" handles and performs resource
-- management.
newtype V a = V { unV :: ReaderT GlobalHandles Xr a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadResource
                   )

instance MonadUnliftIO V where
  withRunInIO a = V $ withRunInIO (\r -> a (r . unV))

instance MonadXr V where
  getInstance = V . lift $ Xr.getInstance
  getSystemId = V . lift $ Xr.getSystemId

newtype CmdT m a = CmdT { unCmdT :: ReaderT CommandBuffer m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadIO
                   , MonadResource
                   , HasVulkan
                   )

instance MonadUnliftIO m => MonadUnliftIO (CmdT m) where
  withRunInIO a = CmdT $ withRunInIO (\r -> a (r . unCmdT))

instance HasVulkan V where
  getInstance       = V (asks ghInstance)
  getGraphicsQueue  = V (asks (snd . graphicsQueue . ghQueues))
  getPhysicalDevice = V (asks ghPhysicalDevice)
  getDevice         = V (asks ghDevice)
  getAllocator      = V (asks ghAllocator)

getGraphicsQueueFamilyIndex :: V QueueFamilyIndex
getGraphicsQueueFamilyIndex = V (asks (fst . graphicsQueue . ghQueues))

getRTInfo :: V RTInfo
getRTInfo = V (asks ghRTInfo)

getQueues :: V (Queues (QueueFamilyIndex, Queue))
getQueues = V (asks ghQueues)

getCommandBuffer :: Monad m => CmdT m CommandBuffer
getCommandBuffer = CmdT ask

useCommandBuffer'
  :: forall a m r
   . (Extendss CommandBufferBeginInfo a, PokeChain a, MonadIO m)
  => CommandBuffer
  -> CommandBufferBeginInfo a
  -> CmdT m r
  -> m r
useCommandBuffer' commandBuffer beginInfo (CmdT a) =
  useCommandBuffer commandBuffer beginInfo (runReaderT a commandBuffer)

runV
  :: Instance
  -> PhysicalDevice
  -> RTInfo
  -> Device
  -> Queues (QueueFamilyIndex, Queue)
  -> Allocator
  -> V a
  -> Xr a
runV ghInstance ghPhysicalDevice ghRTInfo ghDevice ghQueues ghAllocator v = do
  (bin, nib) <- liftIO newChan
  let ghRecycleBin = writeChan bin
      ghRecycleNib = do
        (try, block) <- tryReadChan nib
        maybe (Left block) Right <$> tryRead try

  flip runReaderT GlobalHandles { .. } . unV $ v

-- | A bunch of global, unchanging state we cart around
data GlobalHandles = GlobalHandles
  { ghInstance       :: Instance
  , ghPhysicalDevice :: PhysicalDevice
  , ghDevice         :: Device
  , ghAllocator      :: Allocator
  , ghQueues         :: Queues (QueueFamilyIndex, Queue)
  , ghRecycleBin     :: RecycledResources -> IO ()
    -- ^ Filled with resources which aren't destroyed after finishing a frame,
    -- but instead are used by another frame which executes after that one is
    -- retired, (taken from ghRecycleNib)
    --
    -- Make sure not to pass any resources which were created with a frame-only
    -- scope however!
  , ghRecycleNib     :: IO (Either (IO RecycledResources) RecycledResources)
    -- ^ The resources of prior frames waiting to be taken
  , ghRTInfo         :: RTInfo
  }

-- | Information for ray tracing
data RTInfo = RTInfo
  { rtiShaderGroupHandleSize    :: Word32
  , rtiShaderGroupBaseAlignment :: Word32
  }

-- | These are resources which are reused by a later frame when the current
-- frame is retired
data RecycledResources = RecycledResources
  { fImageAvailableSemaphore :: Semaphore
    -- ^ A binary semaphore passed to 'acquireNextImageKHR'
  , fRenderFinishedSemaphore :: Semaphore
    -- ^ A binary semaphore to synchronize rendering and presenting
  , fCommandPool             :: CommandPool
    -- ^ Pool for this frame's commands (might want more than one of these for
    -- multithreaded recording)
  , fDescriptorSet           :: DescriptorSet
    -- ^ A descriptor set for ray tracing
  , fCameraMatricesOffset    :: Word64
  }
  deriving (Generic, NoThunks)

-- | The shape of all the queues we use for our program, parameterized over the
-- queue type so we can use it with 'Vulkan.Utils.QueueAssignment.assignQueues'
newtype Queues q = Queues { graphicsQueue :: q }
  deriving (Functor, Foldable, Traversable)

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- Start an async thread which will be cancelled by the end of the ResourceT
-- block
spawn :: V a -> V (Async a)
spawn a = do
  aIO <- toIO a
  -- If we don't remove the release key when the thread is done it'll leak,
  -- remove it at the end of the async action when the thread is going to die
  -- anyway.
  --
  -- Mask this so there's no chance we're inturrupted before writing the mvar.
  kv  <- liftIO newEmptyMVar
  UnliftIO.mask $ \_ -> do
    (k, r) <- allocate
      (asyncWithUnmask
        (\unmask -> unmask $ aIO <* (unprotect =<< liftIO (readMVar kv)))
      )
      uninterruptibleCancel
    liftIO $ putMVar kv k
    pure r

spawn_ :: V () -> V ()
spawn_ = void . spawn

----------------------------------------------------------------
-- Commands
----------------------------------------------------------------

--
-- Wrap a bunch of Vulkan commands so that they automatically pull global
-- handles from any `HasVulkan` instance.
--
-- Wrapped functions are suffixed with "'"
--
do
  let vmaCommands =
        [ 'withBuffer
        , 'VMA.withMappedMemory
        , 'VMA.withMemory
        , 'invalidateAllocation
        , 'flushAllocation
        ]
      commands =
        [ 'acquireNextImageKHRSafe
        , 'allocateCommandBuffers
        , 'allocateDescriptorSets
        , 'buildAccelerationStructuresKHR
        , 'cmdBindDescriptorSets
        , 'cmdBindPipeline
        , 'cmdBuildAccelerationStructuresKHR
        , 'cmdDispatch
        , 'cmdDraw
        , 'cmdPipelineBarrier
        , 'cmdPushConstants
        , 'cmdSetScissor
        , 'cmdSetViewport
        , 'cmdTraceRaysKHR
        , 'cmdUseRenderPass
        , 'deviceWaitIdle
        , 'deviceWaitIdleSafe
        , 'getAccelerationStructureBuildSizesKHR
        , 'getAccelerationStructureDeviceAddressKHR
        , 'getBufferDeviceAddress
        , 'getDeviceQueue
        , 'getPhysicalDeviceSurfaceCapabilitiesKHR
        , 'getPhysicalDeviceSurfaceFormatsKHR
        , 'getPhysicalDeviceSurfacePresentModesKHR
        , 'getRayTracingShaderGroupHandlesKHR
        , 'getSemaphoreCounterValue
        , 'getSwapchainImagesKHR
        , 'nameObject
        , 'queuePresentKHR
        , 'resetCommandPool
        , 'updateDescriptorSets
        , 'waitForFences
        , 'waitForFencesSafe
        , 'Timeline.waitSemaphores
        , 'Timeline.waitSemaphoresSafe
        , 'withAccelerationStructureKHR
        , 'withCommandBuffers
        , 'withCommandPool
        , 'withComputePipelines
        , 'withDescriptorPool
        , 'withDescriptorSetLayout
        , 'withFence
        , 'withFramebuffer
        , 'withGraphicsPipelines
        , 'withImageView
        , 'withInstance
        , 'withPipelineLayout
        , 'withRayTracingPipelinesKHR
        , 'withRenderPass
        , 'withSemaphore
        , 'withShaderModule
        , 'withSwapchainKHR
        ]
  addTopDecls =<< [d|checkCommands = $(checkCommandsExp commands)|]
  ds <- autoapplyDecs
    (<> "'")
    [ 'getDevice
    , 'getPhysicalDevice
    , 'getInstance
    , 'getAllocator
    , 'noAllocationCallbacks
    , 'noPipelineCache
    , 'getCommandBuffer
    ]
    -- Allocate doesn't subsume the continuation type on the "with" commands, so
    -- put it in the unifying group.
    ['allocate]
    (vmaCommands <> commands)
  instrumentDecs (Just . init . nameBase) ds
