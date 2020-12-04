{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module MonadVulkan where

import           AutoApply
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Word
import           HasVulkan
import           UnliftIO
import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import           Vulkan.Extensions.VK_KHR_surface
import           Vulkan.Extensions.VK_KHR_swapchain
import           VulkanMemoryAllocator         as VMA
                                         hiding ( getPhysicalDeviceProperties )


----------------------------------------------------------------
-- Define the monad in which most of the program will run
----------------------------------------------------------------

-- | @V@ keeps track of a bunch of "global" handles and performs resource
-- management.
newtype V a = V { unV :: ReaderT GlobalHandles (ResourceT IO) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadFail
                   , MonadIO
                   , MonadResource
                   )

instance MonadUnliftIO V where
  withRunInIO a = V $ withRunInIO (\r -> a (r . unV))

newtype CmdT m a = CmdT { unCmdT :: ReaderT CommandBuffer m a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadFail
                   , MonadIO
                   , MonadResource
                   , HasVulkan
                   )

instance MonadUnliftIO m => MonadUnliftIO (CmdT m) where
  withRunInIO a = CmdT $ withRunInIO (\r -> a (r . unCmdT))

instance HasVulkan V where
  getInstance       = V (asks ghInstance)
  getGraphicsQueue  = V (asks ghGraphicsQueue)
  getPhysicalDevice = V (asks ghPhysicalDevice)
  getDevice         = V (asks ghDevice)
  getAllocator      = V (asks ghAllocator)

getGraphicsQueueFamilyIndex :: V Word32
getGraphicsQueueFamilyIndex = V (asks ghGraphicsQueueFamilyIndex)

getCommandBuffer :: Monad m => CmdT m CommandBuffer
getCommandBuffer = CmdT ask

getCommandPool :: Int -> V CommandPool
getCommandPool i = V (asks ((V.! i) . ghCommandPools))

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
  -> Device
  -> Queue
  -> Word32
  -> Vector CommandPool
  -> Allocator
  -> V a
  -> ResourceT IO a
runV ghInstance ghPhysicalDevice ghDevice ghGraphicsQueue ghGraphicsQueueFamilyIndex ghCommandPools ghAllocator
  = flip runReaderT GlobalHandles { .. } . unV

-- Start an async thread which will be cancelled at the end of the ResourceT
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

data GlobalHandles = GlobalHandles
  { ghInstance                 :: Instance
  , ghPhysicalDevice           :: PhysicalDevice
  , ghDevice                   :: Device
  , ghAllocator                :: Allocator
  , ghGraphicsQueue            :: Queue
  , ghGraphicsQueueFamilyIndex :: Word32
  , ghCommandPools             :: Vector CommandPool
  }

--
-- Wrap a bunch of Vulkan commands so that they automatically pull global
-- handles from any `HasVulkan` instance.
--
-- Wrapped functions are suffixed with "'"
--
autoapplyDecs
  (<> "'")
  [ 'getDevice
  , 'getPhysicalDevice
  , 'getInstance
  , 'getAllocator
  , 'noAllocationCallbacks
  , 'getCommandBuffer
  ]
  -- Allocate doesn't subsume the continuation type on the "with" commands, so
  -- put it in the unifying group.
  ['allocate]
  [ 'invalidateAllocation
  , 'withBuffer
  , 'deviceWaitIdle
  , 'getDeviceQueue
  , 'waitForFences
  , 'waitForFencesSafe
  , 'withCommandBuffers
  , 'withCommandPool
  , 'withFence
  , 'withComputePipelines
  , 'withInstance
  , 'withPipelineLayout
  , 'withShaderModule
  , 'withDescriptorPool
  , 'allocateDescriptorSets
  , 'withDescriptorSetLayout
  , 'updateDescriptorSets
  , 'cmdBindPipeline
  , 'cmdBindDescriptorSets
  , 'cmdDispatch
  , 'withSwapchainKHR
  , 'getPhysicalDeviceSurfaceCapabilitiesKHR
  , 'getPhysicalDeviceSurfacePresentModesKHR
  , 'getPhysicalDeviceSurfaceFormatsKHR
  , 'withGraphicsPipelines
  , 'withRenderPass
  , 'getSwapchainImagesKHR
  , 'withImageView
  , 'withFramebuffer
  , 'acquireNextImageKHR
  , 'withSemaphore
  , 'deviceWaitIdleSafe
  , 'resetCommandPool
  , 'allocateCommandBuffers
  , 'cmdSetViewport
  , 'cmdSetScissor
  , 'cmdUseRenderPass
  , 'cmdDraw
  , 'cmdPushConstants
  ]
