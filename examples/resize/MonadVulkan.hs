{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module MonadVulkan where

import           AutoApply
import           Control.Exception.Safe
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource

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
                   , MonadThrow
                   , MonadCatch
                   , MonadMask
                   , MonadIO
                   , MonadResource
                   )

newtype Cmd a = Cmd { _unCmd :: ReaderT CommandBuffer V a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadFail
                   , MonadThrow
                   , MonadCatch
                   , MonadMask
                   , MonadIO
                   , MonadResource
                   )

class HasVulkan m where
  getInstance :: m Instance
  getGraphicsQueue :: m Queue
  getPhysicalDevice :: m PhysicalDevice
  getDevice :: m Device
  getAllocator :: m Allocator
  getCommandPool :: m CommandPool

instance HasVulkan V where
  getInstance       = V (asks ghInstance)
  getGraphicsQueue  = V (asks ghGraphicsQueue)
  getPhysicalDevice = V (asks ghPhysicalDevice)
  getDevice         = V (asks ghDevice)
  getAllocator      = V (asks ghAllocator)
  getCommandPool    = V (asks ghCommandPool)

instance HasVulkan Cmd where
  getInstance       = Cmd $ lift getInstance
  getGraphicsQueue  = Cmd $ lift getGraphicsQueue
  getPhysicalDevice = Cmd $ lift getPhysicalDevice
  getDevice         = Cmd $ lift getDevice
  getAllocator      = Cmd $ lift getAllocator
  getCommandPool    = Cmd $ lift getCommandPool

noAllocationCallbacks :: Maybe AllocationCallbacks
noAllocationCallbacks = Nothing

getCommandBuffer :: Cmd CommandBuffer
getCommandBuffer = Cmd ask

useCommandBuffer'
  :: forall a r
   . (Extendss CommandBufferBeginInfo a, PokeChain a)
  => CommandBuffer
  -> CommandBufferBeginInfo a
  -> Cmd r
  -> V r
useCommandBuffer' commandBuffer beginInfo (Cmd a) =
  useCommandBuffer commandBuffer beginInfo (runReaderT a commandBuffer)

runV
  :: Instance
  -> PhysicalDevice
  -> Device
  -> Queue
  -> CommandPool
  -> Allocator
  -> V a
  -> ResourceT IO a
runV ghInstance ghPhysicalDevice ghDevice ghGraphicsQueue ghCommandPool ghAllocator
  = flip runReaderT GlobalHandles { .. } . unV

data GlobalHandles = GlobalHandles
  { ghInstance       :: Instance
  , ghPhysicalDevice :: PhysicalDevice
  , ghDevice         :: Device
  , ghAllocator      :: Allocator
  , ghGraphicsQueue  :: Queue
  , ghCommandPool    :: CommandPool
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
  ]
