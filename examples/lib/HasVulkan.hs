module HasVulkan
  ( HasVulkan(..)
  , noAllocationCallbacks
  , noPipelineCache
  ) where

import           Control.Monad.Trans.Class      ( MonadTrans(lift) )
import           Control.Monad.Trans.Reader     ( ReaderT )
import           Vulkan.Core10
import           VulkanMemoryAllocator

-- | A class for Monads which can provide some Vulkan handles
class HasVulkan m where
  getInstance :: m Instance
  getGraphicsQueue :: m Queue
  getPhysicalDevice :: m PhysicalDevice
  getDevice :: m Device
  getAllocator :: m Allocator

instance (Monad m, HasVulkan m) => HasVulkan (ReaderT r m) where
  getInstance       = lift getInstance
  getGraphicsQueue  = lift getGraphicsQueue
  getPhysicalDevice = lift getPhysicalDevice
  getDevice         = lift getDevice
  getAllocator      = lift getAllocator

noAllocationCallbacks :: Maybe AllocationCallbacks
noAllocationCallbacks = Nothing

noPipelineCache :: PipelineCache
noPipelineCache = NULL_HANDLE
