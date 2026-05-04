-- | Application-static Vulkan handles plus the recycle channel ends used by
-- the recycling 'Frame' machinery in "Frame".
module VkResources
  ( VkResources(..)
  , Queues(..)
  , RecycledResources(..)
  , mkVkResources
  ) where

import           Control.Concurrent.Chan.Unagi
import           Vulkan.Core10                  ( CommandPool
                                                , Device
                                                , Instance
                                                , PhysicalDevice
                                                , Semaphore
                                                )
import           Vulkan.Utils.QueueAssignment   ( QueueFamilyIndex )
import           Vulkan.Core10                  ( Queue )
import           VulkanMemoryAllocator          ( Allocator )

-- | A bunch of long-lived handles that the application carries around.
-- Constructed once, never modified.
data VkResources = VkResources
  { vrInstance       :: Instance
  , vrPhysicalDevice :: PhysicalDevice
  , vrDevice         :: Device
  , vrAllocator      :: Allocator
  , vrQueues         :: Queues (QueueFamilyIndex, Queue)
  , vrRecycleBin     :: RecycledResources -> IO ()
    -- ^ Drop a frame's reusable bits back into the pool. Called from the
    -- per-frame wait thread once the GPU is done with the frame.
  , vrRecycleNib     :: IO (Either (IO RecycledResources) RecycledResources)
    -- ^ Pull a frame's reusable bits out. 'Right' if available immediately;
    -- 'Left' is a blocking read.
  }

-- | The shape of the queues each example needs. Single graphics queue covers
-- every windowed example here; parameterised over the queue type so the same
-- shape works with 'Vulkan.Utils.QueueAssignment.assignQueues'.
newtype Queues q = Queues { graphicsQueue :: q }
  deriving (Functor, Foldable, Traversable)

-- | The bits of state recycled between frames: two binary semaphores used
-- for image-acquire / render-done synchronisation, and the command pool the
-- frame's commands are recorded into.
data RecycledResources = RecycledResources
  { rrImageAvailable :: Semaphore
  , rrRenderFinished :: Semaphore
  , rrCommandPool    :: CommandPool
  }

-- | Assemble a 'VkResources' from already-constructed handles. Builds the
-- recycle channel internally.
mkVkResources
  :: Instance
  -> PhysicalDevice
  -> Device
  -> Allocator
  -> Queues (QueueFamilyIndex, Queue)
  -> IO VkResources
mkVkResources vrInstance vrPhysicalDevice vrDevice vrAllocator vrQueues = do
  (binW, binR) <- newChan
  let vrRecycleBin = writeChan binW
      vrRecycleNib = do
        (try, block) <- tryReadChan binR
        maybe (Left block) Right <$> tryRead try
  pure VkResources { .. }
