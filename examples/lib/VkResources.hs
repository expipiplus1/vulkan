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
                                                , Queue
                                                , Semaphore
                                                )
import           Vulkan.Utils.QueueAssignment   ( QueueFamilyIndex )
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

-- | The full G/C/T queue kit each windowed example gets. Fields are filled
-- from 'InitDevice.withDevice' with priorities 1.0/0.5/0.2; on hardware that
-- exposes dedicated families they target async-compute and DMA-only families,
-- otherwise they alias the graphics+present family (with distinct 'Queue'
-- handles allocated within that shared family).
--
-- The same shape is used internally by 'InitDevice' to feed
-- 'Vulkan.Utils.QueueAssignment.assignQueues' (as @Queues (QueueSpec m)@).
data Queues a = Queues
  { qGraphics :: a   -- ^ graphics + present, priority 1.0
  , qCompute  :: a   -- ^ compute (prefers compute-only family), priority 0.5
  , qTransfer :: a   -- ^ transfer (prefers transfer-only family), priority 0.2
  }
  deriving (Functor, Foldable, Traversable)

-- | Elementwise zip — handy for combining priorities with predicates when
-- building a @Queues (QueueSpec m)@.
instance Applicative Queues where
  pure x = Queues x x x
  Queues f g h <*> Queues x y z = Queues (f x) (g y) (h z)

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
