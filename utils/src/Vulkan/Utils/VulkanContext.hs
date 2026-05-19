{-| Application-static Vulkan handles plus the recycle channel ends used by
the recycling 'Vulkan.Utils.Frame.Frame' machinery.

Constructed once at boot, never modified. Sub-systems (swapchain, frame loop,
window loop) accept a 'VulkanContext' so they don't need their own copies of
device/queues plumbing.
-}
module Vulkan.Utils.VulkanContext
  ( VulkanContext (..)
  , RecycledResources (..)
  , mkVulkanContext
  ) where

import Control.Concurrent.Chan.Unagi
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex)
import Vulkan.Utils.Queues (Queues (..))

{- | A bunch of long-lived handles that the application carries around. The
recycle channel ends carry per-frame 'RecycledResources' between the frame
loop and the wait-and-recycle thread.
-}
data VulkanContext = VulkanContext
  { vcInstance :: Vk.Instance
  , vcPhysicalDevice :: Vk.PhysicalDevice
  , vcDevice :: Vk.Device
  , vcQueues :: Queues (QueueFamilyIndex, Vk.Queue)
  , vcRecycleBin :: RecycledResources -> IO ()
  {- ^ Drop a frame's reusable bits back into the pool. Called from the
  per-frame wait thread once the GPU is done with the frame.
  -}
  , vcRecycleNib :: IO (Either (IO RecycledResources) RecycledResources)
  {- ^ Pull a frame's reusable bits out. 'Right' if available immediately;
  'Left' is a blocking read.
  -}
  }

{- | The bits of state recycled between frames: two binary semaphores used
for image-acquire / render-done synchronisation, and the command pool the
frame's commands are recorded into.
-}
data RecycledResources = RecycledResources
  { rrImageAvailable :: Vk.Semaphore
  , rrRenderFinished :: Vk.Semaphore
  , rrCommandPool :: Vk.CommandPool
  }

{- | Assemble a 'VulkanContext' from already-constructed handles. Builds the
recycle channel internally; the channel starts empty and is populated by
'Vulkan.Utils.Frame.initialFrame'.
-}
mkVulkanContext
  :: Vk.Instance
  -> Vk.PhysicalDevice
  -> Vk.Device
  -> Queues (QueueFamilyIndex, Vk.Queue)
  -> IO VulkanContext
mkVulkanContext vcInstance vcPhysicalDevice vcDevice vcQueues = do
  (binW, binR) <- newChan
  let
    vcRecycleBin = writeChan binW
    vcRecycleNib = do
      (try, block) <- tryReadChan binR
      maybe (Left block) Right <$> tryRead try
  pure VulkanContext{..}
