{-| Example-side bundle: the original 'VkResources' shape, now constructed
from an upstream 'Vulkan.Utils.VulkanContext.VulkanContext' plus the VMA
allocator the examples use. 'vrContext' projects back out to a
'VulkanContext' for the upstream Frame/WindowLoop APIs.
-}
module VkResources
  ( VkResources (..)
  , vrContext
  , mkVkResources
  , Queues (..)
  , RecycledResources (..)
  ) where

import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.QueueAssignment (QueueFamilyIndex)
import Vulkan.Utils.Queues (Queues (..))
import Vulkan.Utils.VulkanContext (RecycledResources (..), VulkanContext (..), mkVulkanContext)
import qualified VulkanMemoryAllocator as VMA

{- | A bunch of long-lived handles that the application carries around.
Constructed once, never modified.
-}
data VkResources = VkResources
  { vrInstance :: Vk.Instance
  , vrPhysicalDevice :: Vk.PhysicalDevice
  , vrDevice :: Vk.Device
  , vrAllocator :: VMA.Allocator
  , vrQueues :: Queues (QueueFamilyIndex, Vk.Queue)
  , vrRecycleBin :: RecycledResources -> IO ()
  , vrRecycleNib :: IO (Either (IO RecycledResources) RecycledResources)
  }

-- | Project out the upstream 'VulkanContext' for use with Frame/WindowLoop.
vrContext :: VkResources -> VulkanContext
vrContext VkResources{..} =
  VulkanContext
    { vcInstance = vrInstance
    , vcPhysicalDevice = vrPhysicalDevice
    , vcDevice = vrDevice
    , vcQueues = vrQueues
    , vcRecycleBin = vrRecycleBin
    , vcRecycleNib = vrRecycleNib
    }

{- | Assemble a 'VkResources' from already-constructed handles. Builds the
recycle channel internally via the upstream 'mkVulkanContext'.
-}
mkVkResources
  :: Vk.Instance
  -> Vk.PhysicalDevice
  -> Vk.Device
  -> VMA.Allocator
  -> Queues (QueueFamilyIndex, Vk.Queue)
  -> IO VkResources
mkVkResources inst phys dev vrAllocator queues = do
  vc <- mkVulkanContext inst phys dev queues
  pure
    VkResources
      { vrInstance = vcInstance vc
      , vrPhysicalDevice = vcPhysicalDevice vc
      , vrDevice = vcDevice vc
      , vrAllocator
      , vrQueues = vcQueues vc
      , vrRecycleBin = vcRecycleBin vc
      , vrRecycleNib = vcRecycleNib vc
      }
