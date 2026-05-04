{-# LANGUAGE QuasiQuotes #-}

module Init
  ( myApiVersion
  , deviceRequirements
  , createVMA
  ) where

import           Control.Monad.Trans.Resource
import           Data.Word

import           Frame                          ( frameDeviceRequirements )
import qualified Vma
import           Vulkan.Core10
import           Vulkan.Requirement             ( DeviceRequirement )
import qualified Vulkan.Utils.Requirements.TH  as U
import           Vulkan.Zero
import           VulkanMemoryAllocator          ( Allocator )

myApiVersion :: Word32
myApiVersion = API_VERSION_1_0

-- | Device requirements: API version, swapchain, and the timeline-semaphore
-- bits the recycling 'Frame' machinery needs.
deviceRequirements :: [DeviceRequirement]
deviceRequirements = [U.reqs|
    1.0
    VK_KHR_swapchain
  |] ++ frameDeviceRequirements

createVMA
  :: MonadResource m => Instance -> PhysicalDevice -> Device -> m Allocator
createVMA = Vma.createVMA zero myApiVersion
