{-# LANGUAGE QuasiQuotes #-}

module Init
  ( myApiVersion
  , instanceRequirements
  , deviceRequirements
  , RTInfo (..)
  , getDeviceRTProps
  , createVMA
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Word

import Frame
  ( frameDeviceRequirements
  , frameInstanceRequirements
  )
import qualified Vma
import Vulkan.CStruct.Extends
  ( pattern (:&)
  , pattern (::&)
  )
import Vulkan.Core10
import Vulkan.Core11 (pattern API_VERSION_1_1)
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address
  ( PhysicalDeviceBufferDeviceAddressFeatures (..)
  )
import Vulkan.Extensions.VK_EXT_debug_utils
  ( pattern EXT_DEBUG_UTILS_EXTENSION_NAME
  )
import Vulkan.Extensions.VK_KHR_acceleration_structure
  ( PhysicalDeviceAccelerationStructureFeaturesKHR (..)
  )
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2
  ( getPhysicalDeviceProperties2KHR
  )
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline
  ( PhysicalDeviceRayTracingPipelineFeaturesKHR (..)
  , PhysicalDeviceRayTracingPipelinePropertiesKHR (..)
  )
import Vulkan.Requirement
  ( DeviceRequirement
  , InstanceRequirement (..)
  )
import qualified Vulkan.Utils.Requirements.TH as U
import VulkanMemoryAllocator
  ( Allocator
  , AllocatorCreateFlagBits (..)
  )

myApiVersion :: Word32
myApiVersion = API_VERSION_1_1

{- | Instance requirements: Frame's bits plus debug-utils so the @nameObject@
calls scattered through the example can load their function pointer (we
don't enable the messenger though).
-}
instanceRequirements :: [InstanceRequirement]
instanceRequirements =
  frameInstanceRequirements
    ++ [RequireInstanceExtension Nothing EXT_DEBUG_UTILS_EXTENSION_NAME minBound]

{- | Device requirements: API version, swapchain, Frame's timeline-semaphore
bits, plus the full ray-tracing extension family.
-}
deviceRequirements :: [DeviceRequirement]
deviceRequirements =
  [U.reqs|
      1.0
      VK_KHR_swapchain

      -- Ray tracing
      1.2.162
      PhysicalDeviceRayTracingPipelineFeaturesKHR.rayTracingPipeline
      PhysicalDeviceAccelerationStructureFeaturesKHR.accelerationStructure
      PhysicalDeviceBufferDeviceAddressFeatures.bufferDeviceAddress
      VK_KHR_ray_tracing_pipeline
      VK_KHR_acceleration_structure
      VK_EXT_descriptor_indexing
      VK_KHR_buffer_device_address
      VK_KHR_deferred_host_operations
      VK_KHR_get_memory_requirements2
      VK_KHR_maintenance3
      VK_KHR_pipeline_library
  |]
    ++ frameDeviceRequirements

-- | Information for ray tracing (queried from device properties).
data RTInfo = RTInfo
  { rtiShaderGroupHandleSize :: Word32
  , rtiShaderGroupBaseAlignment :: Word32
  }

getDeviceRTProps :: (MonadIO m) => PhysicalDevice -> m RTInfo
getDeviceRTProps phys = do
  props <- getPhysicalDeviceProperties2KHR phys
  let _ ::& PhysicalDeviceRayTracingPipelinePropertiesKHR{..} :& () = props
  pure
    RTInfo
      { rtiShaderGroupHandleSize = shaderGroupHandleSize
      , rtiShaderGroupBaseAlignment = shaderGroupBaseAlignment
      }

createVMA
  :: (MonadResource m) => Instance -> PhysicalDevice -> Device -> m Allocator
createVMA = Vma.createVMA ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT myApiVersion
