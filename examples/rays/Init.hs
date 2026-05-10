{-# LANGUAGE QuasiQuotes #-}
-- GHC can't prove single-element '::&' chain patterns exhaustive (see
-- 'getDeviceRTProps'). Suppressed locally; the type guarantees the shape.
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-incomplete-uni-patterns #-}

module Init
  ( instanceRequirements
  , deviceRequirements
  , RTInfo (..)
  , getDeviceRTProps
  ) where

import Control.Monad.IO.Class
import Data.Word
import Vulkan.CStruct.Extends (pattern (:&), pattern (::&))
import qualified Vulkan.Core10 as Vk
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeatures (..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (PhysicalDeviceAccelerationStructureFeaturesKHR (..))
import Vulkan.Extensions.VK_KHR_get_physical_device_properties2 (getPhysicalDeviceProperties2KHR)
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (PhysicalDeviceRayTracingPipelineFeaturesKHR (..), PhysicalDeviceRayTracingPipelinePropertiesKHR (..))
import Vulkan.Requirement (DeviceRequirement, InstanceRequirement)
import Vulkan.Utils.Frame (frameDeviceRequirements)
import Vulkan.Utils.Requirements.TH (reqs)

{- | Extra instance requirements on top of the boot helper's defaults.
@WindowedBoot.withWindowedVk@ already requires @EXT_DEBUG_UTILS@ and the
KHRONOS validation layer, so this list is currently empty — kept for
symmetry with 'deviceRequirements' and as a convenient hook for future
extensions.
-}
instanceRequirements :: [InstanceRequirement]
instanceRequirements = []

{- | Device requirements: API version, swapchain, Frame's timeline-semaphore
bits, plus the full ray-tracing extension family.
-}
deviceRequirements :: [DeviceRequirement]
deviceRequirements =
  [reqs|
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

getDeviceRTProps :: (MonadIO m) => Vk.PhysicalDevice -> m RTInfo
getDeviceRTProps phys = do
  props <- getPhysicalDeviceProperties2KHR phys
  let _ ::& PhysicalDeviceRayTracingPipelinePropertiesKHR{..} :& () = props
  pure
    RTInfo
      { rtiShaderGroupHandleSize = shaderGroupHandleSize
      , rtiShaderGroupBaseAlignment = shaderGroupBaseAlignment
      }
