{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_ray_tracing_pipeline  ( PhysicalDeviceRayTracingPipelineFeaturesKHR
                                                      , PhysicalDeviceRayTracingPipelinePropertiesKHR
                                                      , RayTracingPipelineCreateInfoKHR
                                                      , RayTracingPipelineInterfaceCreateInfoKHR
                                                      , RayTracingShaderGroupCreateInfoKHR
                                                      , StridedDeviceAddressRegionKHR
                                                      , TraceRaysIndirectCommandKHR
                                                      , ShaderGroupShaderKHR
                                                      , RayTracingShaderGroupTypeKHR
                                                      ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceRayTracingPipelineFeaturesKHR

instance ToCStruct PhysicalDeviceRayTracingPipelineFeaturesKHR
instance Show PhysicalDeviceRayTracingPipelineFeaturesKHR

instance FromCStruct PhysicalDeviceRayTracingPipelineFeaturesKHR


data PhysicalDeviceRayTracingPipelinePropertiesKHR

instance ToCStruct PhysicalDeviceRayTracingPipelinePropertiesKHR
instance Show PhysicalDeviceRayTracingPipelinePropertiesKHR

instance FromCStruct PhysicalDeviceRayTracingPipelinePropertiesKHR


type role RayTracingPipelineCreateInfoKHR nominal
data RayTracingPipelineCreateInfoKHR (es :: [Type])

instance (Extendss RayTracingPipelineCreateInfoKHR es, PokeChain es) => ToCStruct (RayTracingPipelineCreateInfoKHR es)
instance Show (Chain es) => Show (RayTracingPipelineCreateInfoKHR es)

instance (Extendss RayTracingPipelineCreateInfoKHR es, PeekChain es) => FromCStruct (RayTracingPipelineCreateInfoKHR es)


data RayTracingPipelineInterfaceCreateInfoKHR

instance ToCStruct RayTracingPipelineInterfaceCreateInfoKHR
instance Show RayTracingPipelineInterfaceCreateInfoKHR

instance FromCStruct RayTracingPipelineInterfaceCreateInfoKHR


data RayTracingShaderGroupCreateInfoKHR

instance ToCStruct RayTracingShaderGroupCreateInfoKHR
instance Show RayTracingShaderGroupCreateInfoKHR

instance FromCStruct RayTracingShaderGroupCreateInfoKHR


data StridedDeviceAddressRegionKHR

instance ToCStruct StridedDeviceAddressRegionKHR
instance Show StridedDeviceAddressRegionKHR

instance FromCStruct StridedDeviceAddressRegionKHR


data TraceRaysIndirectCommandKHR

instance ToCStruct TraceRaysIndirectCommandKHR
instance Show TraceRaysIndirectCommandKHR

instance FromCStruct TraceRaysIndirectCommandKHR


data ShaderGroupShaderKHR


data RayTracingShaderGroupTypeKHR

