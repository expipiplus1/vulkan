{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_ray_tracing"
module Vulkan.Extensions.VK_NV_ray_tracing  ( AccelerationStructureCreateInfoNV
                                            , AccelerationStructureInfoNV
                                            , AccelerationStructureMemoryRequirementsInfoNV
                                            , BindAccelerationStructureMemoryInfoNV
                                            , GeometryAABBNV
                                            , GeometryDataNV
                                            , GeometryNV
                                            , GeometryTrianglesNV
                                            , PhysicalDeviceRayTracingPropertiesNV
                                            , RayTracingPipelineCreateInfoNV
                                            , RayTracingShaderGroupCreateInfoNV
                                            , WriteDescriptorSetAccelerationStructureNV
                                            ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
data AccelerationStructureCreateInfoNV

instance ToCStruct AccelerationStructureCreateInfoNV
instance Show AccelerationStructureCreateInfoNV

instance FromCStruct AccelerationStructureCreateInfoNV


data AccelerationStructureInfoNV

instance ToCStruct AccelerationStructureInfoNV
instance Show AccelerationStructureInfoNV

instance FromCStruct AccelerationStructureInfoNV


data AccelerationStructureMemoryRequirementsInfoNV

instance ToCStruct AccelerationStructureMemoryRequirementsInfoNV
instance Show AccelerationStructureMemoryRequirementsInfoNV

instance FromCStruct AccelerationStructureMemoryRequirementsInfoNV


data BindAccelerationStructureMemoryInfoNV

instance ToCStruct BindAccelerationStructureMemoryInfoNV
instance Show BindAccelerationStructureMemoryInfoNV

instance FromCStruct BindAccelerationStructureMemoryInfoNV


data GeometryAABBNV

instance ToCStruct GeometryAABBNV
instance Show GeometryAABBNV

instance FromCStruct GeometryAABBNV


data GeometryDataNV

instance ToCStruct GeometryDataNV
instance Show GeometryDataNV

instance FromCStruct GeometryDataNV


data GeometryNV

instance ToCStruct GeometryNV
instance Show GeometryNV

instance FromCStruct GeometryNV


data GeometryTrianglesNV

instance ToCStruct GeometryTrianglesNV
instance Show GeometryTrianglesNV

instance FromCStruct GeometryTrianglesNV


data PhysicalDeviceRayTracingPropertiesNV

instance ToCStruct PhysicalDeviceRayTracingPropertiesNV
instance Show PhysicalDeviceRayTracingPropertiesNV

instance FromCStruct PhysicalDeviceRayTracingPropertiesNV


type role RayTracingPipelineCreateInfoNV nominal
data RayTracingPipelineCreateInfoNV (es :: [Type])

instance (Extendss RayTracingPipelineCreateInfoNV es, PokeChain es) => ToCStruct (RayTracingPipelineCreateInfoNV es)
instance Show (Chain es) => Show (RayTracingPipelineCreateInfoNV es)

instance (Extendss RayTracingPipelineCreateInfoNV es, PeekChain es) => FromCStruct (RayTracingPipelineCreateInfoNV es)


data RayTracingShaderGroupCreateInfoNV

instance ToCStruct RayTracingShaderGroupCreateInfoNV
instance Show RayTracingShaderGroupCreateInfoNV

instance FromCStruct RayTracingShaderGroupCreateInfoNV


data WriteDescriptorSetAccelerationStructureNV

instance ToCStruct WriteDescriptorSetAccelerationStructureNV
instance Show WriteDescriptorSetAccelerationStructureNV

instance FromCStruct WriteDescriptorSetAccelerationStructureNV

