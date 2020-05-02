{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_ray_tracing  ( AabbPositionsKHR
                                                      , AccelerationStructureBuildGeometryInfoKHR
                                                      , AccelerationStructureBuildOffsetInfoKHR
                                                      , AccelerationStructureCreateGeometryTypeInfoKHR
                                                      , AccelerationStructureCreateInfoKHR
                                                      , AccelerationStructureDeviceAddressInfoKHR
                                                      , AccelerationStructureGeometryAabbsDataKHR
                                                      , AccelerationStructureGeometryInstancesDataKHR
                                                      , AccelerationStructureGeometryKHR
                                                      , AccelerationStructureGeometryTrianglesDataKHR
                                                      , AccelerationStructureInstanceKHR
                                                      , AccelerationStructureMemoryRequirementsInfoKHR
                                                      , AccelerationStructureVersionKHR
                                                      , BindAccelerationStructureMemoryInfoKHR
                                                      , CopyAccelerationStructureInfoKHR
                                                      , CopyAccelerationStructureToMemoryInfoKHR
                                                      , CopyMemoryToAccelerationStructureInfoKHR
                                                      , PhysicalDeviceRayTracingFeaturesKHR
                                                      , PhysicalDeviceRayTracingPropertiesKHR
                                                      , RayTracingPipelineCreateInfoKHR
                                                      , RayTracingPipelineInterfaceCreateInfoKHR
                                                      , RayTracingShaderGroupCreateInfoKHR
                                                      , StridedBufferRegionKHR
                                                      , TraceRaysIndirectCommandKHR
                                                      , TransformMatrixKHR
                                                      , WriteDescriptorSetAccelerationStructureKHR
                                                      , CopyAccelerationStructureModeKHR
                                                      , GeometryFlagBitsKHR
                                                      , GeometryFlagsKHR
                                                      , GeometryInstanceFlagBitsKHR
                                                      , GeometryInstanceFlagsKHR
                                                      , BuildAccelerationStructureFlagBitsKHR
                                                      , BuildAccelerationStructureFlagsKHR
                                                      , AccelerationStructureTypeKHR
                                                      , GeometryTypeKHR
                                                      , RayTracingShaderGroupTypeKHR
                                                      , AccelerationStructureMemoryRequirementsTypeKHR
                                                      ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
data AabbPositionsKHR

instance ToCStruct AabbPositionsKHR
instance Show AabbPositionsKHR

instance FromCStruct AabbPositionsKHR


type role AccelerationStructureBuildGeometryInfoKHR nominal
data AccelerationStructureBuildGeometryInfoKHR (es :: [Type])

instance PokeChain es => ToCStruct (AccelerationStructureBuildGeometryInfoKHR es)
instance Show (Chain es) => Show (AccelerationStructureBuildGeometryInfoKHR es)


data AccelerationStructureBuildOffsetInfoKHR

instance ToCStruct AccelerationStructureBuildOffsetInfoKHR
instance Show AccelerationStructureBuildOffsetInfoKHR

instance FromCStruct AccelerationStructureBuildOffsetInfoKHR


data AccelerationStructureCreateGeometryTypeInfoKHR

instance ToCStruct AccelerationStructureCreateGeometryTypeInfoKHR
instance Show AccelerationStructureCreateGeometryTypeInfoKHR

instance FromCStruct AccelerationStructureCreateGeometryTypeInfoKHR


data AccelerationStructureCreateInfoKHR

instance ToCStruct AccelerationStructureCreateInfoKHR
instance Show AccelerationStructureCreateInfoKHR

instance FromCStruct AccelerationStructureCreateInfoKHR


data AccelerationStructureDeviceAddressInfoKHR

instance ToCStruct AccelerationStructureDeviceAddressInfoKHR
instance Show AccelerationStructureDeviceAddressInfoKHR

instance FromCStruct AccelerationStructureDeviceAddressInfoKHR


data AccelerationStructureGeometryAabbsDataKHR

instance ToCStruct AccelerationStructureGeometryAabbsDataKHR
instance Show AccelerationStructureGeometryAabbsDataKHR


data AccelerationStructureGeometryInstancesDataKHR

instance ToCStruct AccelerationStructureGeometryInstancesDataKHR
instance Show AccelerationStructureGeometryInstancesDataKHR


data AccelerationStructureGeometryKHR

instance ToCStruct AccelerationStructureGeometryKHR
instance Show AccelerationStructureGeometryKHR


data AccelerationStructureGeometryTrianglesDataKHR

instance ToCStruct AccelerationStructureGeometryTrianglesDataKHR
instance Show AccelerationStructureGeometryTrianglesDataKHR


data AccelerationStructureInstanceKHR

instance ToCStruct AccelerationStructureInstanceKHR
instance Show AccelerationStructureInstanceKHR

instance FromCStruct AccelerationStructureInstanceKHR


data AccelerationStructureMemoryRequirementsInfoKHR

instance ToCStruct AccelerationStructureMemoryRequirementsInfoKHR
instance Show AccelerationStructureMemoryRequirementsInfoKHR

instance FromCStruct AccelerationStructureMemoryRequirementsInfoKHR


data AccelerationStructureVersionKHR

instance ToCStruct AccelerationStructureVersionKHR
instance Show AccelerationStructureVersionKHR

instance FromCStruct AccelerationStructureVersionKHR


data BindAccelerationStructureMemoryInfoKHR

instance ToCStruct BindAccelerationStructureMemoryInfoKHR
instance Show BindAccelerationStructureMemoryInfoKHR

instance FromCStruct BindAccelerationStructureMemoryInfoKHR


type role CopyAccelerationStructureInfoKHR nominal
data CopyAccelerationStructureInfoKHR (es :: [Type])

instance PokeChain es => ToCStruct (CopyAccelerationStructureInfoKHR es)
instance Show (Chain es) => Show (CopyAccelerationStructureInfoKHR es)

instance PeekChain es => FromCStruct (CopyAccelerationStructureInfoKHR es)


type role CopyAccelerationStructureToMemoryInfoKHR nominal
data CopyAccelerationStructureToMemoryInfoKHR (es :: [Type])

instance PokeChain es => ToCStruct (CopyAccelerationStructureToMemoryInfoKHR es)
instance Show (Chain es) => Show (CopyAccelerationStructureToMemoryInfoKHR es)


type role CopyMemoryToAccelerationStructureInfoKHR nominal
data CopyMemoryToAccelerationStructureInfoKHR (es :: [Type])

instance PokeChain es => ToCStruct (CopyMemoryToAccelerationStructureInfoKHR es)
instance Show (Chain es) => Show (CopyMemoryToAccelerationStructureInfoKHR es)


data PhysicalDeviceRayTracingFeaturesKHR

instance ToCStruct PhysicalDeviceRayTracingFeaturesKHR
instance Show PhysicalDeviceRayTracingFeaturesKHR

instance FromCStruct PhysicalDeviceRayTracingFeaturesKHR


data PhysicalDeviceRayTracingPropertiesKHR

instance ToCStruct PhysicalDeviceRayTracingPropertiesKHR
instance Show PhysicalDeviceRayTracingPropertiesKHR

instance FromCStruct PhysicalDeviceRayTracingPropertiesKHR


type role RayTracingPipelineCreateInfoKHR nominal
data RayTracingPipelineCreateInfoKHR (es :: [Type])

instance PokeChain es => ToCStruct (RayTracingPipelineCreateInfoKHR es)
instance Show (Chain es) => Show (RayTracingPipelineCreateInfoKHR es)

instance PeekChain es => FromCStruct (RayTracingPipelineCreateInfoKHR es)


data RayTracingPipelineInterfaceCreateInfoKHR

instance ToCStruct RayTracingPipelineInterfaceCreateInfoKHR
instance Show RayTracingPipelineInterfaceCreateInfoKHR

instance FromCStruct RayTracingPipelineInterfaceCreateInfoKHR


data RayTracingShaderGroupCreateInfoKHR

instance ToCStruct RayTracingShaderGroupCreateInfoKHR
instance Show RayTracingShaderGroupCreateInfoKHR

instance FromCStruct RayTracingShaderGroupCreateInfoKHR


data StridedBufferRegionKHR

instance ToCStruct StridedBufferRegionKHR
instance Show StridedBufferRegionKHR

instance FromCStruct StridedBufferRegionKHR


data TraceRaysIndirectCommandKHR

instance ToCStruct TraceRaysIndirectCommandKHR
instance Show TraceRaysIndirectCommandKHR

instance FromCStruct TraceRaysIndirectCommandKHR


data TransformMatrixKHR

instance ToCStruct TransformMatrixKHR
instance Show TransformMatrixKHR

instance FromCStruct TransformMatrixKHR


data WriteDescriptorSetAccelerationStructureKHR

instance ToCStruct WriteDescriptorSetAccelerationStructureKHR
instance Show WriteDescriptorSetAccelerationStructureKHR

instance FromCStruct WriteDescriptorSetAccelerationStructureKHR


data CopyAccelerationStructureModeKHR


data GeometryFlagBitsKHR

type GeometryFlagsKHR = GeometryFlagBitsKHR


data GeometryInstanceFlagBitsKHR

type GeometryInstanceFlagsKHR = GeometryInstanceFlagBitsKHR


data BuildAccelerationStructureFlagBitsKHR

type BuildAccelerationStructureFlagsKHR = BuildAccelerationStructureFlagBitsKHR


data AccelerationStructureTypeKHR


data GeometryTypeKHR


data RayTracingShaderGroupTypeKHR


data AccelerationStructureMemoryRequirementsTypeKHR

