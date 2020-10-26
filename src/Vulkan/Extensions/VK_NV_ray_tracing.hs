{-# language CPP #-}
module Vulkan.Extensions.VK_NV_ray_tracing  ( compileDeferredNV
                                            , createAccelerationStructureNV
                                            , getAccelerationStructureMemoryRequirementsNV
                                            , cmdCopyAccelerationStructureNV
                                            , cmdBuildAccelerationStructureNV
                                            , cmdTraceRaysNV
                                            , getAccelerationStructureHandleNV
                                            , createRayTracingPipelinesNV
                                            , pattern STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV
                                            , pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV
                                            , pattern SHADER_STAGE_RAYGEN_BIT_NV
                                            , pattern SHADER_STAGE_ANY_HIT_BIT_NV
                                            , pattern SHADER_STAGE_CLOSEST_HIT_BIT_NV
                                            , pattern SHADER_STAGE_MISS_BIT_NV
                                            , pattern SHADER_STAGE_INTERSECTION_BIT_NV
                                            , pattern SHADER_STAGE_CALLABLE_BIT_NV
                                            , pattern PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV
                                            , pattern PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV
                                            , pattern BUFFER_USAGE_RAY_TRACING_BIT_NV
                                            , pattern PIPELINE_BIND_POINT_RAY_TRACING_NV
                                            , pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
                                            , pattern ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV
                                            , pattern ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV
                                            , pattern QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV
                                            , pattern OBJECT_TYPE_ACCELERATION_STRUCTURE_NV
                                            , pattern DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT
                                            , pattern INDEX_TYPE_NONE_NV
                                            , pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV
                                            , pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV
                                            , pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV
                                            , pattern GEOMETRY_TYPE_TRIANGLES_NV
                                            , pattern GEOMETRY_TYPE_AABBS_NV
                                            , pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV
                                            , pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV
                                            , pattern GEOMETRY_OPAQUE_BIT_NV
                                            , pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV
                                            , pattern GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV
                                            , pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV
                                            , pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV
                                            , pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV
                                            , pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV
                                            , pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV
                                            , pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV
                                            , pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV
                                            , pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV
                                            , pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV
                                            , pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV
                                            , pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV
                                            , pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV
                                            , pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV
                                            , destroyAccelerationStructureNV
                                            , bindAccelerationStructureMemoryNV
                                            , cmdWriteAccelerationStructuresPropertiesNV
                                            , getRayTracingShaderGroupHandlesNV
                                            , pattern SHADER_UNUSED_NV
                                            , RayTracingShaderGroupCreateInfoNV(..)
                                            , RayTracingPipelineCreateInfoNV(..)
                                            , GeometryTrianglesNV(..)
                                            , GeometryAABBNV(..)
                                            , GeometryDataNV(..)
                                            , GeometryNV(..)
                                            , AccelerationStructureInfoNV(..)
                                            , AccelerationStructureCreateInfoNV(..)
                                            , AccelerationStructureMemoryRequirementsInfoNV(..)
                                            , PhysicalDeviceRayTracingPropertiesNV(..)
                                            , GeometryFlagsNV
                                            , GeometryInstanceFlagsNV
                                            , BuildAccelerationStructureFlagsNV
                                            , AccelerationStructureNV
                                            , GeometryFlagBitsNV
                                            , GeometryInstanceFlagBitsNV
                                            , BuildAccelerationStructureFlagBitsNV
                                            , CopyAccelerationStructureModeNV
                                            , AccelerationStructureTypeNV
                                            , GeometryTypeNV
                                            , RayTracingShaderGroupTypeNV
                                            , AccelerationStructureMemoryRequirementsTypeNV
                                            , BindAccelerationStructureMemoryInfoNV
                                            , WriteDescriptorSetAccelerationStructureNV
                                            , AabbPositionsNV
                                            , TransformMatrixNV
                                            , AccelerationStructureInstanceNV
                                            , NV_RAY_TRACING_SPEC_VERSION
                                            , pattern NV_RAY_TRACING_SPEC_VERSION
                                            , NV_RAY_TRACING_EXTENSION_NAME
                                            , pattern NV_RAY_TRACING_EXTENSION_NAME
                                            , AccelerationStructureKHR(..)
                                            , BindAccelerationStructureMemoryInfoKHR(..)
                                            , WriteDescriptorSetAccelerationStructureKHR(..)
                                            , AabbPositionsKHR(..)
                                            , TransformMatrixKHR(..)
                                            , AccelerationStructureInstanceKHR(..)
                                            , destroyAccelerationStructureKHR
                                            , bindAccelerationStructureMemoryKHR
                                            , cmdWriteAccelerationStructuresPropertiesKHR
                                            , getRayTracingShaderGroupHandlesKHR
                                            , DebugReportObjectTypeEXT(..)
                                            , GeometryInstanceFlagBitsKHR(..)
                                            , GeometryInstanceFlagsKHR
                                            , GeometryFlagBitsKHR(..)
                                            , GeometryFlagsKHR
                                            , BuildAccelerationStructureFlagBitsKHR(..)
                                            , BuildAccelerationStructureFlagsKHR
                                            , CopyAccelerationStructureModeKHR(..)
                                            , AccelerationStructureTypeKHR(..)
                                            , GeometryTypeKHR(..)
                                            , AccelerationStructureMemoryRequirementsTypeKHR(..)
                                            , RayTracingShaderGroupTypeKHR(..)
                                            , MemoryRequirements2KHR
                                            , SHADER_UNUSED_KHR
                                            , pattern SHADER_UNUSED_KHR
                                            ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Extensions.VK_KHR_ray_tracing (bindAccelerationStructureMemoryKHR)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Extensions.VK_KHR_ray_tracing (cmdWriteAccelerationStructuresPropertiesKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (destroyAccelerationStructureKHR)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Extensions.VK_KHR_ray_tracing (getRayTracingShaderGroupHandlesKHR)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.VK_KHR_ray_tracing (AabbPositionsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureInstanceKHR)
import Vulkan.Extensions.Handles (AccelerationStructureKHR)
import Vulkan.Extensions.Handles (AccelerationStructureKHR(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureMemoryRequirementsTypeKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureTypeKHR)
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Extensions.VK_KHR_ray_tracing (BindAccelerationStructureMemoryInfoKHR)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagsKHR)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.VK_KHR_ray_tracing (CopyAccelerationStructureModeKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (CopyAccelerationStructureModeKHR(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBuildAccelerationStructureNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyAccelerationStructureNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdTraceRaysNV))
import Vulkan.Dynamic (DeviceCmds(pVkCompileDeferredNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateAccelerationStructureNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateRayTracingPipelinesNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureHandleNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureMemoryRequirementsNV))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryInstanceFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryTypeKHR)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Extensions.VK_KHR_get_memory_requirements2 (MemoryRequirements2KHR)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Handles (PipelineCache)
import Vulkan.Core10.Handles (PipelineCache(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfoEXT)
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Pipeline (PipelineShaderStageCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (RayTracingShaderGroupTypeKHR)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (TransformMatrixKHR)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (WriteDescriptorSetAccelerationStructureKHR)
import Vulkan.Zero (Zero(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureMemoryRequirementsTypeKHR(ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureMemoryRequirementsTypeKHR(ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureMemoryRequirementsTypeKHR(ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureTypeKHR(ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureTypeKHR(ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR))
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlagBits(BUFFER_USAGE_RAY_TRACING_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (CopyAccelerationStructureModeKHR(COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (CopyAccelerationStructureModeKHR(COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT))
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryInstanceFlagBitsKHR(GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryInstanceFlagBitsKHR(GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryInstanceFlagBitsKHR(GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryInstanceFlagBitsKHR(GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryFlagBitsKHR(GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryFlagBitsKHR(GEOMETRY_OPAQUE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryTypeKHR(GEOMETRY_TYPE_AABBS_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryTypeKHR(GEOMETRY_TYPE_TRIANGLES_KHR))
import Vulkan.Core10.Enums.IndexType (IndexType(INDEX_TYPE_NONE_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(PIPELINE_BIND_POINT_RAY_TRACING_KHR))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR))
import Vulkan.Core10.Enums.QueryType (QueryType(QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (RayTracingShaderGroupTypeKHR(RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (RayTracingShaderGroupTypeKHR(RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing (RayTracingShaderGroupTypeKHR(RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(SHADER_STAGE_ANY_HIT_BIT_KHR))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(SHADER_STAGE_CALLABLE_BIT_KHR))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(SHADER_STAGE_CLOSEST_HIT_BIT_KHR))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(SHADER_STAGE_INTERSECTION_BIT_KHR))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(SHADER_STAGE_MISS_BIT_KHR))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(SHADER_STAGE_RAYGEN_BIT_KHR))
import Vulkan.Core10.APIConstants (pattern SHADER_UNUSED_KHR)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GEOMETRY_AABB_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GEOMETRY_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_ray_tracing (bindAccelerationStructureMemoryKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (cmdWriteAccelerationStructuresPropertiesKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (destroyAccelerationStructureKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (getRayTracingShaderGroupHandlesKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (AabbPositionsKHR(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureInstanceKHR(..))
import Vulkan.Extensions.Handles (AccelerationStructureKHR(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureMemoryRequirementsTypeKHR(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureTypeKHR(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (BindAccelerationStructureMemoryInfoKHR(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (CopyAccelerationStructureModeKHR(..))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryInstanceFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (GeometryTypeKHR(..))
import Vulkan.Extensions.VK_KHR_get_memory_requirements2 (MemoryRequirements2KHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (RayTracingShaderGroupTypeKHR(..))
import Vulkan.Core10.APIConstants (SHADER_UNUSED_KHR)
import Vulkan.Extensions.VK_KHR_ray_tracing (TransformMatrixKHR(..))
import Vulkan.Extensions.VK_KHR_ray_tracing (WriteDescriptorSetAccelerationStructureKHR(..))
import Vulkan.Core10.APIConstants (pattern SHADER_UNUSED_KHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCompileDeferredNV
  :: FunPtr (Ptr Device_T -> Pipeline -> Word32 -> IO Result) -> Ptr Device_T -> Pipeline -> Word32 -> IO Result

-- | vkCompileDeferredNV - Deferred compilation of shaders
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline'
compileDeferredNV :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the logical device containing the ray tracing pipeline.
                     --
                     -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                     Device
                  -> -- | @pipeline@ is the ray tracing pipeline object containing the shaders.
                     --
                     -- @pipeline@ /must/ have been created with
                     -- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEFER_COMPILE_BIT_NV'
                     --
                     -- @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
                     --
                     -- @pipeline@ /must/ have been created, allocated, or retrieved from
                     -- @device@
                     Pipeline
                  -> -- | @shader@ is the index of the shader to compile.
                     --
                     -- @shader@ /must/ not have been called as a deferred compile before
                     ("shader" ::: Word32)
                  -> io ()
compileDeferredNV device pipeline shader = liftIO $ do
  let vkCompileDeferredNVPtr = pVkCompileDeferredNV (deviceCmds (device :: Device))
  unless (vkCompileDeferredNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCompileDeferredNV is null" Nothing Nothing
  let vkCompileDeferredNV' = mkVkCompileDeferredNV vkCompileDeferredNVPtr
  r <- vkCompileDeferredNV' (deviceHandle (device)) (pipeline) (shader)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateAccelerationStructureNV
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureCreateInfoNV -> Ptr AllocationCallbacks -> Ptr AccelerationStructureNV -> IO Result) -> Ptr Device_T -> Ptr AccelerationStructureCreateInfoNV -> Ptr AllocationCallbacks -> Ptr AccelerationStructureNV -> IO Result

-- | vkCreateAccelerationStructureNV - Create a new acceleration structure
-- object
--
-- = Description
--
-- Similar to other objects in Vulkan, the acceleration structure creation
-- merely creates an object with a specific “shape” as specified by the
-- information in 'AccelerationStructureInfoNV' and @compactedSize@ in
-- @pCreateInfo@. Populating the data in the object after allocating and
-- binding memory is done with 'cmdBuildAccelerationStructureNV' and
-- 'cmdCopyAccelerationStructureNV'.
--
-- Acceleration structure creation uses the count and type information from
-- the geometries, but does not use the data references in the structures.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureCreateInfoNV' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pAccelerationStructure@ /must/ be a valid pointer to a
--     'AccelerationStructureNV' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'AccelerationStructureCreateInfoNV', 'AccelerationStructureNV',
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device'
createAccelerationStructureNV :: forall io
                               . (MonadIO io)
                              => -- | @device@ is the logical device that creates the buffer object.
                                 Device
                              -> -- | @pCreateInfo@ is a pointer to a 'AccelerationStructureCreateInfoNV'
                                 -- structure containing parameters affecting creation of the acceleration
                                 -- structure.
                                 AccelerationStructureCreateInfoNV
                              -> -- | @pAllocator@ controls host memory allocation as described in the
                                 -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                 -- chapter.
                                 ("allocator" ::: Maybe AllocationCallbacks)
                              -> io (AccelerationStructureNV)
createAccelerationStructureNV device createInfo allocator = liftIO . evalContT $ do
  let vkCreateAccelerationStructureNVPtr = pVkCreateAccelerationStructureNV (deviceCmds (device :: Device))
  lift $ unless (vkCreateAccelerationStructureNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateAccelerationStructureNV is null" Nothing Nothing
  let vkCreateAccelerationStructureNV' = mkVkCreateAccelerationStructureNV vkCreateAccelerationStructureNVPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPAccelerationStructure <- ContT $ bracket (callocBytes @AccelerationStructureNV 8) free
  r <- lift $ vkCreateAccelerationStructureNV' (deviceHandle (device)) pCreateInfo pAllocator (pPAccelerationStructure)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAccelerationStructure <- lift $ peek @AccelerationStructureNV pPAccelerationStructure
  pure $ (pAccelerationStructure)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureMemoryRequirementsNV
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureMemoryRequirementsInfoNV -> Ptr (SomeStruct MemoryRequirements2KHR) -> IO ()) -> Ptr Device_T -> Ptr AccelerationStructureMemoryRequirementsInfoNV -> Ptr (SomeStruct MemoryRequirements2KHR) -> IO ()

-- | vkGetAccelerationStructureMemoryRequirementsNV - Get acceleration
-- structure memory requirements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureMemoryRequirementsInfoNV',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.VK_KHR_get_memory_requirements2.MemoryRequirements2KHR'
getAccelerationStructureMemoryRequirementsNV :: forall a io
                                              . (Extendss MemoryRequirements2KHR a, PokeChain a, PeekChain a, MonadIO io)
                                             => -- | @device@ is the logical device on which the acceleration structure was
                                                -- created.
                                                --
                                                -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                                Device
                                             -> -- | @pInfo@ specifies the acceleration structure to get memory requirements
                                                -- for.
                                                --
                                                -- @pInfo@ /must/ be a valid pointer to a valid
                                                -- 'AccelerationStructureMemoryRequirementsInfoNV' structure
                                                AccelerationStructureMemoryRequirementsInfoNV
                                             -> io (MemoryRequirements2KHR a)
getAccelerationStructureMemoryRequirementsNV device info = liftIO . evalContT $ do
  let vkGetAccelerationStructureMemoryRequirementsNVPtr = pVkGetAccelerationStructureMemoryRequirementsNV (deviceCmds (device :: Device))
  lift $ unless (vkGetAccelerationStructureMemoryRequirementsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetAccelerationStructureMemoryRequirementsNV is null" Nothing Nothing
  let vkGetAccelerationStructureMemoryRequirementsNV' = mkVkGetAccelerationStructureMemoryRequirementsNV vkGetAccelerationStructureMemoryRequirementsNVPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2KHR _))
  lift $ vkGetAccelerationStructureMemoryRequirementsNV' (deviceHandle (device)) pInfo (forgetExtensions (pPMemoryRequirements))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2KHR _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyAccelerationStructureNV
  :: FunPtr (Ptr CommandBuffer_T -> AccelerationStructureKHR -> AccelerationStructureKHR -> CopyAccelerationStructureModeKHR -> IO ()) -> Ptr CommandBuffer_T -> AccelerationStructureKHR -> AccelerationStructureKHR -> CopyAccelerationStructureModeKHR -> IO ()

-- | vkCmdCopyAccelerationStructureNV - Copy an acceleration structure
--
-- == Valid Usage
--
-- -   @mode@ /must/ be
--     'Vulkan.Extensions.VK_KHR_ray_tracing.COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_ray_tracing.COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR'
--
-- -   @src@ /must/ have been built with
--     'Vulkan.Extensions.VK_KHR_ray_tracing.BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR'
--     if @mode@ is
--     'Vulkan.Extensions.VK_KHR_ray_tracing.COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @dst@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   @src@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   @mode@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_ray_tracing.CopyAccelerationStructureModeKHR'
--     value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Each of @commandBuffer@, @dst@, and @src@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.CopyAccelerationStructureModeKHR'
cmdCopyAccelerationStructureNV :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which the command will be
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @dst@ is a pointer to the target acceleration structure for the copy.
                                  ("dst" ::: AccelerationStructureKHR)
                               -> -- | @src@ is a pointer to the source acceleration structure for the copy.
                                  ("src" ::: AccelerationStructureKHR)
                               -> -- | @mode@ is a
                                  -- 'Vulkan.Extensions.VK_KHR_ray_tracing.CopyAccelerationStructureModeKHR'
                                  -- value specifying additional operations to perform during the copy.
                                  CopyAccelerationStructureModeKHR
                               -> io ()
cmdCopyAccelerationStructureNV commandBuffer dst src mode = liftIO $ do
  let vkCmdCopyAccelerationStructureNVPtr = pVkCmdCopyAccelerationStructureNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdCopyAccelerationStructureNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyAccelerationStructureNV is null" Nothing Nothing
  let vkCmdCopyAccelerationStructureNV' = mkVkCmdCopyAccelerationStructureNV vkCmdCopyAccelerationStructureNVPtr
  vkCmdCopyAccelerationStructureNV' (commandBufferHandle (commandBuffer)) (dst) (src) (mode)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildAccelerationStructureNV
  :: FunPtr (Ptr CommandBuffer_T -> Ptr AccelerationStructureInfoNV -> Buffer -> DeviceSize -> Bool32 -> AccelerationStructureKHR -> AccelerationStructureKHR -> Buffer -> DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Ptr AccelerationStructureInfoNV -> Buffer -> DeviceSize -> Bool32 -> AccelerationStructureKHR -> AccelerationStructureKHR -> Buffer -> DeviceSize -> IO ()

-- | vkCmdBuildAccelerationStructureNV - Build an acceleration structure
--
-- = Description
--
-- Accesses to @scratch@ /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types access type>
-- of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR'
-- or
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR'.
--
-- == Valid Usage
--
-- -   @geometryCount@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxGeometryCount@
--
-- -   @dst@ /must/ have been created with compatible
--     'AccelerationStructureInfoNV' where
--     'AccelerationStructureInfoNV'::@type@ and
--     'AccelerationStructureInfoNV'::@flags@ are identical,
--     'AccelerationStructureInfoNV'::@instanceCount@ and
--     'AccelerationStructureInfoNV'::@geometryCount@ for @dst@ are greater
--     than or equal to the build size and each geometry in
--     'AccelerationStructureInfoNV'::@pGeometries@ for @dst@ has greater
--     than or equal to the number of vertices, indices, and AABBs
--
-- -   If @update@ is 'Vulkan.Core10.FundamentalTypes.TRUE', @src@ /must/
--     not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If @update@ is 'Vulkan.Core10.FundamentalTypes.TRUE', @src@ /must/
--     have been built before with
--     'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV' set in
--     'AccelerationStructureInfoNV'::@flags@
--
-- -   If @update@ is 'Vulkan.Core10.FundamentalTypes.FALSE', the @size@
--     member of the 'Vulkan.Core10.MemoryManagement.MemoryRequirements'
--     structure returned from a call to
--     'getAccelerationStructureMemoryRequirementsNV' with
--     'AccelerationStructureMemoryRequirementsInfoNV'::@accelerationStructure@
--     set to @dst@ and
--     'AccelerationStructureMemoryRequirementsInfoNV'::@type@ set to
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV'
--     /must/ be less than or equal to the size of @scratch@ minus
--     @scratchOffset@
--
-- -   If @update@ is 'Vulkan.Core10.FundamentalTypes.TRUE', the @size@
--     member of the 'Vulkan.Core10.MemoryManagement.MemoryRequirements'
--     structure returned from a call to
--     'getAccelerationStructureMemoryRequirementsNV' with
--     'AccelerationStructureMemoryRequirementsInfoNV'::@accelerationStructure@
--     set to @dst@ and
--     'AccelerationStructureMemoryRequirementsInfoNV'::@type@ set to
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV'
--     /must/ be less than or equal to the size of @scratch@ minus
--     @scratchOffset@
--
-- -   @scratch@ /must/ have been created with
--     'BUFFER_USAGE_RAY_TRACING_BIT_NV' usage flag
--
-- -   If @instanceData@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @instanceData@ /must/ have been created with
--     'BUFFER_USAGE_RAY_TRACING_BIT_NV' usage flag
--
-- -   If @update@ is 'Vulkan.Core10.FundamentalTypes.TRUE', then objects
--     that were previously active /must/ not be made inactive as per
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims>
--
-- -   If @update@ is 'Vulkan.Core10.FundamentalTypes.TRUE', then objects
--     that were previously inactive /must/ not be made active as per
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims>
--
-- -   If @update@ is 'Vulkan.Core10.FundamentalTypes.TRUE', the @src@ and
--     @dst@ objects /must/ either be the same object or not have any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing memory aliasing>
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureInfoNV' structure
--
-- -   If @instanceData@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @instanceData@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer'
--     handle
--
-- -   @dst@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   If @src@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @src@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
--
-- -   @scratch@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Each of @commandBuffer@, @dst@, @instanceData@, @scratch@, and @src@
--     that are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'AccelerationStructureInfoNV',
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdBuildAccelerationStructureNV :: forall io
                                 . (MonadIO io)
                                => -- | @commandBuffer@ is the command buffer into which the command will be
                                   -- recorded.
                                   CommandBuffer
                                -> -- | @pInfo@ contains the shared information for the acceleration structure’s
                                   -- structure.
                                   AccelerationStructureInfoNV
                                -> -- | @instanceData@ is the buffer containing an array of
                                   -- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureInstanceKHR'
                                   -- structures defining acceleration structures. This parameter /must/ be
                                   -- @NULL@ for bottom level acceleration structures.
                                   ("instanceData" ::: Buffer)
                                -> -- | @instanceOffset@ is the offset in bytes (relative to the start of
                                   -- @instanceData@) at which the instance data is located.
                                   ("instanceOffset" ::: DeviceSize)
                                -> -- | @update@ specifies whether to update the @dst@ acceleration structure
                                   -- with the data in @src@.
                                   ("update" ::: Bool)
                                -> -- | @dst@ is a pointer to the target acceleration structure for the build.
                                   ("dst" ::: AccelerationStructureKHR)
                                -> -- | @src@ is a pointer to an existing acceleration structure that is to be
                                   -- used to update the @dst@ acceleration structure.
                                   ("src" ::: AccelerationStructureKHR)
                                -> -- | @scratch@ is the 'Vulkan.Core10.Handles.Buffer' that will be used as
                                   -- scratch memory for the build.
                                   ("scratch" ::: Buffer)
                                -> -- | @scratchOffset@ is the offset in bytes relative to the start of
                                   -- @scratch@ that will be used as a scratch memory.
                                   ("scratchOffset" ::: DeviceSize)
                                -> io ()
cmdBuildAccelerationStructureNV commandBuffer info instanceData instanceOffset update dst src scratch scratchOffset = liftIO . evalContT $ do
  let vkCmdBuildAccelerationStructureNVPtr = pVkCmdBuildAccelerationStructureNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBuildAccelerationStructureNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBuildAccelerationStructureNV is null" Nothing Nothing
  let vkCmdBuildAccelerationStructureNV' = mkVkCmdBuildAccelerationStructureNV vkCmdBuildAccelerationStructureNVPtr
  pInfo <- ContT $ withCStruct (info)
  lift $ vkCmdBuildAccelerationStructureNV' (commandBufferHandle (commandBuffer)) pInfo (instanceData) (instanceOffset) (boolToBool32 (update)) (dst) (src) (scratch) (scratchOffset)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdTraceRaysNV
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Word32 -> Word32 -> Word32 -> IO ()

-- | vkCmdTraceRaysNV - Initialize a ray tracing dispatch
--
-- = Description
--
-- When the command is executed, a ray generation group of @width@ ×
-- @height@ × @depth@ rays is assembled.
--
-- == Valid Usage
--
-- -   If a 'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Any 'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   Any 'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   For each set /n/ that is statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command, a descriptor set /must/ have been bound to /n/
--     at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command, a push constant value /must/ have been set for
--     the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command requires any dynamic state, that
--     state /must/ have been set for @commandBuffer@, and done so after
--     any previously bound pipeline with the corresponding state not
--     specified as dynamic
--
-- -   There /must/ not have been any calls to dynamic state setting
--     commands for any state not specified as dynamic in the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command, since that pipeline was bound
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   If @commandBuffer@ is an unprotected command buffer, any resource
--     accessed by the 'Vulkan.Core10.Handles.Pipeline' object bound to the
--     pipeline bind point used by this command /must/ not be a protected
--     resource
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have at least as many
--     components as the image view’s format.
--
-- -   If a 'Vulkan.Core10.Handles.BufferView' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have at least as many
--     components as the image view’s format.
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   If a 'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   If a 'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   If a 'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   Any shader group handle referenced by this call /must/ have been
--     queried from the currently bound ray tracing shader pipeline
--
-- -   This command /must/ not cause a shader call instruction to be
--     executed from a shader invocation with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-tracing-recursion-depth recursion depth>
--     greater than the value of @maxRecursionDepth@ used to create the
--     bound ray tracing pipeline
--
-- -   If @commandBuffer@ is a protected command buffer, any resource
--     written to by the 'Vulkan.Core10.Handles.Pipeline' object bound to
--     the pipeline bind point used by this command /must/ not be an
--     unprotected resource
--
-- -   If @commandBuffer@ is a protected command buffer, pipeline stages
--     other than the framebuffer-space and compute stages in the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point /must/ not write to any resource
--
-- -   If @raygenShaderBindingTableBuffer@ is non-sparse then it /must/ be
--     bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @raygenShaderBindingOffset@ /must/ be less than the size of
--     @raygenShaderBindingTableBuffer@
--
-- -   @raygenShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   If @missShaderBindingTableBuffer@ is non-sparse then it /must/ be
--     bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @missShaderBindingOffset@ /must/ be less than the size of
--     @missShaderBindingTableBuffer@
--
-- -   @missShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   If @hitShaderBindingTableBuffer@ is non-sparse then it /must/ be
--     bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @hitShaderBindingOffset@ /must/ be less than the size of
--     @hitShaderBindingTableBuffer@
--
-- -   @hitShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   If @callableShaderBindingTableBuffer@ is non-sparse then it /must/
--     be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   @callableShaderBindingOffset@ /must/ be less than the size of
--     @callableShaderBindingTableBuffer@
--
-- -   @callableShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @missShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   @hitShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   @callableShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   @missShaderBindingStride@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   @hitShaderBindingStride@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   @callableShaderBindingStride@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   @width@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   @height@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   @depth@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @raygenShaderBindingTableBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.Buffer' handle
--
-- -   If @missShaderBindingTableBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @missShaderBindingTableBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.Buffer' handle
--
-- -   If @hitShaderBindingTableBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @hitShaderBindingTableBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.Buffer' handle
--
-- -   If @callableShaderBindingTableBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @callableShaderBindingTableBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.Buffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Each of @callableShaderBindingTableBuffer@, @commandBuffer@,
--     @hitShaderBindingTableBuffer@, @missShaderBindingTableBuffer@, and
--     @raygenShaderBindingTableBuffer@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdTraceRaysNV :: forall io
                . (MonadIO io)
               => -- | @commandBuffer@ is the command buffer into which the command will be
                  -- recorded.
                  CommandBuffer
               -> -- | @raygenShaderBindingTableBuffer@ is the buffer object that holds the
                  -- shader binding table data for the ray generation shader stage.
                  ("raygenShaderBindingTableBuffer" ::: Buffer)
               -> -- | @raygenShaderBindingOffset@ is the offset in bytes (relative to
                  -- @raygenShaderBindingTableBuffer@) of the ray generation shader being
                  -- used for the trace.
                  ("raygenShaderBindingOffset" ::: DeviceSize)
               -> -- | @missShaderBindingTableBuffer@ is the buffer object that holds the
                  -- shader binding table data for the miss shader stage.
                  ("missShaderBindingTableBuffer" ::: Buffer)
               -> -- | @missShaderBindingOffset@ is the offset in bytes (relative to
                  -- @missShaderBindingTableBuffer@) of the miss shader being used for the
                  -- trace.
                  ("missShaderBindingOffset" ::: DeviceSize)
               -> -- | @missShaderBindingStride@ is the size in bytes of each shader binding
                  -- table record in @missShaderBindingTableBuffer@.
                  ("missShaderBindingStride" ::: DeviceSize)
               -> -- | @hitShaderBindingTableBuffer@ is the buffer object that holds the shader
                  -- binding table data for the hit shader stages.
                  ("hitShaderBindingTableBuffer" ::: Buffer)
               -> -- | @hitShaderBindingOffset@ is the offset in bytes (relative to
                  -- @hitShaderBindingTableBuffer@) of the hit shader group being used for
                  -- the trace.
                  ("hitShaderBindingOffset" ::: DeviceSize)
               -> -- | @hitShaderBindingStride@ is the size in bytes of each shader binding
                  -- table record in @hitShaderBindingTableBuffer@.
                  ("hitShaderBindingStride" ::: DeviceSize)
               -> -- | @callableShaderBindingTableBuffer@ is the buffer object that holds the
                  -- shader binding table data for the callable shader stage.
                  ("callableShaderBindingTableBuffer" ::: Buffer)
               -> -- | @callableShaderBindingOffset@ is the offset in bytes (relative to
                  -- @callableShaderBindingTableBuffer@) of the callable shader being used
                  -- for the trace.
                  ("callableShaderBindingOffset" ::: DeviceSize)
               -> -- | @callableShaderBindingStride@ is the size in bytes of each shader
                  -- binding table record in @callableShaderBindingTableBuffer@.
                  ("callableShaderBindingStride" ::: DeviceSize)
               -> -- | @width@ is the width of the ray trace query dimensions.
                  ("width" ::: Word32)
               -> -- | @height@ is height of the ray trace query dimensions.
                  ("height" ::: Word32)
               -> -- | @depth@ is depth of the ray trace query dimensions.
                  ("depth" ::: Word32)
               -> io ()
cmdTraceRaysNV commandBuffer raygenShaderBindingTableBuffer raygenShaderBindingOffset missShaderBindingTableBuffer missShaderBindingOffset missShaderBindingStride hitShaderBindingTableBuffer hitShaderBindingOffset hitShaderBindingStride callableShaderBindingTableBuffer callableShaderBindingOffset callableShaderBindingStride width height depth = liftIO $ do
  let vkCmdTraceRaysNVPtr = pVkCmdTraceRaysNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdTraceRaysNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdTraceRaysNV is null" Nothing Nothing
  let vkCmdTraceRaysNV' = mkVkCmdTraceRaysNV vkCmdTraceRaysNVPtr
  vkCmdTraceRaysNV' (commandBufferHandle (commandBuffer)) (raygenShaderBindingTableBuffer) (raygenShaderBindingOffset) (missShaderBindingTableBuffer) (missShaderBindingOffset) (missShaderBindingStride) (hitShaderBindingTableBuffer) (hitShaderBindingOffset) (hitShaderBindingStride) (callableShaderBindingTableBuffer) (callableShaderBindingOffset) (callableShaderBindingStride) (width) (height) (depth)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureHandleNV
  :: FunPtr (Ptr Device_T -> AccelerationStructureKHR -> CSize -> Ptr () -> IO Result) -> Ptr Device_T -> AccelerationStructureKHR -> CSize -> Ptr () -> IO Result

-- | vkGetAccelerationStructureHandleNV - Get opaque acceleration structure
-- handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureKHR',
-- 'Vulkan.Core10.Handles.Device'
getAccelerationStructureHandleNV :: forall io
                                  . (MonadIO io)
                                 => -- | @device@ is the logical device that owns the acceleration structures.
                                    --
                                    -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                    Device
                                 -> -- | @accelerationStructure@ is the acceleration structure.
                                    --
                                    -- @accelerationStructure@ /must/ be bound completely and contiguously to a
                                    -- single 'Vulkan.Core10.Handles.DeviceMemory' object via
                                    -- 'Vulkan.Extensions.VK_KHR_ray_tracing.bindAccelerationStructureMemoryKHR'
                                    --
                                    -- @accelerationStructure@ /must/ be a valid
                                    -- 'Vulkan.Extensions.Handles.AccelerationStructureKHR' handle
                                    --
                                    -- @accelerationStructure@ /must/ have been created, allocated, or
                                    -- retrieved from @device@
                                    AccelerationStructureKHR
                                 -> -- | @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
                                    --
                                    -- @dataSize@ /must/ be large enough to contain the result of the query, as
                                    -- described above
                                    --
                                    -- @dataSize@ /must/ be greater than @0@
                                    ("dataSize" ::: Word64)
                                 -> -- | @pData@ is a pointer to a user-allocated buffer where the results will
                                    -- be written.
                                    --
                                    -- @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
                                    ("data" ::: Ptr ())
                                 -> io ()
getAccelerationStructureHandleNV device accelerationStructure dataSize data' = liftIO $ do
  let vkGetAccelerationStructureHandleNVPtr = pVkGetAccelerationStructureHandleNV (deviceCmds (device :: Device))
  unless (vkGetAccelerationStructureHandleNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetAccelerationStructureHandleNV is null" Nothing Nothing
  let vkGetAccelerationStructureHandleNV' = mkVkGetAccelerationStructureHandleNV vkGetAccelerationStructureHandleNVPtr
  r <- vkGetAccelerationStructureHandleNV' (deviceHandle (device)) (accelerationStructure) (CSize (dataSize)) (data')
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRayTracingPipelinesNV
  :: FunPtr (Ptr Device_T -> PipelineCache -> Word32 -> Ptr (SomeStruct RayTracingPipelineCreateInfoNV) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> PipelineCache -> Word32 -> Ptr (SomeStruct RayTracingPipelineCreateInfoNV) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- | vkCreateRayTracingPipelinesNV - Creates a new ray tracing pipeline
-- object
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- -   If @pipelineCache@ was created with
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT',
--     host access to @pipelineCache@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @pipelineCache@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineCache@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'RayTracingPipelineCreateInfoNV' structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Vulkan.Core10.Handles.Pipeline' handles
--
-- -   @createInfoCount@ /must/ be greater than @0@
--
-- -   If @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.PIPELINE_COMPILE_REQUIRED_EXT'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Handles.PipelineCache', 'RayTracingPipelineCreateInfoNV'
createRayTracingPipelinesNV :: forall io
                             . (MonadIO io)
                            => -- | @device@ is the logical device that creates the ray tracing pipelines.
                               Device
                            -> -- | @pipelineCache@ is either 'Vulkan.Core10.APIConstants.NULL_HANDLE',
                               -- indicating that pipeline caching is disabled, or the handle of a valid
                               -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-cache pipeline cache>
                               -- object, in which case use of that cache is enabled for the duration of
                               -- the command.
                               PipelineCache
                            -> -- | @pCreateInfos@ is a pointer to an array of
                               -- 'RayTracingPipelineCreateInfoNV' structures.
                               ("createInfos" ::: Vector (SomeStruct RayTracingPipelineCreateInfoNV))
                            -> -- | @pAllocator@ controls host memory allocation as described in the
                               -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                               -- chapter.
                               ("allocator" ::: Maybe AllocationCallbacks)
                            -> io (Result, ("pipelines" ::: Vector Pipeline))
createRayTracingPipelinesNV device pipelineCache createInfos allocator = liftIO . evalContT $ do
  let vkCreateRayTracingPipelinesNVPtr = pVkCreateRayTracingPipelinesNV (deviceCmds (device :: Device))
  lift $ unless (vkCreateRayTracingPipelinesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateRayTracingPipelinesNV is null" Nothing Nothing
  let vkCreateRayTracingPipelinesNV' = mkVkCreateRayTracingPipelinesNV vkCreateRayTracingPipelinesNVPtr
  pPCreateInfos <- ContT $ allocaBytesAligned @(RayTracingPipelineCreateInfoNV _) ((Data.Vector.length (createInfos)) * 80) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCreateInfos `plusPtr` (80 * (i)) :: Ptr (RayTracingPipelineCreateInfoNV _))) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ vkCreateRayTracingPipelinesNV' (deviceHandle (device)) (pipelineCache) ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32)) (forgetExtensions (pPCreateInfos)) pAllocator (pPPipelines)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelines <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @Pipeline ((pPPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
  pure $ (r, pPipelines)


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV"
pattern STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV = STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV"
pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV = STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR


-- No documentation found for TopLevel "VK_SHADER_STAGE_RAYGEN_BIT_NV"
pattern SHADER_STAGE_RAYGEN_BIT_NV = SHADER_STAGE_RAYGEN_BIT_KHR


-- No documentation found for TopLevel "VK_SHADER_STAGE_ANY_HIT_BIT_NV"
pattern SHADER_STAGE_ANY_HIT_BIT_NV = SHADER_STAGE_ANY_HIT_BIT_KHR


-- No documentation found for TopLevel "VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV"
pattern SHADER_STAGE_CLOSEST_HIT_BIT_NV = SHADER_STAGE_CLOSEST_HIT_BIT_KHR


-- No documentation found for TopLevel "VK_SHADER_STAGE_MISS_BIT_NV"
pattern SHADER_STAGE_MISS_BIT_NV = SHADER_STAGE_MISS_BIT_KHR


-- No documentation found for TopLevel "VK_SHADER_STAGE_INTERSECTION_BIT_NV"
pattern SHADER_STAGE_INTERSECTION_BIT_NV = SHADER_STAGE_INTERSECTION_BIT_KHR


-- No documentation found for TopLevel "VK_SHADER_STAGE_CALLABLE_BIT_NV"
pattern SHADER_STAGE_CALLABLE_BIT_NV = SHADER_STAGE_CALLABLE_BIT_KHR


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV"
pattern PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV = PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV"
pattern PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV = PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR


-- No documentation found for TopLevel "VK_BUFFER_USAGE_RAY_TRACING_BIT_NV"
pattern BUFFER_USAGE_RAY_TRACING_BIT_NV = BUFFER_USAGE_RAY_TRACING_BIT_KHR


-- No documentation found for TopLevel "VK_PIPELINE_BIND_POINT_RAY_TRACING_NV"
pattern PIPELINE_BIND_POINT_RAY_TRACING_NV = PIPELINE_BIND_POINT_RAY_TRACING_KHR


-- No documentation found for TopLevel "VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV"
pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV = DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR


-- No documentation found for TopLevel "VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV"
pattern ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV = ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR


-- No documentation found for TopLevel "VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV"
pattern ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV = ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR


-- No documentation found for TopLevel "VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV"
pattern QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV = QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR


-- No documentation found for TopLevel "VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV"
pattern OBJECT_TYPE_ACCELERATION_STRUCTURE_NV = OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR


-- No documentation found for TopLevel "VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT = DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT


-- No documentation found for TopLevel "VK_INDEX_TYPE_NONE_NV"
pattern INDEX_TYPE_NONE_NV = INDEX_TYPE_NONE_KHR


-- No documentation found for TopLevel "VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV"
pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV = RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR


-- No documentation found for TopLevel "VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV"
pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV = RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR


-- No documentation found for TopLevel "VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV"
pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV = RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_TYPE_TRIANGLES_NV"
pattern GEOMETRY_TYPE_TRIANGLES_NV = GEOMETRY_TYPE_TRIANGLES_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_TYPE_AABBS_NV"
pattern GEOMETRY_TYPE_AABBS_NV = GEOMETRY_TYPE_AABBS_KHR


-- No documentation found for TopLevel "VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV"
pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV = ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR


-- No documentation found for TopLevel "VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV"
pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV = ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_OPAQUE_BIT_NV"
pattern GEOMETRY_OPAQUE_BIT_NV = GEOMETRY_OPAQUE_BIT_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV"
pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV = GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV"
pattern GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV = GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV"
pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV = GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV"
pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV = GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV"
pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV = GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR


-- No documentation found for TopLevel "VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV = BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR


-- No documentation found for TopLevel "VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV = BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR


-- No documentation found for TopLevel "VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV = BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR


-- No documentation found for TopLevel "VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV = BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR


-- No documentation found for TopLevel "VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV = BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR


-- No documentation found for TopLevel "VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV"
pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV = COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR


-- No documentation found for TopLevel "VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV"
pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV = COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR


-- No documentation found for TopLevel "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV"
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV = ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_KHR


-- No documentation found for TopLevel "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV"
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV = ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_KHR


-- No documentation found for TopLevel "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV"
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV = ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_KHR


-- No documentation found for TopLevel "vkDestroyAccelerationStructureNV"
destroyAccelerationStructureNV = destroyAccelerationStructureKHR


-- No documentation found for TopLevel "vkBindAccelerationStructureMemoryNV"
bindAccelerationStructureMemoryNV = bindAccelerationStructureMemoryKHR


-- No documentation found for TopLevel "vkCmdWriteAccelerationStructuresPropertiesNV"
cmdWriteAccelerationStructuresPropertiesNV = cmdWriteAccelerationStructuresPropertiesKHR


-- No documentation found for TopLevel "vkGetRayTracingShaderGroupHandlesNV"
getRayTracingShaderGroupHandlesNV = getRayTracingShaderGroupHandlesKHR


-- No documentation found for TopLevel "VK_SHADER_UNUSED_NV"
pattern SHADER_UNUSED_NV = SHADER_UNUSED_KHR


-- | VkRayTracingShaderGroupCreateInfoNV - Structure specifying shaders in a
-- shader group
--
-- == Valid Usage
--
-- -   If @type@ is 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV' then
--     @generalShader@ /must/ be a valid index into
--     'RayTracingPipelineCreateInfoNV'::@pStages@ referring to a shader of
--     'SHADER_STAGE_RAYGEN_BIT_NV', 'SHADER_STAGE_MISS_BIT_NV', or
--     'SHADER_STAGE_CALLABLE_BIT_NV'
--
-- -   If @type@ is 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV' then
--     @closestHitShader@, @anyHitShader@, and @intersectionShader@ /must/
--     be 'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV'
--
-- -   If @type@ is 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV'
--     then @intersectionShader@ /must/ be a valid index into
--     'RayTracingPipelineCreateInfoNV'::@pStages@ referring to a shader of
--     'SHADER_STAGE_INTERSECTION_BIT_NV'
--
-- -   If @type@ is 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV'
--     then @intersectionShader@ /must/ be
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV'
--
-- -   @closestHitShader@ /must/ be either
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' or a valid index into
--     'RayTracingPipelineCreateInfoNV'::@pStages@ referring to a shader of
--     'SHADER_STAGE_CLOSEST_HIT_BIT_NV'
--
-- -   @anyHitShader@ /must/ be either
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' or a valid index into
--     'RayTracingPipelineCreateInfoNV'::@pStages@ referring to a shader of
--     'SHADER_STAGE_ANY_HIT_BIT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @type@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_ray_tracing.RayTracingShaderGroupTypeKHR'
--     value
--
-- = See Also
--
-- 'RayTracingPipelineCreateInfoNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.RayTracingShaderGroupTypeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RayTracingShaderGroupCreateInfoNV = RayTracingShaderGroupCreateInfoNV
  { -- | @type@ is the type of hit group specified in this structure.
    type' :: RayTracingShaderGroupTypeKHR
  , -- | @generalShader@ is the index of the ray generation, miss, or callable
    -- shader from 'RayTracingPipelineCreateInfoNV'::@pStages@ in the group if
    -- the shader group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' otherwise.
    generalShader :: Word32
  , -- | @closestHitShader@ is the optional index of the closest hit shader from
    -- 'RayTracingPipelineCreateInfoNV'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV' or
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' otherwise.
    closestHitShader :: Word32
  , -- | @anyHitShader@ is the optional index of the any-hit shader from
    -- 'RayTracingPipelineCreateInfoNV'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV' or
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' otherwise.
    anyHitShader :: Word32
  , -- | @intersectionShader@ is the index of the intersection shader from
    -- 'RayTracingPipelineCreateInfoNV'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' otherwise.
    intersectionShader :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RayTracingShaderGroupCreateInfoNV)
#endif
deriving instance Show RayTracingShaderGroupCreateInfoNV

instance ToCStruct RayTracingShaderGroupCreateInfoNV where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingShaderGroupCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeKHR)) (type')
    poke ((p `plusPtr` 20 :: Ptr Word32)) (generalShader)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (closestHitShader)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (anyHitShader)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (intersectionShader)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeKHR)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct RayTracingShaderGroupCreateInfoNV where
  peekCStruct p = do
    type' <- peek @RayTracingShaderGroupTypeKHR ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeKHR))
    generalShader <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    closestHitShader <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    anyHitShader <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    intersectionShader <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ RayTracingShaderGroupCreateInfoNV
             type' generalShader closestHitShader anyHitShader intersectionShader

instance Storable RayTracingShaderGroupCreateInfoNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RayTracingShaderGroupCreateInfoNV where
  zero = RayTracingShaderGroupCreateInfoNV
           zero
           zero
           zero
           zero
           zero


-- | VkRayTracingPipelineCreateInfoNV - Structure specifying parameters of a
-- newly created ray tracing pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- == Valid Usage
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is @-1@, @basePipelineHandle@ /must/
--     be a valid handle to a ray tracing 'Vulkan.Core10.Handles.Pipeline'
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be a valid index into the calling command’s @pCreateInfos@ parameter
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not @-1@, @basePipelineHandle@
--     /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be @-1@
--
-- -   The @stage@ member of at least one element of @pStages@ /must/ be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_KHR'
--
-- -   The shader code for the entry points identified by @pStages@, and
--     the rest of the state identified by this structure /must/ adhere to
--     the pipeline linking rules described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   @layout@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency consistent>
--     with all shaders specified in @pStages@
--
-- -   The number of resources in @layout@ accessible to each shader stage
--     that is used by the pipeline /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineCreationCacheControl pipelineCreationCacheControl>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- -   @maxRecursionDepth@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxRecursionDepth@
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'
--
-- -   @flags@ /must/ not include both
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEFER_COMPILE_BIT_NV'
--     and
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
--     at the same time
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--     values
--
-- -   @pStages@ /must/ be a valid pointer to an array of @stageCount@
--     valid 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'
--     structures
--
-- -   @pGroups@ /must/ be a valid pointer to an array of @groupCount@
--     valid 'RayTracingShaderGroupCreateInfoNV' structures
--
-- -   @layout@ /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout'
--     handle
--
-- -   @stageCount@ /must/ be greater than @0@
--
-- -   @groupCount@ /must/ be greater than @0@
--
-- -   Both of @basePipelineHandle@, and @layout@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'RayTracingShaderGroupCreateInfoNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createRayTracingPipelinesNV'
data RayTracingPipelineCreateInfoNV (es :: [Type]) = RayTracingPipelineCreateInfoNV
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
    -- specifying how the pipeline will be generated.
    flags :: PipelineCreateFlags
  , -- | @pStages@ is an array of size @stageCount@ structures of type
    -- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' describing the
    -- set of the shader stages to be included in the ray tracing pipeline.
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- | @pGroups@ is an array of size @groupCount@ structures of type
    -- 'RayTracingShaderGroupCreateInfoNV' describing the set of the shader
    -- stages to be included in each shader group in the ray tracing pipeline.
    groups :: Vector RayTracingShaderGroupCreateInfoNV
  , -- | @maxRecursionDepth@ is the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-tracing-recursion-depth maximum recursion depth>
    -- of shaders executed by this pipeline.
    maxRecursionDepth :: Word32
  , -- | @layout@ is the description of binding locations used by both the
    -- pipeline and descriptor sets used with the pipeline.
    layout :: PipelineLayout
  , -- | @basePipelineHandle@ is a pipeline to derive from.
    basePipelineHandle :: Pipeline
  , -- | @basePipelineIndex@ is an index into the @pCreateInfos@ parameter to use
    -- as a pipeline to derive from.
    basePipelineIndex :: Int32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RayTracingPipelineCreateInfoNV (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RayTracingPipelineCreateInfoNV es)

instance Extensible RayTracingPipelineCreateInfoNV where
  extensibleType = STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV
  setNext x next = x{next = next}
  getNext RayTracingPipelineCreateInfoNV{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RayTracingPipelineCreateInfoNV e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineCreationFeedbackCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss RayTracingPipelineCreateInfoNV es, PokeChain es) => ToCStruct (RayTracingPipelineCreateInfoNV es) where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingPipelineCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (stages)) :: Word32))
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (stages)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (stages)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (groups)) :: Word32))
    pPGroups' <- ContT $ allocaBytesAligned @RayTracingShaderGroupCreateInfoNV ((Data.Vector.length (groups)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGroups' `plusPtr` (40 * (i)) :: Ptr RayTracingShaderGroupCreateInfoNV) (e) . ($ ())) (groups)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoNV))) (pPGroups')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (maxRecursionDepth)
    lift $ poke ((p `plusPtr` 56 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 64 :: Ptr Pipeline)) (basePipelineHandle)
    lift $ poke ((p `plusPtr` 72 :: Ptr Int32)) (basePipelineIndex)
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    pPGroups' <- ContT $ allocaBytesAligned @RayTracingShaderGroupCreateInfoNV ((Data.Vector.length (mempty)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGroups' `plusPtr` (40 * (i)) :: Ptr RayTracingShaderGroupCreateInfoNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoNV))) (pPGroups')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr PipelineLayout)) (zero)
    lift $ poke ((p `plusPtr` 72 :: Ptr Int32)) (zero)
    lift $ f

instance (Extendss RayTracingPipelineCreateInfoNV es, PeekChain es) => FromCStruct (RayTracingPipelineCreateInfoNV es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineCreateFlags ((p `plusPtr` 16 :: Ptr PipelineCreateFlags))
    stageCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pStages <- peek @(Ptr (PipelineShaderStageCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo a))))
    pStages' <- generateM (fromIntegral stageCount) (\i -> peekSomeCStruct (forgetExtensions ((pStages `advancePtrBytes` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _)))))
    groupCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pGroups <- peek @(Ptr RayTracingShaderGroupCreateInfoNV) ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoNV)))
    pGroups' <- generateM (fromIntegral groupCount) (\i -> peekCStruct @RayTracingShaderGroupCreateInfoNV ((pGroups `advancePtrBytes` (40 * (i)) :: Ptr RayTracingShaderGroupCreateInfoNV)))
    maxRecursionDepth <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    layout <- peek @PipelineLayout ((p `plusPtr` 56 :: Ptr PipelineLayout))
    basePipelineHandle <- peek @Pipeline ((p `plusPtr` 64 :: Ptr Pipeline))
    basePipelineIndex <- peek @Int32 ((p `plusPtr` 72 :: Ptr Int32))
    pure $ RayTracingPipelineCreateInfoNV
             next flags pStages' pGroups' maxRecursionDepth layout basePipelineHandle basePipelineIndex

instance es ~ '[] => Zero (RayTracingPipelineCreateInfoNV es) where
  zero = RayTracingPipelineCreateInfoNV
           ()
           zero
           mempty
           mempty
           zero
           zero
           zero
           zero


-- | VkGeometryTrianglesNV - Structure specifying a triangle geometry in a
-- bottom-level acceleration structure
--
-- = Description
--
-- If @indexType@ is 'INDEX_TYPE_NONE_NV', then this structure describes a
-- set of triangles determined by @vertexCount@. Otherwise, this structure
-- describes a set of indexed triangles determined by @indexCount@.
--
-- == Valid Usage
--
-- -   @vertexOffset@ /must/ be less than the size of @vertexData@
--
-- -   @vertexOffset@ /must/ be a multiple of the component size of
--     @vertexFormat@
--
-- -   @vertexFormat@ /must/ be one of
--     'Vulkan.Core10.Enums.Format.FORMAT_R32G32B32_SFLOAT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R32G32_SFLOAT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16G16B16_SFLOAT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16G16_SFLOAT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16G16_SNORM', or
--     'Vulkan.Core10.Enums.Format.FORMAT_R16G16B16_SNORM'
--
-- -   @indexOffset@ /must/ be less than the size of @indexData@
--
-- -   @indexOffset@ /must/ be a multiple of the element size of
--     @indexType@
--
-- -   @indexType@ /must/ be
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16',
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32', or
--     'INDEX_TYPE_NONE_NV'
--
-- -   @indexData@ /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE' if
--     @indexType@ is 'INDEX_TYPE_NONE_NV'
--
-- -   @indexData@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--     if @indexType@ is not 'INDEX_TYPE_NONE_NV'
--
-- -   @indexCount@ /must/ be @0@ if @indexType@ is 'INDEX_TYPE_NONE_NV'
--
-- -   @transformOffset@ /must/ be less than the size of @transformData@
--
-- -   @transformOffset@ /must/ be a multiple of @16@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   If @vertexData@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @vertexData@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   @vertexFormat@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format'
--     value
--
-- -   If @indexData@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @indexData@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   @indexType@ /must/ be a valid
--     'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   If @transformData@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @transformData@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer'
--     handle
--
-- -   Each of @indexData@, @transformData@, and @vertexData@ that are
--     valid handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.Format.Format', 'GeometryDataNV',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GeometryTrianglesNV = GeometryTrianglesNV
  { -- | @vertexData@ is the buffer containing vertex data for this geometry.
    vertexData :: Buffer
  , -- | @vertexOffset@ is the offset in bytes within @vertexData@ containing
    -- vertex data for this geometry.
    vertexOffset :: DeviceSize
  , -- | @vertexCount@ is the number of valid vertices.
    vertexCount :: Word32
  , -- | @vertexStride@ is the stride in bytes between each vertex.
    vertexStride :: DeviceSize
  , -- | @vertexFormat@ is a 'Vulkan.Core10.Enums.Format.Format' describing the
    -- format of each vertex element.
    vertexFormat :: Format
  , -- | @indexData@ is the buffer containing index data for this geometry.
    indexData :: Buffer
  , -- | @indexOffset@ is the offset in bytes within @indexData@ containing index
    -- data for this geometry.
    indexOffset :: DeviceSize
  , -- | @indexCount@ is the number of indices to include in this geometry.
    indexCount :: Word32
  , -- | @indexType@ is a 'Vulkan.Core10.Enums.IndexType.IndexType' describing
    -- the format of each index.
    indexType :: IndexType
  , -- | @transformData@ is an optional buffer containing an 'TransformMatrixNV'
    -- structure defining a transformation to be applied to this geometry.
    transformData :: Buffer
  , -- | @transformOffset@ is the offset in bytes in @transformData@ of the
    -- transform information described above.
    transformOffset :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeometryTrianglesNV)
#endif
deriving instance Show GeometryTrianglesNV

instance ToCStruct GeometryTrianglesNV where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryTrianglesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (vertexData)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (vertexOffset)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (vertexCount)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (vertexStride)
    poke ((p `plusPtr` 48 :: Ptr Format)) (vertexFormat)
    poke ((p `plusPtr` 56 :: Ptr Buffer)) (indexData)
    poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (indexOffset)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (indexCount)
    poke ((p `plusPtr` 76 :: Ptr IndexType)) (indexType)
    poke ((p `plusPtr` 80 :: Ptr Buffer)) (transformData)
    poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (transformOffset)
    f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 76 :: Ptr IndexType)) (zero)
    poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct GeometryTrianglesNV where
  peekCStruct p = do
    vertexData <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    vertexOffset <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    vertexCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    vertexStride <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    vertexFormat <- peek @Format ((p `plusPtr` 48 :: Ptr Format))
    indexData <- peek @Buffer ((p `plusPtr` 56 :: Ptr Buffer))
    indexOffset <- peek @DeviceSize ((p `plusPtr` 64 :: Ptr DeviceSize))
    indexCount <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    indexType <- peek @IndexType ((p `plusPtr` 76 :: Ptr IndexType))
    transformData <- peek @Buffer ((p `plusPtr` 80 :: Ptr Buffer))
    transformOffset <- peek @DeviceSize ((p `plusPtr` 88 :: Ptr DeviceSize))
    pure $ GeometryTrianglesNV
             vertexData vertexOffset vertexCount vertexStride vertexFormat indexData indexOffset indexCount indexType transformData transformOffset

instance Storable GeometryTrianglesNV where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeometryTrianglesNV where
  zero = GeometryTrianglesNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkGeometryAABBNV - Structure specifying axis-aligned bounding box
-- geometry in a bottom-level acceleration structure
--
-- = Description
--
-- The AABB data in memory is six 32-bit floats consisting of the minimum
-- x, y, and z values followed by the maximum x, y, and z values.
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of @aabbData@
--
-- -   @offset@ /must/ be a multiple of @8@
--
-- -   @stride@ /must/ be a multiple of @8@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GEOMETRY_AABB_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   If @aabbData@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @aabbData@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize', 'GeometryDataNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GeometryAABBNV = GeometryAABBNV
  { -- | @aabbData@ is the buffer containing axis-aligned bounding box data.
    aabbData :: Buffer
  , -- | @numAABBs@ is the number of AABBs in this geometry.
    numAABBs :: Word32
  , -- | @stride@ is the stride in bytes between AABBs in @aabbData@.
    stride :: Word32
  , -- | @offset@ is the offset in bytes of the first AABB in @aabbData@.
    offset :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeometryAABBNV)
#endif
deriving instance Show GeometryAABBNV

instance ToCStruct GeometryAABBNV where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryAABBNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_AABB_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (aabbData)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (numAABBs)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (stride)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (offset)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_AABB_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct GeometryAABBNV where
  peekCStruct p = do
    aabbData <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    numAABBs <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    stride <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    offset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ GeometryAABBNV
             aabbData numAABBs stride offset

instance Storable GeometryAABBNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeometryAABBNV where
  zero = GeometryAABBNV
           zero
           zero
           zero
           zero


-- | VkGeometryDataNV - Structure specifying geometry in a bottom-level
-- acceleration structure
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'GeometryAABBNV', 'GeometryNV', 'GeometryTrianglesNV'
data GeometryDataNV = GeometryDataNV
  { -- | @triangles@ contains triangle data if 'GeometryNV'::@geometryType@ is
    -- 'GEOMETRY_TYPE_TRIANGLES_NV'.
    --
    -- @triangles@ /must/ be a valid 'GeometryTrianglesNV' structure
    triangles :: GeometryTrianglesNV
  , -- | @aabbs@ contains axis-aligned bounding box data if
    -- 'GeometryNV'::@geometryType@ is 'GEOMETRY_TYPE_AABBS_NV'.
    --
    -- @aabbs@ /must/ be a valid 'GeometryAABBNV' structure
    aabbs :: GeometryAABBNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeometryDataNV)
#endif
deriving instance Show GeometryDataNV

instance ToCStruct GeometryDataNV where
  withCStruct x f = allocaBytesAligned 136 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryDataNV{..} f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr GeometryTrianglesNV)) (triangles) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 96 :: Ptr GeometryAABBNV)) (aabbs) . ($ ())
    lift $ f
  cStructSize = 136
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    ContT $ pokeCStruct ((p `plusPtr` 0 :: Ptr GeometryTrianglesNV)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 96 :: Ptr GeometryAABBNV)) (zero) . ($ ())
    lift $ f

instance FromCStruct GeometryDataNV where
  peekCStruct p = do
    triangles <- peekCStruct @GeometryTrianglesNV ((p `plusPtr` 0 :: Ptr GeometryTrianglesNV))
    aabbs <- peekCStruct @GeometryAABBNV ((p `plusPtr` 96 :: Ptr GeometryAABBNV))
    pure $ GeometryDataNV
             triangles aabbs

instance Zero GeometryDataNV where
  zero = GeometryDataNV
           zero
           zero


-- | VkGeometryNV - Structure specifying a geometry in a bottom-level
-- acceleration structure
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureInfoNV', 'GeometryDataNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.GeometryFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.GeometryTypeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GeometryNV = GeometryNV
  { -- | @geometryType@ specifies the
    -- 'Vulkan.Extensions.VK_KHR_ray_tracing.GeometryTypeKHR' which this
    -- geometry refers to.
    --
    -- @geometryType@ /must/ be 'GEOMETRY_TYPE_TRIANGLES_NV' or
    -- 'GEOMETRY_TYPE_AABBS_NV'
    --
    -- @geometryType@ /must/ be a valid
    -- 'Vulkan.Extensions.VK_KHR_ray_tracing.GeometryTypeKHR' value
    geometryType :: GeometryTypeKHR
  , -- | @geometry@ contains the geometry data as described in 'GeometryDataNV'.
    --
    -- @geometry@ /must/ be a valid 'GeometryDataNV' structure
    geometry :: GeometryDataNV
  , -- | @flags@ has 'Vulkan.Extensions.VK_KHR_ray_tracing.GeometryFlagBitsKHR'
    -- describing options for this geometry.
    --
    -- @flags@ /must/ be a valid combination of
    -- 'Vulkan.Extensions.VK_KHR_ray_tracing.GeometryFlagBitsKHR' values
    flags :: GeometryFlagsKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeometryNV)
#endif
deriving instance Show GeometryNV

instance ToCStruct GeometryNV where
  withCStruct x f = allocaBytesAligned 168 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (geometryType)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr GeometryDataNV)) (geometry) . ($ ())
    lift $ poke ((p `plusPtr` 160 :: Ptr GeometryFlagsKHR)) (flags)
    lift $ f
  cStructSize = 168
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr GeometryDataNV)) (zero) . ($ ())
    lift $ f

instance FromCStruct GeometryNV where
  peekCStruct p = do
    geometryType <- peek @GeometryTypeKHR ((p `plusPtr` 16 :: Ptr GeometryTypeKHR))
    geometry <- peekCStruct @GeometryDataNV ((p `plusPtr` 24 :: Ptr GeometryDataNV))
    flags <- peek @GeometryFlagsKHR ((p `plusPtr` 160 :: Ptr GeometryFlagsKHR))
    pure $ GeometryNV
             geometryType geometry flags

instance Zero GeometryNV where
  zero = GeometryNV
           zero
           zero
           zero


-- | VkAccelerationStructureInfoNV - Structure specifying the parameters of
-- acceleration structure object
--
-- = Description
--
-- 'AccelerationStructureInfoNV' contains information that is used both for
-- acceleration structure creation with 'createAccelerationStructureNV' and
-- in combination with the actual geometric data to build the acceleration
-- structure with 'cmdBuildAccelerationStructureNV'.
--
-- == Valid Usage
--
-- -   @geometryCount@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxGeometryCount@
--
-- -   @instanceCount@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxInstanceCount@
--
-- -   The total number of triangles in all geometries /must/ be less than
--     or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxTriangleCount@
--
-- -   If @type@ is 'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV' then
--     @geometryCount@ /must/ be @0@
--
-- -   If @type@ is 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV' then
--     @instanceCount@ /must/ be @0@
--
-- -   If @type@ is 'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV' then the
--     @geometryType@ member of each geometry in @pGeometries@ /must/ be
--     the same
--
-- -   If @flags@ has the
--     'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV' bit set,
--     then it /must/ not have the
--     'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV' bit set
--
-- -   @scratch@ /must/ have been created with
--     'BUFFER_USAGE_RAY_TRACING_BIT_NV' usage flag
--
-- -   If @instanceData@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @instanceData@ /must/ have been created with
--     'BUFFER_USAGE_RAY_TRACING_BIT_NV' usage flag
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @type@ /must/ be a valid 'AccelerationStructureTypeNV' value
--
-- -   @flags@ /must/ be a valid combination of
--     'BuildAccelerationStructureFlagBitsNV' values
--
-- -   If @geometryCount@ is not @0@, @pGeometries@ /must/ be a valid
--     pointer to an array of @geometryCount@ valid 'GeometryNV' structures
--
-- = See Also
--
-- 'AccelerationStructureCreateInfoNV', 'AccelerationStructureTypeNV',
-- 'BuildAccelerationStructureFlagsNV', 'GeometryNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBuildAccelerationStructureNV'
data AccelerationStructureInfoNV = AccelerationStructureInfoNV
  { -- | @type@ is a 'AccelerationStructureTypeNV' value specifying the type of
    -- acceleration structure that will be created.
    type' :: AccelerationStructureTypeNV
  , -- | @flags@ is a bitmask of 'BuildAccelerationStructureFlagBitsNV'
    -- specifying additional parameters of the acceleration structure.
    flags :: BuildAccelerationStructureFlagsNV
  , -- | @instanceCount@ specifies the number of instances that will be in the
    -- new acceleration structure.
    instanceCount :: Word32
  , -- | @pGeometries@ is a pointer to an array of @geometryCount@ 'GeometryNV'
    -- structures containing the scene data being passed into the acceleration
    -- structure.
    geometries :: Vector GeometryNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureInfoNV)
#endif
deriving instance Show AccelerationStructureInfoNV

instance ToCStruct AccelerationStructureInfoNV where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeNV)) (type')
    lift $ poke ((p `plusPtr` 20 :: Ptr BuildAccelerationStructureFlagsNV)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (instanceCount)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (geometries)) :: Word32))
    pPGeometries' <- ContT $ allocaBytesAligned @GeometryNV ((Data.Vector.length (geometries)) * 168) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGeometries' `plusPtr` (168 * (i)) :: Ptr GeometryNV) (e) . ($ ())) (geometries)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr GeometryNV))) (pPGeometries')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeNV)) (zero)
    pPGeometries' <- ContT $ allocaBytesAligned @GeometryNV ((Data.Vector.length (mempty)) * 168) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGeometries' `plusPtr` (168 * (i)) :: Ptr GeometryNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr GeometryNV))) (pPGeometries')
    lift $ f

instance FromCStruct AccelerationStructureInfoNV where
  peekCStruct p = do
    type' <- peek @AccelerationStructureTypeNV ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeNV))
    flags <- peek @BuildAccelerationStructureFlagsNV ((p `plusPtr` 20 :: Ptr BuildAccelerationStructureFlagsNV))
    instanceCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    geometryCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pGeometries <- peek @(Ptr GeometryNV) ((p `plusPtr` 32 :: Ptr (Ptr GeometryNV)))
    pGeometries' <- generateM (fromIntegral geometryCount) (\i -> peekCStruct @GeometryNV ((pGeometries `advancePtrBytes` (168 * (i)) :: Ptr GeometryNV)))
    pure $ AccelerationStructureInfoNV
             type' flags instanceCount pGeometries'

instance Zero AccelerationStructureInfoNV where
  zero = AccelerationStructureInfoNV
           zero
           zero
           zero
           mempty


-- | VkAccelerationStructureCreateInfoNV - Structure specifying the
-- parameters of a newly created acceleration structure object
--
-- == Valid Usage
--
-- -   If @compactedSize@ is not @0@ then both @info.geometryCount@ and
--     @info.instanceCount@ /must/ be @0@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @info@ /must/ be a valid 'AccelerationStructureInfoNV' structure
--
-- = See Also
--
-- 'AccelerationStructureInfoNV',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createAccelerationStructureNV'
data AccelerationStructureCreateInfoNV = AccelerationStructureCreateInfoNV
  { -- | @compactedSize@ is the size from the result of
    -- 'cmdWriteAccelerationStructuresPropertiesNV' if this acceleration
    -- structure is going to be the target of a compacting copy.
    compactedSize :: DeviceSize
  , -- | @info@ is the 'AccelerationStructureInfoNV' structure specifying further
    -- parameters of the created acceleration structure.
    info :: AccelerationStructureInfoNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureCreateInfoNV)
#endif
deriving instance Show AccelerationStructureCreateInfoNV

instance ToCStruct AccelerationStructureCreateInfoNV where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (compactedSize)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr AccelerationStructureInfoNV)) (info) . ($ ())
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr AccelerationStructureInfoNV)) (zero) . ($ ())
    lift $ f

instance FromCStruct AccelerationStructureCreateInfoNV where
  peekCStruct p = do
    compactedSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    info <- peekCStruct @AccelerationStructureInfoNV ((p `plusPtr` 24 :: Ptr AccelerationStructureInfoNV))
    pure $ AccelerationStructureCreateInfoNV
             compactedSize info

instance Zero AccelerationStructureCreateInfoNV where
  zero = AccelerationStructureCreateInfoNV
           zero
           zero


-- | VkAccelerationStructureMemoryRequirementsInfoNV - Structure specifying
-- acceleration to query for memory requirements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureMemoryRequirementsTypeNV',
-- 'AccelerationStructureNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getAccelerationStructureMemoryRequirementsNV'
data AccelerationStructureMemoryRequirementsInfoNV = AccelerationStructureMemoryRequirementsInfoNV
  { -- | @type@ selects the type of memory requirement being queried.
    -- 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV' returns the
    -- memory requirements for the object itself.
    -- 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV'
    -- returns the memory requirements for the scratch memory when doing a
    -- build.
    -- 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV'
    -- returns the memory requirements for the scratch memory when doing an
    -- update.
    --
    -- @type@ /must/ be a valid 'AccelerationStructureMemoryRequirementsTypeNV'
    -- value
    type' :: AccelerationStructureMemoryRequirementsTypeNV
  , -- | @accelerationStructure@ is the acceleration structure to be queried for
    -- memory requirements.
    --
    -- @accelerationStructure@ /must/ be a valid 'AccelerationStructureNV'
    -- handle
    accelerationStructure :: AccelerationStructureNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureMemoryRequirementsInfoNV)
#endif
deriving instance Show AccelerationStructureMemoryRequirementsInfoNV

instance ToCStruct AccelerationStructureMemoryRequirementsInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureMemoryRequirementsInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureMemoryRequirementsTypeNV)) (type')
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureNV)) (accelerationStructure)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureMemoryRequirementsTypeNV)) (zero)
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureNV)) (zero)
    f

instance FromCStruct AccelerationStructureMemoryRequirementsInfoNV where
  peekCStruct p = do
    type' <- peek @AccelerationStructureMemoryRequirementsTypeNV ((p `plusPtr` 16 :: Ptr AccelerationStructureMemoryRequirementsTypeNV))
    accelerationStructure <- peek @AccelerationStructureNV ((p `plusPtr` 24 :: Ptr AccelerationStructureNV))
    pure $ AccelerationStructureMemoryRequirementsInfoNV
             type' accelerationStructure

instance Storable AccelerationStructureMemoryRequirementsInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureMemoryRequirementsInfoNV where
  zero = AccelerationStructureMemoryRequirementsInfoNV
           zero
           zero


-- | VkPhysicalDeviceRayTracingPropertiesNV - Properties of the physical
-- device for ray tracing
--
-- = Description
--
-- If the 'PhysicalDeviceRayTracingPropertiesNV' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Limits specified by this structure /must/ match those specified with the
-- same name in
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.PhysicalDeviceRayTracingPropertiesKHR'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingPropertiesNV = PhysicalDeviceRayTracingPropertiesNV
  { -- | @shaderGroupHandleSize@ size in bytes of the shader header.
    shaderGroupHandleSize :: Word32
  , -- | @maxRecursionDepth@ is the maximum number of levels of recursion allowed
    -- in a trace command.
    maxRecursionDepth :: Word32
  , -- | @maxShaderGroupStride@ is the maximum stride in bytes allowed between
    -- shader groups in the SBT.
    maxShaderGroupStride :: Word32
  , -- | @shaderGroupBaseAlignment@ is the required alignment in bytes for the
    -- base of the SBTs.
    shaderGroupBaseAlignment :: Word32
  , -- | @maxGeometryCount@ is the maximum number of geometries in the bottom
    -- level acceleration structure.
    maxGeometryCount :: Word64
  , -- | @maxInstanceCount@ is the maximum number of instances in the top level
    -- acceleration structure.
    maxInstanceCount :: Word64
  , -- | @maxTriangleCount@ is the maximum number of triangles in all geometries
    -- in the bottom level acceleration structure.
    maxTriangleCount :: Word64
  , -- | @maxDescriptorSetAccelerationStructures@ is the maximum number of
    -- acceleration structure descriptors that are allowed in a descriptor set.
    maxDescriptorSetAccelerationStructures :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingPropertiesNV)
#endif
deriving instance Show PhysicalDeviceRayTracingPropertiesNV

instance ToCStruct PhysicalDeviceRayTracingPropertiesNV where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (shaderGroupHandleSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxRecursionDepth)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxShaderGroupStride)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (shaderGroupBaseAlignment)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (maxGeometryCount)
    poke ((p `plusPtr` 40 :: Ptr Word64)) (maxInstanceCount)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (maxTriangleCount)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (maxDescriptorSetAccelerationStructures)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceRayTracingPropertiesNV where
  peekCStruct p = do
    shaderGroupHandleSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxRecursionDepth <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxShaderGroupStride <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    shaderGroupBaseAlignment <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    maxGeometryCount <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    maxInstanceCount <- peek @Word64 ((p `plusPtr` 40 :: Ptr Word64))
    maxTriangleCount <- peek @Word64 ((p `plusPtr` 48 :: Ptr Word64))
    maxDescriptorSetAccelerationStructures <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    pure $ PhysicalDeviceRayTracingPropertiesNV
             shaderGroupHandleSize maxRecursionDepth maxShaderGroupStride shaderGroupBaseAlignment maxGeometryCount maxInstanceCount maxTriangleCount maxDescriptorSetAccelerationStructures

instance Storable PhysicalDeviceRayTracingPropertiesNV where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingPropertiesNV where
  zero = PhysicalDeviceRayTracingPropertiesNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- No documentation found for TopLevel "VkGeometryFlagsNV"
type GeometryFlagsNV = GeometryFlagsKHR


-- No documentation found for TopLevel "VkGeometryInstanceFlagsNV"
type GeometryInstanceFlagsNV = GeometryInstanceFlagsKHR


-- No documentation found for TopLevel "VkBuildAccelerationStructureFlagsNV"
type BuildAccelerationStructureFlagsNV = BuildAccelerationStructureFlagsKHR


-- No documentation found for TopLevel "VkAccelerationStructureNV"
type AccelerationStructureNV = AccelerationStructureKHR


-- No documentation found for TopLevel "VkGeometryFlagBitsNV"
type GeometryFlagBitsNV = GeometryFlagBitsKHR


-- No documentation found for TopLevel "VkGeometryInstanceFlagBitsNV"
type GeometryInstanceFlagBitsNV = GeometryInstanceFlagBitsKHR


-- No documentation found for TopLevel "VkBuildAccelerationStructureFlagBitsNV"
type BuildAccelerationStructureFlagBitsNV = BuildAccelerationStructureFlagBitsKHR


-- No documentation found for TopLevel "VkCopyAccelerationStructureModeNV"
type CopyAccelerationStructureModeNV = CopyAccelerationStructureModeKHR


-- No documentation found for TopLevel "VkAccelerationStructureTypeNV"
type AccelerationStructureTypeNV = AccelerationStructureTypeKHR


-- No documentation found for TopLevel "VkGeometryTypeNV"
type GeometryTypeNV = GeometryTypeKHR


-- No documentation found for TopLevel "VkRayTracingShaderGroupTypeNV"
type RayTracingShaderGroupTypeNV = RayTracingShaderGroupTypeKHR


-- No documentation found for TopLevel "VkAccelerationStructureMemoryRequirementsTypeNV"
type AccelerationStructureMemoryRequirementsTypeNV = AccelerationStructureMemoryRequirementsTypeKHR


-- No documentation found for TopLevel "VkBindAccelerationStructureMemoryInfoNV"
type BindAccelerationStructureMemoryInfoNV = BindAccelerationStructureMemoryInfoKHR


-- No documentation found for TopLevel "VkWriteDescriptorSetAccelerationStructureNV"
type WriteDescriptorSetAccelerationStructureNV = WriteDescriptorSetAccelerationStructureKHR


-- No documentation found for TopLevel "VkAabbPositionsNV"
type AabbPositionsNV = AabbPositionsKHR


-- No documentation found for TopLevel "VkTransformMatrixNV"
type TransformMatrixNV = TransformMatrixKHR


-- No documentation found for TopLevel "VkAccelerationStructureInstanceNV"
type AccelerationStructureInstanceNV = AccelerationStructureInstanceKHR


type NV_RAY_TRACING_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_SPEC_VERSION"
pattern NV_RAY_TRACING_SPEC_VERSION :: forall a . Integral a => a
pattern NV_RAY_TRACING_SPEC_VERSION = 3


type NV_RAY_TRACING_EXTENSION_NAME = "VK_NV_ray_tracing"

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_EXTENSION_NAME"
pattern NV_RAY_TRACING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_RAY_TRACING_EXTENSION_NAME = "VK_NV_ray_tracing"

