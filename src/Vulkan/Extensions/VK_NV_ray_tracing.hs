{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_ray_tracing"
module Vulkan.Extensions.VK_NV_ray_tracing  ( compileDeferredNV
                                            , createAccelerationStructureNV
                                            , withAccelerationStructureNV
                                            , destroyAccelerationStructureNV
                                            , getAccelerationStructureMemoryRequirementsNV
                                            , bindAccelerationStructureMemoryNV
                                            , cmdCopyAccelerationStructureNV
                                            , cmdWriteAccelerationStructuresPropertiesNV
                                            , cmdBuildAccelerationStructureNV
                                            , cmdTraceRaysNV
                                            , getAccelerationStructureHandleNV
                                            , createRayTracingPipelinesNV
                                            , withRayTracingPipelinesNV
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
                                            , pattern ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV
                                            , pattern ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV
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
                                            , pattern SHADER_UNUSED_NV
                                            , getRayTracingShaderGroupHandlesNV
                                            , RayTracingShaderGroupCreateInfoNV(..)
                                            , RayTracingPipelineCreateInfoNV(..)
                                            , GeometryTrianglesNV(..)
                                            , GeometryAABBNV(..)
                                            , GeometryDataNV(..)
                                            , GeometryNV(..)
                                            , AccelerationStructureInfoNV(..)
                                            , AccelerationStructureCreateInfoNV(..)
                                            , BindAccelerationStructureMemoryInfoNV(..)
                                            , WriteDescriptorSetAccelerationStructureNV(..)
                                            , AccelerationStructureMemoryRequirementsInfoNV(..)
                                            , PhysicalDeviceRayTracingPropertiesNV(..)
                                            , AccelerationStructureMemoryRequirementsTypeNV( ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV
                                                                                           , ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV
                                                                                           , ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV
                                                                                           , ..
                                                                                           )
                                            , GeometryFlagsNV
                                            , GeometryInstanceFlagsNV
                                            , BuildAccelerationStructureFlagsNV
                                            , GeometryFlagBitsNV
                                            , GeometryInstanceFlagBitsNV
                                            , BuildAccelerationStructureFlagBitsNV
                                            , CopyAccelerationStructureModeNV
                                            , AccelerationStructureTypeNV
                                            , GeometryTypeNV
                                            , RayTracingShaderGroupTypeNV
                                            , AabbPositionsNV
                                            , TransformMatrixNV
                                            , AccelerationStructureInstanceNV
                                            , NV_RAY_TRACING_SPEC_VERSION
                                            , pattern NV_RAY_TRACING_SPEC_VERSION
                                            , NV_RAY_TRACING_EXTENSION_NAME
                                            , pattern NV_RAY_TRACING_EXTENSION_NAME
                                            , AccelerationStructureNV(..)
                                            , AabbPositionsKHR(..)
                                            , TransformMatrixKHR(..)
                                            , AccelerationStructureInstanceKHR(..)
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
                                            , RayTracingShaderGroupTypeKHR(..)
                                            , MemoryRequirements2KHR
                                            , SHADER_UNUSED_KHR
                                            , pattern SHADER_UNUSED_KHR
                                            ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
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
import GHC.Show (showsPrec)
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.Pipeline (destroyPipeline)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (getRayTracingShaderGroupHandlesKHR)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AabbPositionsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureInstanceKHR)
import Vulkan.Extensions.Handles (AccelerationStructureNV)
import Vulkan.Extensions.Handles (AccelerationStructureNV(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureTypeKHR)
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureModeKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureModeKHR(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkBindAccelerationStructureMemoryNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBuildAccelerationStructureNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyAccelerationStructureNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdTraceRaysNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteAccelerationStructuresPropertiesNV))
import Vulkan.Dynamic (DeviceCmds(pVkCompileDeferredNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateAccelerationStructureNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateRayTracingPipelinesNV))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyAccelerationStructureNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureHandleNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureMemoryRequirementsNV))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryTypeKHR)
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
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.QueryType (QueryType)
import Vulkan.Core10.Enums.QueryType (QueryType(..))
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingShaderGroupTypeKHR)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (TransformMatrixKHR)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureTypeKHR(ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureTypeKHR(ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR))
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlagBits(BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureModeKHR(COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureModeKHR(COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagBitsKHR(GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagBitsKHR(GEOMETRY_OPAQUE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryTypeKHR(GEOMETRY_TYPE_AABBS_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryTypeKHR(GEOMETRY_TYPE_TRIANGLES_KHR))
import Vulkan.Core10.Enums.IndexType (IndexType(INDEX_TYPE_NONE_KHR))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(PIPELINE_BIND_POINT_RAY_TRACING_KHR))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingShaderGroupTypeKHR(RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingShaderGroupTypeKHR(RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingShaderGroupTypeKHR(RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR))
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GEOMETRY_AABB_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GEOMETRY_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (getRayTracingShaderGroupHandlesKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (AabbPositionsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureInstanceKHR(..))
import Vulkan.Extensions.Handles (AccelerationStructureNV(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureTypeKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureModeKHR(..))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryTypeKHR(..))
import Vulkan.Extensions.VK_KHR_get_memory_requirements2 (MemoryRequirements2KHR)
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingShaderGroupTypeKHR(..))
import Vulkan.Core10.APIConstants (SHADER_UNUSED_KHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (TransformMatrixKHR(..))
import Vulkan.Core10.APIConstants (pattern SHADER_UNUSED_KHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCompileDeferredNV
  :: FunPtr (Ptr Device_T -> Pipeline -> Word32 -> IO Result) -> Ptr Device_T -> Pipeline -> Word32 -> IO Result

-- No documentation found for TopLevel "vkCompileDeferredNV"
compileDeferredNV :: forall io
                   . (MonadIO io)
                  => -- No documentation found for Nested "vkCompileDeferredNV" "device"
                     Device
                  -> -- No documentation found for Nested "vkCompileDeferredNV" "pipeline"
                     Pipeline
                  -> -- No documentation found for Nested "vkCompileDeferredNV" "shader"
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

-- No documentation found for TopLevel "vkCreateAccelerationStructureNV"
createAccelerationStructureNV :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkCreateAccelerationStructureNV" "device"
                                 Device
                              -> -- No documentation found for Nested "vkCreateAccelerationStructureNV" "pCreateInfo"
                                 AccelerationStructureCreateInfoNV
                              -> -- No documentation found for Nested "vkCreateAccelerationStructureNV" "pAllocator"
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

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createAccelerationStructureNV' and 'destroyAccelerationStructureNV'
--
-- To ensure that 'destroyAccelerationStructureNV' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withAccelerationStructureNV :: forall io r . MonadIO io => Device -> AccelerationStructureCreateInfoNV -> Maybe AllocationCallbacks -> (io AccelerationStructureNV -> (AccelerationStructureNV -> io ()) -> r) -> r
withAccelerationStructureNV device pCreateInfo pAllocator b =
  b (createAccelerationStructureNV device pCreateInfo pAllocator)
    (\(o0) -> destroyAccelerationStructureNV device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyAccelerationStructureNV
  :: FunPtr (Ptr Device_T -> AccelerationStructureNV -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> AccelerationStructureNV -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyAccelerationStructureNV"
destroyAccelerationStructureNV :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "vkDestroyAccelerationStructureNV" "device"
                                  Device
                               -> -- No documentation found for Nested "vkDestroyAccelerationStructureNV" "accelerationStructure"
                                  AccelerationStructureNV
                               -> -- No documentation found for Nested "vkDestroyAccelerationStructureNV" "pAllocator"
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io ()
destroyAccelerationStructureNV device accelerationStructure allocator = liftIO . evalContT $ do
  let vkDestroyAccelerationStructureNVPtr = pVkDestroyAccelerationStructureNV (deviceCmds (device :: Device))
  lift $ unless (vkDestroyAccelerationStructureNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyAccelerationStructureNV is null" Nothing Nothing
  let vkDestroyAccelerationStructureNV' = mkVkDestroyAccelerationStructureNV vkDestroyAccelerationStructureNVPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyAccelerationStructureNV' (deviceHandle (device)) (accelerationStructure) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureMemoryRequirementsNV
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureMemoryRequirementsInfoNV -> Ptr (SomeStruct MemoryRequirements2KHR) -> IO ()) -> Ptr Device_T -> Ptr AccelerationStructureMemoryRequirementsInfoNV -> Ptr (SomeStruct MemoryRequirements2KHR) -> IO ()

-- No documentation found for TopLevel "vkGetAccelerationStructureMemoryRequirementsNV"
getAccelerationStructureMemoryRequirementsNV :: forall a io
                                              . (Extendss MemoryRequirements2KHR a, PokeChain a, PeekChain a, MonadIO io)
                                             => -- No documentation found for Nested "vkGetAccelerationStructureMemoryRequirementsNV" "device"
                                                Device
                                             -> -- No documentation found for Nested "vkGetAccelerationStructureMemoryRequirementsNV" "pInfo"
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
  "dynamic" mkVkBindAccelerationStructureMemoryNV
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr BindAccelerationStructureMemoryInfoNV -> IO Result) -> Ptr Device_T -> Word32 -> Ptr BindAccelerationStructureMemoryInfoNV -> IO Result

-- No documentation found for TopLevel "vkBindAccelerationStructureMemoryNV"
bindAccelerationStructureMemoryNV :: forall io
                                   . (MonadIO io)
                                  => -- No documentation found for Nested "vkBindAccelerationStructureMemoryNV" "device"
                                     Device
                                  -> -- No documentation found for Nested "vkBindAccelerationStructureMemoryNV" "pBindInfos"
                                     ("bindInfos" ::: Vector BindAccelerationStructureMemoryInfoNV)
                                  -> io ()
bindAccelerationStructureMemoryNV device bindInfos = liftIO . evalContT $ do
  let vkBindAccelerationStructureMemoryNVPtr = pVkBindAccelerationStructureMemoryNV (deviceCmds (device :: Device))
  lift $ unless (vkBindAccelerationStructureMemoryNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBindAccelerationStructureMemoryNV is null" Nothing Nothing
  let vkBindAccelerationStructureMemoryNV' = mkVkBindAccelerationStructureMemoryNV vkBindAccelerationStructureMemoryNVPtr
  pPBindInfos <- ContT $ allocaBytesAligned @BindAccelerationStructureMemoryInfoNV ((Data.Vector.length (bindInfos)) * 56) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPBindInfos `plusPtr` (56 * (i)) :: Ptr BindAccelerationStructureMemoryInfoNV) (e) . ($ ())) (bindInfos)
  r <- lift $ vkBindAccelerationStructureMemoryNV' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (bindInfos)) :: Word32)) (pPBindInfos)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyAccelerationStructureNV
  :: FunPtr (Ptr CommandBuffer_T -> AccelerationStructureNV -> AccelerationStructureNV -> CopyAccelerationStructureModeKHR -> IO ()) -> Ptr CommandBuffer_T -> AccelerationStructureNV -> AccelerationStructureNV -> CopyAccelerationStructureModeKHR -> IO ()

-- No documentation found for TopLevel "vkCmdCopyAccelerationStructureNV"
cmdCopyAccelerationStructureNV :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "vkCmdCopyAccelerationStructureNV" "commandBuffer"
                                  CommandBuffer
                               -> -- No documentation found for Nested "vkCmdCopyAccelerationStructureNV" "dst"
                                  ("dst" ::: AccelerationStructureNV)
                               -> -- No documentation found for Nested "vkCmdCopyAccelerationStructureNV" "src"
                                  ("src" ::: AccelerationStructureNV)
                               -> -- No documentation found for Nested "vkCmdCopyAccelerationStructureNV" "mode"
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
  "dynamic" mkVkCmdWriteAccelerationStructuresPropertiesNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureNV -> QueryType -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureNV -> QueryType -> QueryPool -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdWriteAccelerationStructuresPropertiesNV"
cmdWriteAccelerationStructuresPropertiesNV :: forall io
                                            . (MonadIO io)
                                           => -- No documentation found for Nested "vkCmdWriteAccelerationStructuresPropertiesNV" "commandBuffer"
                                              CommandBuffer
                                           -> -- No documentation found for Nested "vkCmdWriteAccelerationStructuresPropertiesNV" "pAccelerationStructures"
                                              ("accelerationStructures" ::: Vector AccelerationStructureNV)
                                           -> -- No documentation found for Nested "vkCmdWriteAccelerationStructuresPropertiesNV" "queryType"
                                              QueryType
                                           -> -- No documentation found for Nested "vkCmdWriteAccelerationStructuresPropertiesNV" "queryPool"
                                              QueryPool
                                           -> -- No documentation found for Nested "vkCmdWriteAccelerationStructuresPropertiesNV" "firstQuery"
                                              ("firstQuery" ::: Word32)
                                           -> io ()
cmdWriteAccelerationStructuresPropertiesNV commandBuffer accelerationStructures queryType queryPool firstQuery = liftIO . evalContT $ do
  let vkCmdWriteAccelerationStructuresPropertiesNVPtr = pVkCmdWriteAccelerationStructuresPropertiesNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdWriteAccelerationStructuresPropertiesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteAccelerationStructuresPropertiesNV is null" Nothing Nothing
  let vkCmdWriteAccelerationStructuresPropertiesNV' = mkVkCmdWriteAccelerationStructuresPropertiesNV vkCmdWriteAccelerationStructuresPropertiesNVPtr
  pPAccelerationStructures <- ContT $ allocaBytesAligned @AccelerationStructureNV ((Data.Vector.length (accelerationStructures)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures `plusPtr` (8 * (i)) :: Ptr AccelerationStructureNV) (e)) (accelerationStructures)
  lift $ vkCmdWriteAccelerationStructuresPropertiesNV' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32)) (pPAccelerationStructures) (queryType) (queryPool) (firstQuery)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildAccelerationStructureNV
  :: FunPtr (Ptr CommandBuffer_T -> Ptr AccelerationStructureInfoNV -> Buffer -> DeviceSize -> Bool32 -> AccelerationStructureNV -> AccelerationStructureNV -> Buffer -> DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Ptr AccelerationStructureInfoNV -> Buffer -> DeviceSize -> Bool32 -> AccelerationStructureNV -> AccelerationStructureNV -> Buffer -> DeviceSize -> IO ()

-- No documentation found for TopLevel "vkCmdBuildAccelerationStructureNV"
cmdBuildAccelerationStructureNV :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkCmdBuildAccelerationStructureNV" "commandBuffer"
                                   CommandBuffer
                                -> -- No documentation found for Nested "vkCmdBuildAccelerationStructureNV" "pInfo"
                                   AccelerationStructureInfoNV
                                -> -- No documentation found for Nested "vkCmdBuildAccelerationStructureNV" "instanceData"
                                   ("instanceData" ::: Buffer)
                                -> -- No documentation found for Nested "vkCmdBuildAccelerationStructureNV" "instanceOffset"
                                   ("instanceOffset" ::: DeviceSize)
                                -> -- No documentation found for Nested "vkCmdBuildAccelerationStructureNV" "update"
                                   ("update" ::: Bool)
                                -> -- No documentation found for Nested "vkCmdBuildAccelerationStructureNV" "dst"
                                   ("dst" ::: AccelerationStructureNV)
                                -> -- No documentation found for Nested "vkCmdBuildAccelerationStructureNV" "src"
                                   ("src" ::: AccelerationStructureNV)
                                -> -- No documentation found for Nested "vkCmdBuildAccelerationStructureNV" "scratch"
                                   ("scratch" ::: Buffer)
                                -> -- No documentation found for Nested "vkCmdBuildAccelerationStructureNV" "scratchOffset"
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

-- No documentation found for TopLevel "vkCmdTraceRaysNV"
cmdTraceRaysNV :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vkCmdTraceRaysNV" "commandBuffer"
                  CommandBuffer
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "raygenShaderBindingTableBuffer"
                  ("raygenShaderBindingTableBuffer" ::: Buffer)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "raygenShaderBindingOffset"
                  ("raygenShaderBindingOffset" ::: DeviceSize)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "missShaderBindingTableBuffer"
                  ("missShaderBindingTableBuffer" ::: Buffer)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "missShaderBindingOffset"
                  ("missShaderBindingOffset" ::: DeviceSize)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "missShaderBindingStride"
                  ("missShaderBindingStride" ::: DeviceSize)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "hitShaderBindingTableBuffer"
                  ("hitShaderBindingTableBuffer" ::: Buffer)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "hitShaderBindingOffset"
                  ("hitShaderBindingOffset" ::: DeviceSize)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "hitShaderBindingStride"
                  ("hitShaderBindingStride" ::: DeviceSize)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "callableShaderBindingTableBuffer"
                  ("callableShaderBindingTableBuffer" ::: Buffer)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "callableShaderBindingOffset"
                  ("callableShaderBindingOffset" ::: DeviceSize)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "callableShaderBindingStride"
                  ("callableShaderBindingStride" ::: DeviceSize)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "width"
                  ("width" ::: Word32)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "height"
                  ("height" ::: Word32)
               -> -- No documentation found for Nested "vkCmdTraceRaysNV" "depth"
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
  :: FunPtr (Ptr Device_T -> AccelerationStructureNV -> CSize -> Ptr () -> IO Result) -> Ptr Device_T -> AccelerationStructureNV -> CSize -> Ptr () -> IO Result

-- No documentation found for TopLevel "vkGetAccelerationStructureHandleNV"
getAccelerationStructureHandleNV :: forall io
                                  . (MonadIO io)
                                 => -- No documentation found for Nested "vkGetAccelerationStructureHandleNV" "device"
                                    Device
                                 -> -- No documentation found for Nested "vkGetAccelerationStructureHandleNV" "accelerationStructure"
                                    AccelerationStructureNV
                                 -> -- No documentation found for Nested "vkGetAccelerationStructureHandleNV" "dataSize"
                                    ("dataSize" ::: Word64)
                                 -> -- No documentation found for Nested "vkGetAccelerationStructureHandleNV" "pData"
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

-- No documentation found for TopLevel "vkCreateRayTracingPipelinesNV"
createRayTracingPipelinesNV :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "vkCreateRayTracingPipelinesNV" "device"
                               Device
                            -> -- No documentation found for Nested "vkCreateRayTracingPipelinesNV" "pipelineCache"
                               PipelineCache
                            -> -- No documentation found for Nested "vkCreateRayTracingPipelinesNV" "pCreateInfos"
                               ("createInfos" ::: Vector (SomeStruct RayTracingPipelineCreateInfoNV))
                            -> -- No documentation found for Nested "vkCreateRayTracingPipelinesNV" "pAllocator"
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

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createRayTracingPipelinesNV' and 'destroyPipeline'
--
-- To ensure that 'destroyPipeline' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withRayTracingPipelinesNV :: forall io r . MonadIO io => Device -> PipelineCache -> Vector (SomeStruct RayTracingPipelineCreateInfoNV) -> Maybe AllocationCallbacks -> (io (Result, Vector Pipeline) -> ((Result, Vector Pipeline) -> io ()) -> r) -> r
withRayTracingPipelinesNV device pipelineCache pCreateInfos pAllocator b =
  b (createRayTracingPipelinesNV device pipelineCache pCreateInfos pAllocator)
    (\(_, o1) -> traverse_ (\o1Elem -> destroyPipeline device o1Elem pAllocator) o1)


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
pattern BUFFER_USAGE_RAY_TRACING_BIT_NV = BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR


-- No documentation found for TopLevel "VK_PIPELINE_BIND_POINT_RAY_TRACING_NV"
pattern PIPELINE_BIND_POINT_RAY_TRACING_NV = PIPELINE_BIND_POINT_RAY_TRACING_KHR


-- No documentation found for TopLevel "VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV"
pattern ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV = ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR


-- No documentation found for TopLevel "VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV"
pattern ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV = ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR


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


-- No documentation found for TopLevel "VK_SHADER_UNUSED_NV"
pattern SHADER_UNUSED_NV = SHADER_UNUSED_KHR


-- No documentation found for TopLevel "vkGetRayTracingShaderGroupHandlesNV"
getRayTracingShaderGroupHandlesNV = getRayTracingShaderGroupHandlesKHR



-- No documentation found for TopLevel "VkRayTracingShaderGroupCreateInfoNV"
data RayTracingShaderGroupCreateInfoNV = RayTracingShaderGroupCreateInfoNV
  { -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoNV" "type"
    type' :: RayTracingShaderGroupTypeKHR
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoNV" "generalShader"
    generalShader :: Word32
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoNV" "closestHitShader"
    closestHitShader :: Word32
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoNV" "anyHitShader"
    anyHitShader :: Word32
  , -- No documentation found for Nested "VkRayTracingShaderGroupCreateInfoNV" "intersectionShader"
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



-- No documentation found for TopLevel "VkRayTracingPipelineCreateInfoNV"
data RayTracingPipelineCreateInfoNV (es :: [Type]) = RayTracingPipelineCreateInfoNV
  { -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "flags"
    flags :: PipelineCreateFlags
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "pStages"
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "pGroups"
    groups :: Vector RayTracingShaderGroupCreateInfoNV
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "maxRecursionDepth"
    maxRecursionDepth :: Word32
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "layout"
    layout :: PipelineLayout
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "basePipelineHandle"
    basePipelineHandle :: Pipeline
  , -- No documentation found for Nested "VkRayTracingPipelineCreateInfoNV" "basePipelineIndex"
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
    lift $ Data.Vector.imapM_ (\i e -> poke (pPGroups' `plusPtr` (40 * (i)) :: Ptr RayTracingShaderGroupCreateInfoNV) (e)) (groups)
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
    lift $ Data.Vector.imapM_ (\i e -> poke (pPGroups' `plusPtr` (40 * (i)) :: Ptr RayTracingShaderGroupCreateInfoNV) (e)) (mempty)
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



-- No documentation found for TopLevel "VkGeometryTrianglesNV"
data GeometryTrianglesNV = GeometryTrianglesNV
  { -- No documentation found for Nested "VkGeometryTrianglesNV" "vertexData"
    vertexData :: Buffer
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "vertexOffset"
    vertexOffset :: DeviceSize
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "vertexCount"
    vertexCount :: Word32
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "vertexStride"
    vertexStride :: DeviceSize
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "vertexFormat"
    vertexFormat :: Format
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "indexData"
    indexData :: Buffer
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "indexOffset"
    indexOffset :: DeviceSize
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "indexCount"
    indexCount :: Word32
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "indexType"
    indexType :: IndexType
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "transformData"
    transformData :: Buffer
  , -- No documentation found for Nested "VkGeometryTrianglesNV" "transformOffset"
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



-- No documentation found for TopLevel "VkGeometryAABBNV"
data GeometryAABBNV = GeometryAABBNV
  { -- No documentation found for Nested "VkGeometryAABBNV" "aabbData"
    aabbData :: Buffer
  , -- No documentation found for Nested "VkGeometryAABBNV" "numAABBs"
    numAABBs :: Word32
  , -- No documentation found for Nested "VkGeometryAABBNV" "stride"
    stride :: Word32
  , -- No documentation found for Nested "VkGeometryAABBNV" "offset"
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



-- No documentation found for TopLevel "VkGeometryDataNV"
data GeometryDataNV = GeometryDataNV
  { -- No documentation found for Nested "VkGeometryDataNV" "triangles"
    triangles :: GeometryTrianglesNV
  , -- No documentation found for Nested "VkGeometryDataNV" "aabbs"
    aabbs :: GeometryAABBNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeometryDataNV)
#endif
deriving instance Show GeometryDataNV

instance ToCStruct GeometryDataNV where
  withCStruct x f = allocaBytesAligned 136 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryDataNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr GeometryTrianglesNV)) (triangles)
    poke ((p `plusPtr` 96 :: Ptr GeometryAABBNV)) (aabbs)
    f
  cStructSize = 136
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr GeometryTrianglesNV)) (zero)
    poke ((p `plusPtr` 96 :: Ptr GeometryAABBNV)) (zero)
    f

instance FromCStruct GeometryDataNV where
  peekCStruct p = do
    triangles <- peekCStruct @GeometryTrianglesNV ((p `plusPtr` 0 :: Ptr GeometryTrianglesNV))
    aabbs <- peekCStruct @GeometryAABBNV ((p `plusPtr` 96 :: Ptr GeometryAABBNV))
    pure $ GeometryDataNV
             triangles aabbs


instance Storable GeometryDataNV where
  sizeOf ~_ = 136
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeometryDataNV where
  zero = GeometryDataNV
           zero
           zero



-- No documentation found for TopLevel "VkGeometryNV"
data GeometryNV = GeometryNV
  { -- No documentation found for Nested "VkGeometryNV" "geometryType"
    geometryType :: GeometryTypeKHR
  , -- No documentation found for Nested "VkGeometryNV" "geometry"
    geometry :: GeometryDataNV
  , -- No documentation found for Nested "VkGeometryNV" "flags"
    flags :: GeometryFlagsKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeometryNV)
#endif
deriving instance Show GeometryNV

instance ToCStruct GeometryNV where
  withCStruct x f = allocaBytesAligned 168 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (geometryType)
    poke ((p `plusPtr` 24 :: Ptr GeometryDataNV)) (geometry)
    poke ((p `plusPtr` 160 :: Ptr GeometryFlagsKHR)) (flags)
    f
  cStructSize = 168
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr GeometryDataNV)) (zero)
    f

instance FromCStruct GeometryNV where
  peekCStruct p = do
    geometryType <- peek @GeometryTypeKHR ((p `plusPtr` 16 :: Ptr GeometryTypeKHR))
    geometry <- peekCStruct @GeometryDataNV ((p `plusPtr` 24 :: Ptr GeometryDataNV))
    flags <- peek @GeometryFlagsKHR ((p `plusPtr` 160 :: Ptr GeometryFlagsKHR))
    pure $ GeometryNV
             geometryType geometry flags


instance Storable GeometryNV where
  sizeOf ~_ = 168
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeometryNV where
  zero = GeometryNV
           zero
           zero
           zero



-- No documentation found for TopLevel "VkAccelerationStructureInfoNV"
data AccelerationStructureInfoNV = AccelerationStructureInfoNV
  { -- No documentation found for Nested "VkAccelerationStructureInfoNV" "type"
    type' :: AccelerationStructureTypeNV
  , -- No documentation found for Nested "VkAccelerationStructureInfoNV" "flags"
    flags :: BuildAccelerationStructureFlagsNV
  , -- No documentation found for Nested "VkAccelerationStructureInfoNV" "instanceCount"
    instanceCount :: Word32
  , -- No documentation found for Nested "VkAccelerationStructureInfoNV" "pGeometries"
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
    lift $ Data.Vector.imapM_ (\i e -> poke (pPGeometries' `plusPtr` (168 * (i)) :: Ptr GeometryNV) (e)) (geometries)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr GeometryNV))) (pPGeometries')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeNV)) (zero)
    pPGeometries' <- ContT $ allocaBytesAligned @GeometryNV ((Data.Vector.length (mempty)) * 168) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPGeometries' `plusPtr` (168 * (i)) :: Ptr GeometryNV) (e)) (mempty)
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



-- No documentation found for TopLevel "VkAccelerationStructureCreateInfoNV"
data AccelerationStructureCreateInfoNV = AccelerationStructureCreateInfoNV
  { -- No documentation found for Nested "VkAccelerationStructureCreateInfoNV" "compactedSize"
    compactedSize :: DeviceSize
  , -- No documentation found for Nested "VkAccelerationStructureCreateInfoNV" "info"
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



-- No documentation found for TopLevel "VkBindAccelerationStructureMemoryInfoNV"
data BindAccelerationStructureMemoryInfoNV = BindAccelerationStructureMemoryInfoNV
  { -- No documentation found for Nested "VkBindAccelerationStructureMemoryInfoNV" "accelerationStructure"
    accelerationStructure :: AccelerationStructureNV
  , -- No documentation found for Nested "VkBindAccelerationStructureMemoryInfoNV" "memory"
    memory :: DeviceMemory
  , -- No documentation found for Nested "VkBindAccelerationStructureMemoryInfoNV" "memoryOffset"
    memoryOffset :: DeviceSize
  , -- No documentation found for Nested "VkBindAccelerationStructureMemoryInfoNV" "pDeviceIndices"
    deviceIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindAccelerationStructureMemoryInfoNV)
#endif
deriving instance Show BindAccelerationStructureMemoryInfoNV

instance ToCStruct BindAccelerationStructureMemoryInfoNV where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindAccelerationStructureMemoryInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureNV)) (accelerationStructure)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (memory)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (memoryOffset)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceIndices)) :: Word32))
    pPDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (deviceIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (deviceIndices)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPDeviceIndices')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureNV)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    pPDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPDeviceIndices')
    lift $ f

instance FromCStruct BindAccelerationStructureMemoryInfoNV where
  peekCStruct p = do
    accelerationStructure <- peek @AccelerationStructureNV ((p `plusPtr` 16 :: Ptr AccelerationStructureNV))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    deviceIndexCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pDeviceIndices <- peek @(Ptr Word32) ((p `plusPtr` 48 :: Ptr (Ptr Word32)))
    pDeviceIndices' <- generateM (fromIntegral deviceIndexCount) (\i -> peek @Word32 ((pDeviceIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ BindAccelerationStructureMemoryInfoNV
             accelerationStructure memory memoryOffset pDeviceIndices'

instance Zero BindAccelerationStructureMemoryInfoNV where
  zero = BindAccelerationStructureMemoryInfoNV
           zero
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkWriteDescriptorSetAccelerationStructureNV"
data WriteDescriptorSetAccelerationStructureNV = WriteDescriptorSetAccelerationStructureNV
  { -- No documentation found for Nested "VkWriteDescriptorSetAccelerationStructureNV" "pAccelerationStructures"
    accelerationStructures :: Vector AccelerationStructureNV }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WriteDescriptorSetAccelerationStructureNV)
#endif
deriving instance Show WriteDescriptorSetAccelerationStructureNV

instance ToCStruct WriteDescriptorSetAccelerationStructureNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteDescriptorSetAccelerationStructureNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32))
    pPAccelerationStructures' <- ContT $ allocaBytesAligned @AccelerationStructureNV ((Data.Vector.length (accelerationStructures)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures' `plusPtr` (8 * (i)) :: Ptr AccelerationStructureNV) (e)) (accelerationStructures)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureNV))) (pPAccelerationStructures')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPAccelerationStructures' <- ContT $ allocaBytesAligned @AccelerationStructureNV ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures' `plusPtr` (8 * (i)) :: Ptr AccelerationStructureNV) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureNV))) (pPAccelerationStructures')
    lift $ f

instance FromCStruct WriteDescriptorSetAccelerationStructureNV where
  peekCStruct p = do
    accelerationStructureCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAccelerationStructures <- peek @(Ptr AccelerationStructureNV) ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureNV)))
    pAccelerationStructures' <- generateM (fromIntegral accelerationStructureCount) (\i -> peek @AccelerationStructureNV ((pAccelerationStructures `advancePtrBytes` (8 * (i)) :: Ptr AccelerationStructureNV)))
    pure $ WriteDescriptorSetAccelerationStructureNV
             pAccelerationStructures'

instance Zero WriteDescriptorSetAccelerationStructureNV where
  zero = WriteDescriptorSetAccelerationStructureNV
           mempty



-- No documentation found for TopLevel "VkAccelerationStructureMemoryRequirementsInfoNV"
data AccelerationStructureMemoryRequirementsInfoNV = AccelerationStructureMemoryRequirementsInfoNV
  { -- No documentation found for Nested "VkAccelerationStructureMemoryRequirementsInfoNV" "type"
    type' :: AccelerationStructureMemoryRequirementsTypeNV
  , -- No documentation found for Nested "VkAccelerationStructureMemoryRequirementsInfoNV" "accelerationStructure"
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



-- No documentation found for TopLevel "VkPhysicalDeviceRayTracingPropertiesNV"
data PhysicalDeviceRayTracingPropertiesNV = PhysicalDeviceRayTracingPropertiesNV
  { -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "shaderGroupHandleSize"
    shaderGroupHandleSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "maxRecursionDepth"
    maxRecursionDepth :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "maxShaderGroupStride"
    maxShaderGroupStride :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "shaderGroupBaseAlignment"
    shaderGroupBaseAlignment :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "maxGeometryCount"
    maxGeometryCount :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "maxInstanceCount"
    maxInstanceCount :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "maxTriangleCount"
    maxTriangleCount :: Word64
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPropertiesNV" "maxDescriptorSetAccelerationStructures"
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


-- No documentation found for TopLevel "VkAccelerationStructureMemoryRequirementsTypeNV"
newtype AccelerationStructureMemoryRequirementsTypeNV = AccelerationStructureMemoryRequirementsTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkAccelerationStructureMemoryRequirementsTypeNV" "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV"
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV = AccelerationStructureMemoryRequirementsTypeNV 0
-- No documentation found for Nested "VkAccelerationStructureMemoryRequirementsTypeNV" "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV"
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV =
  AccelerationStructureMemoryRequirementsTypeNV 1
-- No documentation found for Nested "VkAccelerationStructureMemoryRequirementsTypeNV" "VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV"
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV =
  AccelerationStructureMemoryRequirementsTypeNV 2
{-# complete ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV,
             ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV,
             ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV :: AccelerationStructureMemoryRequirementsTypeNV #-}

conNameAccelerationStructureMemoryRequirementsTypeNV :: String
conNameAccelerationStructureMemoryRequirementsTypeNV = "AccelerationStructureMemoryRequirementsTypeNV"

enumPrefixAccelerationStructureMemoryRequirementsTypeNV :: String
enumPrefixAccelerationStructureMemoryRequirementsTypeNV = "ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_"

showTableAccelerationStructureMemoryRequirementsTypeNV :: [(AccelerationStructureMemoryRequirementsTypeNV, String)]
showTableAccelerationStructureMemoryRequirementsTypeNV =
  [ (ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV        , "OBJECT_NV")
  , (ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV , "BUILD_SCRATCH_NV")
  , (ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV, "UPDATE_SCRATCH_NV")
  ]


instance Show AccelerationStructureMemoryRequirementsTypeNV where
showsPrec = enumShowsPrec enumPrefixAccelerationStructureMemoryRequirementsTypeNV
                          showTableAccelerationStructureMemoryRequirementsTypeNV
                          conNameAccelerationStructureMemoryRequirementsTypeNV
                          (\(AccelerationStructureMemoryRequirementsTypeNV x) -> x)
                          (showsPrec 11)


instance Read AccelerationStructureMemoryRequirementsTypeNV where
  readPrec = enumReadPrec enumPrefixAccelerationStructureMemoryRequirementsTypeNV
                          showTableAccelerationStructureMemoryRequirementsTypeNV
                          conNameAccelerationStructureMemoryRequirementsTypeNV
                          AccelerationStructureMemoryRequirementsTypeNV


-- No documentation found for TopLevel "VkGeometryFlagsNV"
type GeometryFlagsNV = GeometryFlagsKHR


-- No documentation found for TopLevel "VkGeometryInstanceFlagsNV"
type GeometryInstanceFlagsNV = GeometryInstanceFlagsKHR


-- No documentation found for TopLevel "VkBuildAccelerationStructureFlagsNV"
type BuildAccelerationStructureFlagsNV = BuildAccelerationStructureFlagsKHR


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

