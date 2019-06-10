{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_NV_ray_tracing
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  AccelerationStructureCreateInfoNV(..)
  , 
  AccelerationStructureInfoNV(..)
  , AccelerationStructureMemoryRequirementsInfoNV(..)
#endif
  , AccelerationStructureMemoryRequirementsTypeNV
  , pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV
  , pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV
  , pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV
  , AccelerationStructureNV
  , AccelerationStructureTypeNV
  , pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV
  , pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV
#if defined(VK_USE_PLATFORM_GGP)
  , BindAccelerationStructureMemoryInfoNV(..)
#endif
  , BuildAccelerationStructureFlagBitsNV
  , pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV
  , pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV
  , pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV
  , pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV
  , pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV
  , BuildAccelerationStructureFlagsNV
  , CopyAccelerationStructureModeNV
  , pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV
  , pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV
#if defined(VK_USE_PLATFORM_GGP)
  , GeometryAABBNV(..)
#endif
  , GeometryDataNV(..)
  , GeometryFlagBitsNV
  , pattern GEOMETRY_OPAQUE_BIT_NV
  , pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV
  , GeometryFlagsNV
  , GeometryInstanceFlagBitsNV
  , pattern GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV
  , pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV
  , pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV
  , pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV
  , GeometryInstanceFlagsNV
#if defined(VK_USE_PLATFORM_GGP)
  , GeometryNV(..)
  , GeometryTrianglesNV(..)
#endif
  , GeometryTypeNV
  , pattern GEOMETRY_TYPE_TRIANGLES_NV
  , pattern GEOMETRY_TYPE_AABBS_NV
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDeviceRayTracingPropertiesNV(..)
  , RayTracingPipelineCreateInfoNV(..)
  , RayTracingShaderGroupCreateInfoNV(..)
#endif
  , RayTracingShaderGroupTypeNV
  , pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV
  , pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV
  , pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV
#if defined(VK_USE_PLATFORM_GGP)
  , WriteDescriptorSetAccelerationStructureNV(..)
#endif
  , bindAccelerationStructureMemoryNV
  , cmdBuildAccelerationStructureNV
  , cmdCopyAccelerationStructureNV
  , cmdTraceRaysNV
  , cmdWriteAccelerationStructuresPropertiesNV
  , compileDeferredNV
  , createAccelerationStructureNV
  , createRayTracingPipelinesNV
  , destroyAccelerationStructureNV
  , getAccelerationStructureHandleNV
  , getAccelerationStructureMemoryRequirementsNV
  , getRayTracingShaderGroupHandlesNV
  , withAccelerationStructureNV
  , pattern NV_RAY_TRACING_EXTENSION_NAME
  , pattern NV_RAY_TRACING_SPEC_VERSION
  , MemoryRequirements2KHR
  , pattern STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_GEOMETRY_NV
  , pattern STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV
  , pattern STRUCTURE_TYPE_GEOMETRY_AABB_NV
  , pattern STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV
  , pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV
  , pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV
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
  , pattern PIPELINE_CREATE_DEFER_COMPILE_BIT_NV
  , pattern OBJECT_TYPE_ACCELERATION_STRUCTURE_NV
  , pattern DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT
  , pattern INDEX_TYPE_NONE_NV
  ) where

import Control.Exception
  ( bracket
  )
import Data.ByteString
  ( ByteString
  , packCStringLen
  )
import Data.Function
  ( (&)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Int
  ( Int32
  )
#endif
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Data.Word
  ( Word32
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word64
  )
#endif
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( VkAccelerationStructureMemoryRequirementsTypeNV(..)
  , VkAccelerationStructureTypeNV(..)
  , VkBuildAccelerationStructureFlagBitsNV(..)
  , VkCopyAccelerationStructureModeNV(..)
  , VkGeometryFlagBitsNV(..)
  , VkGeometryInstanceFlagBitsNV(..)
  , VkGeometryTypeNV(..)
  , VkRayTracingShaderGroupTypeNV(..)
  , VkAccelerationStructureNV
  , vkBindAccelerationStructureMemoryNV
  , vkCmdBuildAccelerationStructureNV
  , vkCmdCopyAccelerationStructureNV
  , vkCmdTraceRaysNV
  , vkCmdWriteAccelerationStructuresPropertiesNV
  , vkCompileDeferredNV
  , vkCreateAccelerationStructureNV
  , vkCreateRayTracingPipelinesNV
  , vkDestroyAccelerationStructureNV
  , vkGetAccelerationStructureHandleNV
  , vkGetAccelerationStructureMemoryRequirementsNV
  , vkGetRayTracingShaderGroupHandlesNV
  , pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV
  , pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV
  , pattern VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV
  , pattern VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV
  , pattern VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV
  , pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV
  , pattern VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV
  , pattern VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV
  , pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV
  , pattern VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV
  , pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV
  , pattern VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV
  , pattern VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV
  , pattern VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV
  , pattern VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV
  , pattern VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV
  , pattern VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV
  , pattern VK_GEOMETRY_OPAQUE_BIT_NV
  , pattern VK_GEOMETRY_TYPE_AABBS_NV
  , pattern VK_GEOMETRY_TYPE_TRIANGLES_NV
  , pattern VK_NV_RAY_TRACING_EXTENSION_NAME
  , pattern VK_NV_RAY_TRACING_SPEC_VERSION
  , pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV
  , pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV
  , pattern VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( IndexType
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( boolToBool32
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
#endif
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Pipeline
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( PipelineShaderStageCreateInfo(..)
  , PipelineCreateFlags
  , PipelineLayout
  )
#endif
import Graphics.Vulkan.Core10.PipelineCache
  ( PipelineCache
  )
import Graphics.Vulkan.Core10.Query
  ( QueryPool
  , QueryType
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( MemoryRequirements2KHR
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Buffer
  ( pattern BUFFER_USAGE_RAY_TRACING_BIT_NV
  )
import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( pattern INDEX_TYPE_NONE_NV
  )
import Graphics.Vulkan.Core10.Core
  ( pattern OBJECT_TYPE_ACCELERATION_STRUCTURE_NV
  , pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV
  , pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV
  , pattern STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV
  , pattern STRUCTURE_TYPE_GEOMETRY_AABB_NV
  , pattern STRUCTURE_TYPE_GEOMETRY_NV
  , pattern STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV
  , pattern ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV
  , pattern PIPELINE_BIND_POINT_RAY_TRACING_NV
  )
import Graphics.Vulkan.Core10.Pipeline
  ( pattern PIPELINE_CREATE_DEFER_COMPILE_BIT_NV
  , pattern SHADER_STAGE_ANY_HIT_BIT_NV
  , pattern SHADER_STAGE_CALLABLE_BIT_NV
  , pattern SHADER_STAGE_CLOSEST_HIT_BIT_NV
  , pattern SHADER_STAGE_INTERSECTION_BIT_NV
  , pattern SHADER_STAGE_MISS_BIT_NV
  , pattern SHADER_STAGE_RAYGEN_BIT_NV
  )
import Graphics.Vulkan.Core10.Query
  ( pattern QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV
  )
import Graphics.Vulkan.Core10.Queue
  ( pattern PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV
  , pattern PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( pattern DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkAccelerationStructureCreateInfoNV"
data AccelerationStructureCreateInfoNV = AccelerationStructureCreateInfoNV
  { -- No documentation found for Nested "AccelerationStructureCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AccelerationStructureCreateInfoNV" "compactedSize"
  compactedSize :: DeviceSize
  , -- No documentation found for Nested "AccelerationStructureCreateInfoNV" "info"
  info :: AccelerationStructureInfoNV
  }
  deriving (Show, Eq)

instance Zero AccelerationStructureCreateInfoNV where
  zero = AccelerationStructureCreateInfoNV Nothing
                                           zero
                                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkAccelerationStructureInfoNV"
data AccelerationStructureInfoNV = AccelerationStructureInfoNV
  { -- No documentation found for Nested "AccelerationStructureInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AccelerationStructureInfoNV" "type"
  type' :: AccelerationStructureTypeNV
  , -- No documentation found for Nested "AccelerationStructureInfoNV" "flags"
  flags :: BuildAccelerationStructureFlagsNV
  , -- No documentation found for Nested "AccelerationStructureInfoNV" "instanceCount"
  instanceCount :: Word32
  , -- No documentation found for Nested "AccelerationStructureInfoNV" "pGeometries"
  geometries :: Vector GeometryNV
  }
  deriving (Show, Eq)

instance Zero AccelerationStructureInfoNV where
  zero = AccelerationStructureInfoNV Nothing
                                     zero
                                     zero
                                     zero
                                     mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkAccelerationStructureMemoryRequirementsInfoNV"
data AccelerationStructureMemoryRequirementsInfoNV = AccelerationStructureMemoryRequirementsInfoNV
  { -- No documentation found for Nested "AccelerationStructureMemoryRequirementsInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AccelerationStructureMemoryRequirementsInfoNV" "type"
  type' :: AccelerationStructureMemoryRequirementsTypeNV
  , -- No documentation found for Nested "AccelerationStructureMemoryRequirementsInfoNV" "accelerationStructure"
  accelerationStructure :: AccelerationStructureNV
  }
  deriving (Show, Eq)

instance Zero AccelerationStructureMemoryRequirementsInfoNV where
  zero = AccelerationStructureMemoryRequirementsInfoNV Nothing
                                                       zero
                                                       zero

#endif

-- No documentation found for TopLevel "AccelerationStructureMemoryRequirementsTypeNV"
type AccelerationStructureMemoryRequirementsTypeNV = VkAccelerationStructureMemoryRequirementsTypeNV


{-# complete ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV, ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV, ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV :: AccelerationStructureMemoryRequirementsTypeNV #-}


-- No documentation found for Nested "AccelerationStructureMemoryRequirementsTypeNV" "ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV"
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV :: (a ~ AccelerationStructureMemoryRequirementsTypeNV) => a
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV = VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV


-- No documentation found for Nested "AccelerationStructureMemoryRequirementsTypeNV" "ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV"
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV :: (a ~ AccelerationStructureMemoryRequirementsTypeNV) => a
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV = VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV


-- No documentation found for Nested "AccelerationStructureMemoryRequirementsTypeNV" "ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV"
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV :: (a ~ AccelerationStructureMemoryRequirementsTypeNV) => a
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV = VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV

-- No documentation found for TopLevel "AccelerationStructureNV"
type AccelerationStructureNV = VkAccelerationStructureNV

-- No documentation found for TopLevel "AccelerationStructureTypeNV"
type AccelerationStructureTypeNV = VkAccelerationStructureTypeNV


{-# complete ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV, ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV :: AccelerationStructureTypeNV #-}


-- No documentation found for Nested "AccelerationStructureTypeNV" "ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV"
pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV :: (a ~ AccelerationStructureTypeNV) => a
pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV = VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV


-- No documentation found for Nested "AccelerationStructureTypeNV" "ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV"
pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV :: (a ~ AccelerationStructureTypeNV) => a
pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV = VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBindAccelerationStructureMemoryInfoNV"
data BindAccelerationStructureMemoryInfoNV = BindAccelerationStructureMemoryInfoNV
  { -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "accelerationStructure"
  accelerationStructure :: AccelerationStructureNV
  , -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "memoryOffset"
  memoryOffset :: DeviceSize
  , -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "pDeviceIndices"
  deviceIndices :: Vector Word32
  }
  deriving (Show, Eq)

instance Zero BindAccelerationStructureMemoryInfoNV where
  zero = BindAccelerationStructureMemoryInfoNV Nothing
                                               zero
                                               zero
                                               zero
                                               mempty

#endif

-- No documentation found for TopLevel "BuildAccelerationStructureFlagBitsNV"
type BuildAccelerationStructureFlagBitsNV = VkBuildAccelerationStructureFlagBitsNV


{-# complete BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV, BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV, BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV, BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV, BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV :: BuildAccelerationStructureFlagBitsNV #-}


-- No documentation found for Nested "BuildAccelerationStructureFlagBitsNV" "BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV :: (a ~ BuildAccelerationStructureFlagBitsNV) => a
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV


-- No documentation found for Nested "BuildAccelerationStructureFlagBitsNV" "BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV :: (a ~ BuildAccelerationStructureFlagBitsNV) => a
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV


-- No documentation found for Nested "BuildAccelerationStructureFlagBitsNV" "BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV :: (a ~ BuildAccelerationStructureFlagBitsNV) => a
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV


-- No documentation found for Nested "BuildAccelerationStructureFlagBitsNV" "BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV :: (a ~ BuildAccelerationStructureFlagBitsNV) => a
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV


-- No documentation found for Nested "BuildAccelerationStructureFlagBitsNV" "BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV :: (a ~ BuildAccelerationStructureFlagBitsNV) => a
pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV

-- No documentation found for TopLevel "BuildAccelerationStructureFlagsNV"
type BuildAccelerationStructureFlagsNV = BuildAccelerationStructureFlagBitsNV

-- No documentation found for TopLevel "CopyAccelerationStructureModeNV"
type CopyAccelerationStructureModeNV = VkCopyAccelerationStructureModeNV


{-# complete COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV, COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV :: CopyAccelerationStructureModeNV #-}


-- No documentation found for Nested "CopyAccelerationStructureModeNV" "COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV"
pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV :: (a ~ CopyAccelerationStructureModeNV) => a
pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV = VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV


-- No documentation found for Nested "CopyAccelerationStructureModeNV" "COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV"
pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV :: (a ~ CopyAccelerationStructureModeNV) => a
pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV = VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkGeometryAABBNV"
data GeometryAABBNV = GeometryAABBNV
  { -- No documentation found for Nested "GeometryAABBNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "GeometryAABBNV" "aabbData"
  aabbData :: Buffer
  , -- No documentation found for Nested "GeometryAABBNV" "numAABBs"
  numAABBs :: Word32
  , -- No documentation found for Nested "GeometryAABBNV" "stride"
  stride :: Word32
  , -- No documentation found for Nested "GeometryAABBNV" "offset"
  offset :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero GeometryAABBNV where
  zero = GeometryAABBNV Nothing
                        zero
                        zero
                        zero
                        zero

#endif


-- No documentation found for TopLevel "VkGeometryDataNV"
data GeometryDataNV = GeometryDataNV
  { -- No documentation found for Nested "GeometryDataNV" "triangles"
  triangles :: GeometryTrianglesNV
  , -- No documentation found for Nested "GeometryDataNV" "aabbs"
  aabbs :: GeometryAABBNV
  }
  deriving (Show, Eq)

instance Zero GeometryDataNV where
  zero = GeometryDataNV zero
                        zero


-- No documentation found for TopLevel "GeometryFlagBitsNV"
type GeometryFlagBitsNV = VkGeometryFlagBitsNV


{-# complete GEOMETRY_OPAQUE_BIT_NV, GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV :: GeometryFlagBitsNV #-}


-- No documentation found for Nested "GeometryFlagBitsNV" "GEOMETRY_OPAQUE_BIT_NV"
pattern GEOMETRY_OPAQUE_BIT_NV :: (a ~ GeometryFlagBitsNV) => a
pattern GEOMETRY_OPAQUE_BIT_NV = VK_GEOMETRY_OPAQUE_BIT_NV


-- No documentation found for Nested "GeometryFlagBitsNV" "GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV"
pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV :: (a ~ GeometryFlagBitsNV) => a
pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV = VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV

-- No documentation found for TopLevel "GeometryFlagsNV"
type GeometryFlagsNV = GeometryFlagBitsNV

-- No documentation found for TopLevel "GeometryInstanceFlagBitsNV"
type GeometryInstanceFlagBitsNV = VkGeometryInstanceFlagBitsNV


{-# complete GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV, GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV, GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV, GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV :: GeometryInstanceFlagBitsNV #-}


-- No documentation found for Nested "GeometryInstanceFlagBitsNV" "GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV"
pattern GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV :: (a ~ GeometryInstanceFlagBitsNV) => a
pattern GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV = VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV


-- No documentation found for Nested "GeometryInstanceFlagBitsNV" "GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV"
pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV :: (a ~ GeometryInstanceFlagBitsNV) => a
pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV = VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV


-- No documentation found for Nested "GeometryInstanceFlagBitsNV" "GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV"
pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV :: (a ~ GeometryInstanceFlagBitsNV) => a
pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV = VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV


-- No documentation found for Nested "GeometryInstanceFlagBitsNV" "GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV"
pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV :: (a ~ GeometryInstanceFlagBitsNV) => a
pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV = VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV

-- No documentation found for TopLevel "GeometryInstanceFlagsNV"
type GeometryInstanceFlagsNV = GeometryInstanceFlagBitsNV


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkGeometryNV"
data GeometryNV = GeometryNV
  { -- No documentation found for Nested "GeometryNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "GeometryNV" "geometryType"
  geometryType :: GeometryTypeNV
  , -- No documentation found for Nested "GeometryNV" "geometry"
  geometry :: GeometryDataNV
  , -- No documentation found for Nested "GeometryNV" "flags"
  flags :: GeometryFlagsNV
  }
  deriving (Show, Eq)

instance Zero GeometryNV where
  zero = GeometryNV Nothing
                    zero
                    zero
                    zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkGeometryTrianglesNV"
data GeometryTrianglesNV = GeometryTrianglesNV
  { -- No documentation found for Nested "GeometryTrianglesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "GeometryTrianglesNV" "vertexData"
  vertexData :: Buffer
  , -- No documentation found for Nested "GeometryTrianglesNV" "vertexOffset"
  vertexOffset :: DeviceSize
  , -- No documentation found for Nested "GeometryTrianglesNV" "vertexCount"
  vertexCount :: Word32
  , -- No documentation found for Nested "GeometryTrianglesNV" "vertexStride"
  vertexStride :: DeviceSize
  , -- No documentation found for Nested "GeometryTrianglesNV" "vertexFormat"
  vertexFormat :: Format
  , -- No documentation found for Nested "GeometryTrianglesNV" "indexData"
  indexData :: Buffer
  , -- No documentation found for Nested "GeometryTrianglesNV" "indexOffset"
  indexOffset :: DeviceSize
  , -- No documentation found for Nested "GeometryTrianglesNV" "indexCount"
  indexCount :: Word32
  , -- No documentation found for Nested "GeometryTrianglesNV" "indexType"
  indexType :: IndexType
  , -- No documentation found for Nested "GeometryTrianglesNV" "transformData"
  transformData :: Buffer
  , -- No documentation found for Nested "GeometryTrianglesNV" "transformOffset"
  transformOffset :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero GeometryTrianglesNV where
  zero = GeometryTrianglesNV Nothing
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

#endif

-- No documentation found for TopLevel "GeometryTypeNV"
type GeometryTypeNV = VkGeometryTypeNV


{-# complete GEOMETRY_TYPE_TRIANGLES_NV, GEOMETRY_TYPE_AABBS_NV :: GeometryTypeNV #-}


-- No documentation found for Nested "GeometryTypeNV" "GEOMETRY_TYPE_TRIANGLES_NV"
pattern GEOMETRY_TYPE_TRIANGLES_NV :: (a ~ GeometryTypeNV) => a
pattern GEOMETRY_TYPE_TRIANGLES_NV = VK_GEOMETRY_TYPE_TRIANGLES_NV


-- No documentation found for Nested "GeometryTypeNV" "GEOMETRY_TYPE_AABBS_NV"
pattern GEOMETRY_TYPE_AABBS_NV :: (a ~ GeometryTypeNV) => a
pattern GEOMETRY_TYPE_AABBS_NV = VK_GEOMETRY_TYPE_AABBS_NV


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceRayTracingPropertiesNV"
data PhysicalDeviceRayTracingPropertiesNV = PhysicalDeviceRayTracingPropertiesNV
  { -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "shaderGroupHandleSize"
  shaderGroupHandleSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "maxRecursionDepth"
  maxRecursionDepth :: Word32
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "maxShaderGroupStride"
  maxShaderGroupStride :: Word32
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "shaderGroupBaseAlignment"
  shaderGroupBaseAlignment :: Word32
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "maxGeometryCount"
  maxGeometryCount :: Word64
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "maxInstanceCount"
  maxInstanceCount :: Word64
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "maxTriangleCount"
  maxTriangleCount :: Word64
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "maxDescriptorSetAccelerationStructures"
  maxDescriptorSetAccelerationStructures :: Word32
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceRayTracingPropertiesNV where
  zero = PhysicalDeviceRayTracingPropertiesNV Nothing
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkRayTracingPipelineCreateInfoNV"
data RayTracingPipelineCreateInfoNV = RayTracingPipelineCreateInfoNV
  { -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "flags"
  flags :: PipelineCreateFlags
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "pStages"
  stages :: Vector PipelineShaderStageCreateInfo
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "pGroups"
  groups :: Vector RayTracingShaderGroupCreateInfoNV
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "maxRecursionDepth"
  maxRecursionDepth :: Word32
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "layout"
  layout :: PipelineLayout
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "basePipelineHandle"
  basePipelineHandle :: Pipeline
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "basePipelineIndex"
  basePipelineIndex :: Int32
  }
  deriving (Show, Eq)

instance Zero RayTracingPipelineCreateInfoNV where
  zero = RayTracingPipelineCreateInfoNV Nothing
                                        zero
                                        mempty
                                        mempty
                                        zero
                                        zero
                                        zero
                                        zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkRayTracingShaderGroupCreateInfoNV"
data RayTracingShaderGroupCreateInfoNV = RayTracingShaderGroupCreateInfoNV
  { -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "type"
  type' :: RayTracingShaderGroupTypeNV
  , -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "generalShader"
  generalShader :: Word32
  , -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "closestHitShader"
  closestHitShader :: Word32
  , -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "anyHitShader"
  anyHitShader :: Word32
  , -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "intersectionShader"
  intersectionShader :: Word32
  }
  deriving (Show, Eq)

instance Zero RayTracingShaderGroupCreateInfoNV where
  zero = RayTracingShaderGroupCreateInfoNV Nothing
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero

#endif

-- No documentation found for TopLevel "RayTracingShaderGroupTypeNV"
type RayTracingShaderGroupTypeNV = VkRayTracingShaderGroupTypeNV


{-# complete RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV, RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV, RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV :: RayTracingShaderGroupTypeNV #-}


-- No documentation found for Nested "RayTracingShaderGroupTypeNV" "RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV"
pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV :: (a ~ RayTracingShaderGroupTypeNV) => a
pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV = VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV


-- No documentation found for Nested "RayTracingShaderGroupTypeNV" "RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV"
pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV :: (a ~ RayTracingShaderGroupTypeNV) => a
pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV = VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV


-- No documentation found for Nested "RayTracingShaderGroupTypeNV" "RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV"
pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV :: (a ~ RayTracingShaderGroupTypeNV) => a
pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV = VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkWriteDescriptorSetAccelerationStructureNV"
data WriteDescriptorSetAccelerationStructureNV = WriteDescriptorSetAccelerationStructureNV
  { -- No documentation found for Nested "WriteDescriptorSetAccelerationStructureNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "WriteDescriptorSetAccelerationStructureNV" "pAccelerationStructures"
  accelerationStructures :: Vector AccelerationStructureNV
  }
  deriving (Show, Eq)

instance Zero WriteDescriptorSetAccelerationStructureNV where
  zero = WriteDescriptorSetAccelerationStructureNV Nothing
                                                   mempty

#endif


-- No documentation found for TopLevel "vkBindAccelerationStructureMemoryNV"
bindAccelerationStructureMemoryNV :: Device ->  Vector BindAccelerationStructureMemoryInfoNV ->  IO ()
bindAccelerationStructureMemoryNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdBuildAccelerationStructureNV"
cmdBuildAccelerationStructureNV :: CommandBuffer ->  AccelerationStructureInfoNV ->  Buffer ->  DeviceSize ->  Bool ->  AccelerationStructureNV ->  AccelerationStructureNV ->  Buffer ->  DeviceSize ->  IO ()
cmdBuildAccelerationStructureNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdCopyAccelerationStructureNV"
cmdCopyAccelerationStructureNV :: CommandBuffer ->  AccelerationStructureNV ->  AccelerationStructureNV ->  CopyAccelerationStructureModeNV ->  IO ()
cmdCopyAccelerationStructureNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdTraceRaysNV"
cmdTraceRaysNV :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  DeviceSize ->  Buffer ->  DeviceSize ->  DeviceSize ->  Buffer ->  DeviceSize ->  DeviceSize ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdTraceRaysNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdWriteAccelerationStructuresPropertiesNV"
cmdWriteAccelerationStructuresPropertiesNV :: CommandBuffer ->  Vector AccelerationStructureNV ->  QueryType ->  QueryPool ->  Word32 ->  IO ()
cmdWriteAccelerationStructuresPropertiesNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCompileDeferredNV"
compileDeferredNV :: Device ->  Pipeline ->  Word32 ->  IO ()
compileDeferredNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCreateAccelerationStructureNV"
createAccelerationStructureNV :: Device ->  AccelerationStructureCreateInfoNV ->  Maybe AllocationCallbacks ->  IO (AccelerationStructureNV)
createAccelerationStructureNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCreateRayTracingPipelinesNV"
createRayTracingPipelinesNV :: Device ->  PipelineCache ->  Vector RayTracingPipelineCreateInfoNV ->  Maybe AllocationCallbacks ->  IO (Vector Pipeline)
createRayTracingPipelinesNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyAccelerationStructureNV"
destroyAccelerationStructureNV :: Device ->  AccelerationStructureNV ->  Maybe AllocationCallbacks ->  IO ()
destroyAccelerationStructureNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetAccelerationStructureHandleNV"
getAccelerationStructureHandleNV :: Device ->  AccelerationStructureNV ->  CSize ->  IO (ByteString)
getAccelerationStructureHandleNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetAccelerationStructureMemoryRequirementsNV"
getAccelerationStructureMemoryRequirementsNV :: Device ->  AccelerationStructureMemoryRequirementsInfoNV ->  IO (MemoryRequirements2KHR)
getAccelerationStructureMemoryRequirementsNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetRayTracingShaderGroupHandlesNV"
getRayTracingShaderGroupHandlesNV :: Device ->  Pipeline ->  Word32 ->  Word32 ->  CSize ->  IO (ByteString)
getRayTracingShaderGroupHandlesNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createAccelerationStructureNV' and 'destroyAccelerationStructureNV' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withAccelerationStructureNV
  :: Device -> AccelerationStructureCreateInfoNV -> Maybe AllocationCallbacks -> (AccelerationStructureNV -> IO a) -> IO a
withAccelerationStructureNV device accelerationStructureCreateInfoNV allocationCallbacks = bracket
  (createAccelerationStructureNV device accelerationStructureCreateInfoNV allocationCallbacks)
  (\o -> destroyAccelerationStructureNV device o allocationCallbacks)

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_EXTENSION_NAME"
pattern NV_RAY_TRACING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_RAY_TRACING_EXTENSION_NAME = VK_NV_RAY_TRACING_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_SPEC_VERSION"
pattern NV_RAY_TRACING_SPEC_VERSION :: Integral a => a
pattern NV_RAY_TRACING_SPEC_VERSION = VK_NV_RAY_TRACING_SPEC_VERSION
