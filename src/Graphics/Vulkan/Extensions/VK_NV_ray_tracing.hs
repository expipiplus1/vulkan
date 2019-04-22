{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_NV_ray_tracing
  ( withCStructAccelerationStructureCreateInfoNV
  , fromCStructAccelerationStructureCreateInfoNV
  , AccelerationStructureCreateInfoNV(..)
  , withCStructAccelerationStructureInfoNV
  , fromCStructAccelerationStructureInfoNV
  , AccelerationStructureInfoNV(..)
  , withCStructAccelerationStructureMemoryRequirementsInfoNV
  , fromCStructAccelerationStructureMemoryRequirementsInfoNV
  , AccelerationStructureMemoryRequirementsInfoNV(..)
  , AccelerationStructureMemoryRequirementsTypeNV
  , pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV
  , pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV
  , pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV
  , AccelerationStructureNV
  , AccelerationStructureTypeNV
  , pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV
  , pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV
  , withCStructBindAccelerationStructureMemoryInfoNV
  , fromCStructBindAccelerationStructureMemoryInfoNV
  , BindAccelerationStructureMemoryInfoNV(..)
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
  , withCStructGeometryAABBNV
  , fromCStructGeometryAABBNV
  , GeometryAABBNV(..)
  , withCStructGeometryDataNV
  , fromCStructGeometryDataNV
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
  , withCStructGeometryNV
  , fromCStructGeometryNV
  , GeometryNV(..)
  , withCStructGeometryTrianglesNV
  , fromCStructGeometryTrianglesNV
  , GeometryTrianglesNV(..)
  , GeometryTypeNV
  , pattern GEOMETRY_TYPE_TRIANGLES_NV
  , pattern GEOMETRY_TYPE_AABBS_NV
  , withCStructPhysicalDeviceRayTracingPropertiesNV
  , fromCStructPhysicalDeviceRayTracingPropertiesNV
  , PhysicalDeviceRayTracingPropertiesNV(..)
  , withCStructRayTracingPipelineCreateInfoNV
  , fromCStructRayTracingPipelineCreateInfoNV
  , RayTracingPipelineCreateInfoNV(..)
  , withCStructRayTracingShaderGroupCreateInfoNV
  , fromCStructRayTracingShaderGroupCreateInfoNV
  , RayTracingShaderGroupCreateInfoNV(..)
  , RayTracingShaderGroupTypeNV
  , pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV
  , pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV
  , pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV
  , withCStructWriteDescriptorSetAccelerationStructureNV
  , fromCStructWriteDescriptorSetAccelerationStructureNV
  , WriteDescriptorSetAccelerationStructureNV(..)
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
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.ByteString
  ( ByteString
  , packCStringLen
  )
import Data.Function
  ( (&)
  )
import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  , Word64
  )
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
  ( maybePeek
  , maybeWith
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
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( VkAccelerationStructureCreateInfoNV(..)
  , VkAccelerationStructureInfoNV(..)
  , VkAccelerationStructureMemoryRequirementsInfoNV(..)
  , VkAccelerationStructureMemoryRequirementsTypeNV(..)
  , VkAccelerationStructureTypeNV(..)
  , VkBindAccelerationStructureMemoryInfoNV(..)
  , VkBuildAccelerationStructureFlagBitsNV(..)
  , VkCopyAccelerationStructureModeNV(..)
  , VkGeometryAABBNV(..)
  , VkGeometryDataNV(..)
  , VkGeometryFlagBitsNV(..)
  , VkGeometryInstanceFlagBitsNV(..)
  , VkGeometryNV(..)
  , VkGeometryTrianglesNV(..)
  , VkGeometryTypeNV(..)
  , VkPhysicalDeviceRayTracingPropertiesNV(..)
  , VkRayTracingPipelineCreateInfoNV(..)
  , VkRayTracingShaderGroupCreateInfoNV(..)
  , VkRayTracingShaderGroupTypeNV(..)
  , VkWriteDescriptorSetAccelerationStructureNV(..)
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
  , pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV
  , pattern VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV
  , pattern VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV
  , pattern VK_STRUCTURE_TYPE_GEOMETRY_NV
  , pattern VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV
  )
import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( IndexType
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Core10.Pipeline
  ( PipelineShaderStageCreateInfo(..)
  , Pipeline
  , PipelineCreateFlags
  , PipelineLayout
  , fromCStructPipelineShaderStageCreateInfo
  , withCStructPipelineShaderStageCreateInfo
  )
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
  ( MemoryRequirements2(..)
  , fromCStructMemoryRequirements2
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
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
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( MemoryRequirements2KHR
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( pattern DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT
  )



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
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @info@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV'
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCreateAccelerationStructureNV'
data AccelerationStructureCreateInfoNV = AccelerationStructureCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "AccelerationStructureCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AccelerationStructureCreateInfoNV" "compactedSize"
  compactedSize :: DeviceSize
  , -- No documentation found for Nested "AccelerationStructureCreateInfoNV" "info"
  info :: AccelerationStructureInfoNV
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAccelerationStructureCreateInfoNV' and
-- marshal a 'AccelerationStructureCreateInfoNV' into it. The 'VkAccelerationStructureCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAccelerationStructureCreateInfoNV :: AccelerationStructureCreateInfoNV -> (VkAccelerationStructureCreateInfoNV -> IO a) -> IO a
withCStructAccelerationStructureCreateInfoNV marshalled cont = withCStructAccelerationStructureInfoNV (info (marshalled :: AccelerationStructureCreateInfoNV)) (\info'' -> maybeWith withSomeVkStruct (next (marshalled :: AccelerationStructureCreateInfoNV)) (\pPNext -> cont (VkAccelerationStructureCreateInfoNV VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV pPNext (compactedSize (marshalled :: AccelerationStructureCreateInfoNV)) info'')))

-- | A function to read a 'VkAccelerationStructureCreateInfoNV' and all additional
-- structures in the pointer chain into a 'AccelerationStructureCreateInfoNV'.
fromCStructAccelerationStructureCreateInfoNV :: VkAccelerationStructureCreateInfoNV -> IO AccelerationStructureCreateInfoNV
fromCStructAccelerationStructureCreateInfoNV c = AccelerationStructureCreateInfoNV <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAccelerationStructureCreateInfoNV)))
                                                                                   <*> pure (vkCompactedSize (c :: VkAccelerationStructureCreateInfoNV))
                                                                                   <*> (fromCStructAccelerationStructureInfoNV (vkInfo (c :: VkAccelerationStructureCreateInfoNV)))

instance Zero AccelerationStructureCreateInfoNV where
  zero = AccelerationStructureCreateInfoNV Nothing
                                           zero
                                           zero



-- | VkAccelerationStructureInfoNV - Structure specifying the parameters of
-- acceleration structure object
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV'
-- contains information that is used both for acceleration structure
-- creation with
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCreateAccelerationStructureNV'
-- and in combination with the actual geometric data to build the
-- acceleration structure with
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdBuildAccelerationStructureNV'.
--
-- == Valid Usage
--
-- -   @geometryCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@maxGeometryCount@
--
-- -   @instanceCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@maxInstanceCount@
--
-- -   The total number of triangles in all geometries /must/ be less than
--     or equal to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@maxTriangleCount@
--
-- -   If @type@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV'
--     then @geometryCount@ /must/ be @0@
--
-- -   If @type@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV'
--     then @instanceCount@ /must/ be @0@
--
-- -   If @flags@ has the
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV'
--     bit set, then it /must/ not have the
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV'
--     bit set
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @type@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureTypeNV'
--     value
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkBuildAccelerationStructureFlagBitsNV'
--     values
--
-- -   If @geometryCount@ is not @0@, @pGeometries@ /must/ be a valid
--     pointer to an array of @geometryCount@ valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryNV'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureCreateInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureTypeNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkBuildAccelerationStructureFlagsNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdBuildAccelerationStructureNV'
data AccelerationStructureInfoNV = AccelerationStructureInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "AccelerationStructureInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AccelerationStructureInfoNV" "type"
  type' :: AccelerationStructureTypeNV
  , -- No documentation found for Nested "AccelerationStructureInfoNV" "flags"
  flags :: BuildAccelerationStructureFlagsNV
  , -- No documentation found for Nested "AccelerationStructureInfoNV" "instanceCount"
  instanceCount :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "AccelerationStructureInfoNV" "pGeometries"
  geometries :: Vector GeometryNV
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAccelerationStructureInfoNV' and
-- marshal a 'AccelerationStructureInfoNV' into it. The 'VkAccelerationStructureInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAccelerationStructureInfoNV :: AccelerationStructureInfoNV -> (VkAccelerationStructureInfoNV -> IO a) -> IO a
withCStructAccelerationStructureInfoNV marshalled cont = withVec withCStructGeometryNV (geometries (marshalled :: AccelerationStructureInfoNV)) (\pPGeometries -> maybeWith withSomeVkStruct (next (marshalled :: AccelerationStructureInfoNV)) (\pPNext -> cont (VkAccelerationStructureInfoNV VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV pPNext (type' (marshalled :: AccelerationStructureInfoNV)) (flags (marshalled :: AccelerationStructureInfoNV)) (instanceCount (marshalled :: AccelerationStructureInfoNV)) (fromIntegral (Data.Vector.length (geometries (marshalled :: AccelerationStructureInfoNV)))) pPGeometries)))

-- | A function to read a 'VkAccelerationStructureInfoNV' and all additional
-- structures in the pointer chain into a 'AccelerationStructureInfoNV'.
fromCStructAccelerationStructureInfoNV :: VkAccelerationStructureInfoNV -> IO AccelerationStructureInfoNV
fromCStructAccelerationStructureInfoNV c = AccelerationStructureInfoNV <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAccelerationStructureInfoNV)))
                                                                       <*> pure (vkType (c :: VkAccelerationStructureInfoNV))
                                                                       <*> pure (vkFlags (c :: VkAccelerationStructureInfoNV))
                                                                       <*> pure (vkInstanceCount (c :: VkAccelerationStructureInfoNV))
                                                                       -- Length valued member elided
                                                                       <*> (Data.Vector.generateM (fromIntegral (vkGeometryCount (c :: VkAccelerationStructureInfoNV))) (((fromCStructGeometryNV <=<) . peekElemOff) (vkPGeometries (c :: VkAccelerationStructureInfoNV))))

instance Zero AccelerationStructureInfoNV where
  zero = AccelerationStructureInfoNV Nothing
                                     zero
                                     zero
                                     zero
                                     Data.Vector.empty



-- | VkAccelerationStructureMemoryRequirementsInfoNV - Structure specifying
-- acceleration to query for memory requirements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureMemoryRequirementsTypeNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkGetAccelerationStructureMemoryRequirementsNV'
data AccelerationStructureMemoryRequirementsInfoNV = AccelerationStructureMemoryRequirementsInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "AccelerationStructureMemoryRequirementsInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AccelerationStructureMemoryRequirementsInfoNV" "type"
  type' :: AccelerationStructureMemoryRequirementsTypeNV
  , -- No documentation found for Nested "AccelerationStructureMemoryRequirementsInfoNV" "accelerationStructure"
  accelerationStructure :: AccelerationStructureNV
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAccelerationStructureMemoryRequirementsInfoNV' and
-- marshal a 'AccelerationStructureMemoryRequirementsInfoNV' into it. The 'VkAccelerationStructureMemoryRequirementsInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAccelerationStructureMemoryRequirementsInfoNV :: AccelerationStructureMemoryRequirementsInfoNV -> (VkAccelerationStructureMemoryRequirementsInfoNV -> IO a) -> IO a
withCStructAccelerationStructureMemoryRequirementsInfoNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: AccelerationStructureMemoryRequirementsInfoNV)) (\pPNext -> cont (VkAccelerationStructureMemoryRequirementsInfoNV VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV pPNext (type' (marshalled :: AccelerationStructureMemoryRequirementsInfoNV)) (accelerationStructure (marshalled :: AccelerationStructureMemoryRequirementsInfoNV))))

-- | A function to read a 'VkAccelerationStructureMemoryRequirementsInfoNV' and all additional
-- structures in the pointer chain into a 'AccelerationStructureMemoryRequirementsInfoNV'.
fromCStructAccelerationStructureMemoryRequirementsInfoNV :: VkAccelerationStructureMemoryRequirementsInfoNV -> IO AccelerationStructureMemoryRequirementsInfoNV
fromCStructAccelerationStructureMemoryRequirementsInfoNV c = AccelerationStructureMemoryRequirementsInfoNV <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAccelerationStructureMemoryRequirementsInfoNV)))
                                                                                                           <*> pure (vkType (c :: VkAccelerationStructureMemoryRequirementsInfoNV))
                                                                                                           <*> pure (vkAccelerationStructure (c :: VkAccelerationStructureMemoryRequirementsInfoNV))

instance Zero AccelerationStructureMemoryRequirementsInfoNV where
  zero = AccelerationStructureMemoryRequirementsInfoNV Nothing
                                                       zero
                                                       zero


-- | VkAccelerationStructureMemoryRequirementsTypeNV - Acceleration structure
-- memory requirement type
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureMemoryRequirementsInfoNV'
type AccelerationStructureMemoryRequirementsTypeNV = VkAccelerationStructureMemoryRequirementsTypeNV


{-# complete ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV, ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV, ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV :: AccelerationStructureMemoryRequirementsTypeNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV'
-- requests the memory requirement for the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV'
-- backing store.
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV :: (a ~ AccelerationStructureMemoryRequirementsTypeNV) => a
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV = VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV'
-- requests the memory requirement for scratch space during the initial
-- build.
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV :: (a ~ AccelerationStructureMemoryRequirementsTypeNV) => a
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV = VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV'
-- requests the memory requirement for scratch space during an update.
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV :: (a ~ AccelerationStructureMemoryRequirementsTypeNV) => a
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV = VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV

-- | VkAccelerationStructureNV - Opaque handle to an acceleration structure
-- object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureMemoryRequirementsInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkBindAccelerationStructureMemoryInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkWriteDescriptorSetAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdBuildAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdCopyAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdWriteAccelerationStructuresPropertiesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCreateAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkDestroyAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkGetAccelerationStructureHandleNV'
type AccelerationStructureNV = VkAccelerationStructureNV

-- | VkAccelerationStructureTypeNV - Type of acceleration structure
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV'
type AccelerationStructureTypeNV = VkAccelerationStructureTypeNV


{-# complete ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV, ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV :: AccelerationStructureTypeNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV'
-- is a top-level acceleration structure containing instance data referring
-- to bottom-level level acceleration structures.
pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV :: (a ~ AccelerationStructureTypeNV) => a
pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV = VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV'
-- is a bottom-level acceleration structure containing the AABBs or
-- geometry to be intersected.
pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV :: (a ~ AccelerationStructureTypeNV) => a
pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV = VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV


-- | VkBindAccelerationStructureMemoryInfoNV - Structure specifying
-- acceleration structure memory binding
--
-- == Valid Usage
--
-- -   @accelerationStructure@ /must/ not already be backed by a memory
--     object
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   @memory@ /must/ have been allocated using one of the memory types
--     allowed in the @memoryTypeBits@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkGetAccelerationStructureMemoryRequirementsNV'
--     with @accelerationStructure@ and @type@ of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV'
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkGetAccelerationStructureMemoryRequirementsNV'
--     with @accelerationStructure@ and @type@ of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV'
--
-- -   The @size@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkGetAccelerationStructureMemoryRequirementsNV'
--     with @accelerationStructure@ and @type@ of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV'
--     /must/ be less than or equal to the size of @memory@ minus
--     @memoryOffset@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @accelerationStructure@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV'
--     handle
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
--
-- -   If @deviceIndexCount@ is not @0@, @pDeviceIndices@ /must/ be a valid
--     pointer to an array of @deviceIndexCount@ @uint32_t@ values
--
-- -   Both of @accelerationStructure@, and @memory@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkBindAccelerationStructureMemoryNV'
data BindAccelerationStructureMemoryInfoNV = BindAccelerationStructureMemoryInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "accelerationStructure"
  accelerationStructure :: AccelerationStructureNV
  , -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "memoryOffset"
  memoryOffset :: DeviceSize
  -- Length valued member elided
  , -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "pDeviceIndices"
  deviceIndices :: Vector Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBindAccelerationStructureMemoryInfoNV' and
-- marshal a 'BindAccelerationStructureMemoryInfoNV' into it. The 'VkBindAccelerationStructureMemoryInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBindAccelerationStructureMemoryInfoNV :: BindAccelerationStructureMemoryInfoNV -> (VkBindAccelerationStructureMemoryInfoNV -> IO a) -> IO a
withCStructBindAccelerationStructureMemoryInfoNV marshalled cont = withVec (&) (deviceIndices (marshalled :: BindAccelerationStructureMemoryInfoNV)) (\pPDeviceIndices -> maybeWith withSomeVkStruct (next (marshalled :: BindAccelerationStructureMemoryInfoNV)) (\pPNext -> cont (VkBindAccelerationStructureMemoryInfoNV VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV pPNext (accelerationStructure (marshalled :: BindAccelerationStructureMemoryInfoNV)) (memory (marshalled :: BindAccelerationStructureMemoryInfoNV)) (memoryOffset (marshalled :: BindAccelerationStructureMemoryInfoNV)) (fromIntegral (Data.Vector.length (deviceIndices (marshalled :: BindAccelerationStructureMemoryInfoNV)))) pPDeviceIndices)))

-- | A function to read a 'VkBindAccelerationStructureMemoryInfoNV' and all additional
-- structures in the pointer chain into a 'BindAccelerationStructureMemoryInfoNV'.
fromCStructBindAccelerationStructureMemoryInfoNV :: VkBindAccelerationStructureMemoryInfoNV -> IO BindAccelerationStructureMemoryInfoNV
fromCStructBindAccelerationStructureMemoryInfoNV c = BindAccelerationStructureMemoryInfoNV <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindAccelerationStructureMemoryInfoNV)))
                                                                                           <*> pure (vkAccelerationStructure (c :: VkBindAccelerationStructureMemoryInfoNV))
                                                                                           <*> pure (vkMemory (c :: VkBindAccelerationStructureMemoryInfoNV))
                                                                                           <*> pure (vkMemoryOffset (c :: VkBindAccelerationStructureMemoryInfoNV))
                                                                                           -- Length valued member elided
                                                                                           <*> (Data.Vector.generateM (fromIntegral (vkDeviceIndexCount (c :: VkBindAccelerationStructureMemoryInfoNV))) (peekElemOff (vkPDeviceIndices (c :: VkBindAccelerationStructureMemoryInfoNV))))

instance Zero BindAccelerationStructureMemoryInfoNV where
  zero = BindAccelerationStructureMemoryInfoNV Nothing
                                               zero
                                               zero
                                               zero
                                               Data.Vector.empty


-- | VkBuildAccelerationStructureFlagBitsNV - Bitmask specifying additional
-- parameters for acceleration structure builds
--
-- = Description
--
-- __Note__
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV'
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV'
-- /may/ take more time and memory than a normal build, and so /should/
-- only be used when those features are used.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkBuildAccelerationStructureFlagsNV'
type BuildAccelerationStructureFlagBitsNV = VkBuildAccelerationStructureFlagBitsNV


{-# complete BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV, BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV, BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV, BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV, BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV :: BuildAccelerationStructureFlagBitsNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV'
-- indicates that the specified acceleration structure /can/ be updated
-- with @update@ of 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' in
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdBuildAccelerationStructureNV'.
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV :: (a ~ BuildAccelerationStructureFlagBitsNV) => a
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV'
-- indicates that the specified acceleration structure /can/ act as the
-- source for
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdCopyAccelerationStructureNV'
-- with @mode@ of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV'
-- to produce a compacted acceleration structure.
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV :: (a ~ BuildAccelerationStructureFlagBitsNV) => a
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV'
-- indicates that the given acceleration structure build /should/
-- prioritize trace performance over build time.
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV :: (a ~ BuildAccelerationStructureFlagBitsNV) => a
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV'
-- indicates that the given acceleration structure build /should/
-- prioritize build time over trace performance.
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV :: (a ~ BuildAccelerationStructureFlagBitsNV) => a
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV'
-- indicates that this acceleration structure /should/ minimize the size of
-- the scratch memory and the final result build, potentially at the
-- expense of build time or trace performance.
pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV :: (a ~ BuildAccelerationStructureFlagBitsNV) => a
pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV = VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV

-- | VkBuildAccelerationStructureFlagsNV - Bitmask of
-- VkBuildAccelerationStructureFlagBitsNV
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkBuildAccelerationStructureFlagsNV'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkBuildAccelerationStructureFlagBitsNV'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkBuildAccelerationStructureFlagBitsNV'
type BuildAccelerationStructureFlagsNV = BuildAccelerationStructureFlagBitsNV

-- | VkCopyAccelerationStructureModeNV - Acceleration structure copy mode
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdCopyAccelerationStructureNV'
type CopyAccelerationStructureModeNV = VkCopyAccelerationStructureModeNV


{-# complete COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV, COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV :: CopyAccelerationStructureModeNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV'
-- creates a direct copy of the acceleration structure specified in @src@
-- into the one specified by @dst@. The @dst@ acceleration structure /must/
-- have been created with the same parameters as @src@.
pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV :: (a ~ CopyAccelerationStructureModeNV) => a
pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV = VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV'
-- creates a more compact version of an acceleration structure @src@ into
-- @dst@. The acceleration structure @dst@ /must/ have been created with a
-- @compactedSize@ corresponding to the one returned by
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdWriteAccelerationStructuresPropertiesNV'
-- after the build of the acceleration structure specified by @src@.
pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV :: (a ~ CopyAccelerationStructureModeNV) => a
pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV = VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV


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
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   If @aabbData@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @aabbData@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryDataNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data GeometryAABBNV = GeometryAABBNV
  { -- Univalued member elided
  -- No documentation found for Nested "GeometryAABBNV" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkGeometryAABBNV' and
-- marshal a 'GeometryAABBNV' into it. The 'VkGeometryAABBNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructGeometryAABBNV :: GeometryAABBNV -> (VkGeometryAABBNV -> IO a) -> IO a
withCStructGeometryAABBNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: GeometryAABBNV)) (\pPNext -> cont (VkGeometryAABBNV VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV pPNext (aabbData (marshalled :: GeometryAABBNV)) (numAABBs (marshalled :: GeometryAABBNV)) (stride (marshalled :: GeometryAABBNV)) (offset (marshalled :: GeometryAABBNV))))

-- | A function to read a 'VkGeometryAABBNV' and all additional
-- structures in the pointer chain into a 'GeometryAABBNV'.
fromCStructGeometryAABBNV :: VkGeometryAABBNV -> IO GeometryAABBNV
fromCStructGeometryAABBNV c = GeometryAABBNV <$> -- Univalued Member elided
                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkGeometryAABBNV)))
                                             <*> pure (vkAabbData (c :: VkGeometryAABBNV))
                                             <*> pure (vkNumAABBs (c :: VkGeometryAABBNV))
                                             <*> pure (vkStride (c :: VkGeometryAABBNV))
                                             <*> pure (vkOffset (c :: VkGeometryAABBNV))

instance Zero GeometryAABBNV where
  zero = GeometryAABBNV Nothing
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
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryAABBNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryTrianglesNV'
data GeometryDataNV = GeometryDataNV
  { -- No documentation found for Nested "GeometryDataNV" "triangles"
  triangles :: GeometryTrianglesNV
  , -- No documentation found for Nested "GeometryDataNV" "aabbs"
  aabbs :: GeometryAABBNV
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkGeometryDataNV' and
-- marshal a 'GeometryDataNV' into it. The 'VkGeometryDataNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructGeometryDataNV :: GeometryDataNV -> (VkGeometryDataNV -> IO a) -> IO a
withCStructGeometryDataNV marshalled cont = withCStructGeometryAABBNV (aabbs (marshalled :: GeometryDataNV)) (\aabbs'' -> withCStructGeometryTrianglesNV (triangles (marshalled :: GeometryDataNV)) (\triangles'' -> cont (VkGeometryDataNV triangles'' aabbs'')))

-- | A function to read a 'VkGeometryDataNV' and all additional
-- structures in the pointer chain into a 'GeometryDataNV'.
fromCStructGeometryDataNV :: VkGeometryDataNV -> IO GeometryDataNV
fromCStructGeometryDataNV c = GeometryDataNV <$> (fromCStructGeometryTrianglesNV (vkTriangles (c :: VkGeometryDataNV)))
                                             <*> (fromCStructGeometryAABBNV (vkAabbs (c :: VkGeometryDataNV)))

instance Zero GeometryDataNV where
  zero = GeometryDataNV zero
                        zero


-- | VkGeometryFlagBitsNV - Bitmask specifying additional parameters for a
-- geometry
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryFlagsNV'
type GeometryFlagBitsNV = VkGeometryFlagBitsNV


{-# complete GEOMETRY_OPAQUE_BIT_NV, GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV :: GeometryFlagBitsNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_GEOMETRY_OPAQUE_BIT_NV'
-- indicates that this geometry does not invoke the any-hit shaders even if
-- present in a hit group.
pattern GEOMETRY_OPAQUE_BIT_NV :: (a ~ GeometryFlagBitsNV) => a
pattern GEOMETRY_OPAQUE_BIT_NV = VK_GEOMETRY_OPAQUE_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV'
-- indicates that the implementation /must/ only call the any-hit shader a
-- single time for each primitive in this geometry. If this bit is absent
-- an implementation /may/ invoke the any-hit shader more than once for
-- this geometry.
pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV :: (a ~ GeometryFlagBitsNV) => a
pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV = VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV

-- | VkGeometryFlagsNV - Bitmask of VkGeometryFlagBitsNV
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryFlagsNV' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryFlagBitsNV'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryFlagBitsNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryNV'
type GeometryFlagsNV = GeometryFlagBitsNV

-- | VkGeometryInstanceFlagBitsNV - Instance flag bits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV'
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV'
-- /must/ not be used in the same flag.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryInstanceFlagsNV'
type GeometryInstanceFlagBitsNV = VkGeometryInstanceFlagBitsNV


{-# complete GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV, GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV, GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV, GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV :: GeometryInstanceFlagBitsNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV'
-- disables face culling for this instance.
pattern GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV :: (a ~ GeometryInstanceFlagBitsNV) => a
pattern GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV = VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV'
-- indicates that the front face of the triangle for culling purposes is
-- the face that is counter clockwise in object space relative to the ray
-- origin. Because the facing is determined in object space, an instance
-- transform matrix does not change the winding, but a geometry transform
-- does.
pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV :: (a ~ GeometryInstanceFlagBitsNV) => a
pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV = VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV'
-- causes this instance to act as though
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_GEOMETRY_OPAQUE_BIT_NV'
-- were specified on all geometries referenced by this instance. This
-- behavior /can/ be overridden by the ray flag @gl_RayFlagsNoOpaqueNV@.
pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV :: (a ~ GeometryInstanceFlagBitsNV) => a
pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV = VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV'
-- causes this instance to act as though
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_GEOMETRY_OPAQUE_BIT_NV'
-- were not specified on all geometries referenced by this instance. This
-- behavior /can/ be overridden by the ray flag @gl_RayFlagsOpaqueNV@.
pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV :: (a ~ GeometryInstanceFlagBitsNV) => a
pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV = VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV

-- | VkGeometryInstanceFlagsNV - Bitmask of VkGeometryInstanceFlagBitsNV
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryInstanceFlagsNV'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryInstanceFlagBitsNV'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryInstanceFlagBitsNV'
type GeometryInstanceFlagsNV = GeometryInstanceFlagBitsNV


-- | VkGeometryNV - Structure specifying a geometry in a bottom-level
-- acceleration structure
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryDataNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryFlagsNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryTypeNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data GeometryNV = GeometryNV
  { -- Univalued member elided
  -- No documentation found for Nested "GeometryNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "GeometryNV" "geometryType"
  geometryType :: GeometryTypeNV
  , -- No documentation found for Nested "GeometryNV" "geometry"
  geometry :: GeometryDataNV
  , -- No documentation found for Nested "GeometryNV" "flags"
  flags :: GeometryFlagsNV
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkGeometryNV' and
-- marshal a 'GeometryNV' into it. The 'VkGeometryNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructGeometryNV :: GeometryNV -> (VkGeometryNV -> IO a) -> IO a
withCStructGeometryNV marshalled cont = withCStructGeometryDataNV (geometry (marshalled :: GeometryNV)) (\geometry'' -> maybeWith withSomeVkStruct (next (marshalled :: GeometryNV)) (\pPNext -> cont (VkGeometryNV VK_STRUCTURE_TYPE_GEOMETRY_NV pPNext (geometryType (marshalled :: GeometryNV)) geometry'' (flags (marshalled :: GeometryNV)))))

-- | A function to read a 'VkGeometryNV' and all additional
-- structures in the pointer chain into a 'GeometryNV'.
fromCStructGeometryNV :: VkGeometryNV -> IO GeometryNV
fromCStructGeometryNV c = GeometryNV <$> -- Univalued Member elided
                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkGeometryNV)))
                                     <*> pure (vkGeometryType (c :: VkGeometryNV))
                                     <*> (fromCStructGeometryDataNV (vkGeometry (c :: VkGeometryNV)))
                                     <*> pure (vkFlags (c :: VkGeometryNV))

instance Zero GeometryNV where
  zero = GeometryNV Nothing
                    zero
                    zero
                    zero



-- | VkGeometryTrianglesNV - Structure specifying a triangle geometry in a
-- bottom-level acceleration structure
--
-- = Description
--
-- If @indexType@ is
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_INDEX_TYPE_NONE_NV',
-- then this structure describes a set of triangles determined by
-- @vertexCount@. Otherwise, this structure describes a set of indexed
-- triangles determined by @indexCount@.
--
-- == Valid Usage
--
-- -   @vertexOffset@ /must/ be less than the size of @vertexData@
--
-- -   @vertexOffset@ /must/ be a multiple of the component size of
--     @vertexFormat@
--
-- -   @vertexFormat@ /must/ be one of
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32B32_SFLOAT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32_SFLOAT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_SFLOAT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_SFLOAT',
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_SNORM', or
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_SNORM'
--
-- -   @indexOffset@ /must/ be less than the size of @indexData@
--
-- -   @indexOffset@ /must/ be a multiple of the element size of
--     @indexType@
--
-- -   @indexType@ /must/ be
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_INDEX_TYPE_UINT16',
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_INDEX_TYPE_UINT32',
--     or
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_INDEX_TYPE_NONE_NV'
--
-- -   @indexData@ /must/ be
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' if @indexType@
--     is
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_INDEX_TYPE_NONE_NV'
--
-- -   @indexData@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle if
--     @indexType@ is not
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_INDEX_TYPE_NONE_NV'
--
-- -   @indexCount@ /must/ be @0@ if @indexType@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_INDEX_TYPE_NONE_NV'
--
-- -   @transformOffset@ /must/ be less than the size of @transformData@
--
-- -   @transformOffset@ /must/ be a multiple of @16@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   If @vertexData@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @vertexData@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @vertexFormat@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' value
--
-- -   If @indexData@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @indexData@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @indexType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkIndexType' value
--
-- -   If @transformData@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @transformData@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   Each of @indexData@, @transformData@, and @vertexData@ that are
--     valid handles /must/ have been created, allocated, or retrieved from
--     the same 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryDataNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkIndexType',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data GeometryTrianglesNV = GeometryTrianglesNV
  { -- Univalued member elided
  -- No documentation found for Nested "GeometryTrianglesNV" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkGeometryTrianglesNV' and
-- marshal a 'GeometryTrianglesNV' into it. The 'VkGeometryTrianglesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructGeometryTrianglesNV :: GeometryTrianglesNV -> (VkGeometryTrianglesNV -> IO a) -> IO a
withCStructGeometryTrianglesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: GeometryTrianglesNV)) (\pPNext -> cont (VkGeometryTrianglesNV VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV pPNext (vertexData (marshalled :: GeometryTrianglesNV)) (vertexOffset (marshalled :: GeometryTrianglesNV)) (vertexCount (marshalled :: GeometryTrianglesNV)) (vertexStride (marshalled :: GeometryTrianglesNV)) (vertexFormat (marshalled :: GeometryTrianglesNV)) (indexData (marshalled :: GeometryTrianglesNV)) (indexOffset (marshalled :: GeometryTrianglesNV)) (indexCount (marshalled :: GeometryTrianglesNV)) (indexType (marshalled :: GeometryTrianglesNV)) (transformData (marshalled :: GeometryTrianglesNV)) (transformOffset (marshalled :: GeometryTrianglesNV))))

-- | A function to read a 'VkGeometryTrianglesNV' and all additional
-- structures in the pointer chain into a 'GeometryTrianglesNV'.
fromCStructGeometryTrianglesNV :: VkGeometryTrianglesNV -> IO GeometryTrianglesNV
fromCStructGeometryTrianglesNV c = GeometryTrianglesNV <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkGeometryTrianglesNV)))
                                                       <*> pure (vkVertexData (c :: VkGeometryTrianglesNV))
                                                       <*> pure (vkVertexOffset (c :: VkGeometryTrianglesNV))
                                                       <*> pure (vkVertexCount (c :: VkGeometryTrianglesNV))
                                                       <*> pure (vkVertexStride (c :: VkGeometryTrianglesNV))
                                                       <*> pure (vkVertexFormat (c :: VkGeometryTrianglesNV))
                                                       <*> pure (vkIndexData (c :: VkGeometryTrianglesNV))
                                                       <*> pure (vkIndexOffset (c :: VkGeometryTrianglesNV))
                                                       <*> pure (vkIndexCount (c :: VkGeometryTrianglesNV))
                                                       <*> pure (vkIndexType (c :: VkGeometryTrianglesNV))
                                                       <*> pure (vkTransformData (c :: VkGeometryTrianglesNV))
                                                       <*> pure (vkTransformOffset (c :: VkGeometryTrianglesNV))

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


-- | VkGeometryTypeNV - Enum specifying which type of geometry is provided
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryNV'
type GeometryTypeNV = VkGeometryTypeNV


{-# complete GEOMETRY_TYPE_TRIANGLES_NV, GEOMETRY_TYPE_AABBS_NV :: GeometryTypeNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_GEOMETRY_TYPE_TRIANGLES_NV'
-- indicates that the @triangles@ of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryDataNV'
-- contains valid data.
pattern GEOMETRY_TYPE_TRIANGLES_NV :: (a ~ GeometryTypeNV) => a
pattern GEOMETRY_TYPE_TRIANGLES_NV = VK_GEOMETRY_TYPE_TRIANGLES_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_GEOMETRY_TYPE_AABBS_NV'
-- indicates that the @aabbs@ of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryDataNV'
-- contains valid data.
pattern GEOMETRY_TYPE_AABBS_NV :: (a ~ GeometryTypeNV) => a
pattern GEOMETRY_TYPE_AABBS_NV = VK_GEOMETRY_TYPE_AABBS_NV


-- | VkPhysicalDeviceRayTracingPropertiesNV - Properties of the physical
-- device for ray tracing
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceRayTracingPropertiesNV = PhysicalDeviceRayTracingPropertiesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceRayTracingPropertiesNV' and
-- marshal a 'PhysicalDeviceRayTracingPropertiesNV' into it. The 'VkPhysicalDeviceRayTracingPropertiesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceRayTracingPropertiesNV :: PhysicalDeviceRayTracingPropertiesNV -> (VkPhysicalDeviceRayTracingPropertiesNV -> IO a) -> IO a
withCStructPhysicalDeviceRayTracingPropertiesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceRayTracingPropertiesNV)) (\pPNext -> cont (VkPhysicalDeviceRayTracingPropertiesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV pPNext (shaderGroupHandleSize (marshalled :: PhysicalDeviceRayTracingPropertiesNV)) (maxRecursionDepth (marshalled :: PhysicalDeviceRayTracingPropertiesNV)) (maxShaderGroupStride (marshalled :: PhysicalDeviceRayTracingPropertiesNV)) (shaderGroupBaseAlignment (marshalled :: PhysicalDeviceRayTracingPropertiesNV)) (maxGeometryCount (marshalled :: PhysicalDeviceRayTracingPropertiesNV)) (maxInstanceCount (marshalled :: PhysicalDeviceRayTracingPropertiesNV)) (maxTriangleCount (marshalled :: PhysicalDeviceRayTracingPropertiesNV)) (maxDescriptorSetAccelerationStructures (marshalled :: PhysicalDeviceRayTracingPropertiesNV))))

-- | A function to read a 'VkPhysicalDeviceRayTracingPropertiesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceRayTracingPropertiesNV'.
fromCStructPhysicalDeviceRayTracingPropertiesNV :: VkPhysicalDeviceRayTracingPropertiesNV -> IO PhysicalDeviceRayTracingPropertiesNV
fromCStructPhysicalDeviceRayTracingPropertiesNV c = PhysicalDeviceRayTracingPropertiesNV <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceRayTracingPropertiesNV)))
                                                                                         <*> pure (vkShaderGroupHandleSize (c :: VkPhysicalDeviceRayTracingPropertiesNV))
                                                                                         <*> pure (vkMaxRecursionDepth (c :: VkPhysicalDeviceRayTracingPropertiesNV))
                                                                                         <*> pure (vkMaxShaderGroupStride (c :: VkPhysicalDeviceRayTracingPropertiesNV))
                                                                                         <*> pure (vkShaderGroupBaseAlignment (c :: VkPhysicalDeviceRayTracingPropertiesNV))
                                                                                         <*> pure (vkMaxGeometryCount (c :: VkPhysicalDeviceRayTracingPropertiesNV))
                                                                                         <*> pure (vkMaxInstanceCount (c :: VkPhysicalDeviceRayTracingPropertiesNV))
                                                                                         <*> pure (vkMaxTriangleCount (c :: VkPhysicalDeviceRayTracingPropertiesNV))
                                                                                         <*> pure (vkMaxDescriptorSetAccelerationStructures (c :: VkPhysicalDeviceRayTracingPropertiesNV))

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



-- | VkRayTracingPipelineCreateInfoNV - Structure specifying parameters of a
-- newly created ray tracing pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- == Valid Usage
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is @-1@, @basePipelineHandle@ /must/
--     be a valid handle to a ray tracing
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline'
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @basePipelineIndex@ /must/ be a valid index into the calling
--     command’s @pCreateInfos@ parameter
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not @-1@, @basePipelineHandle@
--     /must/ be 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @basePipelineIndex@ /must/ be @-1@
--
-- -   The @stage@ member of one element of @pStages@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_STAGE_RAYGEN_BIT_NV'
--
-- -   The shader code for the entry points identified by @pStages@, and
--     the rest of the state identified by this structure /must/ adhere to
--     the pipeline linking rules described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   @layout@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency consistent>
--     with all shaders specified in @pStages@
--
-- -   The number of resources in @layout@ accessible to each shader stage
--     that is used by the pipeline /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   @maxRecursionDepth@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@maxRecursionDepth@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback.VkPipelineCreationFeedbackCreateInfoEXT'
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineCreateFlagBits' values
--
-- -   @pStages@ /must/ be a valid pointer to an array of @stageCount@
--     valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo'
--     structures
--
-- -   @pGroups@ /must/ be a valid pointer to an array of @groupCount@
--     valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingShaderGroupCreateInfoNV'
--     structures
--
-- -   @layout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' handle
--
-- -   @stageCount@ /must/ be greater than @0@
--
-- -   @groupCount@ /must/ be greater than @0@
--
-- -   Both of @basePipelineHandle@, and @layout@ that are valid handles
--     /must/ have been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingShaderGroupCreateInfoNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCreateRayTracingPipelinesNV'
data RayTracingPipelineCreateInfoNV = RayTracingPipelineCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "flags"
  flags :: PipelineCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "pStages"
  stages :: Vector PipelineShaderStageCreateInfo
  -- Length valued member elided
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

-- | A function to temporarily allocate memory for a 'VkRayTracingPipelineCreateInfoNV' and
-- marshal a 'RayTracingPipelineCreateInfoNV' into it. The 'VkRayTracingPipelineCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructRayTracingPipelineCreateInfoNV :: RayTracingPipelineCreateInfoNV -> (VkRayTracingPipelineCreateInfoNV -> IO a) -> IO a
withCStructRayTracingPipelineCreateInfoNV marshalled cont = withVec withCStructRayTracingShaderGroupCreateInfoNV (groups (marshalled :: RayTracingPipelineCreateInfoNV)) (\pPGroups -> withVec withCStructPipelineShaderStageCreateInfo (stages (marshalled :: RayTracingPipelineCreateInfoNV)) (\pPStages -> maybeWith withSomeVkStruct (next (marshalled :: RayTracingPipelineCreateInfoNV)) (\pPNext -> cont (VkRayTracingPipelineCreateInfoNV VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV pPNext (flags (marshalled :: RayTracingPipelineCreateInfoNV)) (fromIntegral (Data.Vector.length (stages (marshalled :: RayTracingPipelineCreateInfoNV)))) pPStages (fromIntegral (Data.Vector.length (groups (marshalled :: RayTracingPipelineCreateInfoNV)))) pPGroups (maxRecursionDepth (marshalled :: RayTracingPipelineCreateInfoNV)) (layout (marshalled :: RayTracingPipelineCreateInfoNV)) (basePipelineHandle (marshalled :: RayTracingPipelineCreateInfoNV)) (basePipelineIndex (marshalled :: RayTracingPipelineCreateInfoNV))))))

-- | A function to read a 'VkRayTracingPipelineCreateInfoNV' and all additional
-- structures in the pointer chain into a 'RayTracingPipelineCreateInfoNV'.
fromCStructRayTracingPipelineCreateInfoNV :: VkRayTracingPipelineCreateInfoNV -> IO RayTracingPipelineCreateInfoNV
fromCStructRayTracingPipelineCreateInfoNV c = RayTracingPipelineCreateInfoNV <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRayTracingPipelineCreateInfoNV)))
                                                                             <*> pure (vkFlags (c :: VkRayTracingPipelineCreateInfoNV))
                                                                             -- Length valued member elided
                                                                             <*> (Data.Vector.generateM (fromIntegral (vkStageCount (c :: VkRayTracingPipelineCreateInfoNV))) (((fromCStructPipelineShaderStageCreateInfo <=<) . peekElemOff) (vkPStages (c :: VkRayTracingPipelineCreateInfoNV))))
                                                                             -- Length valued member elided
                                                                             <*> (Data.Vector.generateM (fromIntegral (vkGroupCount (c :: VkRayTracingPipelineCreateInfoNV))) (((fromCStructRayTracingShaderGroupCreateInfoNV <=<) . peekElemOff) (vkPGroups (c :: VkRayTracingPipelineCreateInfoNV))))
                                                                             <*> pure (vkMaxRecursionDepth (c :: VkRayTracingPipelineCreateInfoNV))
                                                                             <*> pure (vkLayout (c :: VkRayTracingPipelineCreateInfoNV))
                                                                             <*> pure (vkBasePipelineHandle (c :: VkRayTracingPipelineCreateInfoNV))
                                                                             <*> pure (vkBasePipelineIndex (c :: VkRayTracingPipelineCreateInfoNV))

instance Zero RayTracingPipelineCreateInfoNV where
  zero = RayTracingPipelineCreateInfoNV Nothing
                                        zero
                                        Data.Vector.empty
                                        Data.Vector.empty
                                        zero
                                        zero
                                        zero
                                        zero



-- | VkRayTracingShaderGroupCreateInfoNV - Structure specifying shaders in a
-- shader group
--
-- == Valid Usage
--
-- -   If @type@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV'
--     then @generalShader@ /must/ be a valid index into @pStages@
--     referring to a shader of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_STAGE_RAYGEN_BIT_NV',
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_STAGE_MISS_BIT_NV',
--     or
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_STAGE_CALLABLE_BIT_NV'
--
-- -   If @type@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV'
--     then @closestHitShader@, @anyHitShader@, and @intersectionShader@
--     /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_UNUSED_NV'
--
-- -   If @type@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV'
--     then @intersectionShader@ /must/ be a valid index into @pStages@
--     referring to a shader of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_STAGE_INTERSECTION_BIT_NV'
--
-- -   If @type@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV'
--     then @intersectionShader@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_UNUSED_NV'
--
-- -   @closestHitShader@ /must/ be either
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_UNUSED_NV'
--     or a valid index into @pStages@ referring to a shader of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV'
--
-- -   @anyHitShader@ /must/ be either
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_UNUSED_NV'
--     or a valid index into @pStages@ referring to a shader of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_STAGE_ANY_HIT_BIT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @type@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingShaderGroupTypeNV'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingShaderGroupTypeNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data RayTracingShaderGroupCreateInfoNV = RayTracingShaderGroupCreateInfoNV
  { -- Univalued member elided
  -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkRayTracingShaderGroupCreateInfoNV' and
-- marshal a 'RayTracingShaderGroupCreateInfoNV' into it. The 'VkRayTracingShaderGroupCreateInfoNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructRayTracingShaderGroupCreateInfoNV :: RayTracingShaderGroupCreateInfoNV -> (VkRayTracingShaderGroupCreateInfoNV -> IO a) -> IO a
withCStructRayTracingShaderGroupCreateInfoNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: RayTracingShaderGroupCreateInfoNV)) (\pPNext -> cont (VkRayTracingShaderGroupCreateInfoNV VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV pPNext (type' (marshalled :: RayTracingShaderGroupCreateInfoNV)) (generalShader (marshalled :: RayTracingShaderGroupCreateInfoNV)) (closestHitShader (marshalled :: RayTracingShaderGroupCreateInfoNV)) (anyHitShader (marshalled :: RayTracingShaderGroupCreateInfoNV)) (intersectionShader (marshalled :: RayTracingShaderGroupCreateInfoNV))))

-- | A function to read a 'VkRayTracingShaderGroupCreateInfoNV' and all additional
-- structures in the pointer chain into a 'RayTracingShaderGroupCreateInfoNV'.
fromCStructRayTracingShaderGroupCreateInfoNV :: VkRayTracingShaderGroupCreateInfoNV -> IO RayTracingShaderGroupCreateInfoNV
fromCStructRayTracingShaderGroupCreateInfoNV c = RayTracingShaderGroupCreateInfoNV <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRayTracingShaderGroupCreateInfoNV)))
                                                                                   <*> pure (vkType (c :: VkRayTracingShaderGroupCreateInfoNV))
                                                                                   <*> pure (vkGeneralShader (c :: VkRayTracingShaderGroupCreateInfoNV))
                                                                                   <*> pure (vkClosestHitShader (c :: VkRayTracingShaderGroupCreateInfoNV))
                                                                                   <*> pure (vkAnyHitShader (c :: VkRayTracingShaderGroupCreateInfoNV))
                                                                                   <*> pure (vkIntersectionShader (c :: VkRayTracingShaderGroupCreateInfoNV))

instance Zero RayTracingShaderGroupCreateInfoNV where
  zero = RayTracingShaderGroupCreateInfoNV Nothing
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero


-- | VkRayTracingShaderGroupTypeNV - Shader group types
--
-- = Description
--
-- __Note__
--
-- For current group types, the hit group type could be inferred from the
-- presence or absence of the intersection shader, but we provide the type
-- explicitly for future hit groups that do not have that property.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingShaderGroupCreateInfoNV'
type RayTracingShaderGroupTypeNV = VkRayTracingShaderGroupTypeNV


{-# complete RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV, RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV, RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV :: RayTracingShaderGroupTypeNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV'
-- indicates a shader group with a single
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_STAGE_RAYGEN_BIT_NV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_STAGE_MISS_BIT_NV',
-- or
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_SHADER_STAGE_CALLABLE_BIT_NV'
-- shader in it.
pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV :: (a ~ RayTracingShaderGroupTypeNV) => a
pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV = VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV'
-- specifies a shader group that only hits triangles and /must/ not contain
-- an intersection shader, only closest hit and any-hit.
pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV :: (a ~ RayTracingShaderGroupTypeNV) => a
pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV = VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV'
-- specifies a shader group that only intersects with custom geometry and
-- /must/ contain an intersection shader and /may/ contain closest hit and
-- any-hit shaders.
pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV :: (a ~ RayTracingShaderGroupTypeNV) => a
pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV = VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV


-- | VkWriteDescriptorSetAccelerationStructureNV - Structure specifying
-- acceleration to query for memory requirements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data WriteDescriptorSetAccelerationStructureNV = WriteDescriptorSetAccelerationStructureNV
  { -- Univalued member elided
  -- No documentation found for Nested "WriteDescriptorSetAccelerationStructureNV" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "WriteDescriptorSetAccelerationStructureNV" "pAccelerationStructures"
  accelerationStructures :: Vector AccelerationStructureNV
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkWriteDescriptorSetAccelerationStructureNV' and
-- marshal a 'WriteDescriptorSetAccelerationStructureNV' into it. The 'VkWriteDescriptorSetAccelerationStructureNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructWriteDescriptorSetAccelerationStructureNV :: WriteDescriptorSetAccelerationStructureNV -> (VkWriteDescriptorSetAccelerationStructureNV -> IO a) -> IO a
withCStructWriteDescriptorSetAccelerationStructureNV marshalled cont = withVec (&) (accelerationStructures (marshalled :: WriteDescriptorSetAccelerationStructureNV)) (\pPAccelerationStructures -> maybeWith withSomeVkStruct (next (marshalled :: WriteDescriptorSetAccelerationStructureNV)) (\pPNext -> cont (VkWriteDescriptorSetAccelerationStructureNV VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV pPNext (fromIntegral (Data.Vector.length (accelerationStructures (marshalled :: WriteDescriptorSetAccelerationStructureNV)))) pPAccelerationStructures)))

-- | A function to read a 'VkWriteDescriptorSetAccelerationStructureNV' and all additional
-- structures in the pointer chain into a 'WriteDescriptorSetAccelerationStructureNV'.
fromCStructWriteDescriptorSetAccelerationStructureNV :: VkWriteDescriptorSetAccelerationStructureNV -> IO WriteDescriptorSetAccelerationStructureNV
fromCStructWriteDescriptorSetAccelerationStructureNV c = WriteDescriptorSetAccelerationStructureNV <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWriteDescriptorSetAccelerationStructureNV)))
                                                                                                   -- Length valued member elided
                                                                                                   <*> (Data.Vector.generateM (fromIntegral (vkAccelerationStructureCount (c :: VkWriteDescriptorSetAccelerationStructureNV))) (peekElemOff (vkPAccelerationStructures (c :: VkWriteDescriptorSetAccelerationStructureNV))))

instance Zero WriteDescriptorSetAccelerationStructureNV where
  zero = WriteDescriptorSetAccelerationStructureNV Nothing
                                                   Data.Vector.empty



-- | vkBindAccelerationStructureMemoryNV - Bind acceleration structure memory
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the acceleration structures
--     and memory.
--
-- -   @bindInfoCount@ is the number of elements in @pBindInfos@.
--
-- -   @pBindInfos@ is a pointer to an array of structures of type
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkBindAccelerationStructureMemoryInfoNV',
--     describing images and memory to bind.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkBindAccelerationStructureMemoryInfoNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
bindAccelerationStructureMemoryNV :: Device ->  Vector BindAccelerationStructureMemoryInfoNV ->  IO ()
bindAccelerationStructureMemoryNV = \(Device device' commandTable) -> \bindInfos' -> withVec withCStructBindAccelerationStructureMemoryInfoNV bindInfos' (\pBindInfos' -> vkBindAccelerationStructureMemoryNV commandTable device' (fromIntegral $ Data.Vector.length bindInfos') pBindInfos' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))


-- | vkCmdBuildAccelerationStructureNV - Build an acceleration structure
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pInfo@ contains the shared information for the acceleration
--     structure’s structure.
--
-- -   @instanceData@ is the buffer containing instance data that will be
--     used to build the acceleration structure as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#acceleration-structure-instance Accelerator structure instances.>
--     This parameter /must/ be @NULL@ for bottom level acceleration
--     structures.
--
-- -   @instanceOffset@ is the offset in bytes (relative to the start of
--     @instanceData@) at which the instance data is located.
--
-- -   @update@ specifies whether to update the @dst@ acceleration
--     structure with the data in @src@.
--
-- -   @dst@ points to the target acceleration structure for the build.
--
-- -   @src@ points to an existing acceleration structure that is to be
--     used to update the @dst@ acceleration structure.
--
-- -   @scratch@ is the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' that will be
--     used as scratch memory for the build.
--
-- -   @scratchOffset@ is the offset in bytes relative to the start of
--     @scratch@ that will be used as a scratch memory.
--
-- == Valid Usage
--
-- -   @geometryCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@maxGeometryCount@
--
-- -   @dst@ /must/ have been created with compatible
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV'
--     where
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV'::@type@
--     and
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV'::@flags@
--     are identical,
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV'::@instanceCount@
--     and
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV'::@geometryCount@
--     for @dst@ are greater than or equal to the build size and each
--     geometry in
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV'::@pGeometries@
--     for @dst@ has greater than or equal to the number of vertices,
--     indices, and AABBs.
--
-- -   If @update@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @src@ /must/
--     not be 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @update@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @src@ /must/
--     have been built before with
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV'
--     set in
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV'::@flags@
--
-- -   If @update@ is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', The @size@
--     member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkGetAccelerationStructureMemoryRequirementsNV'
--     with
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureMemoryRequirementsInfoNV'::@accelerationStructure@
--     set to @dst@ and
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureMemoryRequirementsInfoNV'::@type@
--     set to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV'
--     /must/ be less than or equal to the size of @scratch@ minus
--     @scratchOffset@
--
-- -   If @update@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', The @size@
--     member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkGetAccelerationStructureMemoryRequirementsNV'
--     with
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureMemoryRequirementsInfoNV'::@accelerationStructure@
--     set to @dst@ and
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureMemoryRequirementsInfoNV'::@type@
--     set to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV'
--     /must/ be less than or equal to the size of @scratch@ minus
--     @scratchOffset@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV'
--     structure
--
-- -   If @instanceData@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @instanceData@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @dst@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV'
--     handle
--
-- -   If @src@ is not 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @src@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV'
--     handle
--
-- -   @scratch@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   Each of @commandBuffer@, @dst@, @instanceData@, @scratch@, and @src@
--     that are valid handles /must/ have been created, allocated, or
--     retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Compute         |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
cmdBuildAccelerationStructureNV :: CommandBuffer ->  AccelerationStructureInfoNV ->  Buffer ->  DeviceSize ->  Bool ->  AccelerationStructureNV ->  AccelerationStructureNV ->  Buffer ->  DeviceSize ->  IO ()
cmdBuildAccelerationStructureNV = \(CommandBuffer commandBuffer' commandTable) -> \info' -> \instanceData' -> \instanceOffset' -> \update' -> \dst' -> \src' -> \scratch' -> \scratchOffset' -> (\marshalled -> withCStructAccelerationStructureInfoNV marshalled . flip with) info' (\pInfo' -> vkCmdBuildAccelerationStructureNV commandTable commandBuffer' pInfo' instanceData' instanceOffset' (boolToBool32 update') dst' src' scratch' scratchOffset' *> (pure ()))


-- | vkCmdCopyAccelerationStructureNV - Copy an acceleration structure
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @dst@ points to the target acceleration structure for the copy.
--
-- -   @src@ points to the source acceleration structure for the copy.
--
-- -   @mode@ is a
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkCopyAccelerationStructureModeNV'
--     value that specifies additional operations to perform during the
--     copy.
--
-- == Valid Usage
--
-- -   @mode@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV'
--
-- -   @src@ /must/ have been built with
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV'
--     if @mode@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @dst@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV'
--     handle
--
-- -   @src@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV'
--     handle
--
-- -   @mode@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkCopyAccelerationStructureModeNV'
--     value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   Each of @commandBuffer@, @dst@, and @src@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Compute         |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkCopyAccelerationStructureModeNV'
cmdCopyAccelerationStructureNV :: CommandBuffer ->  AccelerationStructureNV ->  AccelerationStructureNV ->  CopyAccelerationStructureModeNV ->  IO ()
cmdCopyAccelerationStructureNV = \(CommandBuffer commandBuffer' commandTable) -> \dst' -> \src' -> \mode' -> vkCmdCopyAccelerationStructureNV commandTable commandBuffer' dst' src' mode' *> (pure ())


-- | vkCmdTraceRaysNV - Initialize a ray tracing dispatch
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @raygenShaderBindingTableBuffer@ is the buffer object that holds the
--     shader binding table data for the ray generation shader stage.
--
-- -   @raygenShaderBindingOffset@ is the offset in bytes (relative to
--     @raygenShaderBindingTableBuffer@) of the ray generation shader being
--     used for the trace.
--
-- -   @missShaderBindingTableBuffer@ is the buffer object that holds the
--     shader binding table data for the miss shader stage.
--
-- -   @missShaderBindingOffset@ is the offset in bytes (relative to
--     @missShaderBindingTableBuffer@) of the miss shader being used for
--     the trace.
--
-- -   @missShaderBindingStride@ is the size in bytes of each shader
--     binding table record in @missShaderBindingTableBuffer@.
--
-- -   @hitShaderBindingTableBuffer@ is the buffer object that holds the
--     shader binding table data for the hit shader stages.
--
-- -   @hitShaderBindingOffset@ is the offset in bytes (relative to
--     @hitShaderBindingTableBuffer@) of the hit shader group being used
--     for the trace.
--
-- -   @hitShaderBindingStride@ is the size in bytes of each shader binding
--     table record in @hitShaderBindingTableBuffer@.
--
-- -   @callableShaderBindingTableBuffer@ is the buffer object that holds
--     the shader binding table data for the callable shader stage.
--
-- -   @callableShaderBindingOffset@ is the offset in bytes (relative to
--     @callableShaderBindingTableBuffer@) of the callable shader being
--     used for the trace.
--
-- -   @callableShaderBindingStride@ is the size in bytes of each shader
--     binding table record in @callableShaderBindingTableBuffer@.
--
-- -   @width@ is the width of the ray trace query dimensions.
--
-- -   @height@ is height of the ray trace query dimensions.
--
-- -   @depth@ is depth of the ray trace query dimensions.
--
-- = Description
--
-- When the command is executed, a ray generation group of @width@ ×
-- @height@ × @depth@ rays is assembled.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   @raygenShaderBindingOffset@ /must/ be less than the size of
--     @raygenShaderBindingTableBuffer@
--
-- -   @raygenShaderBindingOffset@ /must/ be a multiple of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @missShaderBindingOffset@ /must/ be less than the size of
--     @missShaderBindingTableBuffer@
--
-- -   @missShaderBindingOffset@ /must/ be a multiple of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @hitShaderBindingOffset@ /must/ be less than the size of
--     @hitShaderBindingTableBuffer@
--
-- -   @hitShaderBindingOffset@ /must/ be a multiple of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @callableShaderBindingOffset@ /must/ be less than the size of
--     @callableShaderBindingTableBuffer@
--
-- -   @callableShaderBindingOffset@ /must/ be a multiple of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   @missShaderBindingStride@ /must/ be a multiple of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   @hitShaderBindingStride@ /must/ be a multiple of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   @callableShaderBindingStride@ /must/ be a multiple of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   @missShaderBindingStride@ /must/ be a less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   @hitShaderBindingStride@ /must/ be a less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   @callableShaderBindingStride@ /must/ be a less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   @width@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   @height@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   @depth@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @raygenShaderBindingTableBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   If @missShaderBindingTableBuffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @missShaderBindingTableBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   If @hitShaderBindingTableBuffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @hitShaderBindingTableBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   If @callableShaderBindingTableBuffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @callableShaderBindingTableBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   Each of @callableShaderBindingTableBuffer@, @commandBuffer@,
--     @hitShaderBindingTableBuffer@, @missShaderBindingTableBuffer@, and
--     @raygenShaderBindingTableBuffer@ that are valid handles /must/ have
--     been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Compute         |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
cmdTraceRaysNV :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  DeviceSize ->  Buffer ->  DeviceSize ->  DeviceSize ->  Buffer ->  DeviceSize ->  DeviceSize ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdTraceRaysNV = \(CommandBuffer commandBuffer' commandTable) -> \raygenShaderBindingTableBuffer' -> \raygenShaderBindingOffset' -> \missShaderBindingTableBuffer' -> \missShaderBindingOffset' -> \missShaderBindingStride' -> \hitShaderBindingTableBuffer' -> \hitShaderBindingOffset' -> \hitShaderBindingStride' -> \callableShaderBindingTableBuffer' -> \callableShaderBindingOffset' -> \callableShaderBindingStride' -> \width' -> \height' -> \depth' -> vkCmdTraceRaysNV commandTable commandBuffer' raygenShaderBindingTableBuffer' raygenShaderBindingOffset' missShaderBindingTableBuffer' missShaderBindingOffset' missShaderBindingStride' hitShaderBindingTableBuffer' hitShaderBindingOffset' hitShaderBindingStride' callableShaderBindingTableBuffer' callableShaderBindingOffset' callableShaderBindingStride' width' height' depth' *> (pure ())


-- | vkCmdWriteAccelerationStructuresPropertiesNV - Write acceleration
-- structure result parameters to query results.
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @accelerationStructureCount@ is the count of acceleration structures
--     for which to query the property.
--
-- -   @pAccelerationStructures@ points to an array of existing previously
--     built acceleration structures.
--
-- -   @queryType@ is a 'Graphics.Vulkan.C.Core10.Query.VkQueryType' value
--     specifying the type of queries managed by the pool.
--
-- -   @queryPool@ is the query pool that will manage the results of the
--     query.
--
-- -   @firstQuery@ is the first query index within the query pool that
--     will contain the @accelerationStructureCount@ number of results.
--
-- == Valid Usage
--
-- -   @queryType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV'
--
-- -   @queryPool@ /must/ have been created with a @queryType@ matching
--     @queryType@
--
-- -   The queries identified by @queryPool@ and @firstQuery@ /must/ be
--     /unavailable/
--
-- -   All acceleration structures in @accelerationStructures@ /must/ have
--     been built with
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV'
--     if @queryType@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pAccelerationStructures@ /must/ be a valid pointer to an array of
--     @accelerationStructureCount@ valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV'
--     handles
--
-- -   @queryType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Query.VkQueryType' value
--
-- -   @queryPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPool' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   @accelerationStructureCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @queryPool@, and the elements of
--     @pAccelerationStructures@ /must/ have been created, allocated, or
--     retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Compute         |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryType'
cmdWriteAccelerationStructuresPropertiesNV :: CommandBuffer ->  Vector AccelerationStructureNV ->  QueryType ->  QueryPool ->  Word32 ->  IO ()
cmdWriteAccelerationStructuresPropertiesNV = \(CommandBuffer commandBuffer' commandTable) -> \accelerationStructures' -> \queryType' -> \queryPool' -> \firstQuery' -> withVec (&) accelerationStructures' (\pAccelerationStructures' -> vkCmdWriteAccelerationStructuresPropertiesNV commandTable commandBuffer' (fromIntegral $ Data.Vector.length accelerationStructures') pAccelerationStructures' queryType' queryPool' firstQuery' *> (pure ()))


-- | vkCompileDeferredNV - Deferred compilation of shaders
--
-- = Parameters
--
-- -   @device@ is the logical device that contains the ray tracing
--     pipeline.
--
-- -   @pipeline@ is the ray tracing pipeline object that contains the
--     shaders.
--
-- -   @shader@ is the index of the shader to compile.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline'
compileDeferredNV :: Device ->  Pipeline ->  Word32 ->  IO ()
compileDeferredNV = \(Device device' commandTable) -> \pipeline' -> \shader' -> vkCompileDeferredNV commandTable device' pipeline' shader' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))


-- | vkCreateAccelerationStructureNV - Create a new acceleration structure
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the buffer object.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureCreateInfoNV'
--     structure containing parameters affecting creation of the
--     acceleration structure.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pAccelerationStructure@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV'
--     handle in which the resulting acceleration structure object is
--     returned.
--
-- = Description
--
-- Similar to other objects in Vulkan, the acceleration structure creation
-- merely creates an object with a specific “shape” as specified by the
-- information in
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureInfoNV'
-- and @compactedSize@ in @pCreateInfo@. Populating the data in the object
-- after allocating and binding memory is done with
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdBuildAccelerationStructureNV'
-- and
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdCopyAccelerationStructureNV'.
--
-- Acceleration structure creation uses the count and type information from
-- the geometries, but does not use the data references in the structures.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureCreateInfoNV'
--     structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pAccelerationStructure@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV'
--     handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureCreateInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
createAccelerationStructureNV :: Device ->  AccelerationStructureCreateInfoNV ->  Maybe AllocationCallbacks ->  IO (AccelerationStructureNV)
createAccelerationStructureNV = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pAccelerationStructure' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructAccelerationStructureCreateInfoNV marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateAccelerationStructureNV commandTable device' pCreateInfo' pAllocator pAccelerationStructure' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pAccelerationStructure')))))


-- | vkCreateRayTracingPipelinesNV - Creates a new ray tracing pipeline
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the ray tracing
--     pipelines.
--
-- -   @pipelineCache@ is either
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', indicating that
--     pipeline caching is disabled, or the handle of a valid
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#pipelines-cache pipeline cache>
--     object, in which case use of that cache is enabled for the duration
--     of the command.
--
-- -   @createInfoCount@ is the length of the @pCreateInfos@ and
--     @pPipelines@ arrays.
--
-- -   @pCreateInfos@ is an array of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV'
--     structures.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pPipelines@ is a pointer to an array in which the resulting ray
--     tracing pipeline objects are returned.
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @pipelineCache@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @pipelineCache@
--     /must/ be a valid
--     'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache' handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV'
--     structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline'
--     handles
--
-- -   @createInfoCount@ /must/ be greater than @0@
--
-- -   If @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_NV_glsl_shader.VK_ERROR_INVALID_SHADER_NV'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingPipelineCreateInfoNV'
createRayTracingPipelinesNV :: Device ->  PipelineCache ->  Vector RayTracingPipelineCreateInfoNV ->  Maybe AllocationCallbacks ->  IO (Vector Pipeline)
createRayTracingPipelinesNV = \(Device device' commandTable) -> \pipelineCache' -> \createInfos' -> \allocator -> allocaArray ((Data.Vector.length createInfos')) (\pPipelines' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> withVec withCStructRayTracingPipelineCreateInfoNV createInfos' (\pCreateInfos' -> vkCreateRayTracingPipelinesNV commandTable device' pipelineCache' (fromIntegral $ Data.Vector.length createInfos') pCreateInfos' pAllocator pPipelines' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((Data.Vector.generateM ((Data.Vector.length createInfos')) (peekElemOff pPipelines')))))))


-- | vkDestroyAccelerationStructureNV - Destroy an acceleration structure
-- object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the buffer.
--
-- -   @accelerationStructure@ is the acceleration structure to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @accelerationStructure@ /must/
--     have completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @accelerationStructure@ was created, a compatible
--     set of callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @accelerationStructure@ was created, @pAllocator@
--     /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @accelerationStructure@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @accelerationStructure@ /must/ have been created, allocated, or
--     retrieved from @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
destroyAccelerationStructureNV :: Device ->  AccelerationStructureNV ->  Maybe AllocationCallbacks ->  IO ()
destroyAccelerationStructureNV = \(Device device' commandTable) -> \accelerationStructure' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyAccelerationStructureNV commandTable device' accelerationStructure' pAllocator *> (pure ()))


-- | vkGetAccelerationStructureHandleNV - Get opaque acceleration structure
-- handle
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the acceleration
--     structures.
--
-- -   @accelerationStructure@ is the acceleration structure.
--
-- -   @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
--
-- -   @pData@ is a pointer to a user-allocated buffer where the results
--     will be written.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
getAccelerationStructureHandleNV :: Device ->  AccelerationStructureNV ->  CSize ->  IO (ByteString)
getAccelerationStructureHandleNV = \(Device device' commandTable) -> \accelerationStructure' -> \dataSize' -> allocaArray (fromIntegral dataSize') (\pData' -> vkGetAccelerationStructureHandleNV commandTable device' accelerationStructure' dataSize' (castPtr pData') >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((packCStringLen (pData', (fromIntegral dataSize'))))))


-- | vkGetAccelerationStructureMemoryRequirementsNV - Get acceleration
-- structure memory requirements
--
-- = Parameters
--
-- -   @device@ is the logical device on which the acceleration structure
--     was created.
--
-- -   @pInfo@ specifies the acceleration structure to get memory
--     requirements for.
--
-- -   @pMemoryRequirements@ returns the requested acceleration structure
--     memory requirements.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureMemoryRequirementsInfoNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2KHR'
getAccelerationStructureMemoryRequirementsNV :: Device ->  AccelerationStructureMemoryRequirementsInfoNV ->  IO (MemoryRequirements2)
getAccelerationStructureMemoryRequirementsNV = \(Device device' commandTable) -> \info' -> alloca (\pMemoryRequirements' -> (\marshalled -> withCStructAccelerationStructureMemoryRequirementsInfoNV marshalled . flip with) info' (\pInfo' -> vkGetAccelerationStructureMemoryRequirementsNV commandTable device' pInfo' pMemoryRequirements' *> ((fromCStructMemoryRequirements2 <=< peek) pMemoryRequirements')))


-- | vkGetRayTracingShaderGroupHandlesNV - Query ray tracing pipeline shader
-- group handles
--
-- = Parameters
--
-- -   @device@ is the logical device that contains the ray tracing
--     pipeline.
--
-- -   @pipeline@ is the ray tracing pipeline object that contains the
--     shaders.
--
-- -   @firstGroup@ is the index of the first group to retrieve a handle
--     for from the
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkRayTracingShaderGroupCreateInfoNV'::@pGroups@
--     array.
--
-- -   @groupCount@ is the number of shader handles to retrieve.
--
-- -   @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
--
-- -   @pData@ is a pointer to a user-allocated buffer where the results
--     will be written.
--
-- == Valid Usage
--
-- -   The sum of @firstGroup@ and @groupCount@ /must/ be less than the
--     number of shader groups in @pipeline@.
--
-- -   @dataSize@ /must/ be at least
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--     × @groupCount@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pipeline@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' handle
--
-- -   @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   @dataSize@ /must/ be greater than @0@
--
-- -   @pipeline@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline'
getRayTracingShaderGroupHandlesNV :: Device ->  Pipeline ->  Word32 ->  Word32 ->  CSize ->  IO (ByteString)
getRayTracingShaderGroupHandlesNV = \(Device device' commandTable) -> \pipeline' -> \firstGroup' -> \groupCount' -> \dataSize' -> allocaArray (fromIntegral dataSize') (\pData' -> vkGetRayTracingShaderGroupHandlesNV commandTable device' pipeline' firstGroup' groupCount' dataSize' (castPtr pData') >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((packCStringLen (pData', (fromIntegral dataSize'))))))

-- | A safe wrapper for 'createAccelerationStructureNV' and 'destroyAccelerationStructureNV' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withAccelerationStructureNV
  :: Device -> AccelerationStructureCreateInfoNV -> Maybe (AllocationCallbacks) -> (AccelerationStructureNV -> IO a) -> IO a
withAccelerationStructureNV device accelerationStructureCreateInfoNV allocationCallbacks = bracket
  (createAccelerationStructureNV device accelerationStructureCreateInfoNV allocationCallbacks)
  (\o -> destroyAccelerationStructureNV device o allocationCallbacks)

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_EXTENSION_NAME"
pattern NV_RAY_TRACING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_RAY_TRACING_EXTENSION_NAME = VK_NV_RAY_TRACING_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_SPEC_VERSION"
pattern NV_RAY_TRACING_SPEC_VERSION :: Integral a => a
pattern NV_RAY_TRACING_SPEC_VERSION = VK_NV_RAY_TRACING_SPEC_VERSION
