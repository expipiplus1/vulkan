{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

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
  , AccelerationStructureNV
  , AccelerationStructureTypeNV
  , withCStructBindAccelerationStructureMemoryInfoNV
  , fromCStructBindAccelerationStructureMemoryInfoNV
  , BindAccelerationStructureMemoryInfoNV(..)
  , BuildAccelerationStructureFlagBitsNV
  , BuildAccelerationStructureFlagsNV
  , CopyAccelerationStructureModeNV
  , withCStructGeometryAABBNV
  , fromCStructGeometryAABBNV
  , GeometryAABBNV(..)
  , withCStructGeometryDataNV
  , fromCStructGeometryDataNV
  , GeometryDataNV(..)
  , GeometryFlagBitsNV
  , GeometryFlagsNV
  , GeometryInstanceFlagBitsNV
  , GeometryInstanceFlagsNV
  , withCStructGeometryNV
  , fromCStructGeometryNV
  , GeometryNV(..)
  , withCStructGeometryTrianglesNV
  , fromCStructGeometryTrianglesNV
  , GeometryTrianglesNV(..)
  , GeometryTypeNV
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
  , getRayTracingShaderGroupHandlesNV
  , pattern VK_NV_RAY_TRACING_SPEC_VERSION
  , pattern VK_NV_RAY_TRACING_EXTENSION_NAME
  , pattern VK_SHADER_UNUSED_NV
  , pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_GEOMETRY_NV
  , pattern VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV
  , pattern VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV
  , pattern VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV
  , pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV
  , pattern VK_SHADER_STAGE_RAYGEN_BIT_NV
  , pattern VK_SHADER_STAGE_ANY_HIT_BIT_NV
  , pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV
  , pattern VK_SHADER_STAGE_MISS_BIT_NV
  , pattern VK_SHADER_STAGE_INTERSECTION_BIT_NV
  , pattern VK_SHADER_STAGE_CALLABLE_BIT_NV
  , pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV
  , pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV
  , pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_NV
  , pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_NV
  , pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
  , pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV
  , pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV
  , pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV
  , pattern VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV
  , pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT
  , pattern VK_INDEX_TYPE_NONE_NV
  , MemoryRequirements2KHR
  ) where

import Control.Exception
  ( throwIO
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
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
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
import qualified Graphics.Vulkan.C.Dynamic
  ( bindAccelerationStructureMemoryNV
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
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
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
  ( MemoryRequirements2KHR
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
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( pattern VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV
  , pattern VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV
  , pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_NV
  , pattern VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT
  , pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
  , pattern VK_INDEX_TYPE_NONE_NV
  , pattern VK_NV_RAY_TRACING_EXTENSION_NAME
  , pattern VK_NV_RAY_TRACING_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV
  , pattern VK_PIPELINE_BIND_POINT_RAY_TRACING_NV
  , pattern VK_PIPELINE_CREATE_DEFER_COMPILE_BIT_NV
  , pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV
  , pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV
  , pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV
  , pattern VK_SHADER_STAGE_ANY_HIT_BIT_NV
  , pattern VK_SHADER_STAGE_CALLABLE_BIT_NV
  , pattern VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV
  , pattern VK_SHADER_STAGE_INTERSECTION_BIT_NV
  , pattern VK_SHADER_STAGE_MISS_BIT_NV
  , pattern VK_SHADER_STAGE_RAYGEN_BIT_NV
  , pattern VK_SHADER_UNUSED_NV
  )


-- No documentation found for TopLevel "AccelerationStructureCreateInfoNV"
data AccelerationStructureCreateInfoNV = AccelerationStructureCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "AccelerationStructureCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AccelerationStructureCreateInfoNV" "compactedSize"
  vkCompactedSize :: DeviceSize
  , -- No documentation found for Nested "AccelerationStructureCreateInfoNV" "info"
  vkInfo :: AccelerationStructureInfoNV
  }
  deriving (Show, Eq)
withCStructAccelerationStructureCreateInfoNV :: AccelerationStructureCreateInfoNV -> (VkAccelerationStructureCreateInfoNV -> IO a) -> IO a
withCStructAccelerationStructureCreateInfoNV from cont = withCStructAccelerationStructureInfoNV (vkInfo (from :: AccelerationStructureCreateInfoNV)) (\info -> maybeWith withSomeVkStruct (vkPNext (from :: AccelerationStructureCreateInfoNV)) (\pPNext -> cont (VkAccelerationStructureCreateInfoNV VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV pPNext (vkCompactedSize (from :: AccelerationStructureCreateInfoNV)) info)))
fromCStructAccelerationStructureCreateInfoNV :: VkAccelerationStructureCreateInfoNV -> IO AccelerationStructureCreateInfoNV
fromCStructAccelerationStructureCreateInfoNV c = AccelerationStructureCreateInfoNV <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAccelerationStructureCreateInfoNV)))
                                                                                   <*> pure (vkCompactedSize (c :: VkAccelerationStructureCreateInfoNV))
                                                                                   <*> (fromCStructAccelerationStructureInfoNV (vkInfo (c :: VkAccelerationStructureCreateInfoNV)))
-- No documentation found for TopLevel "AccelerationStructureInfoNV"
data AccelerationStructureInfoNV = AccelerationStructureInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "AccelerationStructureInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AccelerationStructureInfoNV" "type"
  vkType :: AccelerationStructureTypeNV
  , -- No documentation found for Nested "AccelerationStructureInfoNV" "flags"
  vkFlags :: BuildAccelerationStructureFlagsNV
  , -- No documentation found for Nested "AccelerationStructureInfoNV" "instanceCount"
  vkInstanceCount :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "AccelerationStructureInfoNV" "pGeometries"
  vkPGeometries :: Vector GeometryNV
  }
  deriving (Show, Eq)
withCStructAccelerationStructureInfoNV :: AccelerationStructureInfoNV -> (VkAccelerationStructureInfoNV -> IO a) -> IO a
withCStructAccelerationStructureInfoNV from cont = withVec withCStructGeometryNV (vkPGeometries (from :: AccelerationStructureInfoNV)) (\pGeometries -> maybeWith withSomeVkStruct (vkPNext (from :: AccelerationStructureInfoNV)) (\pPNext -> cont (VkAccelerationStructureInfoNV VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV pPNext (vkType (from :: AccelerationStructureInfoNV)) (vkFlags (from :: AccelerationStructureInfoNV)) (vkInstanceCount (from :: AccelerationStructureInfoNV)) (fromIntegral (Data.Vector.length (vkPGeometries (from :: AccelerationStructureInfoNV)))) pGeometries)))
fromCStructAccelerationStructureInfoNV :: VkAccelerationStructureInfoNV -> IO AccelerationStructureInfoNV
fromCStructAccelerationStructureInfoNV c = AccelerationStructureInfoNV <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAccelerationStructureInfoNV)))
                                                                       <*> pure (vkType (c :: VkAccelerationStructureInfoNV))
                                                                       <*> pure (vkFlags (c :: VkAccelerationStructureInfoNV))
                                                                       <*> pure (vkInstanceCount (c :: VkAccelerationStructureInfoNV))
                                                                       -- Length valued member elided
                                                                       <*> (Data.Vector.generateM (fromIntegral (vkGeometryCount (c :: VkAccelerationStructureInfoNV))) (((fromCStructGeometryNV <=<) . peekElemOff) (vkPGeometries (c :: VkAccelerationStructureInfoNV))))
-- No documentation found for TopLevel "AccelerationStructureMemoryRequirementsInfoNV"
data AccelerationStructureMemoryRequirementsInfoNV = AccelerationStructureMemoryRequirementsInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "AccelerationStructureMemoryRequirementsInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AccelerationStructureMemoryRequirementsInfoNV" "type"
  vkType :: AccelerationStructureMemoryRequirementsTypeNV
  , -- No documentation found for Nested "AccelerationStructureMemoryRequirementsInfoNV" "accelerationStructure"
  vkAccelerationStructure :: AccelerationStructureNV
  }
  deriving (Show, Eq)
withCStructAccelerationStructureMemoryRequirementsInfoNV :: AccelerationStructureMemoryRequirementsInfoNV -> (VkAccelerationStructureMemoryRequirementsInfoNV -> IO a) -> IO a
withCStructAccelerationStructureMemoryRequirementsInfoNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: AccelerationStructureMemoryRequirementsInfoNV)) (\pPNext -> cont (VkAccelerationStructureMemoryRequirementsInfoNV VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV pPNext (vkType (from :: AccelerationStructureMemoryRequirementsInfoNV)) (vkAccelerationStructure (from :: AccelerationStructureMemoryRequirementsInfoNV))))
fromCStructAccelerationStructureMemoryRequirementsInfoNV :: VkAccelerationStructureMemoryRequirementsInfoNV -> IO AccelerationStructureMemoryRequirementsInfoNV
fromCStructAccelerationStructureMemoryRequirementsInfoNV c = AccelerationStructureMemoryRequirementsInfoNV <$> -- Univalued Member elided
                                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAccelerationStructureMemoryRequirementsInfoNV)))
                                                                                                           <*> pure (vkType (c :: VkAccelerationStructureMemoryRequirementsInfoNV))
                                                                                                           <*> pure (vkAccelerationStructure (c :: VkAccelerationStructureMemoryRequirementsInfoNV))
-- No documentation found for TopLevel "AccelerationStructureMemoryRequirementsTypeNV"
type AccelerationStructureMemoryRequirementsTypeNV = VkAccelerationStructureMemoryRequirementsTypeNV
-- No documentation found for TopLevel "AccelerationStructureNV"
type AccelerationStructureNV = VkAccelerationStructureNV
-- No documentation found for TopLevel "AccelerationStructureTypeNV"
type AccelerationStructureTypeNV = VkAccelerationStructureTypeNV
-- No documentation found for TopLevel "BindAccelerationStructureMemoryInfoNV"
data BindAccelerationStructureMemoryInfoNV = BindAccelerationStructureMemoryInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "accelerationStructure"
  vkAccelerationStructure :: AccelerationStructureNV
  , -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "memory"
  vkMemory :: DeviceMemory
  , -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "memoryOffset"
  vkMemoryOffset :: DeviceSize
  -- Length valued member elided
  , -- No documentation found for Nested "BindAccelerationStructureMemoryInfoNV" "pDeviceIndices"
  vkPDeviceIndices :: Vector Word32
  }
  deriving (Show, Eq)
withCStructBindAccelerationStructureMemoryInfoNV :: BindAccelerationStructureMemoryInfoNV -> (VkBindAccelerationStructureMemoryInfoNV -> IO a) -> IO a
withCStructBindAccelerationStructureMemoryInfoNV from cont = withVec (&) (vkPDeviceIndices (from :: BindAccelerationStructureMemoryInfoNV)) (\pDeviceIndices -> maybeWith withSomeVkStruct (vkPNext (from :: BindAccelerationStructureMemoryInfoNV)) (\pPNext -> cont (VkBindAccelerationStructureMemoryInfoNV VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV pPNext (vkAccelerationStructure (from :: BindAccelerationStructureMemoryInfoNV)) (vkMemory (from :: BindAccelerationStructureMemoryInfoNV)) (vkMemoryOffset (from :: BindAccelerationStructureMemoryInfoNV)) (fromIntegral (Data.Vector.length (vkPDeviceIndices (from :: BindAccelerationStructureMemoryInfoNV)))) pDeviceIndices)))
fromCStructBindAccelerationStructureMemoryInfoNV :: VkBindAccelerationStructureMemoryInfoNV -> IO BindAccelerationStructureMemoryInfoNV
fromCStructBindAccelerationStructureMemoryInfoNV c = BindAccelerationStructureMemoryInfoNV <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindAccelerationStructureMemoryInfoNV)))
                                                                                           <*> pure (vkAccelerationStructure (c :: VkBindAccelerationStructureMemoryInfoNV))
                                                                                           <*> pure (vkMemory (c :: VkBindAccelerationStructureMemoryInfoNV))
                                                                                           <*> pure (vkMemoryOffset (c :: VkBindAccelerationStructureMemoryInfoNV))
                                                                                           -- Length valued member elided
                                                                                           <*> (Data.Vector.generateM (fromIntegral (vkDeviceIndexCount (c :: VkBindAccelerationStructureMemoryInfoNV))) (peekElemOff (vkPDeviceIndices (c :: VkBindAccelerationStructureMemoryInfoNV))))
-- No documentation found for TopLevel "BuildAccelerationStructureFlagBitsNV"
type BuildAccelerationStructureFlagBitsNV = VkBuildAccelerationStructureFlagBitsNV
-- No documentation found for TopLevel "BuildAccelerationStructureFlagsNV"
type BuildAccelerationStructureFlagsNV = BuildAccelerationStructureFlagBitsNV
-- No documentation found for TopLevel "CopyAccelerationStructureModeNV"
type CopyAccelerationStructureModeNV = VkCopyAccelerationStructureModeNV
-- No documentation found for TopLevel "GeometryAABBNV"
data GeometryAABBNV = GeometryAABBNV
  { -- Univalued Member elided
  -- No documentation found for Nested "GeometryAABBNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "GeometryAABBNV" "aabbData"
  vkAabbData :: Buffer
  , -- No documentation found for Nested "GeometryAABBNV" "numAABBs"
  vkNumAABBs :: Word32
  , -- No documentation found for Nested "GeometryAABBNV" "stride"
  vkStride :: Word32
  , -- No documentation found for Nested "GeometryAABBNV" "offset"
  vkOffset :: DeviceSize
  }
  deriving (Show, Eq)
withCStructGeometryAABBNV :: GeometryAABBNV -> (VkGeometryAABBNV -> IO a) -> IO a
withCStructGeometryAABBNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: GeometryAABBNV)) (\pPNext -> cont (VkGeometryAABBNV VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV pPNext (vkAabbData (from :: GeometryAABBNV)) (vkNumAABBs (from :: GeometryAABBNV)) (vkStride (from :: GeometryAABBNV)) (vkOffset (from :: GeometryAABBNV))))
fromCStructGeometryAABBNV :: VkGeometryAABBNV -> IO GeometryAABBNV
fromCStructGeometryAABBNV c = GeometryAABBNV <$> -- Univalued Member elided
                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkGeometryAABBNV)))
                                             <*> pure (vkAabbData (c :: VkGeometryAABBNV))
                                             <*> pure (vkNumAABBs (c :: VkGeometryAABBNV))
                                             <*> pure (vkStride (c :: VkGeometryAABBNV))
                                             <*> pure (vkOffset (c :: VkGeometryAABBNV))
-- No documentation found for TopLevel "GeometryDataNV"
data GeometryDataNV = GeometryDataNV
  { -- No documentation found for Nested "GeometryDataNV" "triangles"
  vkTriangles :: GeometryTrianglesNV
  , -- No documentation found for Nested "GeometryDataNV" "aabbs"
  vkAabbs :: GeometryAABBNV
  }
  deriving (Show, Eq)
withCStructGeometryDataNV :: GeometryDataNV -> (VkGeometryDataNV -> IO a) -> IO a
withCStructGeometryDataNV from cont = withCStructGeometryAABBNV (vkAabbs (from :: GeometryDataNV)) (\aabbs -> withCStructGeometryTrianglesNV (vkTriangles (from :: GeometryDataNV)) (\triangles -> cont (VkGeometryDataNV triangles aabbs)))
fromCStructGeometryDataNV :: VkGeometryDataNV -> IO GeometryDataNV
fromCStructGeometryDataNV c = GeometryDataNV <$> (fromCStructGeometryTrianglesNV (vkTriangles (c :: VkGeometryDataNV)))
                                             <*> (fromCStructGeometryAABBNV (vkAabbs (c :: VkGeometryDataNV)))
-- No documentation found for TopLevel "GeometryFlagBitsNV"
type GeometryFlagBitsNV = VkGeometryFlagBitsNV
-- No documentation found for TopLevel "GeometryFlagsNV"
type GeometryFlagsNV = GeometryFlagBitsNV
-- No documentation found for TopLevel "GeometryInstanceFlagBitsNV"
type GeometryInstanceFlagBitsNV = VkGeometryInstanceFlagBitsNV
-- No documentation found for TopLevel "GeometryInstanceFlagsNV"
type GeometryInstanceFlagsNV = GeometryInstanceFlagBitsNV
-- No documentation found for TopLevel "GeometryNV"
data GeometryNV = GeometryNV
  { -- Univalued Member elided
  -- No documentation found for Nested "GeometryNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "GeometryNV" "geometryType"
  vkGeometryType :: GeometryTypeNV
  , -- No documentation found for Nested "GeometryNV" "geometry"
  vkGeometry :: GeometryDataNV
  , -- No documentation found for Nested "GeometryNV" "flags"
  vkFlags :: GeometryFlagsNV
  }
  deriving (Show, Eq)
withCStructGeometryNV :: GeometryNV -> (VkGeometryNV -> IO a) -> IO a
withCStructGeometryNV from cont = withCStructGeometryDataNV (vkGeometry (from :: GeometryNV)) (\geometry -> maybeWith withSomeVkStruct (vkPNext (from :: GeometryNV)) (\pPNext -> cont (VkGeometryNV VK_STRUCTURE_TYPE_GEOMETRY_NV pPNext (vkGeometryType (from :: GeometryNV)) geometry (vkFlags (from :: GeometryNV)))))
fromCStructGeometryNV :: VkGeometryNV -> IO GeometryNV
fromCStructGeometryNV c = GeometryNV <$> -- Univalued Member elided
                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkGeometryNV)))
                                     <*> pure (vkGeometryType (c :: VkGeometryNV))
                                     <*> (fromCStructGeometryDataNV (vkGeometry (c :: VkGeometryNV)))
                                     <*> pure (vkFlags (c :: VkGeometryNV))
-- No documentation found for TopLevel "GeometryTrianglesNV"
data GeometryTrianglesNV = GeometryTrianglesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "GeometryTrianglesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "GeometryTrianglesNV" "vertexData"
  vkVertexData :: Buffer
  , -- No documentation found for Nested "GeometryTrianglesNV" "vertexOffset"
  vkVertexOffset :: DeviceSize
  , -- No documentation found for Nested "GeometryTrianglesNV" "vertexCount"
  vkVertexCount :: Word32
  , -- No documentation found for Nested "GeometryTrianglesNV" "vertexStride"
  vkVertexStride :: DeviceSize
  , -- No documentation found for Nested "GeometryTrianglesNV" "vertexFormat"
  vkVertexFormat :: Format
  , -- No documentation found for Nested "GeometryTrianglesNV" "indexData"
  vkIndexData :: Buffer
  , -- No documentation found for Nested "GeometryTrianglesNV" "indexOffset"
  vkIndexOffset :: DeviceSize
  , -- No documentation found for Nested "GeometryTrianglesNV" "indexCount"
  vkIndexCount :: Word32
  , -- No documentation found for Nested "GeometryTrianglesNV" "indexType"
  vkIndexType :: IndexType
  , -- No documentation found for Nested "GeometryTrianglesNV" "transformData"
  vkTransformData :: Buffer
  , -- No documentation found for Nested "GeometryTrianglesNV" "transformOffset"
  vkTransformOffset :: DeviceSize
  }
  deriving (Show, Eq)
withCStructGeometryTrianglesNV :: GeometryTrianglesNV -> (VkGeometryTrianglesNV -> IO a) -> IO a
withCStructGeometryTrianglesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: GeometryTrianglesNV)) (\pPNext -> cont (VkGeometryTrianglesNV VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV pPNext (vkVertexData (from :: GeometryTrianglesNV)) (vkVertexOffset (from :: GeometryTrianglesNV)) (vkVertexCount (from :: GeometryTrianglesNV)) (vkVertexStride (from :: GeometryTrianglesNV)) (vkVertexFormat (from :: GeometryTrianglesNV)) (vkIndexData (from :: GeometryTrianglesNV)) (vkIndexOffset (from :: GeometryTrianglesNV)) (vkIndexCount (from :: GeometryTrianglesNV)) (vkIndexType (from :: GeometryTrianglesNV)) (vkTransformData (from :: GeometryTrianglesNV)) (vkTransformOffset (from :: GeometryTrianglesNV))))
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
-- No documentation found for TopLevel "GeometryTypeNV"
type GeometryTypeNV = VkGeometryTypeNV
-- No documentation found for TopLevel "PhysicalDeviceRayTracingPropertiesNV"
data PhysicalDeviceRayTracingPropertiesNV = PhysicalDeviceRayTracingPropertiesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "shaderGroupHandleSize"
  vkShaderGroupHandleSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "maxRecursionDepth"
  vkMaxRecursionDepth :: Word32
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "maxShaderGroupStride"
  vkMaxShaderGroupStride :: Word32
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "shaderGroupBaseAlignment"
  vkShaderGroupBaseAlignment :: Word32
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "maxGeometryCount"
  vkMaxGeometryCount :: Word64
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "maxInstanceCount"
  vkMaxInstanceCount :: Word64
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "maxTriangleCount"
  vkMaxTriangleCount :: Word64
  , -- No documentation found for Nested "PhysicalDeviceRayTracingPropertiesNV" "maxDescriptorSetAccelerationStructures"
  vkMaxDescriptorSetAccelerationStructures :: Word32
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceRayTracingPropertiesNV :: PhysicalDeviceRayTracingPropertiesNV -> (VkPhysicalDeviceRayTracingPropertiesNV -> IO a) -> IO a
withCStructPhysicalDeviceRayTracingPropertiesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceRayTracingPropertiesNV)) (\pPNext -> cont (VkPhysicalDeviceRayTracingPropertiesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV pPNext (vkShaderGroupHandleSize (from :: PhysicalDeviceRayTracingPropertiesNV)) (vkMaxRecursionDepth (from :: PhysicalDeviceRayTracingPropertiesNV)) (vkMaxShaderGroupStride (from :: PhysicalDeviceRayTracingPropertiesNV)) (vkShaderGroupBaseAlignment (from :: PhysicalDeviceRayTracingPropertiesNV)) (vkMaxGeometryCount (from :: PhysicalDeviceRayTracingPropertiesNV)) (vkMaxInstanceCount (from :: PhysicalDeviceRayTracingPropertiesNV)) (vkMaxTriangleCount (from :: PhysicalDeviceRayTracingPropertiesNV)) (vkMaxDescriptorSetAccelerationStructures (from :: PhysicalDeviceRayTracingPropertiesNV))))
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
-- No documentation found for TopLevel "RayTracingPipelineCreateInfoNV"
data RayTracingPipelineCreateInfoNV = RayTracingPipelineCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "flags"
  vkFlags :: PipelineCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "pStages"
  vkPStages :: Vector PipelineShaderStageCreateInfo
  -- Length valued member elided
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "pGroups"
  vkPGroups :: Vector RayTracingShaderGroupCreateInfoNV
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "maxRecursionDepth"
  vkMaxRecursionDepth :: Word32
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "layout"
  vkLayout :: PipelineLayout
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "basePipelineHandle"
  vkBasePipelineHandle :: Pipeline
  , -- No documentation found for Nested "RayTracingPipelineCreateInfoNV" "basePipelineIndex"
  vkBasePipelineIndex :: Int32
  }
  deriving (Show, Eq)
withCStructRayTracingPipelineCreateInfoNV :: RayTracingPipelineCreateInfoNV -> (VkRayTracingPipelineCreateInfoNV -> IO a) -> IO a
withCStructRayTracingPipelineCreateInfoNV from cont = withVec withCStructRayTracingShaderGroupCreateInfoNV (vkPGroups (from :: RayTracingPipelineCreateInfoNV)) (\pGroups -> withVec withCStructPipelineShaderStageCreateInfo (vkPStages (from :: RayTracingPipelineCreateInfoNV)) (\pStages -> maybeWith withSomeVkStruct (vkPNext (from :: RayTracingPipelineCreateInfoNV)) (\pPNext -> cont (VkRayTracingPipelineCreateInfoNV VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV pPNext (vkFlags (from :: RayTracingPipelineCreateInfoNV)) (fromIntegral (Data.Vector.length (vkPStages (from :: RayTracingPipelineCreateInfoNV)))) pStages (fromIntegral (Data.Vector.length (vkPGroups (from :: RayTracingPipelineCreateInfoNV)))) pGroups (vkMaxRecursionDepth (from :: RayTracingPipelineCreateInfoNV)) (vkLayout (from :: RayTracingPipelineCreateInfoNV)) (vkBasePipelineHandle (from :: RayTracingPipelineCreateInfoNV)) (vkBasePipelineIndex (from :: RayTracingPipelineCreateInfoNV))))))
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
-- No documentation found for TopLevel "RayTracingShaderGroupCreateInfoNV"
data RayTracingShaderGroupCreateInfoNV = RayTracingShaderGroupCreateInfoNV
  { -- Univalued Member elided
  -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "type"
  vkType :: RayTracingShaderGroupTypeNV
  , -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "generalShader"
  vkGeneralShader :: Word32
  , -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "closestHitShader"
  vkClosestHitShader :: Word32
  , -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "anyHitShader"
  vkAnyHitShader :: Word32
  , -- No documentation found for Nested "RayTracingShaderGroupCreateInfoNV" "intersectionShader"
  vkIntersectionShader :: Word32
  }
  deriving (Show, Eq)
withCStructRayTracingShaderGroupCreateInfoNV :: RayTracingShaderGroupCreateInfoNV -> (VkRayTracingShaderGroupCreateInfoNV -> IO a) -> IO a
withCStructRayTracingShaderGroupCreateInfoNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: RayTracingShaderGroupCreateInfoNV)) (\pPNext -> cont (VkRayTracingShaderGroupCreateInfoNV VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV pPNext (vkType (from :: RayTracingShaderGroupCreateInfoNV)) (vkGeneralShader (from :: RayTracingShaderGroupCreateInfoNV)) (vkClosestHitShader (from :: RayTracingShaderGroupCreateInfoNV)) (vkAnyHitShader (from :: RayTracingShaderGroupCreateInfoNV)) (vkIntersectionShader (from :: RayTracingShaderGroupCreateInfoNV))))
fromCStructRayTracingShaderGroupCreateInfoNV :: VkRayTracingShaderGroupCreateInfoNV -> IO RayTracingShaderGroupCreateInfoNV
fromCStructRayTracingShaderGroupCreateInfoNV c = RayTracingShaderGroupCreateInfoNV <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRayTracingShaderGroupCreateInfoNV)))
                                                                                   <*> pure (vkType (c :: VkRayTracingShaderGroupCreateInfoNV))
                                                                                   <*> pure (vkGeneralShader (c :: VkRayTracingShaderGroupCreateInfoNV))
                                                                                   <*> pure (vkClosestHitShader (c :: VkRayTracingShaderGroupCreateInfoNV))
                                                                                   <*> pure (vkAnyHitShader (c :: VkRayTracingShaderGroupCreateInfoNV))
                                                                                   <*> pure (vkIntersectionShader (c :: VkRayTracingShaderGroupCreateInfoNV))
-- No documentation found for TopLevel "RayTracingShaderGroupTypeNV"
type RayTracingShaderGroupTypeNV = VkRayTracingShaderGroupTypeNV
-- No documentation found for TopLevel "WriteDescriptorSetAccelerationStructureNV"
data WriteDescriptorSetAccelerationStructureNV = WriteDescriptorSetAccelerationStructureNV
  { -- Univalued Member elided
  -- No documentation found for Nested "WriteDescriptorSetAccelerationStructureNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "WriteDescriptorSetAccelerationStructureNV" "pAccelerationStructures"
  vkPAccelerationStructures :: Vector AccelerationStructureNV
  }
  deriving (Show, Eq)
withCStructWriteDescriptorSetAccelerationStructureNV :: WriteDescriptorSetAccelerationStructureNV -> (VkWriteDescriptorSetAccelerationStructureNV -> IO a) -> IO a
withCStructWriteDescriptorSetAccelerationStructureNV from cont = withVec (&) (vkPAccelerationStructures (from :: WriteDescriptorSetAccelerationStructureNV)) (\pAccelerationStructures -> maybeWith withSomeVkStruct (vkPNext (from :: WriteDescriptorSetAccelerationStructureNV)) (\pPNext -> cont (VkWriteDescriptorSetAccelerationStructureNV VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV pPNext (fromIntegral (Data.Vector.length (vkPAccelerationStructures (from :: WriteDescriptorSetAccelerationStructureNV)))) pAccelerationStructures)))
fromCStructWriteDescriptorSetAccelerationStructureNV :: VkWriteDescriptorSetAccelerationStructureNV -> IO WriteDescriptorSetAccelerationStructureNV
fromCStructWriteDescriptorSetAccelerationStructureNV c = WriteDescriptorSetAccelerationStructureNV <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWriteDescriptorSetAccelerationStructureNV)))
                                                                                                   -- Length valued member elided
                                                                                                   <*> (Data.Vector.generateM (fromIntegral (vkAccelerationStructureCount (c :: VkWriteDescriptorSetAccelerationStructureNV))) (peekElemOff (vkPAccelerationStructures (c :: VkWriteDescriptorSetAccelerationStructureNV))))

-- | Wrapper for vkBindAccelerationStructureMemoryNV
bindAccelerationStructureMemoryNV :: Device ->  Vector BindAccelerationStructureMemoryInfoNV ->  IO ()
bindAccelerationStructureMemoryNV = \(Device device commandTable) -> \bindInfos -> withVec withCStructBindAccelerationStructureMemoryInfoNV bindInfos (\pBindInfos -> Graphics.Vulkan.C.Dynamic.bindAccelerationStructureMemoryNV commandTable device (fromIntegral $ Data.Vector.length bindInfos) pBindInfos >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))

-- | Wrapper for vkCmdBuildAccelerationStructureNV
cmdBuildAccelerationStructureNV :: CommandBuffer ->  AccelerationStructureInfoNV ->  Buffer ->  DeviceSize ->  Bool ->  AccelerationStructureNV ->  AccelerationStructureNV ->  Buffer ->  DeviceSize ->  IO ()
cmdBuildAccelerationStructureNV = \(CommandBuffer commandBuffer commandTable) -> \info -> \instanceData -> \instanceOffset -> \update -> \dst -> \src -> \scratch -> \scratchOffset -> (\a -> withCStructAccelerationStructureInfoNV a . flip with) info (\pInfo -> Graphics.Vulkan.C.Dynamic.cmdBuildAccelerationStructureNV commandTable commandBuffer pInfo instanceData instanceOffset (boolToBool32 update) dst src scratch scratchOffset *> (pure ()))

-- | Wrapper for vkCmdCopyAccelerationStructureNV
cmdCopyAccelerationStructureNV :: CommandBuffer ->  AccelerationStructureNV ->  AccelerationStructureNV ->  CopyAccelerationStructureModeNV ->  IO ()
cmdCopyAccelerationStructureNV = \(CommandBuffer commandBuffer commandTable) -> \dst -> \src -> \mode -> Graphics.Vulkan.C.Dynamic.cmdCopyAccelerationStructureNV commandTable commandBuffer dst src mode *> (pure ())

-- | Wrapper for vkCmdTraceRaysNV
cmdTraceRaysNV :: CommandBuffer ->  Buffer ->  DeviceSize ->  Buffer ->  DeviceSize ->  DeviceSize ->  Buffer ->  DeviceSize ->  DeviceSize ->  Buffer ->  DeviceSize ->  DeviceSize ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdTraceRaysNV = \(CommandBuffer commandBuffer commandTable) -> \raygenShaderBindingTableBuffer -> \raygenShaderBindingOffset -> \missShaderBindingTableBuffer -> \missShaderBindingOffset -> \missShaderBindingStride -> \hitShaderBindingTableBuffer -> \hitShaderBindingOffset -> \hitShaderBindingStride -> \callableShaderBindingTableBuffer -> \callableShaderBindingOffset -> \callableShaderBindingStride -> \width -> \height -> \depth -> Graphics.Vulkan.C.Dynamic.cmdTraceRaysNV commandTable commandBuffer raygenShaderBindingTableBuffer raygenShaderBindingOffset missShaderBindingTableBuffer missShaderBindingOffset missShaderBindingStride hitShaderBindingTableBuffer hitShaderBindingOffset hitShaderBindingStride callableShaderBindingTableBuffer callableShaderBindingOffset callableShaderBindingStride width height depth *> (pure ())

-- | Wrapper for vkCmdWriteAccelerationStructuresPropertiesNV
cmdWriteAccelerationStructuresPropertiesNV :: CommandBuffer ->  Vector AccelerationStructureNV ->  QueryType ->  QueryPool ->  Word32 ->  IO ()
cmdWriteAccelerationStructuresPropertiesNV = \(CommandBuffer commandBuffer commandTable) -> \accelerationStructures -> \queryType -> \queryPool -> \firstQuery -> withVec (&) accelerationStructures (\pAccelerationStructures -> Graphics.Vulkan.C.Dynamic.cmdWriteAccelerationStructuresPropertiesNV commandTable commandBuffer (fromIntegral $ Data.Vector.length accelerationStructures) pAccelerationStructures queryType queryPool firstQuery *> (pure ()))

-- | Wrapper for vkCompileDeferredNV
compileDeferredNV :: Device ->  Pipeline ->  Word32 ->  IO ()
compileDeferredNV = \(Device device commandTable) -> \pipeline -> \shader -> Graphics.Vulkan.C.Dynamic.compileDeferredNV commandTable device pipeline shader >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))

-- | Wrapper for vkCreateAccelerationStructureNV
createAccelerationStructureNV :: Device ->  AccelerationStructureCreateInfoNV ->  Maybe AllocationCallbacks ->  IO (AccelerationStructureNV)
createAccelerationStructureNV = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pAccelerationStructure -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructAccelerationStructureCreateInfoNV a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createAccelerationStructureNV commandTable device pCreateInfo pAllocator pAccelerationStructure >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pAccelerationStructure)))))

-- | Wrapper for vkCreateRayTracingPipelinesNV
createRayTracingPipelinesNV :: Device ->  PipelineCache ->  Vector RayTracingPipelineCreateInfoNV ->  Maybe AllocationCallbacks ->  IO (Pipeline)
createRayTracingPipelinesNV = \(Device device commandTable) -> \pipelineCache -> \createInfos -> \allocator -> alloca (\pPipelines -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> withVec withCStructRayTracingPipelineCreateInfoNV createInfos (\pCreateInfos -> Graphics.Vulkan.C.Dynamic.createRayTracingPipelinesNV commandTable device pipelineCache (fromIntegral $ Data.Vector.length createInfos) pCreateInfos pAllocator pPipelines >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pPipelines)))))

-- | Wrapper for vkDestroyAccelerationStructureNV
destroyAccelerationStructureNV :: Device ->  AccelerationStructureNV ->  Maybe AllocationCallbacks ->  IO ()
destroyAccelerationStructureNV = \(Device device commandTable) -> \accelerationStructure -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyAccelerationStructureNV commandTable device accelerationStructure pAllocator *> (pure ()))

-- | Wrapper for vkGetAccelerationStructureHandleNV
getAccelerationStructureHandleNV :: Device ->  AccelerationStructureNV ->  CSize ->  IO (ByteString)
getAccelerationStructureHandleNV = \(Device device commandTable) -> \accelerationStructure -> \dataSize -> allocaArray (fromIntegral dataSize) (\pData -> Graphics.Vulkan.C.Dynamic.getAccelerationStructureHandleNV commandTable device accelerationStructure dataSize (castPtr pData) >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((packCStringLen (pData, (fromIntegral dataSize))))))


-- | Wrapper for vkGetRayTracingShaderGroupHandlesNV
getRayTracingShaderGroupHandlesNV :: Device ->  Pipeline ->  Word32 ->  Word32 ->  CSize ->  IO (ByteString)
getRayTracingShaderGroupHandlesNV = \(Device device commandTable) -> \pipeline -> \firstGroup -> \groupCount -> \dataSize -> allocaArray (fromIntegral dataSize) (\pData -> Graphics.Vulkan.C.Dynamic.getRayTracingShaderGroupHandlesNV commandTable device pipeline firstGroup groupCount dataSize (castPtr pData) >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((packCStringLen (pData, (fromIntegral dataSize))))))
