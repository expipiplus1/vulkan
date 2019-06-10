{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( VkAccelerationStructureCreateInfoNV
  , VkAccelerationStructureInfoNV
  , VkAccelerationStructureMemoryRequirementsInfoNV
  , VkAccelerationStructureMemoryRequirementsTypeNV
  , VkAccelerationStructureNV
  , VkAccelerationStructureTypeNV
  , VkBindAccelerationStructureMemoryInfoNV
  , VkBuildAccelerationStructureFlagBitsNV
  , VkBuildAccelerationStructureFlagsNV
  , VkCopyAccelerationStructureModeNV
  , VkGeometryAABBNV
  , VkGeometryDataNV
  , VkGeometryFlagBitsNV
  , VkGeometryFlagsNV
  , VkGeometryInstanceFlagBitsNV
  , VkGeometryInstanceFlagsNV
  , VkGeometryNV
  , VkGeometryTrianglesNV
  , VkGeometryTypeNV
  , VkPhysicalDeviceRayTracingPropertiesNV
  , VkRayTracingPipelineCreateInfoNV
  , VkRayTracingShaderGroupCreateInfoNV
  , VkRayTracingShaderGroupTypeNV
  , VkWriteDescriptorSetAccelerationStructureNV
  , FN_vkBindAccelerationStructureMemoryNV
  , PFN_vkBindAccelerationStructureMemoryNV
  , FN_vkCmdBuildAccelerationStructureNV
  , PFN_vkCmdBuildAccelerationStructureNV
  , FN_vkCmdCopyAccelerationStructureNV
  , PFN_vkCmdCopyAccelerationStructureNV
  , FN_vkCmdTraceRaysNV
  , PFN_vkCmdTraceRaysNV
  , FN_vkCmdWriteAccelerationStructuresPropertiesNV
  , PFN_vkCmdWriteAccelerationStructuresPropertiesNV
  , FN_vkCompileDeferredNV
  , PFN_vkCompileDeferredNV
  , FN_vkCreateAccelerationStructureNV
  , PFN_vkCreateAccelerationStructureNV
  , FN_vkCreateRayTracingPipelinesNV
  , PFN_vkCreateRayTracingPipelinesNV
  , FN_vkDestroyAccelerationStructureNV
  , PFN_vkDestroyAccelerationStructureNV
  , FN_vkGetAccelerationStructureHandleNV
  , PFN_vkGetAccelerationStructureHandleNV
  , FN_vkGetAccelerationStructureMemoryRequirementsNV
  , PFN_vkGetAccelerationStructureMemoryRequirementsNV
  , FN_vkGetRayTracingShaderGroupHandlesNV
  , PFN_vkGetRayTracingShaderGroupHandlesNV
  ) where

import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkBool32
  , VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  , VkDeviceSize
  )
import {-# source #-} Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Pipeline
  ( VkPipeline
  )
import {-# source #-} Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCache
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Query
  ( VkQueryPool
  , VkQueryType
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( VkMemoryRequirements2KHR
  )


data VkAccelerationStructureCreateInfoNV

data VkAccelerationStructureInfoNV

data VkAccelerationStructureMemoryRequirementsInfoNV

data VkAccelerationStructureMemoryRequirementsTypeNV

-- | Dummy data to tag the 'Ptr' with
data VkAccelerationStructureNV_T
-- No documentation found for TopLevel "VkAccelerationStructureNV"
type VkAccelerationStructureNV = Ptr VkAccelerationStructureNV_T

data VkAccelerationStructureTypeNV

data VkBindAccelerationStructureMemoryInfoNV

data VkBuildAccelerationStructureFlagBitsNV

-- No documentation found for TopLevel "VkBuildAccelerationStructureFlagsNV"
type VkBuildAccelerationStructureFlagsNV = VkBuildAccelerationStructureFlagBitsNV

data VkCopyAccelerationStructureModeNV

data VkGeometryAABBNV

data VkGeometryDataNV

data VkGeometryFlagBitsNV

-- No documentation found for TopLevel "VkGeometryFlagsNV"
type VkGeometryFlagsNV = VkGeometryFlagBitsNV

data VkGeometryInstanceFlagBitsNV

-- No documentation found for TopLevel "VkGeometryInstanceFlagsNV"
type VkGeometryInstanceFlagsNV = VkGeometryInstanceFlagBitsNV

data VkGeometryNV

data VkGeometryTrianglesNV

data VkGeometryTypeNV

data VkPhysicalDeviceRayTracingPropertiesNV

data VkRayTracingPipelineCreateInfoNV

data VkRayTracingShaderGroupCreateInfoNV

data VkRayTracingShaderGroupTypeNV

data VkWriteDescriptorSetAccelerationStructureNV

type FN_vkBindAccelerationStructureMemoryNV = ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindAccelerationStructureMemoryInfoNV) -> IO VkResult
type PFN_vkBindAccelerationStructureMemoryNV = FunPtr FN_vkBindAccelerationStructureMemoryNV

type FN_vkCmdBuildAccelerationStructureNV = ("commandBuffer" ::: VkCommandBuffer) -> ("pInfo" ::: Ptr VkAccelerationStructureInfoNV) -> ("instanceData" ::: VkBuffer) -> ("instanceOffset" ::: VkDeviceSize) -> ("update" ::: VkBool32) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("scratch" ::: VkBuffer) -> ("scratchOffset" ::: VkDeviceSize) -> IO ()
type PFN_vkCmdBuildAccelerationStructureNV = FunPtr FN_vkCmdBuildAccelerationStructureNV

type FN_vkCmdCopyAccelerationStructureNV = ("commandBuffer" ::: VkCommandBuffer) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("mode" ::: VkCopyAccelerationStructureModeNV) -> IO ()
type PFN_vkCmdCopyAccelerationStructureNV = FunPtr FN_vkCmdCopyAccelerationStructureNV

type FN_vkCmdTraceRaysNV = ("commandBuffer" ::: VkCommandBuffer) -> ("raygenShaderBindingTableBuffer" ::: VkBuffer) -> ("raygenShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingTableBuffer" ::: VkBuffer) -> ("missShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingStride" ::: VkDeviceSize) -> ("hitShaderBindingTableBuffer" ::: VkBuffer) -> ("hitShaderBindingOffset" ::: VkDeviceSize) -> ("hitShaderBindingStride" ::: VkDeviceSize) -> ("callableShaderBindingTableBuffer" ::: VkBuffer) -> ("callableShaderBindingOffset" ::: VkDeviceSize) -> ("callableShaderBindingStride" ::: VkDeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ()
type PFN_vkCmdTraceRaysNV = FunPtr FN_vkCmdTraceRaysNV

type FN_vkCmdWriteAccelerationStructuresPropertiesNV = ("commandBuffer" ::: VkCommandBuffer) -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr VkAccelerationStructureNV) -> ("queryType" ::: VkQueryType) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> IO ()
type PFN_vkCmdWriteAccelerationStructuresPropertiesNV = FunPtr FN_vkCmdWriteAccelerationStructuresPropertiesNV

type FN_vkCompileDeferredNV = ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shader" ::: Word32) -> IO VkResult
type PFN_vkCompileDeferredNV = FunPtr FN_vkCompileDeferredNV

type FN_vkCreateAccelerationStructureNV = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkAccelerationStructureCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr VkAccelerationStructureNV) -> IO VkResult
type PFN_vkCreateAccelerationStructureNV = FunPtr FN_vkCreateAccelerationStructureNV

type FN_vkCreateRayTracingPipelinesNV = ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkRayTracingPipelineCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult
type PFN_vkCreateRayTracingPipelinesNV = FunPtr FN_vkCreateRayTracingPipelinesNV

type FN_vkDestroyAccelerationStructureNV = ("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyAccelerationStructureNV = FunPtr FN_vkDestroyAccelerationStructureNV

type FN_vkGetAccelerationStructureHandleNV = ("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
type PFN_vkGetAccelerationStructureHandleNV = FunPtr FN_vkGetAccelerationStructureHandleNV

type FN_vkGetAccelerationStructureMemoryRequirementsNV = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkAccelerationStructureMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2KHR) -> IO ()
type PFN_vkGetAccelerationStructureMemoryRequirementsNV = FunPtr FN_vkGetAccelerationStructureMemoryRequirementsNV

type FN_vkGetRayTracingShaderGroupHandlesNV = ("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
type PFN_vkGetRayTracingShaderGroupHandlesNV = FunPtr FN_vkGetRayTracingShaderGroupHandlesNV
