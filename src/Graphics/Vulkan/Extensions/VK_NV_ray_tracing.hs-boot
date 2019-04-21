{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_NV_ray_tracing
  ( AccelerationStructureMemoryRequirementsTypeNV
  , AccelerationStructureNV
  , AccelerationStructureTypeNV
  , BuildAccelerationStructureFlagBitsNV
  , BuildAccelerationStructureFlagsNV
  , CopyAccelerationStructureModeNV
  , GeometryFlagBitsNV
  , GeometryFlagsNV
  , GeometryInstanceFlagBitsNV
  , GeometryInstanceFlagsNV
  , GeometryTypeNV
  , RayTracingShaderGroupTypeNV
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( VkAccelerationStructureMemoryRequirementsTypeNV
  , VkAccelerationStructureTypeNV
  , VkBuildAccelerationStructureFlagBitsNV
  , VkCopyAccelerationStructureModeNV
  , VkGeometryFlagBitsNV
  , VkGeometryInstanceFlagBitsNV
  , VkGeometryTypeNV
  , VkRayTracingShaderGroupTypeNV
  , VkAccelerationStructureNV
  )


-- | VkAccelerationStructureMemoryRequirementsTypeNV - Acceleration structure
-- memory requirement type
--
-- = See Also
--
-- No cross-references are available
type AccelerationStructureMemoryRequirementsTypeNV = VkAccelerationStructureMemoryRequirementsTypeNV

-- | VkAccelerationStructureNV - Opaque handle to an acceleration structure
-- object
--
-- = See Also
--
-- No cross-references are available
type AccelerationStructureNV = VkAccelerationStructureNV

-- | VkAccelerationStructureTypeNV - Type of acceleration structure
--
-- = See Also
--
-- No cross-references are available
type AccelerationStructureTypeNV = VkAccelerationStructureTypeNV

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
-- No cross-references are available
type BuildAccelerationStructureFlagBitsNV = VkBuildAccelerationStructureFlagBitsNV

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
-- No cross-references are available
type BuildAccelerationStructureFlagsNV = BuildAccelerationStructureFlagBitsNV

-- | VkCopyAccelerationStructureModeNV - Acceleration structure copy mode
--
-- = See Also
--
-- No cross-references are available
type CopyAccelerationStructureModeNV = VkCopyAccelerationStructureModeNV

-- | VkGeometryFlagBitsNV - Bitmask specifying additional parameters for a
-- geometry
--
-- = See Also
--
-- No cross-references are available
type GeometryFlagBitsNV = VkGeometryFlagBitsNV

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
-- No cross-references are available
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
-- No cross-references are available
type GeometryInstanceFlagBitsNV = VkGeometryInstanceFlagBitsNV

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
-- No cross-references are available
type GeometryInstanceFlagsNV = GeometryInstanceFlagBitsNV

-- | VkGeometryTypeNV - Enum specifying which type of geometry is provided
--
-- = See Also
--
-- No cross-references are available
type GeometryTypeNV = VkGeometryTypeNV

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
-- No cross-references are available
type RayTracingShaderGroupTypeNV = VkRayTracingShaderGroupTypeNV
