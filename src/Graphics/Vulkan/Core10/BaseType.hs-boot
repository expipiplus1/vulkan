{-# language CPP #-}
module Graphics.Vulkan.Core10.BaseType  ( Bool32
                                        , DeviceAddress
                                        , DeviceSize
                                        ) where

import Data.Word (Word64)

data Bool32


-- | VkDeviceAddress - Vulkan device address type
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.BindIndexBufferIndirectCommandNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.BindVertexBufferIndirectCommandNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address.BufferDeviceAddressCreateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.DeviceOrHostAddressConstKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.DeviceOrHostAddressKHR',
-- 'Graphics.Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewAddressPropertiesNVX'
type DeviceAddress = Word64


-- | VkDeviceSize - Vulkan device memory size and offsets
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureGeometryAabbsDataKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureGeometryTrianglesDataKHR',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferPropertiesANDROID',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.BindAccelerationStructureMemoryInfoKHR',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.BufferCopy',
-- 'Graphics.Vulkan.Core10.Buffer.BufferCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.BufferImageCopy',
-- 'Graphics.Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
-- 'Graphics.Vulkan.Core10.BufferView.BufferViewCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering.ConditionalRenderingBeginInfoEXT',
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorBufferInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.GeometryAABBNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.ImageFormatProperties',
-- 'Graphics.Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewAddressPropertiesNVX',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsStreamNV',
-- 'Graphics.Vulkan.Core10.Memory.MappedMemoryRange',
-- 'Graphics.Vulkan.Core10.Memory.MemoryAllocateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.MemoryHeap',
-- 'Graphics.Vulkan.Core10.MemoryManagement.MemoryRequirements',
-- 'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.PhysicalDeviceExternalMemoryHostPropertiesEXT',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.PhysicalDeviceMaintenance3Properties',
-- 'Graphics.Vulkan.Extensions.VK_EXT_memory_budget.PhysicalDeviceMemoryBudgetPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_robustness2.PhysicalDeviceRobustness2PropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'Graphics.Vulkan.Core12.PhysicalDeviceVulkan11Properties',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryRequirements',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseMemoryBind',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.StridedBufferRegionKHR',
-- 'Graphics.Vulkan.Core10.Image.SubresourceLayout',
-- 'Graphics.Vulkan.Core10.MemoryManagement.bindBufferMemory',
-- 'Graphics.Vulkan.Core10.MemoryManagement.bindImageMemory',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.cmdBuildAccelerationStructureIndirectKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndexedIndirectCountAMD',
-- 'Graphics.Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCountKHR',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdDrawIndirectByteCountEXT',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndirectCountAMD',
-- 'Graphics.Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndirectCountKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdFillBuffer',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.cmdTraceRaysIndirectKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdUpdateBuffer',
-- 'Graphics.Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarkerAMD',
-- 'Graphics.Vulkan.Core10.Memory.getDeviceMemoryCommitment',
-- 'Graphics.Vulkan.Core10.Query.getQueryPoolResults',
-- 'Graphics.Vulkan.Core10.Memory.mapMemory'
type DeviceSize = Word64

