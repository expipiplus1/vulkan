{-# language CPP #-}
module Vulkan.Core10.FundamentalTypes  ( Extent2D
                                       , Extent3D
                                       , Offset2D
                                       , Offset3D
                                       , Rect2D
                                       , Bool32
                                       , DeviceAddress
                                       , DeviceSize
                                       ) where

import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data Extent2D

instance ToCStruct Extent2D
instance Show Extent2D

instance FromCStruct Extent2D


data Extent3D

instance ToCStruct Extent3D
instance Show Extent3D

instance FromCStruct Extent3D


data Offset2D

instance ToCStruct Offset2D
instance Show Offset2D

instance FromCStruct Offset2D


data Offset3D

instance ToCStruct Offset3D
instance Show Offset3D

instance FromCStruct Offset3D


data Rect2D

instance ToCStruct Rect2D
instance Show Rect2D

instance FromCStruct Rect2D


data Bool32


-- | VkDeviceAddress - Vulkan device address type
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.BindIndexBufferIndirectCommandNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.BindVertexBufferIndirectCommandNV',
-- 'Vulkan.Extensions.VK_EXT_buffer_device_address.BufferDeviceAddressCreateInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.DeviceOrHostAddressConstKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.DeviceOrHostAddressKHR',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewAddressPropertiesNVX'
type DeviceAddress = Word64


-- | VkDeviceSize - Vulkan device memory size and offsets
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureCreateInfoNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureGeometryAabbsDataKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureGeometryTrianglesDataKHR',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferPropertiesANDROID',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.BindAccelerationStructureMemoryInfoKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.BufferCopy',
-- 'Vulkan.Core10.Buffer.BufferCreateInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.BufferImageCopy',
-- 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
-- 'Vulkan.Core10.BufferView.BufferViewCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.ConditionalRenderingBeginInfoEXT',
-- 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryAABBNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Vulkan.Core10.DeviceInitialization.ImageFormatProperties',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewAddressPropertiesNVX',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsStreamNV',
-- 'Vulkan.Core10.Memory.MappedMemoryRange',
-- 'Vulkan.Core10.Memory.MemoryAllocateInfo',
-- 'Vulkan.Core10.DeviceInitialization.MemoryHeap',
-- 'Vulkan.Core10.MemoryManagement.MemoryRequirements',
-- 'Vulkan.Extensions.VK_EXT_external_memory_host.PhysicalDeviceExternalMemoryHostPropertiesEXT',
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.PhysicalDeviceMaintenance3Properties',
-- 'Vulkan.Extensions.VK_EXT_memory_budget.PhysicalDeviceMemoryBudgetPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_robustness2.PhysicalDeviceRobustness2PropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'Vulkan.Core12.PhysicalDeviceVulkan11Properties',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryRequirements',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseMemoryBind',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.StridedBufferRegionKHR',
-- 'Vulkan.Core10.Image.SubresourceLayout',
-- 'Vulkan.Core10.MemoryManagement.bindBufferMemory',
-- 'Vulkan.Core10.MemoryManagement.bindImageMemory',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.cmdBuildAccelerationStructureIndirectKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount',
-- 'Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndexedIndirectCountAMD',
-- 'Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCountKHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdDrawIndirectByteCountEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount',
-- 'Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndirectCountAMD',
-- 'Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndirectCountKHR',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdFillBuffer',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.cmdTraceRaysIndirectKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdUpdateBuffer',
-- 'Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarkerAMD',
-- 'Vulkan.Core10.Memory.getDeviceMemoryCommitment',
-- 'Vulkan.Core10.Query.getQueryPoolResults',
-- 'Vulkan.Core10.Memory.mapMemory'
type DeviceSize = Word64

